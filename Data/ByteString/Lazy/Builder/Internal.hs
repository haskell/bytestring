{-# LANGUAGE ScopedTypeVariables, CPP, BangPatterns, Rank2Types #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Copyright : (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Core types and functions for the 'Builder' monoid and its generalization,
-- the 'Put' monad.
--
-- The design of the 'Builder' monoid is optimized such that
--
--   1. buffers of arbitrary size can be filled as efficiently as possible and
--
--   2. sequencing of 'Builder's is as cheap as possible.
--
-- We achieve (1) by completely handing over control over writing to the buffer
-- to the 'BuildStep' implementing the 'Builder'. This 'BuildStep' is just told
-- the start and the end of the buffer (represented as a 'BufferRange'). Then,
-- the 'BuildStep' can write to as big a prefix of this 'BufferRange' in any
-- way it desires. If the 'BuildStep' is done, the 'BufferRange' is full, or a
-- long sequence of bytes should be inserted directly, then the 'BuildStep'
-- signals this to its caller using a 'BuildSignal'.
--
-- We achieve (2) by requiring that every 'Builder' is implemented by a
-- 'BuildStep' that takes a continuation 'BuildStep', which it calls with the
-- updated 'BufferRange' after it is done. Therefore, only two pointers have
-- to be passed in a function call to implement concatentation of 'Builder's.
-- Moreover, many 'Builder's are completely inlined, which enables the compiler
-- to sequence them without a function call and with no boxing at all.
--
-- This design gives the implementation of a 'Builder' full access to the 'IO'
-- monad. Therefore, utmost care has to be taken to not overwrite anything
-- outside the given 'BufferRange's. Moreover, further care has to be taken to
-- ensure that 'Builder's and 'Put's are referentially transparent. See the
-- comments of the 'builder' and 'put' functions for further information.
-- Note that there are /no safety belts/ at all, when implementing a 'Builder'
-- using an 'IO' action: you are writing code that might enable the next
-- buffer-overlow attack on a Haskell server!
--
module Data.ByteString.Lazy.Builder.Internal (

  -- * Build signals and steps
    BufferRange(..)
  , LazyByteStringC

  , BuildSignal
  , BuildStep

  , done
  , bufferFull
  , insertChunks

  , fillWithBuildStep

  -- * The Builder monoid
  , Builder
  , builder
  , runBuilder
  , runBuilderWith

  -- ** Primitive combinators
  , empty
  , append
  , flush
  , ensureFree

  , byteStringCopy
  , byteStringInsert
  , byteStringThreshold

  , lazyByteStringCopy
  , lazyByteStringInsert
  , lazyByteStringThreshold

  , lazyByteStringC

  , maximalCopySize
  , byteString
  , lazyByteString

  -- ** Execution strategies
  , toLazyByteStringWith
  , AllocationStrategy
  , safeStrategy
  , untrimmedStrategy
  , L.smallChunkSize
  , L.defaultChunkSize

  -- * The Put monad
  , Put
  , put
  , runPut
  , hPut

  -- ** Streams of chunks interleaved with IO
  , ChunkIOStream(..)
  , buildStepToCIOS
  , ciosToLazyByteString

  -- ** Conversion to and from Builders
  , putBuilder
  , fromPut

  -- ** Lifting IO actions
  -- , putLiftIO

) where

-- TODO: Check if we still require conditional compilation for Applicative

-- #ifdef APPLICATIVE_IN_BASE
import Control.Applicative (Applicative(..))
-- #endif
import Control.Applicative ((<$>))

import Data.Monoid
import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

#if __GLASGOW_HASKELL__ >= 611
import GHC.IO.Buffer (Buffer(..), newByteBuffer)
import GHC.IO.Handle.Internals (wantWritableHandle, flushWriteBuffer)
import GHC.IO.Handle.Types (Handle__, haByteBuffer, haBufferMode)
import System.IO (hFlush, BufferMode(..))
import Data.IORef
#else
import qualified Data.ByteString.Lazy as L
#endif
import System.IO (Handle)

#if MIN_VERSION_base(4,4,0)
import Foreign hiding (unsafePerformIO, unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import System.IO.Unsafe (unsafePerformIO)
#else
import Foreign
#endif


type LazyByteStringC = L.ByteString -> L.ByteString

-- | A range of bytes in a buffer represented by the pointer to the first byte
-- of the range and the pointer to the first byte /after/ the range.
data BufferRange = BufferRange {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
                               {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range


------------------------------------------------------------------------------
-- Build signals
------------------------------------------------------------------------------

-- | 'BuildStep's may assume that they are called at most once. However,
-- they must not execute any function that may rise an async. exception,
-- as this would invalidate the code of 'hPut' below.
type BuildStep a = BufferRange -> IO (BuildSignal a)

-- | 'BuildSignal's abstract signals to the caller of a 'BuildStep'. There are
-- exactly three signals: 'done', 'bufferFull', and 'insertChunks'.
data BuildSignal a =
    Done {-# UNPACK #-} !(Ptr Word8) a
  | BufferFull
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !(Ptr Word8)
                     !(BuildStep a)
  | InsertChunks
      {-# UNPACK #-} !(Ptr Word8)
      {-# UNPACK #-} !Int64                   -- size of bytes in continuation
                      LazyByteStringC
                     !(BuildStep a)

-- | Signal that the current 'BuildStep' is done and has computed a value.
{-# INLINE done #-}
done :: Ptr Word8      -- ^ Next free byte in current 'BufferRange'
     -> a              -- ^ Computed value
     -> BuildSignal a
done = Done

-- | Signal that the current buffer is full.
{-# INLINE bufferFull #-}
bufferFull :: Int
           -- ^ Minimal size of next 'BufferRange'.
           -> Ptr Word8
           -- ^ Next free byte in current 'BufferRange'.
           -> BuildStep a
           -- ^ 'BuildStep' to run on the next 'BufferRange'. This 'BuildStep'
           -- may assume that it is called with a 'BufferRange' of at least the
           -- required minimal size; i.e., the caller of this 'BuildStep' must
           -- guarantee this.
           -> BuildSignal a
bufferFull = BufferFull

-- TODO: Decide whether we should inline the bytestring constructor.
-- Therefore, making builders independent of strict bytestrings.

-- | Signal that several chunks should be inserted directly.
{-# INLINE insertChunks #-}
insertChunks :: Ptr Word8
            -- ^ Next free byte in current 'BufferRange'
            -> Int64
            -- ^ Number of bytes in 'L.ByteString' continuation.
            -> (L.ByteString -> L.ByteString)
            -- ^ Chunks to insert.
            -> BuildStep a
            -- ^ 'BuildStep' to run on next 'BufferRange'
            -> BuildSignal a
insertChunks = InsertChunks

-- | Fill a 'BufferRange' using a 'BuildStep'.
{-# INLINE fillWithBuildStep #-}
fillWithBuildStep
    :: BuildStep a
    -- ^ Build step to use for filling the 'BufferRange'.
    -> (Ptr Word8 -> a -> IO b)
    -- ^ Handling the 'done' signal
    -> (Ptr Word8 -> Int -> BuildStep a -> IO b)
    -- ^ Handling the 'bufferFull' signal
    -> (Ptr Word8 -> Int64 -> LazyByteStringC -> BuildStep a -> IO b)
    -- ^ Handling the 'insertChunks' signal
    -> BufferRange
    -- ^ Buffer range to fill.
    -> IO b
    -- ^ Value computed by filling this 'BufferRange'.
fillWithBuildStep step fDone fFull fChunk !br = do
    signal <- step br
    case signal of
        Done op x                         -> fDone op x
        BufferFull minSize op nextStep    -> fFull op minSize nextStep
        InsertChunks op len lbsC nextStep -> fChunk op len lbsC nextStep



------------------------------------------------------------------------------
-- The 'Builder' monoid
------------------------------------------------------------------------------

-- | 'Builder's denote sequences of bytes.
-- They are 'Monoid's where
--   'mempty' is the zero-length sequence and
--   'mappend' is concatenation, which runs in /O(1)/.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)

-- | Construct a 'Builder'. In contrast to 'BuildStep's, 'Builder's are
-- referentially transparent.
{-# INLINE builder #-}
builder :: (forall r. BuildStep r -> BuildStep r)
        -- ^ A function that fills a 'BufferRange', calls the continuation with
        -- the updated 'BufferRange' once its done, and signals its caller how
        -- to proceed using 'done', 'bufferFull', or 'insertChunk'.
        --
        -- This function must be referentially transparent; i.e., calling it
        -- multiple times must result in the same sequence of bytes being
        -- written. If you need mutable state, then you must allocate it newly
        -- upon each call of this function. Moroever, this function must call
        -- the continuation once its done. Otherwise, concatenation of
        -- 'Builder's does not work. Finally, this function must write to all
        -- bytes that it claims it has written. Otherwise, the resulting
        -- 'Builder' is not guaranteed to be referentially transparent and
        -- sensitive data might leak.
        -> Builder
builder = Builder

-- | Run a 'Builder'.
{-# INLINE runBuilder #-}
runBuilder :: Builder      -- ^ 'Builder' to run
           -> BuildStep () -- ^ 'BuildStep' that writes the byte stream of this
                           -- 'Builder' and signals 'done' upon completion.
runBuilder (Builder b) = b $ \(BufferRange op _) -> return $ done op ()

-- | Run a 'Builder'.
{-# INLINE runBuilderWith #-}
runBuilderWith :: Builder      -- ^ 'Builder' to run
               -> BuildStep a -- ^ Continuation 'BuildStep'
               -> BuildStep a
runBuilderWith (Builder b) = b

-- | The 'Builder' denoting a zero-length sequence of bytes. This function is
-- only exported for use in rewriting rules. Use 'mempty' otherwise.
{-# INLINE[1] empty #-}
empty :: Builder
empty = Builder id

-- | Concatenate two 'Builder's. This function is only exported for use in rewriting
-- rules. Use 'mappend' otherwise.
{-# INLINE[1] append #-}
append :: Builder -> Builder -> Builder
append (Builder b1) (Builder b2) = Builder $ b1 . b2

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty = empty
  {-# INLINE mappend #-}
  mappend = append
  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | Flush the current buffer. This introduces a chunk boundary.
--
{-# INLINE flush #-}
flush :: Builder
flush = builder step
  where
    step k !(BufferRange op _) = return $ insertChunks op 0 id k


------------------------------------------------------------------------------
-- Put
------------------------------------------------------------------------------

-- | A 'Put' action denotes a computation of a value that writes a stream of
-- bytes as a side-effect. 'Put's are strict in their side-effect; i.e., the
-- stream of bytes will always be written before the computed value is
-- returned.
--
-- 'Put's are a generalization of 'Builder's. They are used when values need to
-- be returned during the computation of a stream of bytes. For example, when
-- performing a block-based encoding of 'S.ByteString's like Base64 encoding,
-- there might be a left-over partial block. Using the 'Put' monad, this
-- partial block can be returned after the complete blocks have been encoded.
-- Then, in a later step when more input is known, this partial block can be
-- completed and also encoded.
--
-- @Put ()@ actions are isomorphic to 'Builder's. The functions 'putBuilder'
-- and 'fromPut' convert between these two types. Where possible, you should
-- use 'Builder's, as they are slightly cheaper than 'Put's because they do not
-- carry a computed value.
newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }

-- | Construct a 'Put' action. In contrast to 'BuildStep's, 'Put's are
-- referentially transparent in the sense that sequencing the same 'Put'
-- multiple times yields every time the same value with the same side-effect.
{-# INLINE put #-}
put :: (forall r. (a -> BuildStep r) -> BuildStep r)
       -- ^ A function that fills a 'BufferRange', calls the continuation with
       -- the updated 'BufferRange' and its computed value once its done, and
       -- signals its caller how to proceed using 'done', 'bufferFull', or
       -- 'insertChunk'.
       --
       -- This function must be referentially transparent; i.e., calling it
       -- multiple times must result in the same sequence of bytes being
       -- written and the same value being computed. If you need mutable state,
       -- then you must allocate it newly upon each call of this function.
       -- Moroever, this function must call the continuation once its done.
       -- Otherwise, monadic sequencing of 'Put's does not work. Finally, this
       -- function must write to all bytes that it claims it has written.
       -- Otherwise, the resulting 'Put' is not guaranteed to be referentially
       -- transparent and sensitive data might leak.
       -> Put a
put = Put

-- | Run a 'Put'.
{-# INLINE runPut #-}
runPut :: Put a       -- ^ Put to run
       -> BuildStep a -- ^ 'BuildStep' that first writes the byte stream of
                      -- this 'Put' and then yields the computed value using
                      -- the 'done' signal.
runPut (Put p) = p $ \x (BufferRange op _) -> return $ Done op x

instance Functor Put where
  fmap f p = Put $ \k -> unPut p (\x -> k (f x))
  {-# INLINE fmap #-}

-- #ifdef APPLICATIVE_IN_BASE
instance Applicative Put where
  {-# INLINE pure #-}
  pure x = Put $ \k -> k x
  {-# INLINE (<*>) #-}
  Put f <*> Put a = Put $ \k -> f (\f' -> a (\a' -> k (f' a')))
  {-# INLINE (<*) #-}
  Put a <* Put b = Put $ \k -> a (\a' -> b (\_ -> k a'))
  {-# INLINE (*>) #-}
  Put a *> Put b = Put $ \k -> a (\_ -> b k)
-- #endif

instance Monad Put where
  {-# INLINE return #-}
  return x = Put $ \k -> k x
  {-# INLINE (>>=) #-}
  Put m >>= f = Put $ \k -> m (\m' -> unPut (f m') k)
  {-# INLINE (>>) #-}
  Put m >> Put n = Put $ \k -> m (\_ -> n k)


-- Conversion between Put and Builder
-------------------------------------

-- | Run a 'Builder' as a side-effect of a @Put ()@ action.
{-# INLINE putBuilder #-}
putBuilder :: Builder -> Put ()
putBuilder (Builder b) = Put $ \k -> b (k ())

-- | Convert a @Put ()@ action to a 'Builder'.
{-# INLINE fromPut #-}
fromPut :: Put () -> Builder
fromPut (Put p) = Builder $ \k -> p (\_ -> k)


-- Lifting IO actions
---------------------

{-
-- | Lift an 'IO' action to a 'Put' action.
{-# INLINE putLiftIO #-}
putLiftIO :: IO a -> Put a
putLiftIO io = put $ \k br -> io >>= (`k` br)
-}


------------------------------------------------------------------------------
-- Executing a Put directly on a buffered Handle
------------------------------------------------------------------------------

-- | Run a 'Put' action redirecting the produced output to a 'Handle'.
--
-- The output is buffered using the 'Handle's associated buffer. If this
-- buffer is too small to execute one step of the 'Put' action, then
-- it is replaced with a large enough buffer.
hPut :: forall a. Handle -> Put a -> IO a
#if __GLASGOW_HASKELL__ >= 611
hPut h p = do
    fillHandle 1 (runPut p)
  where
    fillHandle :: Int -> BuildStep a -> IO a
    fillHandle !minFree step = do
        next <- wantWritableHandle "hPut" h fillHandle_
        next
      where
        -- | We need to return an inner IO action that is executed outside
        -- the lock taken on the Handle for two reasons:
        --
        --   1. GHC.IO.Handle.Internals mentions in "Note [async]" that
        --      we should never do any side-effecting operations before
        --      an interuptible operation that may raise an async. exception
        --      as long as we are inside 'wantWritableHandle' and the like.
        --      We possibly run the interuptible 'flushWriteBuffer' right at
        --      the start of 'fillHandle', hence entering it a second time is
        --      not safe, as it could lead to a 'BuildStep' being run twice.
        --
        --   2. We use the 'S.hPut' function to also write to the handle.
        --      This function tries to take the same lock taken by
        --      'wantWritableHandle'. Therefore, we cannot call 'S.hPut'
        --      inside 'wantWritableHandle'.
        --
        fillHandle_ :: Handle__ -> IO (IO a)
        fillHandle_ h_ = do
            makeSpace  =<< readIORef refBuf
            fillBuffer =<< readIORef refBuf
          where
            refBuf        = haByteBuffer h_
            freeSpace buf = bufSize buf - bufR buf

            makeSpace buf
              | bufSize buf < minFree = do
                  flushWriteBuffer h_
                  s <- bufState <$> readIORef refBuf
                  newByteBuffer minFree s >>= writeIORef refBuf

              | freeSpace buf < minFree = flushWriteBuffer h_
              | otherwise               = return ()

            fillBuffer buf
              | freeSpace buf < minFree =
                  error $ unlines
                    [ "Data.ByteString.Lazy.Builder.Internal.hPut: internal error."
                    , "  Not enough space after flush."
                    , "    required: " ++ show minFree
                    , "    free: "     ++ show (freeSpace buf)
                    ]
              | otherwise = do
                  let !br = BufferRange op (pBuf `plusPtr` bufSize buf)
                  res <- fillWithBuildStep step doneH fullH insertChunksH br
                  touchForeignPtr fpBuf
                  return res
              where
                fpBuf = bufRaw buf
                pBuf  = unsafeForeignPtrToPtr fpBuf
                op    = pBuf `plusPtr` bufR buf

                {-# INLINE updateBufR #-}
                updateBufR op' = do
                    let !off' = op' `minusPtr` pBuf
                        !buf' = buf {bufR = off'}
                    writeIORef refBuf buf'

                doneH op' x = do
                    updateBufR op'
                    -- We must flush if this Handle is set to NoBuffering.
                    -- If it is set to LineBuffering, be conservative and
                    -- flush anyway (we didn't check for newlines in the data).
                    -- Flushing must happen outside this 'wantWriteableHandle'
                    -- due to the possible async. exception.
                    case haBufferMode h_ of
                        BlockBuffering _      -> return $ return x
                        _line_or_no_buffering -> return $ hFlush h >> return x

                fullH op' minSize nextStep = do
                    updateBufR op'
                    return $ fillHandle minSize nextStep
                    -- 'fillHandle' will flush the buffer (provided there is
                    -- really less than 'minSize' space left) before executing
                    -- the 'nextStep'.

                insertChunksH op' _ lbsC nextStep = do
                    updateBufR op'
                    return $ do
                        L.foldrChunks (\c rest -> S.hPut h c >> rest) (return ())
                                      (lbsC L.Empty)
                        fillHandle 1 nextStep
#else
hPut h p =
    go =<< buildStepToCIOS strategy (return . Finished) (runPut p)
  where
    go (Finished k)       = return k
    go (Yield1 bs io)     = S.hPut h bs >> io >>= go
    go (YieldC _ lbsC io) = L.hPut h (lbsC L.Empty) >> io >>= go
    strategy = untrimmedStrategy L.smallChunkSize L.defaultChunkSize
#endif

------------------------------------------------------------------------------
-- ByteString insertion / controlling chunk boundaries
------------------------------------------------------------------------------

-- Raw memory
-------------

-- | Ensure that there are at least 'n' free bytes for the following 'Builder'.
{-# INLINE ensureFree #-}
ensureFree :: Int -> Builder
ensureFree minFree =
    builder step
  where
    step k br@(BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ bufferFull minFree op k
      | otherwise                   = k br

-- | Copy the bytes from a 'BufferRange' into the output stream.
{-# INLINE bytesCopyStep #-}
bytesCopyStep :: BufferRange  -- ^ Input 'BufferRange'.
              -> BuildStep a -> BuildStep a
bytesCopyStep !(BufferRange ip0 ipe) k =
    go ip0
  where
    go !ip !(BufferRange op ope)
      | inpRemaining <= outRemaining = do
          copyBytes op ip inpRemaining
          let !br' = BufferRange (op `plusPtr` inpRemaining) ope
          k br'
      | otherwise = do
          copyBytes op ip outRemaining
          let !ip' = ip `plusPtr` outRemaining
          return $ bufferFull 1 ope (go ip')
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe `minusPtr` ip



-- Strict ByteStrings
------------------------------------------------------------------------------


-- | Construct a 'Builder' that copies the strict 'S.ByteString's, if it is
-- smaller than the treshold, and inserts it directly otherwise.
--
-- For example, @byteStringThreshold 1024@ copies strict 'S.ByteString's whose size
-- is less or equal to 1kb, and inserts them directly otherwise. This implies
-- that the average chunk-size of the generated lazy 'L.ByteString' may be as
-- low as 513 bytes, as there could always be just a single byte between the
-- directly inserted 1025 byte, strict 'S.ByteString's.
--
{-# INLINE byteStringThreshold #-}
byteStringThreshold :: Int -> S.ByteString -> Builder
byteStringThreshold maxCopySize =
    \bs -> builder $ step bs
  where
    step !bs@(S.PS _ _ len) !k br@(BufferRange !op _)
      | len <= maxCopySize = byteStringCopyStep bs k br
      | otherwise          =
          return $! insertChunks op (fromIntegral len) (L.chunk bs) k

-- | Construct a 'Builder' that copies the strict 'S.ByteString'.
--
-- Use this function to create 'Builder's from smallish (@<= 4kb@)
-- 'S.ByteString's or if you need to guarantee that the 'S.ByteString' is not
-- shared with the chunks generated by the 'Builder'.
--
{-# INLINE byteStringCopy #-}
byteStringCopy :: S.ByteString -> Builder
byteStringCopy = \bs -> builder $ byteStringCopyStep bs

{-# INLINE byteStringCopyStep #-}
byteStringCopyStep :: S.ByteString -> BuildStep a -> BuildStep a
byteStringCopyStep (S.PS ifp ioff isize) !k0 =
    bytesCopyStep (BufferRange ip ipe) k
  where
    ip   = unsafeForeignPtrToPtr ifp `plusPtr` ioff
    ipe  = ip `plusPtr` isize
    k br = do touchForeignPtr ifp  -- input consumed: OK to release here
              k0 br

-- | Construct a 'Builder' that always inserts the strict 'S.ByteString'
-- directly as a chunk.
--
-- This implies flushing the output buffer, even if it contains just
-- a single byte. You should therefore use 'byteStringInsert' only for large
-- (@> 8kb@) 'S.ByteString's. Otherwise, the generated chunks are too
-- fragmented to be processed efficiently afterwards.
--
{-# INLINE byteStringInsert #-}
byteStringInsert :: S.ByteString -> Builder
byteStringInsert =
    \bs -> builder $ step bs
  where
    step !bs k !br@(BufferRange op _)
      | S.null bs = k br
      | otherwise =
          return $ insertChunks op (fromIntegral $ S.length bs) (L.Chunk bs) k


-- Lazy bytestrings
------------------------------------------------------------------------------

-- | Construct a 'Builder' that uses the thresholding strategy of 'byteStringThreshold'
-- for each chunk of the lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringThreshold #-}
lazyByteStringThreshold :: Int -> L.ByteString -> Builder
lazyByteStringThreshold maxCopySize =
    L.foldrChunks (\bs b -> byteStringThreshold maxCopySize bs `mappend` b) mempty
    -- TODO: We could do better here. Currently, Large, Small, Large, leads to
    -- an unnecessary copy of the 'Small' chunk.

-- | Construct a 'Builder' that copies the lazy 'L.ByteString'.
--
{-# INLINE lazyByteStringCopy #-}
lazyByteStringCopy :: L.ByteString -> Builder
lazyByteStringCopy =
    L.foldrChunks (\bs b -> byteStringCopy bs `mappend` b) mempty


-- | Construct a 'Builder' that inserts all chunks of the lazy 'L.ByteString'
-- directly.
--
{-# INLINE lazyByteStringInsert #-}
lazyByteStringInsert :: L.ByteString -> Builder
lazyByteStringInsert =
    \lbs -> builder $ step lbs
  where
    step L.Empty k br                 = k br
    step lbs     k (BufferRange op _) = case go 0 id lbs of
        (n, lbsC) -> return $ insertChunks op n lbsC k

    go !n lbsC L.Empty          = (n, lbsC)
    go !n lbsC (L.Chunk bs lbs) =
        go (n + fromIntegral (S.length bs)) (lbsC . L.Chunk bs) lbs


-- | Create a 'Builder' denoting the same sequence of bytes as a strict
-- 'S.ByteString'.
-- The 'Builder' inserts large 'S.ByteString's directly, but copies small ones
-- to ensure that the generated chunks are large on average.
--
{-# INLINE byteString #-}
byteString :: S.ByteString -> Builder
byteString = byteStringThreshold maximalCopySize

-- | Create a 'Builder' denoting the same sequence of bytes as a lazy
-- 'S.ByteString'.
-- The 'Builder' inserts large chunks of the lazy 'L.ByteString' directly,
-- but copies small ones to ensure that the generated chunks are large on
-- average.
--
{-# INLINE lazyByteString #-}
lazyByteString :: L.ByteString -> Builder
lazyByteString = lazyByteStringThreshold maximalCopySize
-- FIXME: also insert the small chunk for [large,small,large] directly.
-- Perhaps it makes even sense to concatenate the small chunks in
-- [large,small,small,small,large] and insert them directly afterwards to avoid
-- unnecessary buffer spilling. Hmm, but that uncontrollably increases latency
-- => no good!

-- | The maximal size of a 'S.ByteString' that is copied.
-- @2 * 'L.smallChunkSize'@ to guarantee that on average a chunk is of
-- 'L.smallChunkSize'.
maximalCopySize :: Int
maximalCopySize = 2 * L.smallChunkSize

-- LazyByteStringC: difference lists of lazy bytestrings
--------------------------------------------------------

-- | Insert a 'LazyByteStringC' of the given size directly.
{-# INLINE lazyByteStringC #-}
lazyByteStringC :: Int64 -> LazyByteStringC -> Builder
lazyByteStringC n lbsC =
    builder $ \k (BufferRange op _) -> return $ insertChunks op n lbsC k

------------------------------------------------------------------------------
-- Builder execution
------------------------------------------------------------------------------

-- | A buffer allocation strategy for executing 'Builder's.

-- The strategy
--
-- > 'AllocationStrategy' firstBufSize bufSize trim
--
-- states that the first buffer is of size @firstBufSize@, all following buffers
-- are of size @bufSize@, and a buffer of size @n@ filled with @k@ bytes should
-- be trimmed iff @trim k n@ is 'True'.
data AllocationStrategy = AllocationStrategy
         {-# UNPACK #-} !Int  -- size of first buffer
         {-# UNPACK #-} !Int  -- size of successive buffers
         (Int -> Int -> Bool) -- trim

-- | Sanitize a buffer size; i.e., make it at least the size of a 'Int'.
{-# INLINE sanitize #-}
sanitize :: Int -> Int
sanitize = max (sizeOf (undefined :: Int))

-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- discarded right after they are generated. For example, if you just generate
-- them to write them to a network socket.
{-# INLINE untrimmedStrategy #-}
untrimmedStrategy :: Int -- ^ Size of the first buffer
                  -> Int -- ^ Size of successive buffers
                  -> AllocationStrategy
                  -- ^ An allocation strategy that does not trim any of the
                  -- filled buffers before converting it to a chunk.
untrimmedStrategy firstSize bufSize =
    AllocationStrategy (sanitize firstSize) (sanitize bufSize) (\_ _ -> False)


-- | Use this strategy for generating lazy 'L.ByteString's whose chunks are
-- likely to survive one garbage collection. This strategy trims buffers
-- that are filled less than half in order to avoid spilling too much memory.
{-# INLINE safeStrategy #-}
safeStrategy :: Int  -- ^ Size of first buffer
             -> Int  -- ^ Size of successive buffers
             -> AllocationStrategy
             -- ^ An allocation strategy that guarantees that at least half
             -- of the allocated memory is used for live data
safeStrategy firstSize bufSize =
    AllocationStrategy (sanitize firstSize) (sanitize bufSize)
                       (\used size -> 2*used < size)

-- | Execute a 'Builder' with custom execution parameters.
--
-- This function is forced to be inlined to allow fusing with the allocation
-- strategy despite its rather heavy code-size. We therefore recommend
-- that you introduce a top-level function once you have fixed your strategy.
-- This avoids unnecessary code duplication.
-- For example, the default 'Builder' execution function 'toLazyByteString' is
-- defined as follows.
--
-- @
-- {-# NOINLINE toLazyByteString #-}
-- toLazyByteString =
--   toLazyByteStringWith ('safeStrategy' 'L.smallChunkSize' 'L.defaultChunkSize') empty
-- @
--
-- where @empty@ is the zero-length lazy 'L.ByteString'.
--
-- In most cases, the parameters used by 'toLazyByteString' give good
-- performance. A sub-performing case of 'toLazyByteString' is executing short
-- (<128 bytes) 'Builder's. In this case, the allocation overhead for the first
-- 4kb buffer and the trimming cost dominate the cost of executing the
-- 'Builder'. You can avoid this problem using
--
-- >toLazyByteStringWith (safeStrategy 128 smallChunkSize) empty
--
-- This reduces the allocation and trimming overhead, as all generated
-- 'L.ByteString's fit into the first buffer and there is no trimming
-- required, if more than 64 bytes are written.
--
{-# INLINE toLazyByteStringWith #-}
toLazyByteStringWith
    :: AllocationStrategy
       -- ^ Buffer allocation strategy to use
    -> L.ByteString
       -- ^ Lazy 'L.ByteString' to use as the tail of the generated lazy
       -- 'L.ByteString'
    -> Builder
       -- ^ Builder to execute
    -> L.ByteString
       -- ^ Resulting lazy 'L.ByteString'
toLazyByteStringWith strategy k b =
    ciosToLazyByteString k $ unsafePerformIO $
        buildStepToCIOS strategy (return . Finished) (runBuilder b)

-- | A stream of non-empty chunks interleaved with 'IO'.
data ChunkIOStream a =
       Finished a
     | Yield1 {-# UNPACK #-} !S.ByteString (IO (ChunkIOStream a))
     | YieldC {-# UNPACK #-} !Int64 LazyByteStringC (IO (ChunkIOStream a))

{-# INLINE ciosToLazyByteString #-}
ciosToLazyByteString :: L.ByteString -> ChunkIOStream () -> L.ByteString
ciosToLazyByteString k = go
  where
    go (Finished _)       = k
    go (Yield1 bs io)     = L.Chunk bs $ unsafePerformIO (go <$> io)
    go (YieldC _ lbsC io) = lbsC $ unsafePerformIO (go <$> io)

{-# INLINE buildStepToCIOS #-}
buildStepToCIOS
    :: AllocationStrategy          -- ^ Buffer allocation strategy to use
    -> (a -> IO (ChunkIOStream b)) -- ^ Continuation stream constructor.
    -> BuildStep a                 -- ^ 'Put' to execute
    -> IO (ChunkIOStream b)
buildStepToCIOS (AllocationStrategy firstSize bufSize trim) k =
    \step -> fillNew step firstSize
  where
    fillNew !step0 !size = do
        S.mallocByteString size >>= fill step0
      where
        fill !step !fpbuf = do
            res <- fillWithBuildStep step doneH fullH insertChunksH br
            touchForeignPtr fpbuf
            return res
          where
            op = unsafeForeignPtrToPtr fpbuf -- safe due to mkCIOS
            pe = op `plusPtr` size
            br = BufferRange op pe

            doneH op' x = wrapChunk op' (const $ k x)

            fullH op' minSize nextStep =
                wrapChunk op' (const $ fillNew nextStep (max minSize bufSize))

            insertChunksH op' n lbsC nextStep =
                wrapChunk op' $ \isEmpty -> return $ YieldC n lbsC $
                    -- Checking for empty case avoids allocating 'n-1' empty
                    -- buffers for 'n' insertChunksH right after each other.
                    if isEmpty
                      then fill nextStep fpbuf
                      else fillNew nextStep bufSize

            -- Yield a chunk, trimming it if necesary
            {-# INLINE wrapChunk #-}
            wrapChunk !op' mkCIOS
              | pe < op'            = error $
                  "buildStepToCIOS: overwrite by " ++ show (op' `minusPtr` pe) ++ " bytes"
              | chunkSize == 0      = mkCIOS True
              | trim chunkSize size = do
                  bs <- S.create chunkSize $ \pbuf -> copyBytes pbuf op chunkSize
                  return $ Yield1 bs (mkCIOS False)
              | otherwise            =
                  return $ Yield1 (S.PS fpbuf 0 chunkSize) (mkCIOS False)
              where
                chunkSize = op' `minusPtr` op
