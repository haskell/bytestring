{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans #-}
--
-- Module      : ByteString.Base
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
-- 

-- | A module containing semi-public ByteString internals. This exposes
-- the ByteString representation and low level construction functions.
-- Modules which extend the ByteString system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.ByteString.Base (
        -- * The @ByteString@ type and representation
        ByteString(..),             -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Low level introduction and elimination
        generate,               -- :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> ByteString
        mallocByteString,
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)
        skipIndex,              -- :: ByteString -> Int

#if defined(__GLASGOW_HASKELL__)
        packCStringFinalizer,   -- :: Ptr Word8 -> Int -> IO () -> IO ByteString
        packAddress,            -- :: Addr# -> ByteString
        unsafePackAddress,      -- :: Int -> Addr# -> ByteString
        unsafeFinalize,         -- :: ByteString -> IO ()
#endif

        -- * Fusion utilities
        noAL, NoAL, loopArr, loopAcc, loopSndAcc,
        loopU, mapEFL, filterEFL, foldEFL, foldEFL', fuseEFL, scanEFL,
        mapAccumEFL, mapIndexEFL,

  ) where

import Data.ByteString.Internal

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

import qualified Foreign.Concurrent as FC (newForeignPtr)

import Data.Word                (Word8)

#if defined(__GLASGOW_HASKELL__)
import Data.Generics            (Data(..), Typeable(..))

import GHC.Prim                 (Addr#)
import GHC.Ptr                  (Ptr(..))
#endif

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int
                     {-# UNPACK #-} !Int

#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(1)/ Build a ByteString from a ForeignPtr
fromForeignPtr :: ForeignPtr Word8 -> Int -> ByteString
fromForeignPtr fp l = PS fp 0 l

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int)
toForeignPtr (PS ps s l) = (ps, s, l)

-- | /O(1)/ 'skipIndex' returns the internal skipped index of the
-- current 'ByteString' from any larger string it was created from, as
-- an 'Int'.
skipIndex :: ByteString -> Int
skipIndex (PS _ s _) = s
{-# INLINE skipIndex #-}

-- | A way of creating ForeignPtrs outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike 'generate'
-- the ByteString is not reallocated if the final size is less than the
-- estimated size. Also, unlike 'generate' ByteString's created this way
-- are managed on the Haskell heap.
create :: Int -> (Ptr Word8 -> IO ()) -> ByteString
create l write_ptr = inlinePerformIO $ do
    fp <- mallocByteString (l+1)
    withForeignPtr fp $ \p -> write_ptr p
    return $ PS fp 0 l
{-# INLINE create #-}

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, generate makes the 'ByteString'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.  The
-- string is padded at the end with a null byte.
--
-- generate is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
--
generate :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
generate i f = do
    fp      <- mallocByteString i
    (ptr,n) <- withForeignPtr fp $ \p -> do
        i' <- f p
        if i' == i
            then return (fp,i')
            else do fp_ <- mallocByteString i'      -- realloc
                    withForeignPtr fp_ $ \p' -> memcpy p' p (fromIntegral i')
                    return (fp_,i')
    return (PS ptr 0 n)

-- Wrapper of mallocForeignPtrArray. Any ByteString allocated this way
-- is padded with a null byte.
mallocByteString :: Int -> IO (ForeignPtr Word8)
mallocByteString l = do
    fp <- mallocForeignPtrArray (l+1)
    withForeignPtr fp $ \p -> poke (p `plusPtr` l) (0::Word8)
    return fp

{-
--
-- On the C malloc heap. Less fun.
--
generate i f = do
    p <- mallocArray (i+1)
    i' <- f p
    p' <- reallocArray p (i'+1)
    poke (p' `plusPtr` i') (0::Word8)    -- XXX so CStrings work
    fp <- newForeignFreePtr p'
    return $ PS fp 0 i'
-}

#if defined(__GLASGOW_HASKELL__)
-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @ByteString@. A much faster way to
-- create an Addr\# is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the Addr# must point to a null-terminated buffer (as
-- is the case with "string"# literals in GHC). Use 'unsafePackAddress'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = packAddress "literal"#
--
packAddress :: Addr# -> ByteString
packAddress addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 (fromIntegral $ c_strlen cstr)
  where
    cstr = Ptr addr#
{-# INLINE packAddress #-}

-- | /O(1)/ 'unsafePackAddress' provides constant-time construction of
-- 'ByteStrings' -- which is ideal for string literals. It packs a
-- null-terminated sequence of bytes into a 'ByteString', given a raw
-- 'Addr\#' to the string, and the length of the string. Make sure the
-- length is correct, otherwise use the safer 'packAddress' (where the
-- length will be calculated once at runtime).
unsafePackAddress :: Int -> Addr# -> ByteString
unsafePackAddress len addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 len
    where cstr = Ptr addr#

-- | /O(1)/ Construct a 'ByteString' given a C Ptr Word8 buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
packCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
packCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $ PS fp 0 l

-- | Explicitly run the finaliser associated with a 'ByteString'.
-- Further references to this value may generate invalid memory
-- references. This operation is unsafe, as there may be other
-- 'ByteStrings' referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'ByteString's
-- ever generated from the underlying byte array are no longer live.
unsafeFinalize :: ByteString -> IO ()
unsafeFinalize (PS p _ _) = finalizeForeignPtr p

#endif

-- ---------------------------------------------------------------------
--
-- Functional array fusion for ByteStrings. 
--
-- From the Data Parallel Haskell project, 
--      http://www.cse.unsw.edu.au/~chak/project/dph/
--

-- |Data type for accumulators which can be ignored. The rewrite rules rely on
-- the fact that no bottoms of this type are ever constructed; hence, we can
-- assume @(_ :: NoAL) `seq` x = x@.
--
data NoAL = NoAL

-- | Special forms of loop arguments
--
-- * These are common special cases for the three function arguments of gen
--   and loop; we give them special names to make it easier to trigger RULES
--   applying in the special cases represented by these arguments.  The
--   "INLINE [1]" makes sure that these functions are only inlined in the last
--   two simplifier phases.
--
-- * In the case where the accumulator is not needed, it is better to always
--   explicitly return a value `()', rather than just copy the input to the
--   output, as the former gives GHC better local information.
-- 

-- | Element function expressing a mapping only
mapEFL :: (Word8 -> Word8) -> (NoAL -> Word8 -> (NoAL, Maybe Word8))
mapEFL f = \_ e -> (noAL, (Just $ f e))
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapEFL #-}
#endif

-- | Element function implementing a filter function only
filterEFL :: (Word8 -> Bool) -> (NoAL -> Word8 -> (NoAL, Maybe Word8))
filterEFL p = \_ e -> if p e then (noAL, Just e) else (noAL, Nothing)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] filterEFL #-}
#endif

-- |Element function expressing a reduction only
foldEFL :: (acc -> Word8 -> acc) -> (acc -> Word8 -> (acc, Maybe Word8))
foldEFL f = \a e -> (f a e, Nothing)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL #-}
#endif

-- | A strict foldEFL.
foldEFL' :: (acc -> Word8 -> acc) -> (acc -> Word8 -> (acc, Maybe Word8))
foldEFL' f = \a e -> let a' = f a e in a' `seq` (a', Nothing)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL' #-}
#endif

-- | Element function expressing a prefix reduction only
--
scanEFL :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> (Word8, Maybe Word8)
scanEFL f = \a e -> (f a e, Just a)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] scanEFL #-}
#endif

-- | Element function implementing a map and fold
--
mapAccumEFL :: (acc -> Word8 -> (acc, Word8)) -> acc -> Word8 -> (acc, Maybe Word8)
mapAccumEFL f = \a e -> case f a e of (a', e') -> (a', Just e')
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapAccumEFL #-}
#endif

-- | Element function implementing a map with index
--
mapIndexEFL :: (Int -> Word8 -> Word8) -> Int -> Word8 -> (Int, Maybe Word8)
mapIndexEFL f = \i e -> let i' = i+1 in i' `seq` (i', Just $ f i e)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapIndexEFL #-}
#endif

-- | No accumulator
noAL :: NoAL
noAL = NoAL
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] noAL #-}
#endif

-- | Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
loopArr :: (acc, byteString) -> byteString
loopArr (_, arr) = arr
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopArr #-}
#endif

loopAcc :: (acc, byteString) -> acc
loopAcc (acc, _) = acc
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopAcc #-}
#endif

loopSndAcc :: ((acc1, acc2), byteString) -> (acc2, byteString)
loopSndAcc ((_, acc), arr) = (acc, arr)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopSndAcc #-}
#endif

------------------------------------------------------------------------

--
-- size, and then percentage.
--

-- | Iteration over over ByteStrings
loopU :: (acc -> Word8 -> (acc, Maybe Word8))  -- ^ mapping & folding, once per elem
      -> acc                                   -- ^ initial acc value
      -> ByteString                            -- ^ input ByteString
      -> (acc, ByteString)

loopU f start (PS z s i) = inlinePerformIO $ withForeignPtr z $ \a -> do
    fp          <- mallocByteString i
    (ptr,n,acc) <- withForeignPtr fp $ \p -> do
        (acc, i') <- go (a `plusPtr` s) p start
        if i' == i
            then return (fp,i',acc)                 -- no realloc for map
            else do fp_ <- mallocByteString i'      -- realloc
                    withForeignPtr fp_ $ \p' -> memcpy p' p (fromIntegral i')
                    return (fp_,i',acc)

    return (acc, PS ptr 0 n)
  where
    go p ma = trans 0 0
        where
            STRICT3(trans)
            trans a_off ma_off acc
                | a_off >= i = return (acc, ma_off)
                | otherwise  = do
                    x <- peekByteOff p a_off
                    let (acc', oe) = f acc x
                    ma_off' <- case oe of
                        Nothing  -> return ma_off
                        Just e   -> do pokeByteOff ma ma_off e
                                       return $ ma_off + 1
                    trans (a_off+1) ma_off' acc'

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopU #-}
#endif


infixr 9 `fuseEFL`

-- |Fuse to flat loop functions
fuseEFL :: (a1 -> Word8  -> (a1, Maybe Word8))
        -> (a2 -> Word8  -> (a2, Maybe Word8))
        -> (a1, a2)
        -> Word8
        -> ((a1, a2), Maybe Word8)
fuseEFL f g (acc1, acc2) e1 =
    case f acc1 e1 of
        (acc1', Nothing) -> ((acc1', acc2), Nothing)
        (acc1', Just e2) ->
            case g acc2 e2 of
                (acc2', res) -> ((acc1', acc2'), res)

{-# RULES

"loop/loop fusion!" forall em1 em2 start1 start2 arr.
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) =
    loopSndAcc (loopU (em1 `fuseEFL` em2) (start1, start2) arr)

"loopArr/loopSndAcc" forall x.
  loopArr (loopSndAcc x) = loopArr x

"seq/NoAL" forall (u::NoAL) e.
  u `seq` e = e

  #-}
