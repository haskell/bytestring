{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes, MagicHash,
            UnboxedTuples, DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Internal
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2012
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- A module containing semi-public 'ByteString' internals. This exposes the
-- 'ByteString' representation and low level construction functions. As such
-- all the functions in this module are unsafe. The API is also not stable.
--
-- Where possible application should instead use the functions from the normal
-- public interface modules, such as "Data.ByteString.Unsafe". Packages that
-- extend the ByteString system at a low level will need to use this module.
--
module Data.ByteString.Internal (

        -- * The @ByteString@ type and representation
        ByteString
        ( BS
        , PS -- backwards compatibility shim
        ),

        StrictByteString,

        -- * Internal indexing
        findIndexOrLength,

        -- * Conversion with lists: packing and unpacking
        packBytes, packUptoLenBytes, unsafePackLenBytes,
        packChars, packUptoLenChars, unsafePackLenChars,
        unpackBytes, unpackAppendBytesLazy, unpackAppendBytesStrict,
        unpackChars, unpackAppendCharsLazy, unpackAppendCharsStrict,
        unsafePackAddress, unsafePackLiteral,

        -- * Low level imperative construction
        create,
        createUptoN,
        createUptoN',
        createAndTrim,
        createAndTrim',
        unsafeCreate,
        unsafeCreateUptoN,
        unsafeCreateUptoN',
        mallocByteString,

        -- * Conversion to and from ForeignPtrs
        fromForeignPtr,
        toForeignPtr,
        fromForeignPtr0,
        toForeignPtr0,

        -- * Utilities
        nullForeignPtr,
        checkedAdd,

        -- * Standard C Functions
        c_strlen,
        c_free_finalizer,

        memchr,
        memcmp,
        memcpy,
        memset,

        -- * cbits functions
        c_reverse,
        c_intersperse,
        c_maximum,
        c_minimum,
        c_count,
        c_sort,

        -- * Chars
        w2c, c2w, isSpaceWord8, isSpaceChar8,

        -- * Deprecated and unmentionable
        accursedUnutterablePerformIO,

        -- * Exported compatibility shim
        plusForeignPtr,
        unsafeWithForeignPtr
  ) where

import Prelude hiding (concat, null)
import qualified Data.List as List

import Control.Monad            (void)

import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, FunPtr, plusPtr, minusPtr)
import Foreign.Storable         (Storable(..))
import Foreign.C.Types          (CInt(..), CSize(..))
import Foreign.C.String         (CString)

#if MIN_VERSION_base(4,13,0)
import Data.Semigroup           (Semigroup (sconcat, stimes))
#else
import Data.Semigroup           (Semigroup ((<>), sconcat, stimes))
#endif
import Data.List.NonEmpty       (NonEmpty ((:|)))

import Control.DeepSeq          (NFData(rnf))

import Data.String              (IsString(..))

import Control.Exception        (assert)

import Data.Bits                ((.&.))
import Data.Char                (ord)
import Data.Word

import Data.Typeable            (Typeable)
import Data.Data                (Data(..), mkNoRepType)

import GHC.Base                 (nullAddr#,realWorld#,unsafeChr)
import GHC.Exts                 (IsList(..))
import GHC.CString              (unpackCString#)
import GHC.Prim                 (Addr#)
import GHC.IO                   (IO(IO),unsafeDupablePerformIO)
import GHC.ForeignPtr           (ForeignPtr(ForeignPtr)
#if __GLASGOW_HASKELL__ < 900
                                , newForeignPtr_
#endif
                                , mallocPlainForeignPtrBytes)

#if MIN_VERSION_base(4,10,0)
import GHC.ForeignPtr           (plusForeignPtr)
#else
import GHC.Prim                 (plusAddr#)
#endif

#if __GLASGOW_HASKELL__ >= 811
import GHC.CString              (cstringLength#)
import GHC.ForeignPtr           (ForeignPtrContents(FinalPtr))
#else
import GHC.Ptr                  (Ptr(..), castPtr)
#endif

#if (__GLASGOW_HASKELL__ < 802) || (__GLASGOW_HASKELL__ >= 811)
import GHC.Types                (Int (..))
#endif

#if MIN_VERSION_base(4,15,0)
import GHC.ForeignPtr           (unsafeWithForeignPtr)
#endif

#if !MIN_VERSION_base(4,15,0)
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif

-- CFILES stuff is Hugs only
{-# CFILES cbits/fpstring.c #-}

#if !MIN_VERSION_base(4,10,0)
-- |Advances the given address by the given offset in bytes.
--
-- The new 'ForeignPtr' shares the finalizer of the original,
-- equivalent from a finalization standpoint to just creating another
-- reference to the original. That is, the finalizer will not be
-- called before the new 'ForeignPtr' is unreachable, nor will it be
-- called an additional time due to this call, and the finalizer will
-- be called with the same address that it would have had this call
-- not happened, *not* the new address.
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr guts) (I# offset) = ForeignPtr (plusAddr# addr offset) guts
{-# INLINE [0] plusForeignPtr #-}
{-# RULES
"ByteString plusForeignPtr/0" forall fp .
   plusForeignPtr fp 0 = fp
 #-}
#endif

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.
--
-- A 'ByteString' contains 8-bit bytes, or by using the operations from
-- "Data.ByteString.Char8" it can be interpreted as containing 8-bit
-- characters.
--
data ByteString = BS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- length
                     -- ^ @since 0.11.0.0
    deriving (Typeable)

-- | Type synonym for the strict flavour of 'ByteString'.
type StrictByteString = ByteString

-- |
-- @'PS' foreignPtr offset length@ represents a 'ByteString' with data
-- backed by a given @foreignPtr@, starting at a given @offset@ in bytes
-- and of a specified @length@.
--
-- This pattern is used to emulate the legacy 'ByteString' data
-- constructor, so that pre-existing code generally doesn't need to
-- change to benefit from the simplified 'BS' constructor and can
-- continue to function unchanged.
--
-- /Note:/ Matching with this constructor will always be given a 0 'offset',
-- as the base will be manipulated by 'plusForeignPtr' instead.
--
pattern PS :: ForeignPtr Word8 -> Int -> Int -> ByteString
pattern PS fp zero len <- BS fp ((0,) -> (zero, len)) where
  PS fp o len = BS (plusForeignPtr fp o) len
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE PS #-}
#endif

instance Eq  ByteString where
    (==)    = eq

instance Ord ByteString where
    compare = compareBytes

instance Semigroup ByteString where
    (<>)    = append
    sconcat (b:|bs) = concat (b:bs)
    stimes  = times

instance Monoid ByteString where
    mempty  = BS nullForeignPtr 0
    mappend = (<>)
    mconcat = concat

instance NFData ByteString where
    rnf BS{} = ()

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackChars ps) r

instance Read ByteString where
    readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

-- | @since 0.10.12.0
instance IsList ByteString where
  type Item ByteString = Word8
  fromList = packBytes
  toList   = unpackBytes

-- | Beware: 'fromString' truncates multi-byte characters to octets.
-- e.g. "枯朶に烏のとまりけり秋の暮" becomes �6k�nh~�Q��n�
instance IsString ByteString where
    {-# INLINE fromString #-}
    fromString = packChars

instance Data ByteString where
  gfoldl f z txt = z packBytes `f` unpackBytes txt
  toConstr _     = error "Data.ByteString.ByteString.toConstr"
  gunfold _ _    = error "Data.ByteString.ByteString.gunfold"
  dataTypeOf _   = mkNoRepType "Data.ByteString.ByteString"

------------------------------------------------------------------------
-- Internal indexing

-- | 'findIndexOrLength' is a variant of findIndex, that returns the length
-- of the string if no element is found, rather than Nothing.
findIndexOrLength :: (Word8 -> Bool) -> ByteString -> Int
findIndexOrLength k (BS x l) =
    accursedUnutterablePerformIO $ withForeignPtr x g
  where
    g ptr = go 0
      where
        go !n | n >= l    = return l
              | otherwise = do w <- peek $ ptr `plusPtr` n
                               if k w
                                 then return n
                                 else go (n+1)
{-# INLINE findIndexOrLength #-}

------------------------------------------------------------------------
-- Packing and unpacking from lists

packBytes :: [Word8] -> ByteString
packBytes ws = unsafePackLenBytes (List.length ws) ws

packChars :: [Char] -> ByteString
packChars cs = unsafePackLenChars (List.length cs) cs

{-# INLINE [0] packChars #-}

{-# RULES
"ByteString packChars/packAddress" forall s .
   packChars (unpackCString# s) = unsafePackLiteral s
 #-}

unsafePackLenBytes :: Int -> [Word8] -> ByteString
unsafePackLenBytes len xs0 =
    unsafeCreate len $ \p -> go p xs0
  where
    go !_ []     = return ()
    go !p (x:xs) = poke p x >> go (p `plusPtr` 1) xs

unsafePackLenChars :: Int -> [Char] -> ByteString
unsafePackLenChars len cs0 =
    unsafeCreate len $ \p -> go p cs0
  where
    go !_ []     = return ()
    go !p (c:cs) = poke p (c2w c) >> go (p `plusPtr` 1) cs


-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @ByteString@. A much faster way to
-- create an 'Addr#' is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the 'Addr#' must point to a null-terminated buffer (as
-- is the case with @\"string\"\#@ literals in GHC). Use 'Data.ByteString.Unsafe.unsafePackAddressLen'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = unsafePackAddress "literal"#
--
-- This function is /unsafe/. If you modify the buffer pointed to by the
-- original 'Addr#' this modification will be reflected in the resulting
-- @ByteString@, breaking referential transparency.
--
-- Note this also won't work if your 'Addr#' has embedded @\'\\0\'@ characters in
-- the string, as @strlen@ will return too short a length.
--
unsafePackAddress :: Addr# -> IO ByteString
unsafePackAddress addr# = do
#if __GLASGOW_HASKELL__ >= 811
    return (BS (ForeignPtr addr# FinalPtr) (I# (cstringLength# addr#)))
#else
    p <- newForeignPtr_ (castPtr cstr)
    l <- c_strlen cstr
    return $ BS p (fromIntegral l)
  where
    cstr :: CString
    cstr = Ptr addr#
#endif
{-# INLINE unsafePackAddress #-}

-- | See 'unsafePackAddress'. This function has similar behavior. Prefer
-- this function when the address in known to be an @Addr#@ literal. In
-- that context, there is no need for the sequencing guarantees that 'IO'
-- provides. On GHC 9.0 and up, this function uses the @FinalPtr@ data
-- constructor for @ForeignPtrContents@.
--
-- @since 0.11.0.0
unsafePackLiteral :: Addr# -> ByteString
unsafePackLiteral addr# =
#if __GLASGOW_HASKELL__ >= 811
  BS (ForeignPtr addr# FinalPtr) (I# (cstringLength# addr#))
#else
  let len = accursedUnutterablePerformIO (c_strlen (Ptr addr#))
   in BS (accursedUnutterablePerformIO (newForeignPtr_ (Ptr addr#))) (fromIntegral len)
#endif
{-# INLINE unsafePackLiteral #-}


packUptoLenBytes :: Int -> [Word8] -> (ByteString, [Word8])
packUptoLenBytes len xs0 =
    unsafeCreateUptoN' len $ \p0 ->
      let p_end = plusPtr p0 len
          go !p []              = return (p `minusPtr` p0, [])
          go !p xs | p == p_end = return (len, xs)
          go !p (x:xs)          = poke p x >> go (p `plusPtr` 1) xs
      in go p0 xs0

packUptoLenChars :: Int -> [Char] -> (ByteString, [Char])
packUptoLenChars len cs0 =
    unsafeCreateUptoN' len $ \p0 ->
      let p_end = plusPtr p0 len
          go !p []              = return (p `minusPtr` p0, [])
          go !p cs | p == p_end = return (len, cs)
          go !p (c:cs)          = poke p (c2w c) >> go (p `plusPtr` 1) cs
      in go p0 cs0

-- Unpacking bytestrings into lists efficiently is a tradeoff: on the one hand
-- we would like to write a tight loop that just blasts the list into memory, on
-- the other hand we want it to be unpacked lazily so we don't end up with a
-- massive list data structure in memory.
--
-- Our strategy is to combine both: we will unpack lazily in reasonable sized
-- chunks, where each chunk is unpacked strictly.
--
-- unpackBytes and unpackChars do the lazy loop, while unpackAppendBytes and
-- unpackAppendChars do the chunks strictly.

unpackBytes :: ByteString -> [Word8]
unpackBytes bs = unpackAppendBytesLazy bs []

unpackChars :: ByteString -> [Char]
unpackChars bs = unpackAppendCharsLazy bs []

unpackAppendBytesLazy :: ByteString -> [Word8] -> [Word8]
unpackAppendBytesLazy (BS fp len) xs
  | len <= 100 = unpackAppendBytesStrict (BS fp len) xs
  | otherwise  = unpackAppendBytesStrict (BS fp 100) remainder
  where
    remainder  = unpackAppendBytesLazy (BS (plusForeignPtr fp 100) (len-100)) xs

  -- Why 100 bytes you ask? Because on a 64bit machine the list we allocate
  -- takes just shy of 4k which seems like a reasonable amount.
  -- (5 words per list element, 8 bytes per word, 100 elements = 4000 bytes)

unpackAppendCharsLazy :: ByteString -> [Char] -> [Char]
unpackAppendCharsLazy (BS fp len) cs
  | len <= 100 = unpackAppendCharsStrict (BS fp len) cs
  | otherwise  = unpackAppendCharsStrict (BS fp 100) remainder
  where
    remainder  = unpackAppendCharsLazy (BS (plusForeignPtr fp 100) (len-100)) cs

-- For these unpack functions, since we're unpacking the whole list strictly we
-- build up the result list in an accumulator. This means we have to build up
-- the list starting at the end. So our traversal starts at the end of the
-- buffer and loops down until we hit the sentinal:

unpackAppendBytesStrict :: ByteString -> [Word8] -> [Word8]
unpackAppendBytesStrict (BS fp len) xs =
    accursedUnutterablePerformIO $ unsafeWithForeignPtr fp $ \base ->
      loop (base `plusPtr` (-1)) (base `plusPtr` (-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (x:acc)

unpackAppendCharsStrict :: ByteString -> [Char] -> [Char]
unpackAppendCharsStrict (BS fp len) xs =
    accursedUnutterablePerformIO $ unsafeWithForeignPtr fp $ \base ->
      loop (base `plusPtr` (-1)) (base `plusPtr` (-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (w2c x:acc)

------------------------------------------------------------------------

-- | The 0 pointer. Used to indicate the empty Bytestring.
nullForeignPtr :: ForeignPtr Word8
#if __GLASGOW_HASKELL__ >= 811
nullForeignPtr = ForeignPtr nullAddr# FinalPtr
#else
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr")
#endif

-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(1)/ Build a ByteString from a ForeignPtr.
--
-- If you do not need the offset parameter then you do should be using
-- 'Data.ByteString.Unsafe.unsafePackCStringLen' or
-- 'Data.ByteString.Unsafe.unsafePackCStringFinalizer' instead.
--
fromForeignPtr :: ForeignPtr Word8
               -> Int -- ^ Offset
               -> Int -- ^ Length
               -> ByteString
fromForeignPtr fp o = BS (plusForeignPtr fp o)
{-# INLINE fromForeignPtr #-}

-- | @since 0.11.0.0
fromForeignPtr0 :: ForeignPtr Word8
               -> Int -- ^ Length
               -> ByteString
fromForeignPtr0 = BS
{-# INLINE fromForeignPtr0 #-}

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int) -- ^ (ptr, offset, length)
toForeignPtr (BS ps l) = (ps, 0, l)
{-# INLINE toForeignPtr #-}

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
--
-- @since 0.11.0.0
toForeignPtr0 :: ByteString -> (ForeignPtr Word8, Int) -- ^ (ptr, length)
toForeignPtr0 (BS ps l) = (ps, l)
{-# INLINE toForeignPtr0 #-}

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> ByteString
unsafeCreate l f = unsafeDupablePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Like 'unsafeCreate' but instead of giving the final size of the
-- ByteString, it is just an upper bound. The inner action returns
-- the actual size. Unlike 'createAndTrim' the ByteString is not
-- reallocated if the final size is less than the estimated size.
unsafeCreateUptoN :: Int -> (Ptr Word8 -> IO Int) -> ByteString
unsafeCreateUptoN l f = unsafeDupablePerformIO (createUptoN l f)
{-# INLINE unsafeCreateUptoN #-}

-- | @since 0.10.12.0
unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

-- | Create ByteString of size @l@ and use action @f@ to fill its contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> f p
    return $! BS fp l
{-# INLINE create #-}

-- | Given a maximum size @l@ and an action @f@ that fills the 'ByteString'
-- starting at the given 'Ptr' and returns the actual utilized length,
-- @`createUpToN'` l f@ returns the filled 'ByteString'.
createUptoN :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
createUptoN l f = do
    fp <- mallocByteString l
    l' <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return $! BS fp l'
{-# INLINE createUptoN #-}

-- | Like 'createUpToN', but also returns an additional value created by the
-- action.
--
-- @since 0.10.12.0
createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (ByteString, a)
createUptoN' l f = do
    fp <- mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return (BS fp l', res)
{-# INLINE createUptoN' #-}

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, createAndTrim makes the 'ByteString'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.
--
-- createAndTrim is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
--
createAndTrim :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
createAndTrim l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> do
        l' <- f p
        if assert (l' <= l) $ l' >= l
            then return $! BS fp l
            else create l' $ \p' -> memcpy p' p l'
{-# INLINE createAndTrim #-}

createAndTrim' :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
createAndTrim' l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> do
        (off, l', res) <- f p
        if assert (l' <= l) $ l' >= l
            then return (BS fp l, res)
            else do ps <- create l' $ \p' ->
                            memcpy p' (p `plusPtr` off) l'
                    return (ps, res)

-- | Wrapper of 'Foreign.ForeignPtr.mallocForeignPtrBytes' with faster implementation for GHC
--
mallocByteString :: Int -> IO (ForeignPtr a)
mallocByteString = mallocPlainForeignPtrBytes
{-# INLINE mallocByteString #-}

------------------------------------------------------------------------
-- Implementations for Eq, Ord and Monoid instances

eq :: ByteString -> ByteString -> Bool
eq a@(BS fp len) b@(BS fp' len')
  | len /= len' = False    -- short cut on length
  | fp == fp'   = True     -- short cut for the same string
  | otherwise   = compareBytes a b == EQ
{-# INLINE eq #-}
-- ^ still needed

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (BS _   0)    (BS _   0)    = EQ  -- short cut for empty strings
compareBytes (BS fp1 len1) (BS fp2 len2) =
    accursedUnutterablePerformIO $
      unsafeWithForeignPtr fp1 $ \p1 ->
      unsafeWithForeignPtr fp2 $ \p2 -> do
        i <- memcmp p1 p2 (min len1 len2)
        return $! case i `compare` 0 of
                    EQ  -> len1 `compare` len2
                    x   -> x

append :: ByteString -> ByteString -> ByteString
append (BS _   0)    b                  = b
append a             (BS _   0)    = a
append (BS fp1 len1) (BS fp2 len2) =
    unsafeCreate (len1+len2) $ \destptr1 -> do
      let destptr2 = destptr1 `plusPtr` len1
      unsafeWithForeignPtr fp1 $ \p1 -> memcpy destptr1 p1 len1
      unsafeWithForeignPtr fp2 $ \p2 -> memcpy destptr2 p2 len2

concat :: [ByteString] -> ByteString
concat = \bss0 -> goLen0 bss0 bss0
    -- The idea here is we first do a pass over the input list to determine:
    --
    --  1. is a copy necessary? e.g. @concat []@, @concat [mempty, "hello"]@,
    --     and @concat ["hello", mempty, mempty]@ can all be handled without
    --     copying.
    --  2. if a copy is necessary, how large is the result going to be?
    --
    -- If a copy is necessary then we create a buffer of the appropriate size
    -- and do another pass over the input list, copying the chunks into the
    -- buffer. Also, since foreign calls aren't entirely free we skip over
    -- empty chunks while copying.
    --
    -- We pass the original [ByteString] (bss0) through as an argument through
    -- goLen0, goLen1, and goLen since we will need it again in goCopy. Passing
    -- it as an explicit argument avoids capturing it in these functions'
    -- closures which would result in unnecessary closure allocation.
  where
    -- It's still possible that the result is empty
    goLen0 _    []                     = mempty
    goLen0 bss0 (BS _ 0     :bss)    = goLen0 bss0 bss
    goLen0 bss0 (bs           :bss)    = goLen1 bss0 bs bss

    -- It's still possible that the result is a single chunk
    goLen1 _    bs []                  = bs
    goLen1 bss0 bs (BS _ 0  :bss)    = goLen1 bss0 bs bss
    goLen1 bss0 bs (BS _ len:bss)    = goLen bss0 (checkedAdd "concat" len' len) bss
      where BS _ len' = bs

    -- General case, just find the total length we'll need
    goLen bss0 !total (BS _ len:bss) = goLen bss0 total' bss
      where total' = checkedAdd "concat" total len
    goLen bss0 total [] =
      unsafeCreate total $ \ptr -> goCopy bss0 ptr

    -- Copy the data
    goCopy []                  !_   = return ()
    goCopy (BS _  0  :bss) !ptr = goCopy bss ptr
    goCopy (BS fp len:bss) !ptr = do
      unsafeWithForeignPtr fp $ \p -> memcpy ptr p len
      goCopy bss (ptr `plusPtr` len)
{-# NOINLINE concat #-}

{-# RULES
"ByteString concat [] -> mempty"
   concat [] = mempty
"ByteString concat [bs] -> bs" forall x.
   concat [x] = x
 #-}

-- | /O(log n)/ Repeats the given ByteString n times.
times :: Integral a => a -> ByteString -> ByteString
times n (BS fp len)
  | n < 0 = error "stimes: non-negative multiplier expected"
  | n == 0 = mempty
  | n == 1 = BS fp len
  | len == 0 = mempty
  | len == 1 = unsafeCreate size $ \destptr ->
    unsafeWithForeignPtr fp $ \p -> do
      byte <- peek p
      void $ memset destptr byte (fromIntegral size)
  | otherwise = unsafeCreate size $ \destptr ->
    unsafeWithForeignPtr fp $ \p -> do
      memcpy destptr p len
      fillFrom destptr len
  where
    size = len * fromIntegral n

    fillFrom :: Ptr Word8 -> Int -> IO ()
    fillFrom destptr copied
      | 2 * copied < size = do
        memcpy (destptr `plusPtr` copied) destptr copied
        fillFrom destptr (copied * 2)
      | otherwise = memcpy (destptr `plusPtr` copied) destptr (size - copied)

-- | Add two non-negative numbers. Errors out on overflow.
checkedAdd :: String -> Int -> Int -> Int
checkedAdd fun x y
  | r >= 0    = r
  | otherwise = overflowError fun
  where r = x + y
{-# INLINE checkedAdd #-}

------------------------------------------------------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
w2c = unsafeChr . fromIntegral
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | Selects words corresponding to white-space characters in the Latin-1 range
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w8 =
    -- Avoid the cost of narrowing arithmetic results to Word8,
    -- the conversion from Word8 to Word is free.
    let w :: Word
        !w = fromIntegral w8
     in w .&. 0x50 == 0    -- Quick non-whitespace filter
        && w - 0x21 > 0x7e -- Second non-whitespace filter
        && ( w == 0x20     -- SP
          || w == 0xa0     -- NBSP
          || w - 0x09 < 5) -- HT, NL, VT, FF, CR
{-# INLINE isSpaceWord8 #-}

-- | Selects white-space characters in the Latin-1 range
isSpaceChar8 :: Char -> Bool
isSpaceChar8 = isSpaceWord8 . c2w
{-# INLINE isSpaceChar8 #-}

overflowError :: String -> a
overflowError fun = error $ "Data.ByteString." ++ fun ++ ": size overflow"

------------------------------------------------------------------------

-- | This \"function\" has a superficial similarity to 'System.IO.Unsafe.unsafePerformIO' but
-- it is in fact a malevolent agent of chaos. It unpicks the seams of reality
-- (and the 'IO' monad) so that the normal rules no longer apply. It lulls you
-- into thinking it is reasonable, but when you are not looking it stabs you
-- in the back and aliases all of your mutable buffers. The carcass of many a
-- seasoned Haskell programmer lie strewn at its feet.
--
-- Witness the trail of destruction:
--
-- * <https://github.com/haskell/bytestring/commit/71c4b438c675aa360c79d79acc9a491e7bbc26e7>
--
-- * <https://github.com/haskell/bytestring/commit/210c656390ae617d9ee3b8bcff5c88dd17cef8da>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3486>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/3487>
--
-- * <https://ghc.haskell.org/trac/ghc/ticket/7270>
--
-- Do not talk about \"safe\"! You do not know what is safe!
--
-- Yield not to its blasphemous call! Flee traveller! Flee or you will be
-- corrupted and devoured!
--
{-# INLINE accursedUnutterablePerformIO #-}
accursedUnutterablePerformIO :: IO a -> a
accursedUnutterablePerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-- ---------------------------------------------------------------------
--
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

foreign import ccall unsafe "static stdlib.h &free" c_free_finalizer
    :: FunPtr (Ptr Word8 -> IO ())

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

memchr :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)
memchr p w = c_memchr p (fromIntegral w)

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO CInt
memcmp p q s = c_memcmp p q (fromIntegral s)

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = void $ c_memcpy p q (fromIntegral s)

{-
foreign import ccall unsafe "string.h memmove" c_memmove
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memmove :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memmove p q s = do c_memmove p q s
                   return ()
-}

foreign import ccall unsafe "string.h memset" c_memset
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

memset :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)
memset p w = c_memset p (fromIntegral w)

-- ---------------------------------------------------------------------
--
-- Uses our C code
--

foreign import ccall unsafe "static fpstring.h fps_reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "static fpstring.h fps_intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> CSize -> Word8 -> IO ()

foreign import ccall unsafe "static fpstring.h fps_maximum" c_maximum
    :: Ptr Word8 -> CSize -> IO Word8

foreign import ccall unsafe "static fpstring.h fps_minimum" c_minimum
    :: Ptr Word8 -> CSize -> IO Word8

foreign import ccall unsafe "static fpstring.h fps_count" c_count
    :: Ptr Word8 -> CSize -> Word8 -> IO CSize

foreign import ccall unsafe "static fpstring.h fps_sort" c_sort
    :: Ptr Word8 -> CSize -> IO ()
