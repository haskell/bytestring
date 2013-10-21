{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE UnliftedFFITypes, MagicHash,
            UnboxedTuples, DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Unsafe #-}
#endif
#endif
{-# OPTIONS_HADDOCK hide #-}

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
        ByteString(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Conversion with lists: packing and unpacking
        packBytes, packUptoLenBytes, unsafePackLenBytes,
        packChars, packUptoLenChars, unsafePackLenChars,
        unpackBytes, unpackAppendBytesLazy, unpackAppendBytesStrict,
        unpackChars, unpackAppendCharsLazy, unpackAppendCharsStrict,
#if defined(__GLASGOW_HASKELL__)
        unsafePackAddress,
#endif

        -- * Low level imperative construction
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
        createUptoN,            -- :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
        createAndTrim,          -- :: Int -> (Ptr Word8 -> IO Int) -> IO  ByteString
        createAndTrim',         -- :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
        unsafeCreate,           -- :: Int -> (Ptr Word8 -> IO ()) ->  ByteString
        unsafeCreateUptoN,      -- :: Int -> (Ptr Word8 -> IO Int) ->  ByteString
        mallocByteString,       -- :: Int -> IO (ForeignPtr a)

        -- * Conversion to and from ForeignPtrs
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)

        -- * Utilities
        inlinePerformIO,        -- :: IO a -> a
        nullForeignPtr,         -- :: ForeignPtr Word8

        -- * Standard C Functions
        c_strlen,               -- :: CString -> IO CInt
        c_free_finalizer,       -- :: FunPtr (Ptr Word8 -> IO ())

        memchr,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO Ptr Word8
        memcmp,                 -- :: Ptr Word8 -> Ptr Word8 -> Int -> IO CInt
        memcpy,                 -- :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
        memset,                 -- :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

        -- * cbits functions
        c_reverse,              -- :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()
        c_intersperse,          -- :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()
        c_maximum,              -- :: Ptr Word8 -> CInt -> IO Word8
        c_minimum,              -- :: Ptr Word8 -> CInt -> IO Word8
        c_count,                -- :: Ptr Word8 -> CInt -> Word8 -> IO CInt

        -- * Chars
        w2c, c2w, isSpaceWord8, isSpaceChar8

  ) where

import Prelude hiding (concat)
import qualified Data.List as List

import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, FunPtr, plusPtr)
import Foreign.Storable         (Storable(..))
#if MIN_VERSION_base(4,5,0) || __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types          (CInt(..), CSize(..), CULong(..))
#else
import Foreign.C.Types          (CInt, CSize, CULong)
#endif
import Foreign.C.String         (CString)

import Data.Monoid              (Monoid(..))
import Control.DeepSeq          (NFData)

#if MIN_VERSION_base(3,0,0)
import Data.String              (IsString(..))
#endif

#ifndef __NHC__
import Control.Exception        (assert)
#endif

import Data.Char                (ord)
import Data.Word                (Word8)

import Data.Typeable            (Typeable)
#if MIN_VERSION_base(4,1,0)
import Data.Data                (Data(..))
#if MIN_VERSION_base(4,2,0)
import Data.Data                (mkNoRepType)
#else
import Data.Data                (mkNorepType)
#endif
#else
import Data.Generics            (Data(..), mkNorepType)
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.Base                 (realWorld#,unsafeChr)
#if MIN_VERSION_base(4,4,0)
import GHC.CString              (unpackCString#)
#else
import GHC.Base                 (unpackCString#)
#endif
import GHC.Prim                 (Addr#)
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO                   (IO(IO))
#else
import GHC.IOBase               (IO(IO),RawBuffer)
#endif
#if __GLASGOW_HASKELL__ >= 611
import GHC.IO                   (unsafeDupablePerformIO)
#else
import GHC.IOBase               (unsafeDupablePerformIO)
#endif
#else
import Data.Char                (chr)
import System.IO.Unsafe         (unsafePerformIO)
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.ForeignPtr           (newForeignPtr_, mallocPlainForeignPtrBytes)
import GHC.Ptr                  (Ptr(..), castPtr)
#else
import Foreign.ForeignPtr       (mallocForeignPtrBytes)
#endif

#ifdef __GLASGOW_HASKELL__
import GHC.ForeignPtr           (ForeignPtr(ForeignPtr))
import GHC.Base                 (nullAddr#)
#else
import Foreign.Ptr              (nullPtr)
#endif

#if __HUGS__
import Hugs.ForeignPtr          (newForeignPtr_)
#elif __GLASGOW_HASKELL__<=604
import Foreign.ForeignPtr       (newForeignPtr_)
#endif

-- CFILES stuff is Hugs only
{-# CFILES cbits/fpstring.c #-}

-- An alternative to Control.Exception (assert) for nhc98
#ifdef __NHC__
#define assert	assertS "__FILE__ : __LINE__"
assertS :: String -> Bool -> a -> a
assertS _ True  = id
assertS s False = error ("assertion failed at "++s)
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

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.
--
-- A 'ByteString' contains 8-bit bytes, or by using the operations from
-- "Data.ByteString.Char8" it can be interpreted as containing 8-bit
-- characters.
--
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length

#if defined(__GLASGOW_HASKELL__)
    deriving (Typeable)
#endif

instance Eq  ByteString where
    (==)    = eq

instance Ord ByteString where
    compare = compareBytes

instance Monoid ByteString where
    mempty  = PS nullForeignPtr 0 0
    mappend = append
    mconcat = concat

instance NFData ByteString

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackChars ps) r

instance Read ByteString where
    readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

#if MIN_VERSION_base(3,0,0)
instance IsString ByteString where
    fromString = packChars
#endif

instance Data ByteString where
  gfoldl f z txt = z packBytes `f` (unpackBytes txt)
  toConstr _     = error "Data.ByteString.ByteString.toConstr"
  gunfold _ _    = error "Data.ByteString.ByteString.gunfold"
#if MIN_VERSION_base(4,2,0)
  dataTypeOf _   = mkNoRepType "Data.ByteString.ByteString"
#else
  dataTypeOf _   = mkNorepType "Data.ByteString.ByteString"
#endif

------------------------------------------------------------------------
-- Packing and unpacking from lists

packBytes :: [Word8] -> ByteString
packBytes ws = unsafePackLenBytes (List.length ws) ws

packChars :: [Char] -> ByteString
packChars cs = unsafePackLenChars (List.length cs) cs

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [0] packChars #-}

{-# RULES
"ByteString packChars/packAddress" forall s .
   packChars (unpackCString# s) = inlinePerformIO (unsafePackAddress s)
 #-}
#endif

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

#if defined(__GLASGOW_HASKELL__)
-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @ByteString@. A much faster way to
-- create an Addr\# is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the Addr# must point to a null-terminated buffer (as
-- is the case with "string"# literals in GHC). Use 'unsafePackAddressLen'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = unsafePackAddress "literal"#
--
-- This function is /unsafe/. If you modify the buffer pointed to by the
-- original Addr# this modification will be reflected in the resulting
-- @ByteString@, breaking referential transparency.
--
-- Note this also won't work if your Addr# has embedded '\0' characters in
-- the string, as @strlen@ will return too short a length.
--
unsafePackAddress :: Addr# -> IO ByteString
unsafePackAddress addr# = do
    p <- newForeignPtr_ (castPtr cstr)
    l <- c_strlen cstr
    return $ PS p 0 (fromIntegral l)
  where
    cstr :: CString
    cstr = Ptr addr#
{-# INLINE unsafePackAddress #-}
#endif

packUptoLenBytes :: Int -> [Word8] -> (ByteString, [Word8])
packUptoLenBytes len xs0 =
    unsafeCreateUptoN' len $ \p -> go p len xs0
  where
    go !_ !n []     = return (len-n, [])
    go !_ !0 xs     = return (len,   xs)
    go !p !n (x:xs) = poke p x >> go (p `plusPtr` 1) (n-1) xs

packUptoLenChars :: Int -> [Char] -> (ByteString, [Char])
packUptoLenChars len cs0 =
    unsafeCreateUptoN' len $ \p -> go p len cs0
  where
    go !_ !n []     = return (len-n, [])
    go !_ !0 cs     = return (len,   cs)
    go !p !n (c:cs) = poke p (c2w c) >> go (p `plusPtr` 1) (n-1) cs

-- Unpacking bytestrings into lists effeciently is a tradeoff: on the one hand
-- we would like to write a tight loop that just blats the list into memory, on
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
unpackAppendBytesLazy (PS fp off len) xs
  | len <= 100 = unpackAppendBytesStrict (PS fp off len) xs
  | otherwise  = unpackAppendBytesStrict (PS fp off 100) remainder
  where
    remainder  = unpackAppendBytesLazy (PS fp (off+100) (len-100)) xs

  -- Why 100 bytes you ask? Because on a 64bit machine the list we allocate
  -- takes just shy of 4k which seems like a reasonable amount.
  -- (5 words per list element, 8 bytes per word, 100 elements = 4000 bytes)

unpackAppendCharsLazy :: ByteString -> [Char] -> [Char]
unpackAppendCharsLazy (PS fp off len) cs
  | len <= 100 = unpackAppendCharsStrict (PS fp off len) cs
  | otherwise  = unpackAppendCharsStrict (PS fp off 100) remainder
  where
    remainder  = unpackAppendCharsLazy (PS fp (off+100) (len-100)) cs

-- For these unpack functions, since we're unpacking the whole list strictly we
-- build up the result list in an accumulator. This means we have to build up
-- the list starting at the end. So our traversal starts at the end of the
-- buffer and loops down until we hit the sentinal:

unpackAppendBytesStrict :: ByteString -> [Word8] -> [Word8]
unpackAppendBytesStrict (PS fp off len) xs =
    inlinePerformIO $ withForeignPtr fp $ \base -> do
      loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (x:acc)

unpackAppendCharsStrict :: ByteString -> [Char] -> [Char]
unpackAppendCharsStrict (PS fp off len) xs =
    inlinePerformIO $ withForeignPtr fp $ \base ->
      loop (base `plusPtr` (off-1)) (base `plusPtr` (off-1+len)) xs
  where
    loop !sentinal !p acc
      | p == sentinal = return acc
      | otherwise     = do x <- peek p
                           loop sentinal (p `plusPtr` (-1)) (w2c x:acc)

------------------------------------------------------------------------

-- | The 0 pointer. Used to indicate the empty Bytestring.
nullForeignPtr :: ForeignPtr Word8
#ifdef __GLASGOW_HASKELL__
nullForeignPtr = ForeignPtr nullAddr# (error "nullForeignPtr") --TODO: should ForeignPtrContents be strict?
#else
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr
{-# NOINLINE nullForeignPtr #-}
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
fromForeignPtr fp s l = PS fp s l
{-# INLINE fromForeignPtr #-}

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int) -- ^ (ptr, offset, length)
toForeignPtr (PS ps s l) = (ps, s, l)
{-# INLINE toForeignPtr #-}

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

unsafeCreateUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> (ByteString, a)
unsafeCreateUptoN' l f = unsafeDupablePerformIO (createUptoN' l f)
{-# INLINE unsafeCreateUptoN' #-}

#ifndef __GLASGOW_HASKELL__
-- for Hugs, NHC etc
unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO
#endif

-- | Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> f p
    return $! PS fp 0 l
{-# INLINE create #-}

-- | Create ByteString of up to size size @l@ and use action @f@ to fill it's
-- contents which returns its true size.
createUptoN :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
createUptoN l f = do
    fp <- mallocByteString l
    l' <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return $! PS fp 0 l'
{-# INLINE createUptoN #-}

-- | Create ByteString of up to size @l@ and use action @f@ to fill it's contents which returns its true size.
createUptoN' :: Int -> (Ptr Word8 -> IO (Int, a)) -> IO (ByteString, a)
createUptoN' l f = do
    fp <- mallocByteString l
    (l', res) <- withForeignPtr fp $ \p -> f p
    assert (l' <= l) $ return (PS fp 0 l', res)
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
            then return $! PS fp 0 l
            else create l' $ \p' -> memcpy p' p l'
{-# INLINE createAndTrim #-}

createAndTrim' :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
createAndTrim' l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> do
        (off, l', res) <- f p
        if assert (l' <= l) $ l' >= l
            then return $! (PS fp 0 l, res)
            else do ps <- create l' $ \p' ->
                            memcpy p' (p `plusPtr` off) l'
                    return $! (ps, res)

-- | Wrapper of 'mallocForeignPtrBytes' with faster implementation for GHC
--
mallocByteString :: Int -> IO (ForeignPtr a)
mallocByteString l = do
#ifdef __GLASGOW_HASKELL__
    mallocPlainForeignPtrBytes l
#else
    mallocForeignPtrBytes l
#endif
{-# INLINE mallocByteString #-}

------------------------------------------------------------------------
-- Implementations for Eq, Ord and Monoid instances

eq :: ByteString -> ByteString -> Bool
eq a@(PS fp off len) b@(PS fp' off' len')
  | len /= len'              = False    -- short cut on length
  | fp == fp' && off == off' = True     -- short cut for the same string
  | otherwise                = compareBytes a b == EQ
{-# INLINE eq #-}
-- ^ still needed

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (PS _   _    0)    (PS _   _    0)    = EQ  -- short cut for empty strings
compareBytes (PS fp1 off1 len1) (PS fp2 off2 len2) =
    inlinePerformIO $
      withForeignPtr fp1 $ \p1 ->
      withForeignPtr fp2 $ \p2 -> do
        i <- memcmp (p1 `plusPtr` off1) (p2 `plusPtr` off2) (min len1 len2)
        return $! case i `compare` 0 of
                    EQ  -> len1 `compare` len2
                    x   -> x

append :: ByteString -> ByteString -> ByteString
append (PS _   _    0)    b                  = b
append a                  (PS _   _    0)    = a
append (PS fp1 off1 len1) (PS fp2 off2 len2) =
    unsafeCreate (len1+len2) $ \destptr1 -> do
      let destptr2 = destptr1 `plusPtr` len1
      withForeignPtr fp1 $ \p1 -> memcpy destptr1 (p1 `plusPtr` off1) len1
      withForeignPtr fp2 $ \p2 -> memcpy destptr2 (p2 `plusPtr` off2) len2

concat :: [ByteString] -> ByteString
concat []     = mempty
concat [bs]   = bs
concat bss0   = unsafeCreate totalLen $ \ptr -> go bss0 ptr
  where
    totalLen = List.sum [ len | (PS _ _ len) <- bss0 ]
    go []                  !_   = return ()
    go (PS fp off len:bss) !ptr = do
      withForeignPtr fp $ \p -> memcpy ptr (p `plusPtr` off) len
      go bss (ptr `plusPtr` len)

------------------------------------------------------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
#if !defined(__GLASGOW_HASKELL__)
w2c = chr . fromIntegral
#else
w2c = unsafeChr . fromIntegral
#endif
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- | Selects words corresponding to white-space characters in the Latin-1 range
-- ordered by frequency.
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w =
    w == 0x20 ||
    w == 0x0A || -- LF, \n
    w == 0x09 || -- HT, \t
    w == 0x0C || -- FF, \f
    w == 0x0D || -- CR, \r
    w == 0x0B || -- VT, \v
    w == 0xA0    -- spotted by QC..
{-# INLINE isSpaceWord8 #-}

-- | Selects white-space characters in the Latin-1 range
isSpaceChar8 :: Char -> Bool
isSpaceChar8 c =
    c == ' '     ||
    c == '\t'    ||
    c == '\n'    ||
    c == '\r'    ||
    c == '\f'    ||
    c == '\v'    ||
    c == '\xa0'
{-# INLINE isSpaceChar8 #-}

------------------------------------------------------------------------

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

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
memchr p w s = c_memchr p (fromIntegral w) s

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO CInt
memcmp p q s = c_memcmp p q (fromIntegral s)

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = c_memcpy p q (fromIntegral s) >> return ()

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
memset p w s = c_memset p (fromIntegral w) s

-- ---------------------------------------------------------------------
--
-- Uses our C code
--

foreign import ccall unsafe "static fpstring.h fps_reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> IO ()

foreign import ccall unsafe "static fpstring.h fps_intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> Word8 -> IO ()

foreign import ccall unsafe "static fpstring.h fps_maximum" c_maximum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static fpstring.h fps_minimum" c_minimum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static fpstring.h fps_count" c_count
    :: Ptr Word8 -> CULong -> Word8 -> IO CULong

