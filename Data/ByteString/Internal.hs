{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-- |
-- Module      : Data.ByteString.Internal
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
-- 
-- A module containing semi-public 'ByteString' internals. This exposes
-- the 'ByteString' representation and low level construction functions.
-- Modules which extend the 'ByteString' system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.ByteString.Internal (

        -- * The @ByteString@ type and representation
        ByteString(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Low level introduction and elimination
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
        createAndTrim,          -- :: Int -> (Ptr Word8 -> IO Int) -> IO  ByteString
        createAndTrim',         -- :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
        unsafeCreate,           -- :: Int -> (Ptr Word8 -> IO ()) ->  ByteString
        mallocByteString,       -- :: Int -> IO (ForeignPtr a)

        -- * Conversion to and from ForeignPtrs
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)

        -- * Utilities
        inlinePerformIO,            -- :: IO a -> a
        nullForeignPtr,             -- :: ForeignPtr Word8

        countOccurrences,           -- :: (Storable a, Num a) => Ptr a -> Ptr Word8 -> Int -> IO ()

        -- * Standard C Functions
        c_strlen,                   -- :: CString -> IO CInt
        c_free_finalizer,           -- :: FunPtr (Ptr Word8 -> IO ())

        memchr,                     -- :: Ptr Word8 -> Word8 -> CSize -> IO Ptr Word8
        memcmp,                     -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
        memcpy,                     -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
        memmove,                    -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
        memset,                     -- :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

        -- * cbits functions
        c_reverse,                  -- :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()
        c_intersperse,              -- :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()
        c_maximum,                  -- :: Ptr Word8 -> CInt -> IO Word8
        c_minimum,                  -- :: Ptr Word8 -> CInt -> IO Word8
        c_count,                    -- :: Ptr Word8 -> CInt -> Word8 -> IO CInt

#if defined(__GLASGOW_HASKELL__)
        -- * Internal GHC magic
        memcpy_ptr_baoff,           -- :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif

        -- * Chars
        w2c, c2w, isSpaceWord8

  ) where

import Foreign.ForeignPtr       (ForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, FunPtr, plusPtr)
import Foreign.Storable         (Storable(..))
import Foreign.C.Types          (CInt, CSize, CULong)
import Foreign.C.String         (CString)

#ifndef __NHC__
import Control.Exception        (assert)
#endif

import Data.Char                (ord)
import Data.Word                (Word8)

#if defined(__GLASGOW_HASKELL__)
import Data.Generics            (Data(..), Typeable(..))
import GHC.Ptr                  (Ptr(..))
import GHC.Base                 (realWorld#,unsafeChr)
import GHC.IOBase               (IO(IO), unsafePerformIO, RawBuffer)
#else
import Data.Char                (chr)
import System.IO.Unsafe         (unsafePerformIO)
#endif

#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)
#else
import Foreign.ForeignPtr       (mallocForeignPtrBytes)
#endif

#if __GLASGOW_HASKELL__>=605
import GHC.ForeignPtr           (ForeignPtr(ForeignPtr))
import GHC.Base                 (nullAddr#)
#else
import Foreign.Ptr              (nullPtr)
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

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length

#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackWith w2c ps) r

instance Read ByteString where
    readsPrec p str = [ (packWith c2w x, y) | (x, y) <- readsPrec p str ]

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith _ (PS _  _ 0) = []
unpackWith k (PS ps s l) = inlinePerformIO $ withForeignPtr ps $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        STRICT3(go)
        go p 0 acc = peek p          >>= \e -> return (k e : acc)
        go p n acc = peekByteOff p n >>= \e -> go p (n-1) (k e : acc)
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}

-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = unsafeCreate (length str) $ \p -> go p str
    where
        STRICT2(go)
        go _ []     = return ()
        go p (x:xs) = poke p (k x) >> go (p `plusPtr` 1) xs -- less space than pokeElemOff
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}

------------------------------------------------------------------------

nullForeignPtr :: ForeignPtr Word8
#if __GLASGOW_HASKELL__>=605
nullForeignPtr = ForeignPtr nullAddr# undefined --TODO: should ForeignPtrContents be strict?
#else
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr
{-# NOINLINE nullForeignPtr #-}
#endif

-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(1)/ Build a ByteString from a ForeignPtr
fromForeignPtr :: ForeignPtr Word8 -> Int -> Int -> ByteString
fromForeignPtr fp s l = PS fp s l

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int)
toForeignPtr (PS ps s l) = (ps, s, l)

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike
-- 'createAndTrim' the ByteString is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> ByteString
unsafeCreate l f = unsafePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> f p
    return $! PS fp 0 l

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
            else create l' $ \p' -> memcpy p' p (fromIntegral l')

createAndTrim' :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
createAndTrim' l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> do
        (off, l', res) <- f p
        if assert (l' <= l) $ l' >= l
            then return $! (PS fp 0 l, res)
            else do ps <- create l' $ \p' ->
                            memcpy p' (p `plusPtr` off) (fromIntegral l')
                    return $! (ps, res)

-- | Wrapper of mallocForeignPtrBytes with faster implementation
-- for GHC 6.5 builds newer than 06/06/06
mallocByteString :: Int -> IO (ForeignPtr a)
mallocByteString l = do
#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
    mallocPlainForeignPtrBytes l
#else
    mallocForeignPtrBytes l
#endif

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

-- Selects white-space characters in the Latin-1 range
-- ordered by frequency
-- Idea from Ketil
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w = case w of
    0x20 -> True -- SPACE
    0x0A -> True -- LF, \n
    0x09 -> True -- HT, \t
    0x0C -> True -- FF, \f
    0x0D -> True -- CR, \r
    0x0B -> True -- VT, \v
    0xA0 -> True -- spotted by QC..
    _    -> False
{-# INLINE isSpaceWord8 #-}

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

-- | Count the number of occurrences of each byte.
--
countOccurrences :: (Storable a, Num a) => Ptr a -> Ptr Word8 -> Int -> IO ()
STRICT3(countOccurrences)
countOccurrences counts str l = go 0
 where
    STRICT1(go)
    go i | i == l    = return ()
         | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                          x <- peekElemOff counts k
                          pokeElemOff counts k (x + 1)
                          go (i + 1)
{-# SPECIALIZE countOccurrences :: Ptr CSize -> Ptr Word8 -> Int -> IO () #-}

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

foreign import ccall unsafe "string.h memcmp" memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memcpy p q s = do c_memcpy p q s
                  return ()

foreign import ccall unsafe "string.h memmove" c_memmove
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memmove :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
memmove p q s = do c_memmove p q s
                   return ()

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

-- ---------------------------------------------------------------------
-- Internal GHC Haskell magic

#if defined(__GLASGOW_HASKELL__)
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif
