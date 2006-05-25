{-# OPTIONS_GHC -cpp -fffi -fglasgow-exts #-}
--
-- Module      : ByteString.Internal
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
-- 

-- | A library of functions useful to several modules in the package,
-- which are not suitable for exposure to the user.
--
module Data.ByteString.Internal (

    -- * Utilities
    inlinePerformIO,            -- :: IO a -> a

    countOccurrences,           -- :: (Storable a, Num a) => Ptr a -> Ptr Word8 -> Int -> IO ()

    -- * Standard C Functions
    c_strlen,                   -- :: CString -> CInt
    c_malloc,                   -- :: CInt -> IO (Ptr Word8)
    c_free,                     -- :: Ptr Word8 -> IO ()

#if !defined(__GLASGOW_HASKELL__)
    c_free_finalizer,           -- :: FunPtr (Ptr Word8 -> IO ())
#endif

    memchr,                     -- :: Ptr Word8 -> Word8 -> CSize -> Ptr Word8
    memcmp,                     -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
    memcpy,                     -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
    memset,                     -- :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

    -- * cbits functions

    c_reverse,                  -- :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()
    c_intersperse,              -- :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()
    c_maximum,                  -- :: Ptr Word8 -> CInt -> Word8
    c_minimum,                  -- :: Ptr Word8 -> CInt -> Word8
    c_count,                    -- :: Ptr Word8 -> CInt -> Word8 -> CInt

    -- * Internal GHC magic
#if defined(__GLASGOW_HASKELL__)
    getProgArgv,                -- :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
    memcpy_ptr_baoff,           -- :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif

    -- * Chars
    w2c, c2w,
    isSpaceWord8

  ) where

import Data.Char                (ord)
import Data.Word                (Word8)
import Foreign.C.Types
import Foreign.C.String         (CString)
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

#if defined(__GLASGOW_HASKELL__)
import GHC.Base                 (realWorld#,unsafeChr)
import GHC.IOBase
#else
import Data.Char                (chr)
import System.IO.Unsafe         (unsafePerformIO)
#endif

-- CFILES stuff is Hugs only
{-# CFILES cbits/fpstring.c #-}

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined

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
-- it exposes lots of things to further inlining
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
{-# SPECIALIZE countOccurrences :: Ptr CSize -> Ptr Word8 -> Int -> IO () #-}
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

-- ---------------------------------------------------------------------
-- 
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> CInt

foreign import ccall unsafe "stdlib.h malloc" c_malloc
    :: CInt -> IO (Ptr Word8)

foreign import ccall unsafe "static stdlib.h free" c_free
    :: Ptr Word8 -> IO ()

#if !defined(__GLASGOW_HASKELL__)
foreign import ccall unsafe "static stdlib.h &free" c_free_finalizer
    :: FunPtr (Ptr Word8 -> IO ())
#endif

foreign import ccall unsafe "string.h memchr" memchr
    :: Ptr Word8 -> Word8 -> CSize -> Ptr Word8

foreign import ccall unsafe "string.h memcmp" memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "string.h memcpy" memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "string.h memset" memset
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)


-- ---------------------------------------------------------------------
--
-- Uses our C code
--

foreign import ccall unsafe "static fpstring.h reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()

foreign import ccall unsafe "static fpstring.h intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()

foreign import ccall unsafe "static fpstring.h maximum" c_maximum
    :: Ptr Word8 -> CInt -> Word8

foreign import ccall unsafe "static fpstring.h minimum" c_minimum
    :: Ptr Word8 -> CInt -> Word8

foreign import ccall unsafe "static fpstring.h count" c_count
    :: Ptr Word8 -> CInt -> Word8 -> CInt

-- ---------------------------------------------------------------------
-- MMap

{-
foreign import ccall unsafe "static fpstring.h my_mmap" my_mmap
    :: Int -> Int -> IO (Ptr Word8)

foreign import ccall unsafe "static unistd.h close" c_close
    :: Int -> IO Int

#  if !defined(__OpenBSD__)
foreign import ccall unsafe "static sys/mman.h munmap" c_munmap
    :: Ptr Word8 -> Int -> IO Int
#  endif
-}

-- ---------------------------------------------------------------------
-- Internal GHC Haskell magic

#if defined(__GLASGOW_HASKELL__)
foreign import ccall unsafe "RtsAPI.h getProgArgv"
    getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif
