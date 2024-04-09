{-# LANGUAGE CPP #-}

#include "bytestring-cpp-macros.h"

-- |
-- Module      : Data.ByteString.Utils.UnalignedAccess
-- Copyright   : (c) Matthew Craven 2023-2024
-- License     : BSD-style
-- Maintainer  : clyring@gmail.com
-- Stability   : internal
-- Portability : non-portable
--
-- Primitives for reading and writing at potentially-unaligned memory locations

module Data.ByteString.Utils.UnalignedAccess
  ( unalignedWriteU16
  , unalignedWriteU32
  , unalignedWriteU64
  , unalignedWriteFloat
  , unalignedWriteDouble
  , unalignedReadU64
  ) where

import Foreign.Ptr
import Data.Word


#if HS_UNALIGNED_ADDR_PRIMOPS_AVAILABLE
import GHC.IO (IO(..))
import GHC.Word (Word16(..), Word32(..), Word64(..))
import GHC.Exts

unalignedWriteU16 :: Word16 -> Ptr Word8 -> IO ()
unalignedWriteU16 = coerce $ \(W16# x#) (Ptr p#) s
  -> (# writeWord8OffAddrAsWord16# p# 0# x# s, () #)

unalignedWriteU32 :: Word32 -> Ptr Word8 -> IO ()
unalignedWriteU32 = coerce $ \(W32# x#) (Ptr p#) s
  -> (# writeWord8OffAddrAsWord32# p# 0# x# s, () #)

unalignedWriteU64 :: Word64 -> Ptr Word8 -> IO ()
unalignedWriteU64 = coerce $ \(W64# x#) (Ptr p#) s
  -> (# writeWord8OffAddrAsWord64# p# 0# x# s, () #)

unalignedWriteFloat :: Float -> Ptr Word8 -> IO ()
unalignedWriteFloat = coerce $ \(F# x#) (Ptr p#) s
  -> (# writeWord8OffAddrAsFloat# p# 0# x# s, () #)

unalignedWriteDouble :: Double -> Ptr Word8 -> IO ()
unalignedWriteDouble = coerce $ \(D# x#) (Ptr p#) s
  -> (# writeWord8OffAddrAsDouble# p# 0# x# s, () #)

unalignedReadU64 :: Ptr Word8 -> IO Word64
unalignedReadU64 = coerce $ \(Ptr p#) s
  -> case readWord8OffAddrAsWord64# p# 0# s of
       (# s', w64# #) -> (# s', W64# w64# #)

#elif HS_UNALIGNED_POKES_OK
import Foreign.Storable

unalignedWriteU16 :: Word16 -> Ptr Word8 -> IO ()
unalignedWriteU16 x p = poke (castPtr p) x

unalignedWriteU32 :: Word32 -> Ptr Word8 -> IO ()
unalignedWriteU32 x p = poke (castPtr p) x

unalignedWriteU64 :: Word64 -> Ptr Word8 -> IO ()
unalignedWriteU64 x p = poke (castPtr p) x

unalignedWriteFloat :: Float -> Ptr Word8 -> IO ()
unalignedWriteFloat x p = poke (castPtr p) x

unalignedWriteDouble :: Double -> Ptr Word8 -> IO ()
unalignedWriteDouble x p = poke (castPtr p) x

unalignedReadU64 :: Ptr Word8 -> IO Word64
unalignedReadU64 p = peek (castPtr p)

#else
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_u16"
  unalignedWriteU16 :: Word16 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_u32"
  unalignedWriteU32 :: Word32 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_u64"
  unalignedWriteU64 :: Word64 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_HsFloat"
  unalignedWriteFloat :: Float -> Ptr Word8 -> IO ()
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_HsDouble"
  unalignedWriteDouble :: Double -> Ptr Word8 -> IO ()
foreign import ccall unsafe "static fpstring.h fps_unaligned_read_u64"
  unalignedReadU64 :: Ptr Word8 -> IO Word64
#endif

