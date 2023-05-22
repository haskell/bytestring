{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE TypeApplications #-}

-- | Copyright   : (c) 2010-2011 Simon Meier
-- License       : BSD3-style (see LICENSE)
--
-- Maintainer    : Simon Meier <iridcode@gmail.com>
-- Portability   : GHC
--
module Data.ByteString.Builder.Prim.Binary (

  -- ** Binary encodings
    int8
  , word8

  -- *** Big-endian
  , int16BE
  , int32BE
  , int64BE

  , word16BE
  , word32BE
  , word64BE

  , floatBE
  , doubleBE

  -- *** Little-endian
  , int16LE
  , int32LE
  , int64LE

  , word16LE
  , word32LE
  , word64LE

  , floatLE
  , doubleLE

  -- *** Non-portable, host-dependent
  , intHost
  , int16Host
  , int32Host
  , int64Host

  , wordHost
  , word16Host
  , word32Host
  , word64Host

  , floatHost
  , doubleHost

  ) where

import Data.ByteString.Builder.Prim.Internal
import Data.ByteString.Builder.Prim.Internal.Floating

import Foreign

#include "MachDeps.h"
#include "bytestring-cpp-macros.h"

------------------------------------------------------------------------------
-- Binary encoding
------------------------------------------------------------------------------

-- Word encodings
-----------------

-- | Encoding single unsigned bytes as-is.
--
{-# INLINE word8 #-}
word8 :: FixedPrim Word8
word8 = fixedPrim 1 (flip poke) -- Word8 is always aligned

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Encoding 'Word16's in big endian format.
{-# INLINE word16BE #-}
word16BE :: FixedPrim Word16
#ifdef WORDS_BIGENDIAN
word16BE = word16Host
#else
word16BE = byteSwap16 >$< word16Host
#endif

-- | Encoding 'Word16's in little endian format.
{-# INLINE word16LE #-}
word16LE :: FixedPrim Word16
#ifdef WORDS_BIGENDIAN
word16LE = byteSwap16 >$< word16Host
#else
word16LE = word16Host
#endif

-- | Encoding 'Word32's in big endian format.
{-# INLINE word32BE #-}
word32BE :: FixedPrim Word32
#ifdef WORDS_BIGENDIAN
word32BE = word32Host
#else
word32BE = byteSwap32 >$< word32Host
#endif

-- | Encoding 'Word32's in little endian format.
{-# INLINE word32LE #-}
word32LE :: FixedPrim Word32
#ifdef WORDS_BIGENDIAN
word32LE = byteSwap32 >$< word32Host
#else
word32LE = word32Host
#endif

-- on a little endian machine:
-- word32LE w32 = fixedPrim 4 (\w p -> poke (castPtr p) w32)

-- | Encoding 'Word64's in big endian format.
{-# INLINE word64BE #-}
word64BE :: FixedPrim Word64
#ifdef WORDS_BIGENDIAN
word64BE = word64Host
#else
word64BE = byteSwap64 >$< word64Host
#endif

-- | Encoding 'Word64's in little endian format.
{-# INLINE word64LE #-}
word64LE :: FixedPrim Word64
#ifdef WORDS_BIGENDIAN
word64LE = byteSwap64 >$< word64Host
#else
word64LE = word64Host
#endif


-- | Encode a single native machine 'Word'. The 'Word's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: FixedPrim Word
wordHost = case finiteBitSize (0 :: Word) of
  32 -> fromIntegral @Word @Word32 >$< word32Host
  64 -> fromIntegral @Word @Word64 >$< word64Host
  _ -> error "Data.ByteString.Builder.Prim.Binary.wordHost: unexpected word size"

-- | Encoding 'Word16's in native host order and host endianness.
{-# INLINE word16Host #-}
word16Host :: FixedPrim Word16
word16Host = fixedPrim 2 unaligned_write_u16

-- | Encoding 'Word32's in native host order and host endianness.
{-# INLINE word32Host #-}
word32Host :: FixedPrim Word32
word32Host = fixedPrim 4 unaligned_write_u32

-- | Encoding 'Word64's in native host order and host endianness.
{-# INLINE word64Host #-}
word64Host :: FixedPrim Word64
word64Host = fixedPrim 8 unaligned_write_u64

#if HS_BYTESTRING_UNALIGNED_POKES_OK
unaligned_write_u16 :: Word16 -> Ptr Word8 -> IO ()
unaligned_write_u16 x p = poke (castPtr p) x

unaligned_write_u32 :: Word32 -> Ptr Word8 -> IO ()
unaligned_write_u32 x p = poke (castPtr p) x

unaligned_write_u64 :: Word64 -> Ptr Word8 -> IO ()
unaligned_write_u64 x p = poke (castPtr p) x
#else
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_u16"
  unaligned_write_u16 :: Word16 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_u32"
  unaligned_write_u32 :: Word32 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "static fpstring.h fps_unaligned_write_u64"
  unaligned_write_u64 :: Word64 -> Ptr Word8 -> IO ()
#endif

------------------------------------------------------------------------------
-- Int encodings
------------------------------------------------------------------------------
--
-- We rely on 'fromIntegral' to do a loss-less conversion to the corresponding
-- 'Word' type
--
------------------------------------------------------------------------------

-- | Encoding single signed bytes as-is.
--
{-# INLINE int8 #-}
int8 :: FixedPrim Int8
int8 = fromIntegral >$< word8

-- | Encoding 'Int16's in big endian format.
{-# INLINE int16BE #-}
int16BE :: FixedPrim Int16
int16BE = fromIntegral >$< word16BE

-- | Encoding 'Int16's in little endian format.
{-# INLINE int16LE #-}
int16LE :: FixedPrim Int16
int16LE = fromIntegral >$< word16LE

-- | Encoding 'Int32's in big endian format.
{-# INLINE int32BE #-}
int32BE :: FixedPrim Int32
int32BE = fromIntegral >$< word32BE

-- | Encoding 'Int32's in little endian format.
{-# INLINE int32LE #-}
int32LE :: FixedPrim Int32
int32LE = fromIntegral >$< word32LE

-- | Encoding 'Int64's in big endian format.
{-# INLINE int64BE #-}
int64BE :: FixedPrim Int64
int64BE = fromIntegral >$< word64BE

-- | Encoding 'Int64's in little endian format.
{-# INLINE int64LE #-}
int64LE :: FixedPrim Int64
int64LE = fromIntegral >$< word64LE


-- | Encode a single native machine 'Int'. The 'Int's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: FixedPrim Int
intHost = fromIntegral @Int @Word >$< wordHost

-- | Encoding 'Int16's in native host order and host endianness.
{-# INLINE int16Host #-}
int16Host :: FixedPrim Int16
int16Host = fromIntegral @Int16 @Word16 >$< word16Host

-- | Encoding 'Int32's in native host order and host endianness.
{-# INLINE int32Host #-}
int32Host :: FixedPrim Int32
int32Host = fromIntegral @Int32 @Word32 >$< word32Host

-- | Encoding 'Int64's in native host order and host endianness.
{-# INLINE int64Host #-}
int64Host :: FixedPrim Int64
int64Host = fromIntegral @Int64 @Word64 >$< word64Host

-- IEEE Floating Point Numbers
------------------------------

-- | Encode a 'Float' in big endian format.
{-# INLINE floatBE #-}
floatBE :: FixedPrim Float
floatBE = encodeFloatViaWord32F word32BE

-- | Encode a 'Float' in little endian format.
{-# INLINE floatLE #-}
floatLE :: FixedPrim Float
floatLE = encodeFloatViaWord32F word32LE

-- | Encode a 'Double' in big endian format.
{-# INLINE doubleBE #-}
doubleBE :: FixedPrim Double
doubleBE = encodeDoubleViaWord64F word64BE

-- | Encode a 'Double' in little endian format.
{-# INLINE doubleLE #-}
doubleLE :: FixedPrim Double
doubleLE = encodeDoubleViaWord64F word64LE


-- | Encode a 'Float' in native host order and host endianness. Values written
-- this way are not portable to different endian machines, without conversion.
--
{-# INLINE floatHost #-}
floatHost :: FixedPrim Float
floatHost = storableToF

-- | Encode a 'Double' in native host order and host endianness.
{-# INLINE doubleHost #-}
doubleHost :: FixedPrim Double
doubleHost = storableToF
