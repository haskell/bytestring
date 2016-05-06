{-# LANGUAGE CPP, BangPatterns #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
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
  , int16be
  , int32be
  , int64be

  , word16be
  , word32be
  , word64be

  , float32be
  , float64be

  -- *** Little-endian
  , int16le
  , int32le
  , int64le

  , word16le
  , word32le
  , word64le

  , float32le
  , float64le

  -- *** Non-portable, host-dependent
  , intHost
  , int16host
  , int32host
  , int64host

  , wordHost
  , word16host
  , word32host
  , word64host

  , float32host
  , float64host

  ) where

import Data.ByteString.Builder.Prim.Internal
import Data.ByteString.Builder.Prim.Internal.UncheckedShifts
import Data.ByteString.Builder.Prim.Internal.Floating

import Foreign

#include "MachDeps.h"

------------------------------------------------------------------------------
-- Binary encoding
------------------------------------------------------------------------------

-- Word encodings
-----------------

-- | Encoding single unsigned bytes as-is.
--
{-# INLINE word8 #-}
word8 :: FixedPrim Word8
word8 = storableToF

--
-- We rely on the fromIntegral to do the right masking for us.
-- The inlining here is critical, and can be worth 4x performance
--

-- | Encoding 'Word16's in big endian format.
{-# INLINE word16be #-}
word16be :: FixedPrim Word16
#ifdef WORD_BIGENDIAN
word16be = word16host
#else
word16be = fixedPrim 2 $ \w p -> do
    poke p               (fromIntegral (shiftr_w16 w 8) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (w)              :: Word8)
#endif

-- | Encoding 'Word16's in little endian format.
{-# INLINE word16le #-}
word16le :: FixedPrim Word16
#ifdef WORD_BIGENDIAN
word16le = fixedPrim 2 $ \w p -> do
    poke p               (fromIntegral (w)              :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w16 w 8) :: Word8)
#else
word16le = word16host
#endif

-- | Encoding 'Word32's in big endian format.
{-# INLINE word32be #-}
word32be :: FixedPrim Word32
#ifdef WORD_BIGENDIAN
word32be = word32host
#else
word32be = fixedPrim 4 $ \w p -> do
    poke p               (fromIntegral (shiftr_w32 w 24) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (w)               :: Word8)
#endif

-- | Encoding 'Word32's in little endian format.
{-# INLINE word32le #-}
word32le :: FixedPrim Word32
#ifdef WORD_BIGENDIAN
word32le = fixedPrim 4 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 w 24) :: Word8)
#else
word32le = word32host
#endif

-- on a little endian machine:
-- word32le w32 = fixedPrim 4 (\w p -> poke (castPtr p) w32)

-- | Encoding 'Word64's in big endian format.
{-# INLINE word64be #-}
word64be :: FixedPrim Word64
#ifdef WORD_BIGENDIAN
word64be = word64host
#else
#if WORD_SIZE_IN_BITS < 64
--
-- To avoid expensive 64 bit shifts on 32 bit machines, we cast to
-- Word32, and write that
--
word64be =
    fixedPrim 8 $ \w p -> do
        let a = fromIntegral (shiftr_w64 w 32) :: Word32
            b = fromIntegral w                 :: Word32
        poke p               (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (shiftr_w32 b 24) :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (b)               :: Word8)
#else
word64be = fixedPrim 8 $ \w p -> do
    poke p               (fromIntegral (shiftr_w64 w 56) :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (w)               :: Word8)
#endif
#endif

-- | Encoding 'Word64's in little endian format.
{-# INLINE word64le #-}
word64le :: FixedPrim Word64
#ifdef WORD_BIGENDIAN
#if WORD_SIZE_IN_BITS < 64
word64le =
    fixedPrim 8 $ \w p -> do
        let b = fromIntegral (shiftr_w64 w 32) :: Word32
            a = fromIntegral w                 :: Word32
        poke (p)             (fromIntegral (a)               :: Word8)
        poke (p `plusPtr` 1) (fromIntegral (shiftr_w32 a  8) :: Word8)
        poke (p `plusPtr` 2) (fromIntegral (shiftr_w32 a 16) :: Word8)
        poke (p `plusPtr` 3) (fromIntegral (shiftr_w32 a 24) :: Word8)
        poke (p `plusPtr` 4) (fromIntegral (b)               :: Word8)
        poke (p `plusPtr` 5) (fromIntegral (shiftr_w32 b  8) :: Word8)
        poke (p `plusPtr` 6) (fromIntegral (shiftr_w32 b 16) :: Word8)
        poke (p `plusPtr` 7) (fromIntegral (shiftr_w32 b 24) :: Word8)
#else
word64le = fixedPrim 8 $ \w p -> do
    poke p               (fromIntegral (w)               :: Word8)
    poke (p `plusPtr` 1) (fromIntegral (shiftr_w64 w  8) :: Word8)
    poke (p `plusPtr` 2) (fromIntegral (shiftr_w64 w 16) :: Word8)
    poke (p `plusPtr` 3) (fromIntegral (shiftr_w64 w 24) :: Word8)
    poke (p `plusPtr` 4) (fromIntegral (shiftr_w64 w 32) :: Word8)
    poke (p `plusPtr` 5) (fromIntegral (shiftr_w64 w 40) :: Word8)
    poke (p `plusPtr` 6) (fromIntegral (shiftr_w64 w 48) :: Word8)
    poke (p `plusPtr` 7) (fromIntegral (shiftr_w64 w 56) :: Word8)
#endif
#else
word64le = word64host
#endif


-- | Encode a single native machine 'Word'. The 'Word's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Word'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or word sized machines, without
-- conversion.
--
{-# INLINE wordHost #-}
wordHost :: FixedPrim Word
wordHost = storableToF

-- | Encoding 'Word16's in native host order and host endianness.
{-# INLINE word16host #-}
word16host :: FixedPrim Word16
word16host = storableToF

-- | Encoding 'Word32's in native host order and host endianness.
{-# INLINE word32host #-}
word32host :: FixedPrim Word32
word32host = storableToF

-- | Encoding 'Word64's in native host order and host endianness.
{-# INLINE word64host #-}
word64host :: FixedPrim Word64
word64host = storableToF


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
{-# INLINE int16be #-}
int16be :: FixedPrim Int16
int16be = fromIntegral >$< word16be

-- | Encoding 'Int16's in little endian format.
{-# INLINE int16le #-}
int16le :: FixedPrim Int16
int16le = fromIntegral >$< word16le

-- | Encoding 'Int32's in big endian format.
{-# INLINE int32be #-}
int32be :: FixedPrim Int32
int32be = fromIntegral >$< word32be

-- | Encoding 'Int32's in little endian format.
{-# INLINE int32le #-}
int32le :: FixedPrim Int32
int32le = fromIntegral >$< word32le

-- | Encoding 'Int64's in big endian format.
{-# INLINE int64be #-}
int64be :: FixedPrim Int64
int64be = fromIntegral >$< word64be

-- | Encoding 'Int64's in little endian format.
{-# INLINE int64le #-}
int64le :: FixedPrim Int64
int64le = fromIntegral >$< word64le


-- TODO: Ensure that they are safe on architectures where an unaligned write is
-- an error.

-- | Encode a single native machine 'Int'. The 'Int's is encoded in host order,
-- host endian form, for the machine you are on. On a 64 bit machine the 'Int'
-- is an 8 byte value, on a 32 bit machine, 4 bytes. Values encoded this way
-- are not portable to different endian or integer sized machines, without
-- conversion.
--
{-# INLINE intHost #-}
intHost :: FixedPrim Int
intHost = storableToF

-- | Encoding 'Int16's in native host order and host endianness.
{-# INLINE int16host #-}
int16host :: FixedPrim Int16
int16host = storableToF

-- | Encoding 'Int32's in native host order and host endianness.
{-# INLINE int32host #-}
int32host :: FixedPrim Int32
int32host = storableToF

-- | Encoding 'Int64's in native host order and host endianness.
{-# INLINE int64host #-}
int64host :: FixedPrim Int64
int64host = storableToF

-- IEEE Floating Point Numbers
------------------------------

-- | Encode a 'Float' in big endian format.
{-# INLINE float32be #-}
float32be :: FixedPrim Float
float32be = encodeFloatViaWord32F word32be

-- | Encode a 'Float' in little endian format.
{-# INLINE float32le #-}
float32le :: FixedPrim Float
float32le = encodeFloatViaWord32F word32le

-- | Encode a 'Double' in big endian format.
{-# INLINE float64be #-}
float64be :: FixedPrim Double
float64be = encodeDoubleViaWord64F word64be

-- | Encode a 'Double' in little endian format.
{-# INLINE float64le #-}
float64le :: FixedPrim Double
float64le = encodeDoubleViaWord64F word64le


-- | Encode a 'Float' in native host order and host endianness. Values written
-- this way are not portable to different endian machines, without conversion.
--
{-# INLINE float32host #-}
float32host :: FixedPrim Float
float32host = storableToF

-- | Encode a 'Double' in native host order and host endianness.
{-# INLINE float64host #-}
float64host :: FixedPrim Double
float64host = storableToF


