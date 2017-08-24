{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables, ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-- | Copyright   : (c) 2010 Jasper Van der Jeugt
--                 (c) 2010 - 2011 Simon Meier
-- License       : BSD3-style (see LICENSE)
--
-- Maintainer    : Simon Meier <iridcode@gmail.com>
-- Portability   : GHC
--
-- Encodings using ASCII encoded Unicode characters.
--
module Data.ByteString.Builder.Prim.ASCII
    (

     -- *** ASCII
     char7

      -- **** Decimal numbers
      -- | Decimal encoding of numbers using ASCII encoded characters.
    , int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , intDec

    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    , wordDec

    {-
    -- These are the functions currently provided by Bryan O'Sullivans
    -- double-conversion library.
    --
    -- , float
    -- , floatWith
    -- , double
    -- , doubleWith
    -}

      -- **** Hexadecimal numbers

      -- | Encoding positive integers as hexadecimal numbers using lower-case
      -- ASCII characters. The shortest possible representation is used. For
      -- example,
      --
      -- > toLazyByteString (primBounded word16Hex 0x0a10) = "a10"
      --
    , word8Hex
    , word16Hex
    , word32Hex
    , word64Hex
    , wordHex

      -- | Encoding positive integers as hexadecimal numbers using upper-case
      -- ASCII characters. The shortest possible representation is used. For
      -- example,
      --
      -- > toLazyByteString (primBounded word16Hex 0x0a10) = "A10"
      --
    , word8HexUpper
    , word16HexUpper
    , word32HexUpper
    , word64HexUpper
    , wordHexUpper

      -- **** Fixed-width hexadecimal numbers
      --
      -- | Encoding the bytes of fixed-width types as hexadecimal
      -- numbers using lower-case ASCII characters. For example,
      --
      -- > toLazyByteString (primFixed word16HexFixed 0x0a10) = "0a10"
      --
    , int8HexFixed
    , int16HexFixed
    , int32HexFixed
    , int64HexFixed
    , word8HexFixed
    , word16HexFixed
    , word32HexFixed
    , word64HexFixed

    , int32HexFixedWidth
    , int64HexFixedWidth
    , word32HexFixedWidth
    , word64HexFixedWidth

    , floatHexFixed
    , doubleHexFixed

      -- **** Fixed-width upper-case hexadecimal numbers
      --
      -- | Encoding the bytes of fixed-width types as hexadecimal
      -- numbers using upper-case ASCII characters. For example,
      --
      -- > toLazyByteString (primFixed word16HexUpperFixed 0x0a10) = "0A10"
      --
    , int8HexUpperFixed
    , int16HexUpperFixed
    , int32HexUpperFixed
    , int64HexUpperFixed
    , word8HexUpperFixed
    , word16HexUpperFixed
    , word32HexUpperFixed
    , word64HexUpperFixed

    , int32HexUpperFixedWidth
    , int64HexUpperFixedWidth
    , word32HexUpperFixedWidth
    , word64HexUpperFixedWidth

    , floatHexUpperFixed
    , doubleHexUpperFixed

    ) where

import Data.ByteString.Builder.Prim.Binary
import Data.ByteString.Builder.Prim.Internal
import Data.ByteString.Builder.Prim.Internal.Floating
import Data.ByteString.Builder.Prim.Internal.UncheckedShifts

import Data.Char (ord)

import Foreign
import Foreign.C.Types

-- | Encode the least 7-bits of a 'Char' using the ASCII encoding.
{-# INLINE char7 #-}
char7 :: FixedPrim Char
char7 = (\c -> fromIntegral $ ord c .&. 0x7f) >$< word8


------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

foreign import ccall unsafe "static _hs_bytestring_int_dec" c_int_dec
    :: CInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_int64_dec" c_int64_dec
    :: Int64 -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeIntDecimal #-}
encodeIntDecimal :: Integral a => Int -> BoundedPrim a
encodeIntDecimal bound = boudedPrim bound $ c_int_dec . fromIntegral

-- | Decimal encoding of an 'Int8'.
{-# INLINE int8Dec #-}
int8Dec :: BoundedPrim Int8
int8Dec = encodeIntDecimal 4

-- | Decimal encoding of an 'Int16'.
{-# INLINE int16Dec #-}
int16Dec :: BoundedPrim Int16
int16Dec = encodeIntDecimal 6


-- | Decimal encoding of an 'Int32'.
{-# INLINE int32Dec #-}
int32Dec :: BoundedPrim Int32
int32Dec = encodeIntDecimal 11

-- | Decimal encoding of an 'Int64'.
{-# INLINE int64Dec #-}
int64Dec :: BoundedPrim Int64
int64Dec = boudedPrim 20 $ c_int64_dec . fromIntegral

-- | Decimal encoding of an 'Int'.
{-# INLINE intDec #-}
intDec :: BoundedPrim Int
intDec = caseWordSize_32_64
    (fromIntegral >$< int32Dec)
    (fromIntegral >$< int64Dec)


-- Unsigned integers
--------------------

foreign import ccall unsafe "static _hs_bytestring_uint_dec" c_uint_dec
    :: CUInt -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_uint64_dec" c_uint64_dec
    :: Word64 -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordDecimal #-}
encodeWordDecimal :: Integral a => Int -> BoundedPrim a
encodeWordDecimal bound = boudedPrim bound $ c_uint_dec . fromIntegral

-- | Decimal encoding of a 'Word8'.
{-# INLINE word8Dec #-}
word8Dec :: BoundedPrim Word8
word8Dec = encodeWordDecimal 3

-- | Decimal encoding of a 'Word16'.
{-# INLINE word16Dec #-}
word16Dec :: BoundedPrim Word16
word16Dec = encodeWordDecimal 5

-- | Decimal encoding of a 'Word32'.
{-# INLINE word32Dec #-}
word32Dec :: BoundedPrim Word32
word32Dec = encodeWordDecimal 10

-- | Decimal encoding of a 'Word64'.
{-# INLINE word64Dec #-}
word64Dec :: BoundedPrim Word64
word64Dec = boudedPrim 20 $ c_uint64_dec . fromIntegral

-- | Decimal encoding of a 'Word'.
{-# INLINE wordDec #-}
wordDec :: BoundedPrim Word
wordDec = caseWordSize_32_64
    (fromIntegral >$< word32Dec)
    (fromIntegral >$< word64Dec)

------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

foreign import ccall unsafe "static _hs_bytestring_uint32_hex" c_uint32_hex
    :: Word32 -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_uint64_hex" c_uint64_hex
    :: Word64 -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordHex #-}
encodeWordHex :: forall a. (Storable a, Integral a) => BoundedPrim a
encodeWordHex =
    boudedPrim (2 * sizeOf (undefined :: a)) $ c_uint32_hex  . fromIntegral

-- | Hexadecimal encoding of a 'Word8'.
{-# INLINE word8Hex #-}
word8Hex :: BoundedPrim Word8
word8Hex = encodeWordHex

-- | Hexadecimal encoding of a 'Word16'.
{-# INLINE word16Hex #-}
word16Hex :: BoundedPrim Word16
word16Hex = encodeWordHex

-- | Hexadecimal encoding of a 'Word32'.
{-# INLINE word32Hex #-}
word32Hex :: BoundedPrim Word32
word32Hex = encodeWordHex

-- | Hexadecimal encoding of a 'Word64'.
{-# INLINE word64Hex #-}
word64Hex :: BoundedPrim Word64
word64Hex = boudedPrim 16 $ c_uint64_hex . fromIntegral

-- | Hexadecimal encoding of a 'Word'.
{-# INLINE wordHex #-}
wordHex :: BoundedPrim Word
wordHex = caseWordSize_32_64
    (fromIntegral >$< word32Hex)
    (fromIntegral >$< word64Hex)

foreign import ccall unsafe "static _hs_bytestring_uint32_hex_upper" c_uint32_hex_upper
    :: Word32 -> Ptr Word8 -> IO (Ptr Word8)

foreign import ccall unsafe "static _hs_bytestring_uint64_hex_upper" c_uint64_hex_upper
    :: Word64 -> Ptr Word8 -> IO (Ptr Word8)

{-# INLINE encodeWordHexUpper #-}
encodeWordHexUpper :: forall a. (Storable a, Integral a) => BoundedPrim a
encodeWordHexUpper =
    boudedPrim (2 * sizeOf (undefined :: a)) $ c_uint32_hex_upper  . fromIntegral

-- | Shortest hexadecimal encoding of a 'Word8' using upper-case characters.
{-# INLINE word8HexUpper #-}
word8HexUpper :: BoundedPrim Word8
word8HexUpper = encodeWordHexUpper

-- | Shortest hexadecimal encoding of a 'Word16' using upper-case characters.
{-# INLINE word16HexUpper #-}
word16HexUpper :: BoundedPrim Word16
word16HexUpper = encodeWordHexUpper

-- | Shortest hexadecimal encoding of a 'Word32' using upper-case characters.
{-# INLINE word32HexUpper #-}
word32HexUpper :: BoundedPrim Word32
word32HexUpper = encodeWordHexUpper

-- | Shortest hexadecimal encoding of a 'Word64' using upper-case characters.
{-# INLINE word64HexUpper #-}
word64HexUpper :: BoundedPrim Word64
word64HexUpper = boudedPrim 16 $ c_uint64_hex_upper . fromIntegral

-- | Shortest hexadecimal encoding of a 'Word' using upper-case characters.
{-# INLINE wordHexUpper #-}
wordHexUpper :: BoundedPrim Word
wordHexUpper = caseWordSize_32_64
    (fromIntegral >$< word32HexUpper)
    (fromIntegral >$< word64HexUpper)


-- fixed width; leading zeroes
------------------------------

foreign import ccall unsafe "static _hs_bytestring_builder_uint32_fixed_width_hex" c_uint32_fixed_hex
    :: CInt -> Word32 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static _hs_bytestring_builder_uint64_fixed_width_hex" c_uint64_fixed_hex
    :: CInt -> Word64 -> Ptr Word8 -> IO ()

{-# INLINE encodeWordHexFixedWidth #-}
encodeWordHexFixedWidth :: forall a. (Storable a, Integral a) => Int -> FixedPrim a
encodeWordHexFixedWidth width = fixedPrim width $ c_uint32_fixed_hex (CInt (fromIntegral width)) . fromIntegral

{-# INLINE encodeWordHexFixed #-}
encodeWordHexFixed :: forall a. (Storable a, Integral a) => FixedPrim a
encodeWordHexFixed = encodeWordHexFixedWidth (2 * sizeOf (undefined :: a))

{-# INLINE encodeWord64HexFixedWidth #-}
encodeWord64HexFixedWidth :: forall a. (Storable a, Integral a) => Int -> FixedPrim a
encodeWord64HexFixedWidth width = fixedPrim width $ c_uint64_fixed_hex (CInt (fromIntegral width)) . fromIntegral

-- | Hexadecimal encoding of an 'Int8' using 2 lower-case characters.
{-# INLINE int8HexFixed #-}
int8HexFixed :: FixedPrim Int8
int8HexFixed = fromIntegral >$< word8HexFixed

-- | Hexadecimal encoding of an 'Int16' using 4 lower-case characters.
{-# INLINE int16HexFixed #-}
int16HexFixed :: FixedPrim Int16
int16HexFixed = fromIntegral >$< word16HexFixed

-- | Hexadecimal encoding of an 'Int32' using 8 lower-case characters.
{-# INLINE int32HexFixed #-}
int32HexFixed :: FixedPrim Int32
int32HexFixed = fromIntegral >$< word32HexFixed

-- | Hexadecimal encoding of an 'Int64' using 16 lower-case characters.
{-# INLINE int64HexFixed #-}
int64HexFixed :: FixedPrim Int64
int64HexFixed = fromIntegral >$< word64HexFixed

-- | Hexadecimal encoding of a 'Word8' using 2 lower-case characters.
{-# INLINE word8HexFixed #-}
word8HexFixed :: FixedPrim Word8
word8HexFixed = encodeWordHexFixed

-- | Hexadecimal encoding of a 'Word16' using 4 lower-case characters.
{-# INLINE word16HexFixed #-}
word16HexFixed :: FixedPrim Word16
word16HexFixed = encodeWordHexFixed

-- | Hexadecimal encoding of a 'Word32' using 8 lower-case characters.
{-# INLINE word32HexFixed #-}
word32HexFixed :: FixedPrim Word32
word32HexFixed = encodeWordHexFixed

-- | Hexadecimal encoding of a 'Word64' using 16 lower-case characters.
{-# INLINE word64HexFixed #-}
word64HexFixed :: FixedPrim Word64
word64HexFixed = encodeWord64HexFixedWidth 16

-- | Hexadecimal encoding of an 'Int32' using a specified number of
--   lower-case characters.
{-# INLINE int32HexFixedWidth #-}
int32HexFixedWidth :: Int -> FixedPrim Int32
int32HexFixedWidth width = fromIntegral >$< word32HexFixedWidth width

-- | Hexadecimal encoding of an 'Int64' using a specified number of
--   lower-case characters.
{-# INLINE int64HexFixedWidth #-}
int64HexFixedWidth :: Int -> FixedPrim Int64
int64HexFixedWidth width = fromIntegral >$< word64HexFixedWidth width

-- | Hexadecimal encoding of a 'Word32' using a specified number of
--   lower-case characters.
{-# INLINE word32HexFixedWidth #-}
word32HexFixedWidth :: Int -> FixedPrim Word32
word32HexFixedWidth = encodeWordHexFixedWidth

-- | Hexadecimal encoding of a 'Word64' using a specified number of
--   lower-case characters.
{-# INLINE word64HexFixedWidth #-}
word64HexFixedWidth :: Int -> FixedPrim Word64
word64HexFixedWidth = encodeWord64HexFixedWidth

-- | Encode an IEEE 'Float' using 8 lower-case hexadecimal digits.
{-# INLINE floatHexFixed #-}
floatHexFixed :: FixedPrim Float
floatHexFixed = encodeFloatViaWord32F word32HexFixed

-- | Encode an IEEE 'Double' using 16 lower-case hexadecimal digits.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: FixedPrim Double
doubleHexFixed = encodeDoubleViaWord64F word64HexFixed


-- fixed width; leading zeroes; upper-case
------------------------------------------

foreign import ccall unsafe "static _hs_bytestring_builder_uint32_fixed_width_hex_upper" c_uint32_fixed_hex_upper
    :: CInt -> Word32 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static _hs_bytestring_builder_uint64_fixed_width_hex_upper" c_uint64_fixed_hex_upper
    :: CInt -> Word64 -> Ptr Word8 -> IO ()

{-# INLINE encodeWordHexUpperFixedWidth #-}
encodeWordHexUpperFixedWidth :: forall a. (Storable a, Integral a) => Int -> FixedPrim a
encodeWordHexUpperFixedWidth width = fixedPrim width $ c_uint32_fixed_hex_upper (CInt (fromIntegral width)) . fromIntegral

{-# INLINE encodeWordHexUpperFixed #-}
encodeWordHexUpperFixed :: forall a. (Storable a, Integral a) => FixedPrim a
encodeWordHexUpperFixed = encodeWordHexUpperFixedWidth (2 * sizeOf (undefined :: a))

{-# INLINE encodeWord64HexUpperFixedWidth #-}
encodeWord64HexUpperFixedWidth :: forall a. (Storable a, Integral a) => Int -> FixedPrim a
encodeWord64HexUpperFixedWidth width = fixedPrim width $ c_uint64_fixed_hex_upper (CInt (fromIntegral width)) . fromIntegral

-- | Hexadecimal encoding of an 'Int8' using 2 upper-case characters.
{-# INLINE int8HexUpperFixed #-}
int8HexUpperFixed :: FixedPrim Int8
int8HexUpperFixed = fromIntegral >$< word8HexUpperFixed

-- | Hexadecimal encoding of an 'Int16' using 4 upper-case characters.
{-# INLINE int16HexUpperFixed #-}
int16HexUpperFixed :: FixedPrim Int16
int16HexUpperFixed = fromIntegral >$< word16HexUpperFixed

-- | Hexadecimal encoding of an 'Int32' using 8 upper-case characters.
{-# INLINE int32HexUpperFixed #-}
int32HexUpperFixed :: FixedPrim Int32
int32HexUpperFixed = fromIntegral >$< word32HexUpperFixed

-- | Hexadecimal encoding of an 'Int64' using 16 upper-case characters.
{-# INLINE int64HexUpperFixed #-}
int64HexUpperFixed :: FixedPrim Int64
int64HexUpperFixed = fromIntegral >$< word64HexUpperFixed

-- | Hexadecimal encoding of a 'Word8' using 2 upper-case characters.
{-# INLINE word8HexUpperFixed #-}
word8HexUpperFixed :: FixedPrim Word8
word8HexUpperFixed = encodeWordHexUpperFixed

-- | Hexadecimal encoding of a 'Word16' using 4 upper-case characters.
{-# INLINE word16HexUpperFixed #-}
word16HexUpperFixed :: FixedPrim Word16
word16HexUpperFixed = encodeWordHexUpperFixed

-- | Hexadecimal encoding of a 'Word32' using 8 upper-case characters.
{-# INLINE word32HexUpperFixed #-}
word32HexUpperFixed :: FixedPrim Word32
word32HexUpperFixed = encodeWordHexUpperFixed

-- | Hexadecimal encoding of a 'Word64' using 16 upper-case characters.
{-# INLINE word64HexUpperFixed #-}
word64HexUpperFixed :: FixedPrim Word64
word64HexUpperFixed = encodeWord64HexUpperFixedWidth 16

-- | Hexadecimal encoding of an 'Int32' using a specified number of
--   upper-case characters.
{-# INLINE int32HexUpperFixedWidth #-}
int32HexUpperFixedWidth :: Int -> FixedPrim Int32
int32HexUpperFixedWidth width = fromIntegral >$< word32HexUpperFixedWidth width

-- | Hexadecimal encoding of an 'Int64' using a specified number of
--   upper-case characters.
{-# INLINE int64HexUpperFixedWidth #-}
int64HexUpperFixedWidth :: Int -> FixedPrim Int64
int64HexUpperFixedWidth width = fromIntegral >$< word64HexUpperFixedWidth width

-- | Hexadecimal encoding of a 'Word32' using a specified number of
--   upper-case characters.
{-# INLINE word32HexUpperFixedWidth #-}
word32HexUpperFixedWidth :: Int -> FixedPrim Word32
word32HexUpperFixedWidth = encodeWordHexUpperFixedWidth

-- | Hexadecimal encoding of a 'Word64' using a specified number of
--   upper-case characters.
{-# INLINE word64HexUpperFixedWidth #-}
word64HexUpperFixedWidth :: Int -> FixedPrim Word64
word64HexUpperFixedWidth = encodeWord64HexUpperFixedWidth

-- | Encode an IEEE 'Float' using 8 upper-case hexadecimal digits.
{-# INLINE floatHexUpperFixed #-}
floatHexUpperFixed :: FixedPrim Float
floatHexUpperFixed = encodeFloatViaWord32F word32HexUpperFixed

-- | Encode an IEEE 'Double' using 16 upper-case hexadecimal digits.
{-# INLINE doubleHexUpperFixed #-}
doubleHexUpperFixed :: FixedPrim Double
doubleHexUpperFixed = encodeDoubleViaWord64F word64HexUpperFixed
