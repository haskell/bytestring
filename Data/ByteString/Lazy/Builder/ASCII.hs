{-# LANGUAGE ScopedTypeVariables, CPP, ForeignFunctionInterface #-}
-- | Copyright : (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC
--
-- Constructing 'Builder's using ASCII-based encodings.
--
module Data.ByteString.Lazy.Builder.ASCII
    (
      -- * Decimal numbers
      -- | Decimal encoding of numbers using ASCII encoded characters.
      int8Dec
    , int16Dec
    , int32Dec
    , int64Dec
    , intDec
    , integerDec

    , word8Dec
    , word16Dec
    , word32Dec
    , word64Dec
    , wordDec

    , floatDec
    , doubleDec

      -- * Hexadecimal numbers

      -- | Encoding positive integers as hexadecimal numbers using lower-case
      -- ASCII characters. The shortest
      -- possible representation is used. For example,
      --
      -- >>> toLazyByteString (word16Hex 0x0a10)
      -- Chunk "a10" Empty
      --
      -- Note that there is no support for using upper-case characters. Please
      -- contact the maintainer, if your application cannot work without
      -- hexadecimal encodings that use upper-case characters.
      --
    , word8Hex
    , word16Hex
    , word32Hex
    , word64Hex
    , wordHex

      -- * Fixed-width hexadecimal numbers
      --
    , int8HexFixed
    , int16HexFixed
    , int32HexFixed
    , int64HexFixed
    , word8HexFixed
    , word16HexFixed
    , word32HexFixed
    , word64HexFixed

    , floatHexFixed
    , doubleHexFixed

    , byteStringHexFixed
    , lazyByteStringHexFixed

    ) where

import           Data.ByteString                                  as S
import           Data.ByteString.Lazy.Internal                    as L
import           Data.ByteString.Lazy.Builder.Internal (Builder)
import qualified Data.ByteString.Lazy.Builder.BasicEncoding       as E

import           Foreign

------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------


-- | Encode a 'String' using 'E.char7'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = E.encodeListWithF E.char7

------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

-- | Decimal encoding of an 'Int8' using the ASCII digits.
{-# INLINE int8Dec #-}
int8Dec :: Int8 -> Builder
int8Dec = E.encodeWithB E.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE int16Dec #-}
int16Dec :: Int16 -> Builder
int16Dec = E.encodeWithB E.int16Dec

-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE int32Dec #-}
int32Dec :: Int32 -> Builder
int32Dec = E.encodeWithB E.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE int64Dec #-}
int64Dec :: Int64 -> Builder
int64Dec = E.encodeWithB E.int64Dec

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec = E.encodeWithB E.intDec

-- | /Currently slow./ Decimal encoding of an 'Integer' using the ASCII digits.
{-# INLINE integerDec #-}
integerDec :: Integer -> Builder
integerDec =  string7 . show


-- Unsigned integers
--------------------

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE word8Dec #-}
word8Dec :: Word8 -> Builder
word8Dec = E.encodeWithB E.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE word16Dec #-}
word16Dec :: Word16 -> Builder
word16Dec = E.encodeWithB E.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE word32Dec #-}
word32Dec :: Word32 -> Builder
word32Dec = E.encodeWithB E.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE word64Dec #-}
word64Dec :: Word64 -> Builder
word64Dec = E.encodeWithB E.word64Dec

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE wordDec #-}
wordDec :: Word -> Builder
wordDec = E.encodeWithB E.wordDec


-- Floating point numbers
-------------------------

-- TODO: Use Bryan O'Sullivan's double-conversion package to speed it up.

-- | /Currently slow./ Decimal encoding of an IEEE 'Float'.
{-# INLINE floatDec #-}
floatDec :: Float -> Builder
floatDec = string7 . show

-- | /Currently slow./ Decimal encoding of an IEEE 'Double'.
{-# INLINE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = string7 . show


------------------------------------------------------------------------------
-- Hexadecimal Encoding
------------------------------------------------------------------------------

-- without lead
---------------

-- | Shortest hexadecimal encoding of a 'Word8' using lower-case characters.
{-# INLINE word8Hex #-}
word8Hex :: Word8 -> Builder
word8Hex = E.encodeWithB E.word8Hex

-- | Shortest hexadecimal encoding of a 'Word16' using lower-case characters.
{-# INLINE word16Hex #-}
word16Hex :: Word16 -> Builder
word16Hex = E.encodeWithB E.word16Hex

-- | Shortest hexadecimal encoding of a 'Word32' using lower-case characters.
{-# INLINE word32Hex #-}
word32Hex :: Word32 -> Builder
word32Hex = E.encodeWithB E.word32Hex

-- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.
{-# INLINE word64Hex #-}
word64Hex :: Word64 -> Builder
word64Hex = E.encodeWithB E.word64Hex

-- | Shortest hexadecimal encoding of a 'Word' using lower-case characters.
{-# INLINE wordHex #-}
wordHex :: Word -> Builder
wordHex = E.encodeWithB E.wordHex


-- fixed width; leading zeroes
------------------------------

-- | Encode a 'Int8' using 2 nibbles (hexadecimal digits).
{-# INLINE int8HexFixed #-}
int8HexFixed :: Int8 -> Builder
int8HexFixed = E.encodeWithF E.int8HexFixed

-- | Encode a 'Int16' using 4 nibbles.
{-# INLINE int16HexFixed #-}
int16HexFixed :: Int16 -> Builder
int16HexFixed = E.encodeWithF E.int16HexFixed

-- | Encode a 'Int32' using 8 nibbles.
{-# INLINE int32HexFixed #-}
int32HexFixed :: Int32 -> Builder
int32HexFixed = E.encodeWithF E.int32HexFixed

-- | Encode a 'Int64' using 16 nibbles.
{-# INLINE int64HexFixed #-}
int64HexFixed :: Int64 -> Builder
int64HexFixed = E.encodeWithF E.int64HexFixed

-- | Encode a 'Word8' using 2 nibbles (hexadecimal digits).
{-# INLINE word8HexFixed #-}
word8HexFixed :: Word8 -> Builder
word8HexFixed = E.encodeWithF E.word8HexFixed

-- | Encode a 'Word16' using 4 nibbles.
{-# INLINE word16HexFixed #-}
word16HexFixed :: Word16 -> Builder
word16HexFixed = E.encodeWithF E.word16HexFixed

-- | Encode a 'Word32' using 8 nibbles.
{-# INLINE word32HexFixed #-}
word32HexFixed :: Word32 -> Builder
word32HexFixed = E.encodeWithF E.word32HexFixed

-- | Encode a 'Word64' using 16 nibbles.
{-# INLINE word64HexFixed #-}
word64HexFixed :: Word64 -> Builder
word64HexFixed = E.encodeWithF E.word64HexFixed

-- | Encode an IEEE 'Float' using 8 nibbles.
{-# INLINE floatHexFixed #-}
floatHexFixed :: Float -> Builder
floatHexFixed = E.encodeWithF E.floatHexFixed

-- | Encode an IEEE 'Double' using 16 nibbles.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: Double -> Builder
doubleHexFixed = E.encodeWithF E.doubleHexFixed

-- | Encode each byte of a 'S.ByteString' using its fixed-width hex encoding.
{-# NOINLINE byteStringHexFixed #-} -- share code
byteStringHexFixed :: S.ByteString -> Builder
byteStringHexFixed = E.encodeByteStringWithF E.word8HexFixed

-- | Encode each byte of a lazy 'L.ByteString' using its fixed-width hex encoding.
{-# NOINLINE lazyByteStringHexFixed #-} -- share code
lazyByteStringHexFixed :: L.ByteString -> Builder
lazyByteStringHexFixed = E.encodeLazyByteStringWithF E.word8HexFixed
