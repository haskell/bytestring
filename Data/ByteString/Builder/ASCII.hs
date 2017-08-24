{-# LANGUAGE ScopedTypeVariables, CPP, ForeignFunctionInterface,
             MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-- | Copyright : (c) 2010 - 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC
--
-- Constructing 'Builder's using ASCII-based encodings.
--
module Data.ByteString.Builder.ASCII
    (
      -- ** Formatting numbers as text
      -- | Formatting of numbers as ASCII text.
      --
      -- Note that you can also use these functions for the ISO/IEC 8859-1 and
      -- UTF-8 encodings, as the ASCII encoding is equivalent on the 
      -- codepoints 0-127.

      -- *** Decimal numbers
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

      -- *** Hexadecimal numbers

      -- | Encoding positive integers as hexadecimal numbers using lower-case
      -- ASCII characters. The shortest
      -- possible representation is used. For example,
      --
      -- >>> toLazyByteString (word16Hex 0x0a10)
      -- Chunk "a10" Empty
      --
    , word8Hex
    , word16Hex
    , word32Hex
    , word64Hex
    , wordHex

      -- | Encoding positive integers as hexadecimal numbers using upper-case
      -- ASCII characters. The shortest
      -- possible representation is used. For example,
      --
      -- >>> toLazyByteString (word16Hex 0x0a10)
      -- Chunk "A10" Empty
      --
    , word8HexUpper
    , word16HexUpper
    , word32HexUpper
    , word64HexUpper
    , wordHexUpper

      -- *** Fixed-width hexadecimal numbers
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

    , byteStringHex
    , lazyByteStringHex

      -- *** Fixed-width upper-case hexadecimal numbers
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

    , byteStringHexUpper
    , lazyByteStringHexUpper

    ) where

import           Data.ByteString                                as S
import           Data.ByteString.Lazy                           as L
import           Data.ByteString.Builder.Internal (Builder)
import qualified Data.ByteString.Builder.Prim                   as P

import           Foreign


#if defined(__GLASGOW_HASKELL__) && defined(INTEGER_GMP)

#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (mappend)
# endif
import           Foreign.C.Types

import qualified Data.ByteString.Builder.Prim.Internal          as P
import           Data.ByteString.Builder.Prim.Internal.UncheckedShifts
                   ( caseWordSize_32_64 )

# if __GLASGOW_HASKELL__ < 710
import           GHC.Num     (quotRemInteger)
# endif
import           GHC.Types   (Int(..))


# if __GLASGOW_HASKELL__ < 611
import GHC.Integer.Internals
# else
import GHC.Integer.GMP.Internals
# endif
#endif

------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------


-- | Encode a 'String' using 'P.char7'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = P.primMapListFixed P.char7

------------------------------------------------------------------------------
-- Decimal Encoding
------------------------------------------------------------------------------

-- Signed integers
------------------

-- | Decimal encoding of an 'Int8' using the ASCII digits.
--
-- e.g.
--
-- > toLazyByteString (int8Dec 42)   = "42"
-- > toLazyByteString (int8Dec (-1)) = "-1"
--
{-# INLINE int8Dec #-}
int8Dec :: Int8 -> Builder
int8Dec = P.primBounded P.int8Dec

-- | Decimal encoding of an 'Int16' using the ASCII digits.
{-# INLINE int16Dec #-}
int16Dec :: Int16 -> Builder
int16Dec = P.primBounded P.int16Dec

-- | Decimal encoding of an 'Int32' using the ASCII digits.
{-# INLINE int32Dec #-}
int32Dec :: Int32 -> Builder
int32Dec = P.primBounded P.int32Dec

-- | Decimal encoding of an 'Int64' using the ASCII digits.
{-# INLINE int64Dec #-}
int64Dec :: Int64 -> Builder
int64Dec = P.primBounded P.int64Dec

-- | Decimal encoding of an 'Int' using the ASCII digits.
{-# INLINE intDec #-}
intDec :: Int -> Builder
intDec = P.primBounded P.intDec


-- Unsigned integers
--------------------

-- | Decimal encoding of a 'Word8' using the ASCII digits.
{-# INLINE word8Dec #-}
word8Dec :: Word8 -> Builder
word8Dec = P.primBounded P.word8Dec

-- | Decimal encoding of a 'Word16' using the ASCII digits.
{-# INLINE word16Dec #-}
word16Dec :: Word16 -> Builder
word16Dec = P.primBounded P.word16Dec

-- | Decimal encoding of a 'Word32' using the ASCII digits.
{-# INLINE word32Dec #-}
word32Dec :: Word32 -> Builder
word32Dec = P.primBounded P.word32Dec

-- | Decimal encoding of a 'Word64' using the ASCII digits.
{-# INLINE word64Dec #-}
word64Dec :: Word64 -> Builder
word64Dec = P.primBounded P.word64Dec

-- | Decimal encoding of a 'Word' using the ASCII digits.
{-# INLINE wordDec #-}
wordDec :: Word -> Builder
wordDec = P.primBounded P.wordDec


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
word8Hex = P.primBounded P.word8Hex

-- | Shortest hexadecimal encoding of a 'Word16' using lower-case characters.
{-# INLINE word16Hex #-}
word16Hex :: Word16 -> Builder
word16Hex = P.primBounded P.word16Hex

-- | Shortest hexadecimal encoding of a 'Word32' using lower-case characters.
{-# INLINE word32Hex #-}
word32Hex :: Word32 -> Builder
word32Hex = P.primBounded P.word32Hex

-- | Shortest hexadecimal encoding of a 'Word64' using lower-case characters.
{-# INLINE word64Hex #-}
word64Hex :: Word64 -> Builder
word64Hex = P.primBounded P.word64Hex

-- | Shortest hexadecimal encoding of a 'Word' using lower-case characters.
{-# INLINE wordHex #-}
wordHex :: Word -> Builder
wordHex = P.primBounded P.wordHex

-- | Shortest hexadecimal encoding of a 'Word8' using upper-case characters.
{-# INLINE word8HexUpper #-}
word8HexUpper :: Word8 -> Builder
word8HexUpper = P.primBounded P.word8HexUpper

-- | Shortest hexadecimal encoding of a 'Word16' using upper-case characters.
{-# INLINE word16HexUpper #-}
word16HexUpper :: Word16 -> Builder
word16HexUpper = P.primBounded P.word16HexUpper

-- | Shortest hexadecimal encoding of a 'Word32' using upper-case characters.
{-# INLINE word32HexUpper #-}
word32HexUpper :: Word32 -> Builder
word32HexUpper = P.primBounded P.word32HexUpper

-- | Shortest hexadecimal encoding of a 'Word64' using upper-case characters.
{-# INLINE word64HexUpper #-}
word64HexUpper :: Word64 -> Builder
word64HexUpper = P.primBounded P.word64HexUpper

-- | Shortest hexadecimal encoding of a 'Word' using upper-case characters.
{-# INLINE wordHexUpper #-}
wordHexUpper :: Word -> Builder
wordHexUpper = P.primBounded P.wordHexUpper

-- fixed width; leading zeroes
------------------------------

-- | Hexadecimal encoding of an 'Int8' using 2 lower-case characters.
{-# INLINE int8HexFixed #-}
int8HexFixed :: Int8 -> Builder
int8HexFixed = P.primFixed P.int8HexFixed

-- | Hexadecimal encoding of an 'Int16' using 4 lower-case characters.
{-# INLINE int16HexFixed #-}
int16HexFixed :: Int16 -> Builder
int16HexFixed = P.primFixed P.int16HexFixed

-- | Hexadecimal encoding of an 'Int32' using 8 lower-case characters.
{-# INLINE int32HexFixed #-}
int32HexFixed :: Int32 -> Builder
int32HexFixed = P.primFixed P.int32HexFixed

-- | Hexadecimal encoding of an 'Int64' using 16 lower-case characters.
{-# INLINE int64HexFixed #-}
int64HexFixed :: Int64 -> Builder
int64HexFixed = P.primFixed P.int64HexFixed

-- | Hexadecimal encoding of a 'Word8' using 2 lower-case characters.
{-# INLINE word8HexFixed #-}
word8HexFixed :: Word8 -> Builder
word8HexFixed = P.primFixed P.word8HexFixed

-- | Hexadecimal encoding of a 'Word16' using 4 lower-case characters.
{-# INLINE word16HexFixed #-}
word16HexFixed :: Word16 -> Builder
word16HexFixed = P.primFixed P.word16HexFixed

-- | Hexadecimal encoding of a 'Word32' using 8 lower-case characters.
{-# INLINE word32HexFixed #-}
word32HexFixed :: Word32 -> Builder
word32HexFixed = P.primFixed P.word32HexFixed

-- | Hexadecimal encoding of a 'Word64' using 16 lower-case characters.
{-# INLINE word64HexFixed #-}
word64HexFixed :: Word64 -> Builder
word64HexFixed = P.primFixed P.word64HexFixed

-- | Hexadecimal encoding of an 'Int32' using a specified number of
--   lower-case characters.
{-# INLINE int32HexFixedWidth #-}
int32HexFixedWidth :: Int -> Int32 -> Builder
int32HexFixedWidth = P.primFixed . P.int32HexFixedWidth

-- | Hexadecimal encoding of an 'Int64' using a specified number of
--   lower-case characters.
{-# INLINE int64HexFixedWidth #-}
int64HexFixedWidth :: Int -> Int64 -> Builder
int64HexFixedWidth = P.primFixed . P.int64HexFixedWidth

-- | Hexadecimal encoding of a 'Word32' using a specified number of
--   lower-case characters.
{-# INLINE word32HexFixedWidth #-}
word32HexFixedWidth :: Int -> Word32 -> Builder
word32HexFixedWidth = P.primFixed . P.word32HexFixedWidth

-- | Hexadecimal encoding of a 'Word64' using a specified number of
--   lower-case characters.
{-# INLINE word64HexFixedWidth #-}
word64HexFixedWidth :: Int -> Word64 -> Builder
word64HexFixedWidth = P.primFixed . P.word64HexFixedWidth

-- | Encode an IEEE 'Float' using 8 lower-case hexadecimal digits.
{-# INLINE floatHexFixed #-}
floatHexFixed :: Float -> Builder
floatHexFixed = P.primFixed P.floatHexFixed

-- | Encode an IEEE 'Double' using 16 lower-case hexadecimal digits.
{-# INLINE doubleHexFixed #-}
doubleHexFixed :: Double -> Builder
doubleHexFixed = P.primFixed P.doubleHexFixed

-- | Encode each byte of a 'S.ByteString' using its fixed-width
--   lower-case hex encoding.
{-# NOINLINE byteStringHex #-} -- share code
byteStringHex :: S.ByteString -> Builder
byteStringHex = P.primMapByteStringFixed P.word8HexFixed

-- | Encode each byte of a lazy 'L.ByteString' using its fixed-width
--   lower-case hex encoding.
{-# NOINLINE lazyByteStringHex #-} -- share code
lazyByteStringHex :: L.ByteString -> Builder
lazyByteStringHex = P.primMapLazyByteStringFixed P.word8HexFixed


-- fixed width; leading zeroes; upper-case
------------------------------------------

-- | Hexadecimal encoding of an 'Int8' using 2 upper-case characters.
{-# INLINE int8HexUpperFixed #-}
int8HexUpperFixed :: Int8 -> Builder
int8HexUpperFixed = P.primFixed P.int8HexUpperFixed

-- | Hexadecimal encoding of an 'Int16' using 4 upper-case characters.
{-# INLINE int16HexUpperFixed #-}
int16HexUpperFixed :: Int16 -> Builder
int16HexUpperFixed = P.primFixed P.int16HexUpperFixed

-- | Hexadecimal encoding of an 'Int32' using 8 upper-case characters.
{-# INLINE int32HexUpperFixed #-}
int32HexUpperFixed :: Int32 -> Builder
int32HexUpperFixed = P.primFixed P.int32HexUpperFixed

-- | Hexadecimal encoding of an 'Int64' using 16 upper-case characters.
{-# INLINE int64HexUpperFixed #-}
int64HexUpperFixed :: Int64 -> Builder
int64HexUpperFixed = P.primFixed P.int64HexUpperFixed

-- | Hexadecimal encoding of a 'Word8' using 2 upper-case characters.
{-# INLINE word8HexUpperFixed #-}
word8HexUpperFixed :: Word8 -> Builder
word8HexUpperFixed = P.primFixed P.word8HexUpperFixed

-- | Hexadecimal encoding of a 'Word16' using 4 upper-case characters.
{-# INLINE word16HexUpperFixed #-}
word16HexUpperFixed :: Word16 -> Builder
word16HexUpperFixed = P.primFixed P.word16HexUpperFixed

-- | Hexadecimal encoding of a 'Word32' using 8 upper-case characters.
{-# INLINE word32HexUpperFixed #-}
word32HexUpperFixed :: Word32 -> Builder
word32HexUpperFixed = P.primFixed P.word32HexUpperFixed

-- | Hexadecimal encoding of a 'Word64' using 16 upper-case characters.
{-# INLINE word64HexUpperFixed #-}
word64HexUpperFixed :: Word64 -> Builder
word64HexUpperFixed = P.primFixed P.word64HexUpperFixed

-- | Hexadecimal encoding of an 'Int32' using a specified number of
--   upper-case characters.
{-# INLINE int32HexUpperFixedWidth #-}
int32HexUpperFixedWidth :: Int -> Int32 -> Builder
int32HexUpperFixedWidth = P.primFixed . P.int32HexUpperFixedWidth

-- | Hexadecimal encoding of an 'Int64' using a specified number of
--   upper-case characters.
{-# INLINE int64HexUpperFixedWidth #-}
int64HexUpperFixedWidth :: Int -> Int64 -> Builder
int64HexUpperFixedWidth = P.primFixed . P.int64HexUpperFixedWidth

-- | Hexadecimal encoding of a 'Word32' using a specified number of
--   upper-case characters.
{-# INLINE word32HexUpperFixedWidth #-}
word32HexUpperFixedWidth :: Int -> Word32 -> Builder
word32HexUpperFixedWidth = P.primFixed . P.word32HexUpperFixedWidth

-- | Hexadecimal encoding of a 'Word64' using a specified number of
--   upper-case characters.
{-# INLINE word64HexUpperFixedWidth #-}
word64HexUpperFixedWidth :: Int -> Word64 -> Builder
word64HexUpperFixedWidth = P.primFixed . P.word64HexUpperFixedWidth

-- | Encode an IEEE 'Float' using 8 upper-case hexadecimal digits.
{-# INLINE floatHexUpperFixed #-}
floatHexUpperFixed :: Float -> Builder
floatHexUpperFixed = P.primFixed P.floatHexUpperFixed

-- | Encode an IEEE 'Double' using 16 upper-case hexadecimal digits.
{-# INLINE doubleHexUpperFixed #-}
doubleHexUpperFixed :: Double -> Builder
doubleHexUpperFixed = P.primFixed P.doubleHexUpperFixed

-- | Encode each byte of a 'S.ByteString' using its fixed-width hex
--   upper-case encoding.
{-# NOINLINE byteStringHexUpper #-} -- share code
byteStringHexUpper :: S.ByteString -> Builder
byteStringHexUpper = P.primMapByteStringFixed P.word8HexUpperFixed

-- | Encode each byte of a lazy 'L.ByteString' using its fixed-width hex
--   upper-case encoding.
{-# NOINLINE lazyByteStringHexUpper #-} -- share code
lazyByteStringHexUpper :: L.ByteString -> Builder
lazyByteStringHexUpper = P.primMapLazyByteStringFixed P.word8HexUpperFixed


------------------------------------------------------------------------------
-- Fast decimal 'Integer' encoding.
------------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__) && defined(INTEGER_GMP)
-- An optimized version of the integer serialization code
-- in blaze-textual (c) 2011 MailRank, Inc. Bryan O'Sullivan
-- <bos@mailrank.com>. It is 2.5x faster on Int-sized integers and 4.5x faster
-- on larger integers.

# define PAIR(a,b) (# a,b #)

-- | Maximal power of 10 fitting into an 'Int' without using the MSB.
--     10 ^ 9  for 32 bit ints  (31 * log 2 / log 10 =  9.33)
--     10 ^ 18 for 64 bit ints  (63 * log 2 / log 10 = 18.96)
--
-- FIXME: Think about also using the MSB. For 64 bit 'Int's this makes a
-- difference.
maxPow10 :: Integer
maxPow10 = toInteger $ (10 :: Int) ^ caseWordSize_32_64 (9 :: Int) 18

-- | Decimal encoding of an 'Integer' using the ASCII digits.
integerDec :: Integer -> Builder
integerDec (S# i#) = intDec (I# i#)
integerDec i
    | i < 0     = P.primFixed P.char8 '-' `mappend` go (-i)
    | otherwise =                                   go ( i)
  where
    errImpossible fun =
        error $ "integerDec: " ++ fun ++ ": the impossible happened."

    go :: Integer -> Builder
    go n | n < maxPow10 = intDec (fromInteger n)
         | otherwise    =
             case putH (splitf (maxPow10 * maxPow10) n) of
               (x:xs) -> intDec x `mappend` P.primMapListBounded intDecPadded xs
               []     -> errImpossible "integerDec: go"

    splitf :: Integer -> Integer -> [Integer]
    splitf pow10 n0
      | pow10 > n0  = [n0]
      | otherwise   = splith (splitf (pow10 * pow10) n0)
      where
        splith []     = errImpossible "splith"
        splith (n:ns) =
            case n `quotRemInteger` pow10 of
                PAIR(q,r) | q > 0     -> q : r : splitb ns
                          | otherwise ->     r : splitb ns

        splitb []     = []
        splitb (n:ns) = case n `quotRemInteger` pow10 of
                            PAIR(q,r) -> q : r : splitb ns

    putH :: [Integer] -> [Int]
    putH []     = errImpossible "putH"
    putH (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(x,y)
                        | q > 0     -> q : r : putB ns
                        | otherwise ->     r : putB ns
                        where q = fromInteger x
                              r = fromInteger y

    putB :: [Integer] -> [Int]
    putB []     = []
    putB (n:ns) = case n `quotRemInteger` maxPow10 of
                    PAIR(q,r) -> fromInteger q : fromInteger r : putB ns


foreign import ccall unsafe "static _hs_bytestring_int32_dec_padded9"
    c_int_dec_padded9 :: CInt -> Ptr Word8 -> IO ()

foreign import ccall unsafe "static _hs_bytestring_int64_dec_padded18"
    c_int64_dec_padded18 :: Int64 -> Ptr Word8 -> IO ()

{-# INLINE intDecPadded #-}
intDecPadded :: P.BoundedPrim Int
intDecPadded = P.liftFixedToBounded $ caseWordSize_32_64
    (P.fixedPrim  9 $ c_int_dec_padded9    . fromIntegral)
    (P.fixedPrim 18 $ c_int64_dec_padded18 . fromIntegral)

#else
-- compilers other than GHC

-- | Decimal encoding of an 'Integer' using the ASCII digits. Implemented
-- using via the 'Show' instance of 'Integer's.
integerDec :: Integer -> Builder
integerDec = string7 . show
#endif
