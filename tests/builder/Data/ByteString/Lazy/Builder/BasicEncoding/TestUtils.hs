-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing utilities for comparing
-- for an example on how to use the functions provided here.
--
module Data.ByteString.Lazy.Builder.BasicEncoding.TestUtils (

  -- * Testing 'FixedEncoding's
    testF
  , testBoundedF

  , testFixedBoundF

  , compareImpls

  -- * Testing 'BoundedEncoding's
  , testBoundedB

  -- * Encoding reference implementations

  , charUtf8_list
  , char8_list

  -- ** ASCII-based encodings
  , encodeASCII
  , encodeForcedASCII
  , charASCII_list
  , dec_list
  , hex_list
  , wordHexFixed_list
  , int8HexFixed_list
  , int16HexFixed_list
  , int32HexFixed_list
  , int64HexFixed_list
  , floatHexFixed_list
  , doubleHexFixed_list

  -- ** Binary
  , parseVar

  , bigEndian_list
  , littleEndian_list
  , hostEndian_list
  , float_list
  , double_list
  , coerceFloatToWord32
  , coerceDoubleToWord64

  ) where

import           Control.Arrow (first)

import           Data.ByteString.Lazy.Builder.BasicEncoding
import           Data.Char (chr, ord)

import           Numeric (showHex)

import           Foreign

import           System.ByteOrder
import           Unsafe.Coerce (unsafeCoerce)

import           Test.QuickCheck (Arbitrary(..))
import           TestFramework


-- Helper functions
-------------------

-- | Quickcheck test that includes a check that the property holds on the
-- bounds of a bounded value.
testBoundedProperty :: forall a. (Arbitrary a, Show a, Bounded a)
                    => String -> (a -> Bool) -> Test
testBoundedProperty name p = testGroup name
  [ testProperty "arbitrary" p
  , testCase "bounds" $ p (minBound :: a)
                     && p (maxBound :: a)
  ]

-- | Quote a 'String' nicely.
quote :: String -> String
quote cs = '`' : cs ++ "'"

-- | Quote a @[Word8]@ list as as 'String'.
quoteWord8s :: [Word8] -> String
quoteWord8s = quote . map (chr . fromIntegral)


-- FixedEncoding
----------------

-- TODO: Port code that checks for low-level properties of basic encodings (no
-- overwrites, all bytes written, etc.) from old 'system-io-write' library

-- | Test a 'FixedEncoding' against a reference implementation.
testF :: (Arbitrary a, Show a)
      => String
      -> (a -> [Word8])
      -> FixedEncoding a
      -> Test
testF name ref fe =
    testProperty name prop
  where
    prop x
      | y == y'   = True
      | otherwise = error $ unlines $
          [ "testF: results disagree for " ++ quote (show x)
          , " fixed encoding: " ++ show y ++ " " ++ quoteWord8s y
          , " reference:      " ++ show y'++ " " ++ quoteWord8s y'
          ]
      where
        y  = evalF fe x
        y' = ref x

-- | Test a 'FixedEncoding' of a bounded value against a reference implementation
-- and ensure that the bounds are always included as testcases.
testBoundedF :: (Arbitrary a, Bounded a, Show a)
             => String
             -> (a -> [Word8])
             -> FixedEncoding a
             -> Test
testBoundedF name ref fe =
    testBoundedProperty name $ \x -> evalF fe x == ref x

-- FixedEncoding derived from a bound on a given value.

testFixedBoundF :: (Arbitrary a, Show a, Integral a)
                => String
                -> (a -> a -> [Word8])
                -> (a -> FixedEncoding a)
                -> Test
testFixedBoundF name ref bfe =
    testProperty name prop
  where
    prop (b, x0)
      | y == y'   = True
      | otherwise = error $ unlines $
          [ "testF: results disagree for " ++ quote (show (b, x))
          , " fixed encoding: " ++ show y ++ " " ++ quoteWord8s y
          , " reference:      " ++ show y'++ " " ++ quoteWord8s y'
          ]
      where
        x  | b == 0    = 0
           | otherwise = x0 `mod` b
        y  = evalF (bfe b) x
        y' = ref b x


-- BoundedEncoding
------------------

-- | Test a 'BoundedEncoding' of a bounded value against a reference implementation
-- and ensure that the bounds are always included as testcases.
testBoundedB :: (Arbitrary a, Bounded a, Show a)
             => String
             -> (a -> [Word8])
             -> BoundedEncoding a
             -> Test
testBoundedB name ref fe =
    testBoundedProperty name check
  where
    check x
      | y == y'   = True
      | otherwise = error $ unlines $
          [ "testBoundedB: results disagree for " ++ quote (show x)
          , " fixed encoding: " ++ show y ++ " " ++ quoteWord8s y
          , " reference:      " ++ show y'++ " " ++ quoteWord8s y'
          ]
      where
        y  = evalB fe x
        y' = ref x

-- | Compare two implementations of a function.
compareImpls :: (Arbitrary a, Show a, Show b, Eq b)
             => TestName -> (a -> b) -> (a -> b) -> Test
compareImpls name f1 f2 =
    testProperty name check
  where
    check x
      | y1 == y2  = True
      | otherwise = error $ unlines $
          [ "compareImpls: results disagree for " ++ quote (show x)
          , " f1: " ++ show y1
          , " f2: " ++ show y2
          ]
      where
        y1 = f1 x
        y2 = f2 x



------------------------------------------------------------------------------
-- Encoding reference implementations
------------------------------------------------------------------------------

-- | Char8 encoding: truncate Unicode codepoint to 8-bits.
char8_list :: Char -> [Word8]
char8_list = return . fromIntegral . ord

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
--
-- Copied from 'utf8-string-0.3.6' to make tests self-contained.
-- Copyright (c) 2007, Galois Inc. All rights reserved.
--
charUtf8_list :: Char -> [Word8]
charUtf8_list =
    map fromIntegral . encode . ord
  where
    encode oc
      | oc <= 0x7f       = [oc]

      | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                           , 0x80 + oc .&. 0x3f
                           ]

      | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                           , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                           , 0x80 + oc .&. 0x3f
                           ]
      | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                           , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                           , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                           , 0x80 + oc .&. 0x3f
                           ]

-- ASCII-based encodings
------------------------

-- | Encode a 'String' of only ASCII characters using the ASCII encoding.
encodeASCII :: String -> [Word8]
encodeASCII =
    map encode
  where
    encode c
      | c < '\x7f' = fromIntegral $ ord c
      | otherwise  = error $ "encodeASCII: non-ASCII character '" ++ [c] ++ "'"

-- | Encode an arbitrary 'String' by truncating its characters to the least
-- significant 7-bits.
encodeForcedASCII :: String -> [Word8]
encodeForcedASCII = map ((.&. 0x7f) . fromIntegral . ord)

charASCII_list :: Char -> [Word8]
charASCII_list = encodeForcedASCII . return

dec_list :: Show a =>  a -> [Word8]
dec_list = encodeASCII . show

hex_list :: (Integral a, Show a) => a -> [Word8]
hex_list = encodeASCII . (\x -> showHex x "")

wordHexFixed_list :: (Storable a, Integral a, Show a) => a -> [Word8]
wordHexFixed_list x =
   encodeASCII $ pad (2 * sizeOf x) $ showHex x ""
 where
   pad n cs = replicate (n - length cs) '0' ++ cs

int8HexFixed_list :: Int8 -> [Word8]
int8HexFixed_list  = wordHexFixed_list . (fromIntegral :: Int8  -> Word8 )

int16HexFixed_list :: Int16 -> [Word8]
int16HexFixed_list = wordHexFixed_list . (fromIntegral :: Int16 -> Word16)

int32HexFixed_list :: Int32 -> [Word8]
int32HexFixed_list = wordHexFixed_list . (fromIntegral :: Int32 -> Word32)

int64HexFixed_list :: Int64 -> [Word8]
int64HexFixed_list = wordHexFixed_list . (fromIntegral :: Int64 -> Word64)

floatHexFixed_list :: Float -> [Word8]
floatHexFixed_list  = float_list wordHexFixed_list

doubleHexFixed_list :: Double -> [Word8]
doubleHexFixed_list = double_list wordHexFixed_list

-- Binary
---------

bigEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
bigEndian_list = reverse . littleEndian_list

littleEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
littleEndian_list x =
    map (fromIntegral . (x `shiftR`) . (8*)) $ [0..sizeOf x - 1]

hostEndian_list :: (Storable a, Bits a, Integral a) => a -> [Word8]
hostEndian_list = case byteOrder of
    LittleEndian -> littleEndian_list
    BigEndian    -> bigEndian_list
    _            -> error $
        "bounded-encoding: unsupported byteorder '" ++ show byteOrder ++ "'"


float_list :: (Word32 -> [Word8]) -> Float -> [Word8]
float_list f  = f . coerceFloatToWord32

double_list :: (Word64 -> [Word8]) -> Double -> [Word8]
double_list f = f . coerceDoubleToWord64

-- Note that the following use of unsafeCoerce is not guaranteed to be
-- safe on GHC 7.0 and less. The reason is probably the following ticket:
--
--   http://hackage.haskell.org/trac/ghc/ticket/4092
--
-- However, that only applies if the value is loaded in a register. We
-- avoid this by coercing only boxed values and ensuring that they
-- remain boxed using a NOINLINE pragma.
--

-- | Super unsafe coerce a 'Float' to a 'Word32'. We have to explicitly mask
-- out the higher bits in case we are working on a 64-bit machine.
{-# NOINLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = (.&. maxBound) . unsafeCoerce

-- | Super unsafe coerce a 'Double' to a 'Word64'. Currently, there are no
-- > 64 bit machines supported by GHC. But we just play it safe.
{-# NOINLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = (.&. maxBound) . unsafeCoerce

-- | Parse a variable length encoding
parseVar :: (Num a, Bits a) => [Word8] -> (a, [Word8])
parseVar =
    go
  where
    go []    = error "parseVar: unterminated variable length int"
    go (w:ws)
      | w .&. 0x80 == 0 = (fromIntegral w, ws)
      | otherwise       = first add (go ws)
      where
        add x = (x `shiftL` 7) .|. (fromIntegral w .&. 0x7f)


