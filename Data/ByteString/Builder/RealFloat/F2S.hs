{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, MagicHash #-}
-- |
-- Module      : Data.ByteString.Builder.RealFloat.F2S
-- Copyright   : (c) Lawrence Wu 2021
-- License     : BSD-style
-- Maintainer  : lawrencejwu@gmail.com
--
-- Implementation of float-to-string conversion

module Data.ByteString.Builder.RealFloat.F2S
    ( FloatingDecimal(..)
    , f2s
    , f2Intermediate
    ) where

import Control.Arrow (first)
import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import GHC.Int (Int32(..))
import GHC.Word (Word32(..), Word64(..))

-- See Data.ByteString.Builder.RealFloat.TableGenerator for a high-level
-- explanation of the ryu algorithm

-- | Table of 2^k / 5^q + 1
-- Byte-swapped version of
-- > fmap (finv float_pow5_inv_bitcount) [0..float_max_inv_split]
--
-- Displayed here as 2 Word64 table values per line
float_pow5_inv_split :: Addr
float_pow5_inv_split = Addr
  "\x01\x00\x00\x00\x00\x00\x00\x08\x67\x66\x66\x66\x66\x66\x66\x06\
  \\xb9\x1e\x85\xeb\x51\xb8\x1e\x05\xfa\x7e\x6a\xbc\x74\x93\x18\x04\
  \\x2a\xcb\x10\xc7\xba\xb8\x8d\x06\x22\x3c\xda\x38\x62\x2d\x3e\x05\
  \\x4e\x63\x7b\x2d\xe8\xbd\x31\x04\x16\xd2\x2b\xaf\xa6\xfc\xb5\x06\
  \\x78\x0e\x23\x8c\xb8\x63\x5e\x05\x2d\xa5\xb5\x09\xfa\x82\x4b\x04\
  \\xae\x6e\xef\x75\xf6\x37\xdf\x06\x58\x25\x59\x5e\xf8\x5f\x7f\x05\
  \\x47\x84\x7a\x4b\x60\xe6\x65\x04\x71\xa0\x5d\x12\x9a\x70\x09\x07\
  \\xc1\xe6\x4a\xa8\xe1\x26\xa1\x05\x67\x85\xd5\xb9\xe7\xeb\x80\x04\
  \\x0b\x6f\x22\xf6\xa5\xac\x34\x07\xa3\x25\xb5\x91\x51\xbd\xc3\x05\
  \\xe9\xea\x90\x74\x74\x97\x9c\x04\x0e\xab\xb4\xed\x53\xf2\x60\x07\
  \\xd8\x88\x90\x24\x43\x28\xe7\x05\xe0\xd3\xa6\x83\x02\xed\xb8\x04\
  \\x66\xb9\xd7\x05\x04\x48\x8e\x07\x52\x94\xac\x04\xd0\x6c\x0b\x06\
  \\xdb\xa9\x23\x6a\xa6\xf0\xd5\x04\x2b\x76\x9f\x76\x3d\xb4\xbc\x07\
  \\xef\xc4\xb2\x2b\x31\x90\x30\x06\xf3\x03\x8f\xbc\x8d\xa6\xf3\x04\
  \\x51\x06\x18\x94\xaf\x3d\xec\x07\xda\xd1\xac\xa9\xbf\x97\x56\x06\
  \\xe2\xa7\xf0\xba\xff\x12\x12\x05"#

-- | Table of 5^(-e2-q) / 2^k + 1
-- Byte-swapped version of
-- > fmap (fnorm float_pow5_bitcount) [0..float_max_split]
--
-- Displayed here as 2 Word64 table values per line
float_pow5_split :: Addr
float_pow5_split = Addr
  "\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x14\
  \\x00\x00\x00\x00\x00\x00\x00\x19\x00\x00\x00\x00\x00\x00\x40\x1f\
  \\x00\x00\x00\x00\x00\x00\x88\x13\x00\x00\x00\x00\x00\x00\x6a\x18\
  \\x00\x00\x00\x00\x00\x80\x84\x1e\x00\x00\x00\x00\x00\xd0\x12\x13\
  \\x00\x00\x00\x00\x00\x84\xd7\x17\x00\x00\x00\x00\x00\x65\xcd\x1d\
  \\x00\x00\x00\x00\x20\x5f\xa0\x12\x00\x00\x00\x00\xe8\x76\x48\x17\
  \\x00\x00\x00\x00\xa2\x94\x1a\x1d\x00\x00\x00\x40\xe5\x9c\x30\x12\
  \\x00\x00\x00\x90\x1e\xc4\xbc\x16\x00\x00\x00\x34\x26\xf5\x6b\x1c\
  \\x00\x00\x80\xe0\x37\x79\xc3\x11\x00\x00\xa0\xd8\x85\x57\x34\x16\
  \\x00\x00\xc8\x4e\x67\x6d\xc1\x1b\x00\x00\x3d\x91\x60\xe4\x58\x11\
  \\x00\x40\x8c\xb5\x78\x1d\xaf\x15\x00\x50\xef\xe2\xd6\xe4\x1a\x1b\
  \\x00\x92\xd5\x4d\x06\xcf\xf0\x10\x80\xf6\x4a\xe1\xc7\x02\x2d\x15\
  \\x20\xb4\x9d\xd9\x79\x43\x78\x1a\x94\x90\x02\x28\x2c\x2a\x8b\x10\
  \\xb9\x34\x03\x32\xb7\xf4\xad\x14\xe7\x01\x84\xfe\xe4\x71\xd9\x19\
  \\x30\x81\x12\x1f\x2f\xe7\x27\x10\x7c\x21\xd7\xe6\xfa\xe0\x31\x14\
  \\xdb\xe9\x8c\xa0\x39\x59\x3e\x19\x52\x24\xb0\x08\x88\xef\x8d\x1f\
  \\xb3\x16\x6e\x05\xb5\xb5\xb8\x13\x60\x9c\xc9\x46\x22\xe3\xa6\x18\
  \\x78\x03\x7c\xd8\xea\x9b\xd0\x1e\x2b\x82\x4d\xc7\x72\x61\x42\x13\
  \\xb6\xe2\x20\x79\xcf\xf9\x12\x18\x64\x1b\x69\x57\x43\xb8\x17\x1e\
  \\x1e\xb1\xa1\x16\x2a\xd3\xce\x12\x66\x1d\x4a\x9c\xf4\x87\x82\x17\
  \\xbf\xa4\x5c\xc3\xf1\x29\x63\x1d\xf7\xe6\x19\x1a\x37\xfa\x5d\x12\
  \\xb5\x60\xa0\xe0\xc4\x78\xf5\x16\xe3\x78\xc8\x18\xf6\xd6\xb2\x1c\
  \\x8d\x4b\x7d\xcf\x59\xc6\xef\x11\x71\x9e\x5c\x43\xf0\xb7\x6b\x16\
  \\x0d\xc6\x33\x54\xec\xa5\x06\x1c"#

-- | Number of mantissa bits of a 32-bit float. The number of significant bits
-- (floatDigits (undefined :: Float)) is 24 since we have a leading 1 for
-- normal floats and 0 for subnormal floats
float_mantissa_bits :: Int
float_mantissa_bits = 23

-- | Number of exponent bits of a 32-bit float
float_exponent_bits :: Int
float_exponent_bits = 8

-- | Bias in encoded 32-bit float representation (2^7 - 1)
float_bias :: Int
float_bias = 127

data FloatingDecimal = FloatingDecimal
  { fmantissa :: !Word32
  , fexponent :: !Int32
  } deriving (Show, Eq)

-- | Multiply a 32-bit number with a 64-bit number while keeping the upper 64
-- bits. Then shift by specified amount minus 32
mulShift32 :: Word32 -> Word64 -> Int -> Word32
mulShift32 m factor shift =
  let factorLo = factor .&. mask 32
      factorHi = factor `unsafeShiftR` 32
      bits0 = word32ToWord64 m * factorLo
      bits1 = word32ToWord64 m * factorHi
      total  = (bits0 `unsafeShiftR` 32) + bits1
   in word64ToWord32 $ total `unsafeShiftR` (shift - 32)

-- | Index into the 64-bit word lookup table float_pow5_inv_split
get_float_pow5_inv_split :: Int -> Word64
get_float_pow5_inv_split =
  let !(Addr arr) = float_pow5_inv_split
   in getWord64At arr

-- | Index into the 64-bit word lookup table float_pow5_split
get_float_pow5_split :: Int -> Word64
get_float_pow5_split =
  let !(Addr arr) = float_pow5_split
   in getWord64At arr

-- | Take the high bits of m * 2^k / 5^q / 2^-e2+q+k
mulPow5InvDivPow2 :: Word32 -> Int -> Int -> Word32
mulPow5InvDivPow2 m q j = mulShift32 m (get_float_pow5_inv_split q) j

-- | Take the high bits of m * 5^-e2-q / 2^k / 2^q-k
mulPow5DivPow2 :: Word32 -> Int -> Int -> Word32
mulPow5DivPow2 m i j = mulShift32 m (get_float_pow5_split i) j

-- | Handle case e2 >= 0
f2dGT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState Word32, Int32)
f2dGT e2' u v w =
  let e2 = int32ToInt e2'
      -- q = e10 = log_10(2^e2)
      q = log10pow2 e2
      -- k = B0 + log_2(5^q)
      k = float_pow5_inv_bitcount + pow5bits q - 1
      i = -e2 + q + k
      -- (u, v, w) * 2^k / 5^q / 2^-e2+q+k
      u' = mulPow5InvDivPow2 u q i
      v' = mulPow5InvDivPow2 v q i
      w' = mulPow5InvDivPow2 w q i
      !lastRemoved =
        if q /= 0 && fquot10 (w' - 1) <= fquot10 u'
          -- We need to know one removed digit even if we are not going to loop
          -- below. We could use q = X - 1 above, except that would require 33
          -- bits for the result, and we've found that 32-bit arithmetic is
          -- faster even on 64-bit machines.
          then let l = float_pow5_inv_bitcount + pow5bits (q - 1) - 1
                in frem10 (mulPow5InvDivPow2 v (q - 1) (-e2 + q - 1 + l))
          else 0
      !(vvTrailing, vuTrailing, vw') =
        case () of
          _ | q < 9 && frem5 v == 0
                -> (multipleOfPowerOf5 v q, False, w')
            | q < 9 && acceptBounds v
                -> (False, multipleOfPowerOf5 u q, w')
            | q < 9
                -> (False, False, w' - boolToWord32 (multipleOfPowerOf5 w q))
            | otherwise
                -> (False, False, w')
   in (BoundsState u' v' vw' lastRemoved vuTrailing vvTrailing, intToInt32 q)

-- | Handle case e2 < 0
f2dLT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState Word32, Int32)
f2dLT e2' u v w =
  let e2 = int32ToInt e2'
      q = log10pow5 (-e2)
      e10 = q + e2
      i = (-e2) - q
      -- k = log_2(5^-e2-q) - B1
      k = pow5bits i - float_pow5_bitcount
      j = q - k
      -- (u, v, w) * 5^-e2-q / 2^k / 2^q-k
      u' = mulPow5DivPow2 u i j
      v' = mulPow5DivPow2 v i j
      w' = mulPow5DivPow2 w i j
      !lastRemoved =
        if q /= 0 && fquot10 (w' - 1) <= fquot10 u'
          then let j' = q - 1 - (pow5bits (i + 1) - float_pow5_bitcount)
                in frem10 (mulPow5DivPow2 v (i + 1) j')
          else 0
      !(vvTrailing , vuTrailing, vw') =
        case () of
          _ | q <= 1 && acceptBounds v
                -> (True, v - u == 2, w') -- mmShift == 1
            | q <= 1
                -> (True, False, w' - 1)
            | q < 31
                -> (multipleOfPowerOf2 v (q - 1), False, w')
            | otherwise
                -> (False, False, w')
   in (BoundsState u' v' vw' lastRemoved vuTrailing vvTrailing, intToInt32 e10)

-- | Returns the decimal representation of the given mantissa and exponent of a
-- 32-bit Float using the ryu algorithm.
f2d :: Word32 -> Word32 -> FloatingDecimal
f2d m e =
  let !mf = if e == 0
              then m
              else (1 `unsafeShiftL` float_mantissa_bits) .|. m
      !ef = intToInt32 $ if e == 0
              then 1 - (float_bias + float_mantissa_bits)
              else word32ToInt e - (float_bias + float_mantissa_bits)
      !e2 = ef - 2
      -- Step 2. 3-tuple (u, v, w) * 2**e2
      !u = 4 * mf - 1 - boolToWord32 (m /= 0 || e <= 1)
      !v = 4 * mf
      !w = 4 * mf + 2
      -- Step 3. convert to decimal power base
      !(state, e10) =
        if e2 >= 0
           then f2dGT e2 u v w
           else f2dLT e2 u v w
      -- Step 4: Find the shortest decimal representation in the interval of
      -- valid representations.
      !(output, removed) =
        let rounded = closestCorrectlyRounded (acceptBounds v)
         in first rounded $ if vvIsTrailingZeros state || vuIsTrailingZeros state
           then trimTrailing state
           else trimNoTrailing state
      !e' = e10 + removed
   in FloatingDecimal output e'

-- | Split a Float into (sign, mantissa, exponent)
breakdown :: Float -> (Bool, Word32, Word32)
breakdown f =
  let bits = castFloatToWord32 f
      sign = ((bits `unsafeShiftR` (float_mantissa_bits + float_exponent_bits)) .&. 1) /= 0
      mantissa = bits .&. mask float_mantissa_bits
      expo = (bits `unsafeShiftR` float_mantissa_bits) .&. mask float_exponent_bits
   in (sign, mantissa, expo)

-- | Dispatches to `f2d` and applies the given formatters
{-# INLINE f2s' #-}
f2s' :: (Bool -> Word32 -> Int32 -> a) -> (NonNumbersAndZero -> a) -> Float -> a
f2s' formatter specialFormatter f =
  let (sign, mantissa, expo) = breakdown f
   in if (expo == mask float_exponent_bits) || (expo == 0 && mantissa == 0)
         then specialFormatter NonNumbersAndZero
                  { negative=sign
                  , exponent_all_one=expo > 0
                  , mantissa_non_zero=mantissa > 0 }
         else let FloatingDecimal m e = f2d mantissa expo
               in formatter sign m e

-- | Render a Float in scientific notation
f2s :: Float -> Builder
f2s f = primBounded (f2s' toCharsScientific toCharsNonNumbersAndZero f) ()

-- | Returns the decimal representation of a Float. NaN and Infinity will
-- return `FloatingDecimal 0 0`
f2Intermediate :: Float -> FloatingDecimal
f2Intermediate = f2s' (const FloatingDecimal) (const $ FloatingDecimal 0 0)
