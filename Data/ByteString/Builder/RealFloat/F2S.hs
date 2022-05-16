{-# LANGUAGE CPP #-}
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

#if !PURE_HASKELL
import GHC.Ptr (Ptr(..))
#endif

-- See Data.ByteString.Builder.RealFloat.TableGenerator for a high-level
-- explanation of the ryu algorithm

#if !PURE_HASKELL
-- | Table of 2^k / 5^q + 1
--
-- > fmap (finv float_pow5_inv_bitcount) [0..float_max_inv_split]
foreign import ccall "&hs_bytestring_float_pow5_inv_split"
  float_pow5_inv_split :: Ptr Word64

-- | Table of 5^(-e2-q) / 2^k + 1
--
-- > fmap (fnorm float_pow5_bitcount) [0..float_max_split]
foreign import ccall "&hs_bytestring_float_pow5_split"
  float_pow5_split :: Ptr Word64
#endif

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
#if !PURE_HASKELL
get_float_pow5_inv_split = getWord64At float_pow5_inv_split
#else
-- > putStr $ case64 (finv float_pow5_inv_bitcount) [0..float_max_inv_split]
get_float_pow5_inv_split i = case i of
  0  -> 0x800000000000001
  1  -> 0x666666666666667
  2  -> 0x51eb851eb851eb9
  3  -> 0x4189374bc6a7efa
  4  -> 0x68db8bac710cb2a
  5  -> 0x53e2d6238da3c22
  6  -> 0x431bde82d7b634e
  7  -> 0x6b5fca6af2bd216
  8  -> 0x55e63b88c230e78
  9  -> 0x44b82fa09b5a52d
  10 -> 0x6df37f675ef6eae
  11 -> 0x57f5ff85e592558
  12 -> 0x465e6604b7a8447
  13 -> 0x709709a125da071
  14 -> 0x5a126e1a84ae6c1
  15 -> 0x480ebe7b9d58567
  16 -> 0x734aca5f6226f0b
  17 -> 0x5c3bd5191b525a3
  18 -> 0x49c97747490eae9
  19 -> 0x760f253edb4ab0e
  20 -> 0x5e72843249088d8
  21 -> 0x4b8ed0283a6d3e0
  22 -> 0x78e480405d7b966
  23 -> 0x60b6cd004ac9452
  24 -> 0x4d5f0a66a23a9db
  25 -> 0x7bcb43d769f762b
  26 -> 0x63090312bb2c4ef
  27 -> 0x4f3a68dbc8f03f3
  28 -> 0x7ec3daf94180651
  29 -> 0x65697bfa9acd1da
  _  -> 0x51212ffbaf0a7e2
#endif

-- | Index into the 64-bit word lookup table float_pow5_split
get_float_pow5_split :: Int -> Word64
#if !PURE_HASKELL
get_float_pow5_split = getWord64At float_pow5_split
#else
-- > putStr $ case64 (fnorm float_pow5_bitcount) [0..float_max_split]
get_float_pow5_split i = case i of
  0  -> 0x1000000000000000
  1  -> 0x1400000000000000
  2  -> 0x1900000000000000
  3  -> 0x1f40000000000000
  4  -> 0x1388000000000000
  5  -> 0x186a000000000000
  6  -> 0x1e84800000000000
  7  -> 0x1312d00000000000
  8  -> 0x17d7840000000000
  9  -> 0x1dcd650000000000
  10 -> 0x12a05f2000000000
  11 -> 0x174876e800000000
  12 -> 0x1d1a94a200000000
  13 -> 0x12309ce540000000
  14 -> 0x16bcc41e90000000
  15 -> 0x1c6bf52634000000
  16 -> 0x11c37937e0800000
  17 -> 0x16345785d8a00000
  18 -> 0x1bc16d674ec80000
  19 -> 0x1158e460913d0000
  20 -> 0x15af1d78b58c4000
  21 -> 0x1b1ae4d6e2ef5000
  22 -> 0x10f0cf064dd59200
  23 -> 0x152d02c7e14af680
  24 -> 0x1a784379d99db420
  25 -> 0x108b2a2c28029094
  26 -> 0x14adf4b7320334b9
  27 -> 0x19d971e4fe8401e7
  28 -> 0x1027e72f1f128130
  29 -> 0x1431e0fae6d7217c
  30 -> 0x193e5939a08ce9db
  31 -> 0x1f8def8808b02452
  32 -> 0x13b8b5b5056e16b3
  33 -> 0x18a6e32246c99c60
  34 -> 0x1ed09bead87c0378
  35 -> 0x13426172c74d822b
  36 -> 0x1812f9cf7920e2b6
  37 -> 0x1e17b84357691b64
  38 -> 0x12ced32a16a1b11e
  39 -> 0x178287f49c4a1d66
  40 -> 0x1d6329f1c35ca4bf
  41 -> 0x125dfa371a19e6f7
  42 -> 0x16f578c4e0a060b5
  43 -> 0x1cb2d6f618c878e3
  44 -> 0x11efc659cf7d4b8d
  45 -> 0x166bb7f0435c9e71
  _  -> 0x1c06a5ec5433c60d
#endif

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
