{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, MagicHash #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.ByteString.Builder.RealFloat.F2S
-- Copyright   : (c) Lawrence Wu 2021
-- License     : BSD-style
-- Maintainer  : lawrencejwu@gmail.com
--
-- Implementation of float-to-string conversion

module Data.ByteString.Builder.RealFloat.F2S
    ( f2s
    , f2Intermediate
    ) where

import Control.Arrow (first)
import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import GHC.Int (Int32(..))
import GHC.Ptr (Ptr(..))
import GHC.Word (Word32(..), Word64(..))
import GHC.Prim (Word8#)

-- See Data.ByteString.Builder.RealFloat.TableGenerator for a high-level
-- explanation of the ryu algorithm

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

-- | Bias in encoded 32-bit float representation (2^7 - 1)
float_bias :: Int
float_bias = 127

type FD = FloatingDecimal Float

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
get_float_pow5_inv_split = getWord64At float_pow5_inv_split

-- | Index into the 64-bit word lookup table float_pow5_split
get_float_pow5_split :: Int -> Word64
get_float_pow5_split = getWord64At float_pow5_split

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
f2d :: Word32 -> Word32 -> FD
f2d m e =
  let float_mantissa_bits = mantissaBits @Float
      !mf = if e == 0
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

-- | Dispatches to `f2d` and applies the given formatters
{-# INLINE f2s' #-}
f2s' :: (Bool -> Word32 -> Int32 -> a) -> (NonNumbersAndZero -> a) -> Float -> a
f2s' formatter specialFormatter f =
  let (sign, mantissa, expo) = breakdown f
   in if (expo == mask (exponentBits @Float)) || (expo == 0 && mantissa == 0)
         then specialFormatter NonNumbersAndZero
                  { negative=sign
                  , exponent_all_one=expo > 0
                  , mantissa_non_zero=mantissa > 0 }
         else let FloatingDecimal m e = f2d mantissa expo
               in formatter sign m e

-- | Render a Float in scientific notation
f2s :: Word8# -> SpecialStrings -> Float -> Builder
f2s eE ss f = primBounded (f2s' (toCharsScientific eE) (toCharsNonNumbersAndZero ss) f) ()

-- | Returns the decimal representation of a Float. NaN and Infinity will
-- return `FloatingDecimal 0 0`
{-# INLINE f2Intermediate #-}
f2Intermediate :: Float -> FD
f2Intermediate = f2s' (const FloatingDecimal) (const $ FloatingDecimal 0 0)
