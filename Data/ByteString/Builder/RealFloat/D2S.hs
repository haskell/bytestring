{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module      : Data.ByteString.Builder.RealFloat.D2S
-- Copyright   : (c) Lawrence Wu 2021
-- License     : BSD-style
-- Maintainer  : lawrencejwu@gmail.com
--
-- Implementation of double-to-string conversion

module Data.ByteString.Builder.RealFloat.D2S
    ( d2Intermediate
    , d2s'
    ) where

import Control.Arrow (first)
import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(Proxy))
import GHC.Int (Int32(..))
import GHC.Ptr (Ptr(..))
import GHC.Word (Word64(..))
import GHC.Prim (Word8#)
import Data.Proxy (Proxy(Proxy))

-- See Data.ByteString.Builder.RealFloat.TableGenerator for a high-level
-- explanation of the ryu algorithm

-- | Table of 2^k / 5^q + 1
--
-- > splitWord128s $ fmap (finv double_pow5_inv_bitcount) [0..double_max_inv_split]
foreign import ccall "&hs_bytestring_double_pow5_inv_split"
  double_pow5_inv_split :: Ptr Word64

-- | Table of 5^(-e2-q) / 2^k + 1
--
-- > splitWord128s $ fmap (fnorm double_pow5_bitcount) [0..double_max_split]
foreign import ccall "&hs_bytestring_double_pow5_split"
  double_pow5_split :: Ptr Word64

double_mantissa_bits = mantissaBits @Double

-- | Bias in encoded 64-bit float representation (2^10 - 1)
double_bias :: Int
double_bias = 1023

type FD = FloatingDecimal Double

-- | Quick check for small integers
d2dSmallInt :: Word64 -> Word64 -> Maybe FD
d2dSmallInt m e =
  let m2 = (1 `unsafeShiftL` double_mantissa_bits) .|. m
      e2 = word64ToInt e - (double_bias + double_mantissa_bits)
      fraction = m2 .&. mask (-e2)
   in case () of
        _ -- f = m2 * 2^e2 >= 2^53 is an integer.
          -- Ignore this case for now.
          | e2 > 0 -> Nothing
          -- f < 1
          | e2 < -52 -> Nothing
          -- Since 2^52 <= m2 < 2^53 and 0 <= -e2 <= 52:
          --    1 <= f = m2 / 2^-e2 < 2^53.
          -- Test if the lower -e2 bits of the significand are 0, i.e.
          -- whether the fraction is 0.
          | fraction /= 0 -> Nothing
          -- f is an integer in the range [1, 2^53).
          -- Note: mantissa might contain trailing (decimal) 0's.
          -- Note: since 2^53 < 10^16, there is no need to adjust decimalLength17().
          | otherwise -> Just $ FloatingDecimal (m2 `unsafeShiftR` (-e2)) 0


-- | Removes trailing (decimal) zeros for small integers in the range [1, 2^53)
unifySmallTrailing :: FD -> FD
unifySmallTrailing fd@(FloatingDecimal m e) =
  let !(q, r) = dquotRem10 m
   in if r == 0
        then unifySmallTrailing $ FloatingDecimal q (e + 1)
        else fd

-- TODO: 128-bit intrinsics
-- | Multiply a 64-bit number with a 128-bit number while keeping the upper 64
-- bits. Then shift by specified amount minus 64
mulShift64 :: Word64 -> (Word64, Word64) -> Int -> Word64
mulShift64 m (factorHi, factorLo) shift =
  let !(b0Hi, _   ) = m `timesWord2` factorLo
      !(b1Hi, b1Lo) = m `timesWord2` factorHi
      total = b0Hi + b1Lo
      high  = b1Hi + boolToWord64 (total < b0Hi)
      dist  = shift - 64
   in (high `unsafeShiftL` (64 - dist)) .|. (total `unsafeShiftR` dist)

-- | Index into the 128-bit word lookup table double_pow5_inv_split
get_double_pow5_inv_split :: Int -> (Word64, Word64)
get_double_pow5_inv_split = getWord128At double_pow5_inv_split

-- | Index into the 128-bit word lookup table double_pow5_split
get_double_pow5_split :: Int -> (Word64, Word64)
get_double_pow5_split = getWord128At double_pow5_split

-- | Take the high bits of m * 5^-e2-q / 2^k / 2^q-k
mulPow5DivPow2 :: Word64 -> Int -> Int -> Word64
mulPow5DivPow2 m i j = mulShift64 m (get_double_pow5_split i) j

-- | Take the high bits of m * 2^k / 5^q / 2^-e2+q+k
mulPow5InvDivPow2 :: Word64 -> Int -> Int -> Word64
mulPow5InvDivPow2 m q j = mulShift64 m (get_double_pow5_inv_split q) j

-- | Handle case e2 >= 0
d2dGT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState Word64, Int32)
d2dGT e2' u v w =
  let e2 = int32ToInt e2'
      q = log10pow2 e2 - fromEnum (e2 > 3)
      -- k = B0 + log_2(5^q)
      k = double_pow5_inv_bitcount + pow5bits q - 1
      i = -e2 + q + k
      -- (u, v, w) * 2^k / 5^q / 2^-e2+q+k
      u' = mulPow5InvDivPow2 u q i
      v' = mulPow5InvDivPow2 v q i
      w' = mulPow5InvDivPow2 w q i
      !(vvTrailing, vuTrailing, vw') =
        case () of
          _ | q <= 21 && (drem5 v == 0)
                -> (multipleOfPowerOf5 v q, False, w')
            | q <= 21 && acceptBounds v
                -> (False, multipleOfPowerOf5 u q, w')
            | q <= 21
                -> (False, False, w' - boolToWord64 (multipleOfPowerOf5 w q))
            | otherwise
                -> (False, False, w')
   in (BoundsState u' v' vw' 0 vuTrailing vvTrailing, intToInt32 q)

-- | Handle case e2 < 0
d2dLT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState Word64, Int32)
d2dLT e2' u v w =
  let e2 = int32ToInt e2'
      q = log10pow5 (-e2) - fromEnum (-e2 > 1)
      e10 = q + e2
      i = -e2 - q
      -- k = log_2(5^-e2-q) - B1
      k = pow5bits i - double_pow5_bitcount
      j = q - k
      -- (u, v, w) * 5^-e2-q / 2^k / 2^q-k
      u' = mulPow5DivPow2 u i j
      v' = mulPow5DivPow2 v i j
      w' = mulPow5DivPow2 w i j
      !(vvTrailing, vuTrailing, vw') =
        case () of
          _ | q <= 1 && acceptBounds v
                -> (True, v - u == 2, w') -- mmShift == 1
            | q <= 1
                -> (True, False, w' - 1)
            | q < 63
                -> (multipleOfPowerOf2 v (q - 1), False, w')
            | otherwise
                -> (False, False, w')
   in (BoundsState u' v' vw' 0 vuTrailing vvTrailing, intToInt32 e10)

-- | Returns the decimal representation of the given mantissa and exponent of a
-- 64-bit Double using the ryu algorithm.
d2d :: Word64 -> Word64 -> FD
d2d m e =
  let !mf = if e == 0
              then m
              else (1 `unsafeShiftL` double_mantissa_bits) .|. m
      !ef = intToInt32 $ if e == 0
              then 1 - (double_bias + double_mantissa_bits)
              else word64ToInt e - (double_bias + double_mantissa_bits)
      !e2 = ef - 2
      -- Step 2. 3-tuple (u, v, w) * 2**e2
      !u = 4 * mf - 1 - boolToWord64 (m /= 0 || e <= 1)
      !v = 4 * mf
      !w = 4 * mf + 2
      -- Step 3. convert to decimal power base
      !(state, e10) =
        if e2 >= 0
           then d2dGT e2 u v w
           else d2dLT e2 u v w
      -- Step 4: Find the shortest decimal representation in the interval of
      -- valid representations.
      !(output, removed) =
        let rounded = closestCorrectlyRounded (acceptBounds v)
         in first rounded $ if vvIsTrailingZeros state || vuIsTrailingZeros state
           then trimTrailing state
           else trimNoTrailing state
      !e' = e10 + removed
   in FloatingDecimal output e'

-- | Dispatches to `d2d` or `d2dSmallInt` and applies the given formatters
{-# INLINE d2s' #-}
d2s' :: (Bool -> Word64 -> Int32 -> a) -> (Double -> Maybe a) -> Double -> a
d2s' formatter specialFormatter d = flip fromMaybe (specialFormatter d) $
  let FloatingDecimal m e = d2d mantissa expo
      (sign, mantissa, expo) = breakdown d
  in formatter sign m e

-- | Returns the decimal representation of a Double. NaN and Infinity will
-- return `FloatingDecimal 0 0`
{-# INLINE d2Intermediate #-}
d2Intermediate :: Double -> FD
d2Intermediate = d2s' (const FloatingDecimal) (const Nothing)
