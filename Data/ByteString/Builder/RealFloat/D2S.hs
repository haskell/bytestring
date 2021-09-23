{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
-- | Implementation of double-to-string conversion

module Data.ByteString.Builder.RealFloat.D2S
    ( FloatingDecimal(..)
    , d2s
    , d2Intermediate
    ) where

import Control.Arrow (first)
import Data.Bits ((.|.), (.&.), unsafeShiftL, unsafeShiftR)
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import Data.ByteString.Builder.RealFloat.TableGenerator
import Data.Maybe (fromMaybe)
import GHC.Exts
import GHC.Int (Int32(..))
import GHC.ST (ST(..), runST)
import GHC.Word (Word64(..))

-- See Data.ByteString.Builder.RealFloat.TableGenerator for a high-level
-- explanation of the ryu algorithm

-- | Convert a [Word128] into a raw array of Word64 bytes laid out contiguously
-- for fast indexing
listArray :: [Word128] -> ByteArray
listArray es = runST (ST $ \s1 ->
  let !(I# n) = length es + 1
      !(# s2, marr #) = newByteArray# (n *# 16#) s1
      go (Word128 (W64# hi) (W64# lo)) r = \i s ->
        let s'  = writeWord64Array# marr (i *# 2#) hi s
            s'' = writeWord64Array# marr (i *# 2# +# 1#) lo s'
         in if isTrue# (i ==# n) then s'' else r (i +# 1#) s''
      !(# s3, bs #) = unsafeFreezeByteArray# marr (foldr go (\_ s -> s) es 0# s2)
   in (# s3, ByteArray bs #))

-- | Table of 2^k / 5^q + 1
double_pow5_inv_split :: ByteArray
double_pow5_inv_split = listArray
    $(gen_table_d double_max_inv_split (finv double_pow5_inv_bitcount))

-- | Table of 5^(-e2-q) / 2^k + 1
double_pow5_split :: ByteArray
double_pow5_split = listArray
    $(gen_table_d double_max_split (fnorm double_pow5_bitcount))

-- | Number of mantissa bits of a 64-bit float. The number of significant bits
-- (floatDigits (undefined :: Double)) is 53 since we have a leading 1 for
-- normal floats and 0 for subnormal floats
double_mantissa_bits :: Int
double_mantissa_bits = 52

-- | Number of exponent bits of a 64-bit float
double_exponent_bits :: Int
double_exponent_bits = 11

-- | Bias in encoded 64-bit float representation (2^10 - 1)
double_bias :: Int
double_bias = 1023

data FloatingDecimal = FloatingDecimal
  { dmantissa :: !Word64
  , dexponent :: !Int32
  } deriving (Show, Eq)

-- | Quick check for small integers
d2dSmallInt :: Word64 -> Word64 -> Maybe FloatingDecimal
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
unifySmallTrailing :: FloatingDecimal -> FloatingDecimal
unifySmallTrailing fd@(FloatingDecimal (W64# m) e) =
  let !(# q, r #) = dquotRem10 m
   in case r `neWord#` 0## of
        0# -> unifySmallTrailing $ FloatingDecimal (W64# q) (e + 1)
        _  -> fd

-- TODO: 128-bit intrinsics
-- | Multiply a 64-bit number with a 128-bit number while keeping the upper 64
-- bits. Then shift by specified amount minus 64
mulShift64Unboxed :: Word# -> (# Word#, Word# #) -> Int# -> Word#
mulShift64Unboxed m (# factorHi, factorLo #) shift =
  let !(# b0Hi, _ #) = m `timesWord2#` factorLo
      !(# b1Hi, b1Lo #) = m `timesWord2#` factorHi
      total = b0Hi `plusWord#` b1Lo
      high = b1Hi `plusWord#` (int2Word# (total `ltWord#` b0Hi))
      dist = shift -# 64#
   in (high `uncheckedShiftL#` (64# -# dist)) `or#` (total `uncheckedShiftRL#` dist)

-- | Index into the 128-bit word lookup table double_pow5_inv_split
get_double_pow5_inv_split :: Int# -> (# Word#, Word# #)
get_double_pow5_inv_split i =
  let !(ByteArray arr) = double_pow5_inv_split
   in (# indexWord64Array# arr (i *# 2#), indexWord64Array# arr (i *# 2# +# 1#) #)

-- | Index into the 128-bit word lookup table double_pow5_split
get_double_pow5_split :: Int# -> (# Word#, Word# #)
get_double_pow5_split i =
  let !(ByteArray arr) = double_pow5_split
   in (# indexWord64Array# arr (i *# 2#), indexWord64Array# arr (i *# 2# +# 1#) #)

-- | Take the high bits of m * 5^-e2-q / 2^k / 2^q-k
mulPow5DivPow2 :: Word# -> Int# -> Int# -> Word#
mulPow5DivPow2 m i j = mulShift64Unboxed m (get_double_pow5_split i) j

-- | Take the high bits of m * 2^k / 5^q / 2^-e2+q+k
mulPow5InvDivPow2 :: Word# -> Word# -> Int# -> Word#
mulPow5InvDivPow2 m q j = mulShift64Unboxed m (get_double_pow5_inv_split (word2Int# q)) j

-- | Wrapper around acceptBoundsUnboxed for Word64
acceptBounds :: Word64 -> Bool
acceptBounds !(W64# v) = isTrue# (acceptBoundsUnboxed v)

-- | Handle case e2 >= 0
d2dGT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState Word64, Int32)
d2dGT (I32# e2) (W64# u) (W64# v) (W64# w) =
  let q = int2Word# (log10pow2Unboxed e2 -# (e2 ># 3#))
      e10 = word2Int# q
      -- k = B0 + log_2(5^q)
      k = unbox double_pow5_inv_bitcount +# pow5bitsUnboxed (word2Int# q) -# 1#
      i = (negateInt# e2) +# word2Int# q +# k
      -- (u, v, w) * 2^k / 5^q / 2^-e2+q+k
      u' = mulPow5InvDivPow2 u q i
      v' = mulPow5InvDivPow2 v q i
      w' = mulPow5InvDivPow2 w q i
      !(# vvTrailing, vuTrailing, vw' #) =
        case () of
          _ | isTrue# ((q `leWord#` 21##) `andI#` (frem5 v `eqWord#` 0##))
                -> (# multipleOfPowerOf5_UnboxedB v q, False, w' #)
            | isTrue# ((q `leWord#` 21##) `andI#` acceptBoundsUnboxed v)
                -> (# False, multipleOfPowerOf5_UnboxedB u q, w' #)
            | isTrue# (q `leWord#` 21##)
                -> (# False, False, w' `minusWord#` int2Word# (multipleOfPowerOf5_Unboxed w q) #)
            | otherwise
                -> (# False, False, w' #)
   in (BoundsState (W64# u') (W64# v') (W64# vw') 0 vuTrailing vvTrailing, (I32# e10))

-- | Handle case e2 < 0
d2dLT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState Word64, Int32)
d2dLT (I32# e2) (W64# u) (W64# v) (W64# w) =
  let nege2 = negateInt# e2
      q = int2Word# (log10pow5Unboxed nege2 -# (nege2 ># 1#))
      e10 = word2Int# q +# e2
      i = nege2 -# word2Int# q
      -- k = log_2(5^-e2-q) - B1
      k = pow5bitsUnboxed i -# unbox double_pow5_bitcount
      j = word2Int# q -# k
      -- (u, v, w) * 5^-e2-q / 2^k / 2^q-k
      u' = mulPow5DivPow2 u i j
      v' = mulPow5DivPow2 v i j
      w' = mulPow5DivPow2 w i j
      !(# vvTrailing, vuTrailing, vw' #) =
        case () of
          _ | isTrue# ((q `leWord#` 1##) `andI#` acceptBoundsUnboxed v)
                -> (# True, isTrue# ((w `minusWord#` v) `eqWord#` 2##), w' #) -- mmShift == 1
            | isTrue# (q `leWord#` 1##)
                -> (# True, False, w' `minusWord#` 1## #)
            | isTrue# (q `ltWord#` 63##)
                -> (# isTrue# (multipleOfPowerOf2Unboxed v (q `minusWord#` 1##)), False, w' #)
            | otherwise
                -> (# False, False, w' #)
   in (BoundsState (W64# u') (W64# v') (W64# vw') 0 vuTrailing vvTrailing, (I32# e10))

-- | Returns the decimal representation of the given mantissa and exponent of a
-- 64-bit Double using the ryu algorithm.
d2d :: Word64 -> Word64 -> FloatingDecimal
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

-- | Split a Double into (sign, mantissa, exponent)
breakdown :: Double -> (Bool, Word64, Word64)
breakdown f =
  let bits = castDoubleToWord64 f
      sign = ((bits `unsafeShiftR` (double_mantissa_bits + double_exponent_bits)) .&. 1) /= 0
      mantissa = bits .&. mask double_mantissa_bits
      expo = (bits `unsafeShiftR` double_mantissa_bits) .&. mask double_exponent_bits
   in (sign, mantissa, expo)

-- | Dispatches to `d2d` or `d2dSmallInt` and applies the given formatters
{-# INLINE d2s' #-}
d2s' :: (Bool -> Word64 -> Int32 -> a) -> (NonNumbersAndZero -> a) -> Double -> a
d2s' formatter specialFormatter d =
  let (sign, mantissa, expo) = breakdown d
   in if (expo == mask double_exponent_bits) || (expo == 0 && mantissa == 0)
         then specialFormatter NonNumbersAndZero
                  { negative=sign
                  , exponent_all_one=expo > 0
                  , mantissa_non_zero=mantissa > 0 }
         else let v = unifySmallTrailing <$> d2dSmallInt mantissa expo
                  FloatingDecimal m e = fromMaybe (d2d mantissa expo) v
               in formatter sign m e

-- | Render a Double in scientific notation
d2s :: Double -> Builder
d2s d = primBounded (d2s' toCharsScientific toCharsNonNumbersAndZero d) ()

-- | Returns the decimal representation of a Double. NaN and Infinity will
-- return `FloatingDecimal 0 0`
d2Intermediate :: Double -> FloatingDecimal
d2Intermediate = d2s' (const FloatingDecimal) (const $ FloatingDecimal 0 0)
