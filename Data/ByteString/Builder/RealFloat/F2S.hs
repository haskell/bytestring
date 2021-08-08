{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Data.ByteString.Builder.RealFloat.F2S
    ( FloatingDecimal(..)
    , f2s
    , f2Intermediate
    ) where

import Control.Arrow (first)
import Data.Bits ((.|.), (.&.))
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import Data.ByteString.Builder.RealFloat.TableGenerator
import GHC.Exts
import GHC.Int (Int32(..))
import GHC.ST (ST(..), runST)
import GHC.Word (Word32(..), Word64(..))

-- See Data.ByteString.Builder.RealFloat.TableGenerator for a high-level
-- explanation of the ryu algorithm

-- | Convert a [Word64] into a raw array of Word64 bytes laid out contiguously
-- for fast indexing
listArray :: [Word64] -> ByteArray
listArray es = runST (ST $ \s1 ->
  let !(I# n) = length es + 1
      !(# s2, marr #) = newByteArray# (n *# 8#) s1
      go (W64# y) r = \i s ->
        let s' = writeWord64Array# marr i y s
         in if isTrue# (i ==# n) then s' else r (i +# 1#) s'
      !(# s3, bs #) = unsafeFreezeByteArray# marr (foldr go (\_ s -> s) es 0# s2)
   in (# s3, ByteArray bs #))

-- | Table of 2^k / 5^q + 1
float_pow5_inv_split :: ByteArray
float_pow5_inv_split = listArray
    $(gen_table_f float_max_inv_split (finv $ fromIntegral float_pow5_inv_bitcount))

-- | Table of 5^(-e2-q) / 2^k + 1
float_pow5_split :: ByteArray
float_pow5_split = listArray
    $(gen_table_f float_max_split (fnorm $ fromIntegral float_pow5_bitcount))

-- | Number of mantissa bits of a 32-bit float. The number of significant bits
-- (floatDigits (undefined :: Float)) is 24 since we have a leading 1 for
-- normal floats and 0 for subnormal floats
float_mantissa_bits :: Word32
float_mantissa_bits = 23

-- | Number of exponent bits of a 32-bit float
float_exponent_bits :: Word32
float_exponent_bits = 8

-- | Bias in encoded 32-bit float representation (2^7 - 1)
float_bias :: Word32
float_bias = 127

data FloatingDecimal = FloatingDecimal
  { fmantissa :: !Word32
  , fexponent :: !Int32
  } deriving (Show, Eq)

-- | Multiply a 32-bit number with a 64-bit number while keeping the upper 64
-- bits. Then shift by specified amount minus 32
mulShift32Unboxed :: Word# -> Word# -> Int# -> Word#
mulShift32Unboxed m factor shift =
  let factorLo = narrow32Word# factor
      factorHi = factor `uncheckedShiftRL#` 32#
      bits0 = m `timesWord#` factorLo
      bits1 = m `timesWord#` factorHi
      total  = (bits0 `uncheckedShiftRL#` 32#) `plusWord#` bits1
   in narrow32Word# (total `uncheckedShiftRL#` (shift -# 32#))

-- | Index into the 64-bit word lookup table float_pow5_inv_split
get_float_pow5_inv_split :: Int# -> Word#
get_float_pow5_inv_split i =
  let !(ByteArray arr) = float_pow5_inv_split
   in indexWord64Array# arr i

-- | Index into the 64-bit word lookup table float_pow5_split
get_float_pow5_split :: Int# -> Word#
get_float_pow5_split i =
  let !(ByteArray arr) = float_pow5_split
   in indexWord64Array# arr i

-- | Take the high bits of m * 2^k / 5^q / 2^-e2+q+k
mulPow5InvDivPow2 :: Word# -> Word# -> Int# -> Word#
mulPow5InvDivPow2 m q j = mulShift32Unboxed m (get_float_pow5_inv_split (word2Int# q)) j

-- | Take the high bits of m * 5^-e2-q / 2^k / 2^q-k
mulPow5DivPow2 :: Word# -> Int# -> Int# -> Word#
mulPow5DivPow2 m i j = mulShift32Unboxed m (get_float_pow5_split i) j

-- | Wrapper around acceptBoundsUnboxed for Word32
acceptBounds :: Word32 -> Bool
acceptBounds !(W32# v) = isTrue# (acceptBoundsUnboxed v)

-- | Handle case e2 >= 0
f2dGT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState Word32, Int32)
f2dGT (I32# e2) (W32# u) (W32# v) (W32# w) =
  let q = int2Word# (log10pow2Unboxed e2)
      e10 = word2Int# q
      -- k = B0 + log_2(5^q)
      k = unbox float_pow5_inv_bitcount +# pow5bitsUnboxed (word2Int# q) -# 1#
      i = negateInt# e2 +# word2Int# q +# k
      -- (u, v, w) * 2^k / 5^q / 2^-e2+q+k
      u' = mulPow5InvDivPow2 u q i
      v' = mulPow5InvDivPow2 v q i
      w' = mulPow5InvDivPow2 w q i
      !lastRemoved =
        case (q `neWord#` 0##) `andI#` ((fquot10 (w' `minusWord#` 1##)) `leWord#` fquot10 u') of
          -- We need to know one removed digit even if we are not going to loop
          -- below. We could use q = X - 1 above, except that would require 33
          -- bits for the result, and we've found that 32-bit arithmetic is
          -- faster even on 64-bit machines.
          0# -> 0##
          _  -> let l = unbox float_pow5_inv_bitcount +# pow5bitsUnboxed (word2Int# q -# 1#) -# 1#
                 in frem10 (mulPow5InvDivPow2 v (q `minusWord#` 1##) (negateInt# e2 +# word2Int# q -# 1# +# l))
      !(# vvTrailing, vuTrailing, vw' #) =
        case () of
          _ | isTrue# ((q `leWord#` 9##) `andI#` (frem5 v `eqWord#` 0##))
                -> (# multipleOfPowerOf5_UnboxedB v q, False, w' #)
            | isTrue# ((q `leWord#` 9##) `andI#` acceptBoundsUnboxed v)
                -> (# False, multipleOfPowerOf5_UnboxedB u q, w' #)
            | isTrue# (q `leWord#` 9##)
                -> (# False, False, w' `minusWord#` int2Word# (multipleOfPowerOf5_Unboxed w q) #)
            | otherwise
                -> (# False, False, w' #)
   in (BoundsState (W32# u') (W32# v') (W32# vw') (W32# lastRemoved) vuTrailing vvTrailing , (I32# e10))

-- | Handle case e2 < 0
f2dLT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState Word32, Int32)
f2dLT (I32# e2) (W32# u) (W32# v) (W32# w) =
  let q = int2Word# (log10pow5Unboxed (negateInt# e2))
      e10 = word2Int# q +# e2
      i = (negateInt# e2) -# word2Int# q
      -- k = log_2(5^-e2-q) - B1
      k = pow5bitsUnboxed i -# unbox float_pow5_bitcount
      j = word2Int# q -# k
      -- (u, v, w) * 5^-e2-q / 2^k / 2^q-k
      u' = mulPow5DivPow2 u i j
      v' = mulPow5DivPow2 v i j
      w' = mulPow5DivPow2 w i j
      !lastRemoved =
        case (q `neWord#` 0##) `andI#` ((fquot10 (u'`minusWord#` 1##)) `leWord#` fquot10 u') of
          0# -> 0##
          _  -> let j' = word2Int# q -# 1# -# (pow5bitsUnboxed (i +# 1#) -# unbox float_pow5_bitcount)
                 in frem10 (mulPow5DivPow2 v (i +# 1#) j')
      !(# vvTrailing , vuTrailing, vw' #) =
        case () of
          _ | isTrue# ((q `leWord#` 1##) `andI#` acceptBoundsUnboxed v)
                -> (# True, isTrue# ((w `minusWord#` v) `eqWord#` 2##), w' #) -- mmShift == 1
            | isTrue# (q `leWord#` 1##)
                -> (# True, False, w' `minusWord#` 1## #)
            | isTrue# (q `ltWord#` 31##)
                -> (# isTrue# (multipleOfPowerOf2Unboxed v (q `minusWord#` 1##)), False, w' #)
            | otherwise
                -> (# False, False, w' #)
   in (BoundsState (W32# u') (W32# v') (W32# vw') (W32# lastRemoved) vuTrailing vvTrailing , (I32# e10))

-- | Returns the decimal representation of the given mantissa and exponent of a
-- 32-bit Float using the ryu algorithm.
f2d :: Word32 -> Word32 -> FloatingDecimal
f2d m e =
  let !mf = if e == 0
              then m
              else (1 .<< float_mantissa_bits) .|. m
      !ef = fromIntegral $ if e == 0
              then 1 - (float_bias + float_mantissa_bits)
              else e - (float_bias + float_mantissa_bits)
      !e2 = ef - 2
      -- Step 2. 3-tuple (u, v, w) * 2**e2
      !u = 4 * mf - 1 - asWord (m /= 0 || e <= 1)
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
      sign = ((bits .>> (float_mantissa_bits + float_exponent_bits)) .&. 1) /= 0
      mantissa = bits .&. mask float_mantissa_bits
      expo = (bits .>> float_mantissa_bits) .&. mask float_exponent_bits
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
