{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Data.ByteString.Builder.RealFloat.F2S
    ( FloatingDecimal(..)
    , f2s
    , f2Intermediate
    ) where

import Data.Bits ((.|.), (.&.))
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import Data.ByteString.Builder.RealFloat.TableGenerator
import GHC.Exts
import GHC.Float (castFloatToWord32)
import GHC.Int (Int32(..))
import GHC.ST (ST(..), runST)
import GHC.Word (Word32(..), Word64(..))

listArray :: Int -> [Word64] -> ByteArray
listArray (I# n) es = runST (ST $ \s1 ->
  let !(# s2, marr #) = newByteArray# (n *# 8#) s1
      go (W64# y) r = \i s ->
        let s' = writeWord64Array# marr i y s
         in if isTrue# (i ==# n) then s' else r (i +# 1#) s'
      !(# s3, bs #) = unsafeFreezeByteArray# marr (foldr go (\_ s -> s) es 0# s2)
   in (# s3, ByteArray bs #))

float_pow5_inv_split :: ByteArray
float_pow5_inv_split = listArray (fromIntegral float_max_inv_split + 1)
    $(gen_table_f float_max_inv_split (finv $ fromIntegral float_pow5_inv_bitcount))

float_pow5_split :: ByteArray
float_pow5_split = listArray (fromIntegral float_max_split + 1)
    $(gen_table_f float_max_split (fnorm $ fromIntegral float_pow5_bitcount))

float_mantissa_bits :: Word32
float_mantissa_bits = 23

float_exponent_bits :: Word32
float_exponent_bits = 8

float_bias :: Word32
float_bias = 127

data FloatingDecimal = FloatingDecimal
  { fmantissa :: !Word32
  , fexponent :: !Int32
  } deriving (Show, Eq)

toS :: Word32 -> Int32
toS = fromIntegral

mulShift32Unboxed :: Word# -> Word# -> Int# -> Word#
mulShift32Unboxed m factor shift =
  let factorLo = narrow32Word# factor
      factorHi = factor `uncheckedShiftRL#` 32#
      bits0 = m `timesWord#` factorLo
      bits1 = m `timesWord#` factorHi
      total  = (bits0 `uncheckedShiftRL#` 32#) `plusWord#` bits1
   in narrow32Word# (total `uncheckedShiftRL#` (shift -# 32#))

get_float_pow5_inv_split :: Int# -> Word#
get_float_pow5_inv_split i =
  let !(ByteArray arr) = float_pow5_inv_split
   in indexWord64Array# arr i

get_float_pow5_split :: Int# -> Word#
get_float_pow5_split i =
  let !(ByteArray arr) = float_pow5_split
   in indexWord64Array# arr i

mulPow5InvDivPow2 :: Word# -> Word# -> Int# -> Word#
mulPow5InvDivPow2 m q j = mulShift32Unboxed m (get_float_pow5_inv_split (word2Int# q)) j

mulPow5DivPow2 :: Word# -> Int# -> Int# -> Word#
mulPow5DivPow2 m i j = mulShift32Unboxed m (get_float_pow5_split i) j

acceptBounds :: Word32 -> Bool
acceptBounds !(W32# v) = isTrue# (acceptBoundsUnboxed v)

data BoundsState = BoundsState
    { vu :: !Word32
    , vv :: !Word32
    , vw :: !Word32
    , lastRemovedDigit :: !Word32
    , vuIsTrailingZeros :: !Bool
    , vvIsTrailingZeros :: !Bool
    }

trimTrailing' :: BoundsState -> (BoundsState, Int32)
trimTrailing' !d
  | vw' > vu' =
    let !(vv', vvRem) = fquotRem10Boxed $ vv d
     in fmap ((+) 1) . trimTrailing' $
         d { vu = vu'
           , vv = vv'
           , vw = vw'
           , lastRemovedDigit = vvRem
           , vuIsTrailingZeros = vuIsTrailingZeros d && vuRem == 0
           , vvIsTrailingZeros = vvIsTrailingZeros d && lastRemovedDigit d == 0
           }
  | otherwise = (d, 0)
  where
    (vu', vuRem) = fquotRem10Boxed $ vu d
    vw' = fwrapped fquot10 (vw d)

trimTrailing'' :: BoundsState -> (BoundsState, Int32)
trimTrailing'' !d
  | vuRem == 0 =
    let !(vv', vvRem) = fquotRem10Boxed $ vv d
        !vw' = fwrapped fquot10 (vw d)
     in fmap ((+) 1) . trimTrailing'' $
         d { vu = vu'
           , vv = vv'
           , vw = vw'
           , lastRemovedDigit = vvRem
           , vvIsTrailingZeros = vvIsTrailingZeros d && lastRemovedDigit d == 0
           }
  | otherwise = (d, 0)
  where
    (vu', vuRem) = fquotRem10Boxed $ vu d

trimTrailing :: BoundsState -> (BoundsState, Int32)
trimTrailing !d =
  let !(d', r) = trimTrailing' d
      !(d'', r') = if vuIsTrailingZeros d'
                     then trimTrailing'' d'
                     else (d', 0)
      res = if vvIsTrailingZeros d'' && lastRemovedDigit d'' == 5 && vv d'' `rem` 2 == 0
               -- set `{ lastRemovedDigit = 4 }` to round-even
               then d''
               else d''
   in (res, r + r')

trimNoTrailing' :: Word# -> Word# -> Word# -> Word# -> Int# -> (# Word#, Word#, Word#, Int# #)
trimNoTrailing' u' v' w' lastRemoved count =
  case vw' `gtWord#` vu' of
    0# -> (# u', v', lastRemoved , count #)
    _  -> let !(# vv', ld #) = fquotRem10 v'
           in trimNoTrailing' vu' vv' vw' ld (count +# 1#)
  where
    !vu' = fquot10 u'
    !vw' = fquot10 w'

trimNoTrailing :: BoundsState -> (BoundsState, Int32)
trimNoTrailing !(BoundsState (W32# u') (W32# v') (W32# w') (W32# ld) _ _) =
  let !(# vu', vv', ld', c' #) = trimNoTrailing' u' v' w' ld 0#
   in (BoundsState (W32# vu') (W32# vv') 0 (W32# ld') False False, I32# c')

f2dGT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState, Int32)
f2dGT (I32# e2) (W32# u) (W32# v) (W32# w) =
  let q = int2Word# (log10pow2Unboxed e2)
      e10 = word2Int# q
      k = unbox float_pow5_inv_bitcount +# pow5bitsUnboxed (word2Int# q) -# 1#
      i = negateInt# e2 +# word2Int# q +# k
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

f2dLT :: Int32 -> Word32 -> Word32 -> Word32 -> (BoundsState, Int32)
f2dLT (I32# e2) (W32# u) (W32# v) (W32# w) =
  let q = int2Word# (log10pow5Unboxed (negateInt# e2))
      e10 = word2Int# q +# e2
      i = (negateInt# e2) -# word2Int# q
      k = pow5bitsUnboxed i -# unbox float_pow5_bitcount
      j = word2Int# q -# k
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

roundUp :: Bool -> BoundsState -> Bool
roundUp b s = (vv s == vu s && b) || lastRemovedDigit s >= 5

calculate :: Bool -> BoundsState -> Word32
calculate b s = vv s + asWord (roundUp b s)

f2d :: Word32 -> Word32 -> FloatingDecimal
f2d m e =
  let !mf = if e == 0
              then m
              else (1 .<< float_mantissa_bits) .|. m
      !ef = if e == 0
              then toS 1 - toS (float_bias + float_mantissa_bits)
              else toS e - toS (float_bias + float_mantissa_bits)
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
        if vvIsTrailingZeros state || vuIsTrailingZeros state
           then pmap (\s -> calculate (not (acceptBounds v)
                                    || not (vuIsTrailingZeros s)) s)
                                      $ trimTrailing state
           else pmap (calculate True) $ trimNoTrailing state
      !e' = e10 + removed
   in FloatingDecimal output e'

breakdown :: Float -> (Bool, Word32, Word32)
breakdown f =
  let bits = castFloatToWord32 f
      sign = ((bits .>> (float_mantissa_bits + float_exponent_bits)) .&. 1) /= 0
      mantissa = bits .&. mask float_mantissa_bits
      expo = (bits .>> float_mantissa_bits) .&. mask float_exponent_bits
   in (sign, mantissa, expo)

{-# INLINE f2s' #-}
f2s' :: (Bool -> Word32 -> Int32 -> a) -> (Bool -> Bool -> Bool -> a) -> Float -> a
f2s' formatter specialFormatter f =
  let (sign, mantissa, expo) = breakdown f
   in if (expo == mask float_exponent_bits) || (expo == 0 && mantissa == 0)
         then specialFormatter sign (expo > 0) (mantissa > 0)
         else let FloatingDecimal m e = f2d mantissa expo
               in formatter sign m e

f2s :: Float -> Builder
f2s f = primBounded (f2s' toCharsScientific special f) ()

f2Intermediate :: Float -> FloatingDecimal
f2Intermediate = f2s' (const FloatingDecimal) (\_ _ _ -> FloatingDecimal 0 0)
