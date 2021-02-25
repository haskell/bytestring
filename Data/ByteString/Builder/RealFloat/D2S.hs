{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes #-}

module Data.ByteString.Builder.RealFloat.D2S
    ( FloatingDecimal(..)
    , d2s
    , d2Intermediate
    ) where

import Data.Bits ((.|.), (.&.))
import Data.ByteString.Builder.Internal (Builder)
import Data.ByteString.Builder.Prim (primBounded)
import Data.ByteString.Builder.RealFloat.Internal
import Data.ByteString.Builder.RealFloat.TableGenerator
import Data.Maybe (fromMaybe)
import GHC.Exts
import GHC.Int (Int32(..), Int64(..))
import GHC.ST (ST(..), runST)
import GHC.Word (Word32(..), Word64(..))

listArray :: Int -> [Word128] -> ByteArray
listArray (I# n) es = runST (ST $ \s1 ->
  let !(# s2, marr #) = newByteArray# (n *# 16#) s1
      go (Word128 (W64# hi) (W64# lo)) r = \i s ->
        let s'  = writeWord64Array# marr (i *# 2#) hi s
            s'' = writeWord64Array# marr (i *# 2# +# 1#) lo s'
         in if isTrue# (i ==# n) then s'' else r (i +# 1#) s''
      !(# s3, bs #) = unsafeFreezeByteArray# marr (foldr go (\_ s -> s) es 0# s2)
   in (# s3, ByteArray bs #))

double_pow5_inv_split :: ByteArray
double_pow5_inv_split = listArray (fromIntegral double_max_inv_split + 1)
    $(gen_table_d double_max_inv_split (finv $ fromIntegral double_pow5_inv_bitcount))

double_pow5_split :: ByteArray
double_pow5_split = listArray (fromIntegral double_max_split + 1)
    $(gen_table_d double_max_split (fnorm $ fromIntegral double_pow5_bitcount))

double_mantissa_bits :: Word64
double_mantissa_bits = 52

double_exponent_bits :: Word64
double_exponent_bits = 11

double_bias :: Word64
double_bias = 1023

data FloatingDecimal = FloatingDecimal
  { dmantissa :: !Word64
  , dexponent :: !Int32
  } deriving (Show, Eq)

toS :: Word64 -> Int64
toS = fromIntegral

toU :: Int64 -> Word64
toU = fromIntegral

d2dSmallInt :: Word64 -> Word32 -> Maybe FloatingDecimal
d2dSmallInt m e =
  let m2 = (1 .<< double_mantissa_bits) .|. m
      e2 = fromIntegral e - toS double_bias - toS double_mantissa_bits
      fraction = m2 .&. mask (toU $ -e2)
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
          | otherwise -> Just $ FloatingDecimal (m2 .>> (toU $ -e2)) 0

unifySmallTrailing :: FloatingDecimal -> FloatingDecimal
unifySmallTrailing fd@(FloatingDecimal (W64# m) e) =
  let !(# q, r #) = dquotRem10 m
   in case r `neWord#` 0## of
        0# -> unifySmallTrailing $ FloatingDecimal (W64# q) (e + 1)
        _  -> fd

-- TODO: 128-bit intrinsics
mulShift64Unboxed :: Word# -> (# Word#, Word# #) -> Int# -> Word#
mulShift64Unboxed m (# factorHi, factorLo #) shift =
  let !(# b0Hi, _ #) = m `timesWord2#` factorLo
      !(# b1Hi, b1Lo #) = m `timesWord2#` factorHi
      total = b0Hi `plusWord#` b1Lo
      high = b1Hi `plusWord#` (int2Word# (total `ltWord#` b0Hi))
      dist = shift -# 64#
   in (high `uncheckedShiftL#` (64# -# dist)) `or#` (total `uncheckedShiftRL#` dist)

get_double_pow5_inv_split :: Int# -> (# Word#, Word# #)
get_double_pow5_inv_split i =
  let !(ByteArray arr) = double_pow5_inv_split
   in (# indexWord64Array# arr (i *# 2#), indexWord64Array# arr (i *# 2# +# 1#) #)

get_double_pow5_split :: Int# -> (# Word#, Word# #)
get_double_pow5_split i =
  let !(ByteArray arr) = double_pow5_split
   in (# indexWord64Array# arr (i *# 2#), indexWord64Array# arr (i *# 2# +# 1#) #)

mulPow5DivPow2 :: Word# -> Int# -> Int# -> Word#
mulPow5DivPow2 m i j = mulShift64Unboxed m (get_double_pow5_split i) j

mulPow5InvDivPow2 :: Word# -> Word# -> Int# -> Word#
mulPow5InvDivPow2 m q j = mulShift64Unboxed m (get_double_pow5_inv_split (word2Int# q)) j


acceptBounds :: Word64 -> Bool
acceptBounds !(W64# v) = isTrue# (acceptBoundsUnboxed v)

data BoundsState = BoundsState
    { vu :: !Word64
    , vv :: !Word64
    , vw :: !Word64
    , lastRemovedDigit :: !Word64
    , vuIsTrailingZeros :: !Bool
    , vvIsTrailingZeros :: !Bool
    } deriving Show

trimTrailing' :: BoundsState -> (BoundsState, Int32)
trimTrailing' !d
  | vw' > vu' =
    let !(vv', vvRem) = dquotRem10Boxed $ vv d
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
    !(vu', vuRem) = dquotRem10Boxed $ vu d
    !vw' = dwrapped dquot10 (vw d)

trimTrailing'' :: BoundsState -> (BoundsState, Int32)
trimTrailing'' d
  | vuRem == 0 =
    let !(vv', vvRem) = dquotRem10Boxed $ vv d
        !vw' = dwrapped dquot10 (vw d)
     in fmap ((+) 1) . trimTrailing'' $
         d { vu = vu'
           , vv = vv'
           , vw = vw'
           , lastRemovedDigit = vvRem
           , vvIsTrailingZeros = vvIsTrailingZeros d && lastRemovedDigit d == 0
           }
  | otherwise = (d, 0)
  where
    !(vu', vuRem) = dquotRem10Boxed $ vu d

trimTrailing :: BoundsState -> (BoundsState, Int32)
trimTrailing d =
  let !(d', r) = trimTrailing' d
      !(d'', r') = if vuIsTrailingZeros d'
                     then trimTrailing'' d'
                     else (d', 0)
      res = if vvIsTrailingZeros d'' && lastRemovedDigit d'' == 5 && vv d'' `rem` 2 == 0
               -- set `{ lastRemovedDigit = 4 }` to round-even
               then d''
               else d''
   in (res, r + r')

trimNoTrailing'' :: Word# -> Word# -> Word# -> Word# -> Int# -> (# Word#, Word#, Word#, Int# #)
trimNoTrailing'' u' v' w' lastRemoved count =
  case vw' `gtWord#` vu' of
    0# -> (# u', v', lastRemoved , count #)
    _  -> let !(# vv', ld #) = dquotRem10 v'
           in trimNoTrailing' vu' vv' vw' ld (count +# 1#)
  where
    !vu' = dquot10 u'
    !vw' = dquot10 w'

trimNoTrailing' :: Word# -> Word# -> Word# -> Word# -> Int# -> (# Word#, Word#, Word#, Int# #)
trimNoTrailing' u' v' w' lastRemoved count =
  -- Loop iterations below (approximately), without div 100 optimization:
  -- 0: 0.03%, 1: 13.8%, 2: 70.6%, 3: 14.0%, 4: 1.40%, 5: 0.14%, 6+: 0.02%
  -- Loop iterations below (approximately), with div 100 optimization:
  -- 0: 70.6%, 1: 27.8%, 2: 1.40%, 3: 0.14%, 4+: 0.02%
  let !vw' = dquot100 w'
      !vu' = dquot100 u'
   in case vw' `gtWord#` vu' of
        0# -> trimNoTrailing'' u' v' w' lastRemoved count
        _  -> let !vv' = dquot100 v'
                  !ld = dquot10 (v' `minusWord#` (vv' `timesWord#` 100##))
               in trimNoTrailing'' vu' vv' vw' ld (count +# 2#)

trimNoTrailing :: BoundsState -> (BoundsState, Int32)
trimNoTrailing !(BoundsState (W64# u' ) (W64# v') (W64# w') (W64# ld) _ _) =
  let !(# vu', vv', ld', c' #) = trimNoTrailing' u' v' w' ld 0#
   in (BoundsState (W64# vu') (W64# vv') 0 (W64# ld') False False, I32# c')

d2dGT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState, Int32)
d2dGT (I32# e2) (W64# u) (W64# v) (W64# w) =
  let q = int2Word# (log10pow2Unboxed e2 -# (e2 ># 3#))
      e10 = word2Int# q
      k = unbox double_pow5_inv_bitcount +# pow5bitsUnboxed (word2Int# q) -# 1#
      i = (negateInt# e2) +# word2Int# q +# k
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

d2dLT :: Int32 -> Word64 -> Word64 -> Word64 -> (BoundsState, Int32)
d2dLT (I32# e2) (W64# u) (W64# v) (W64# w) =
  let nege2 = negateInt# e2
      q = int2Word# (log10pow5Unboxed nege2 -# (nege2 ># 1#))
      e10 = word2Int# q +# e2
      i = nege2 -# word2Int# q
      k = pow5bitsUnboxed i -# unbox double_pow5_bitcount
      j = word2Int# q -# k
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

roundUp :: Bool -> BoundsState -> Bool
roundUp b s = (vv s == vu s && b) || lastRemovedDigit s >= 5

calculate :: Bool -> BoundsState -> Word64
calculate b s = vv s + asWord (roundUp b s)

d2d :: Word64 -> Word32 -> FloatingDecimal
d2d m e =
  let !mf = if e == 0
              then m
              else (1 .<< double_mantissa_bits) .|. m
      !ef = if e == 0
              then toS 1 - toS double_bias - toS double_mantissa_bits
              else fromIntegral e - toS double_bias - toS double_mantissa_bits
      !e2 = fromIntegral ef - 2 :: Int32
      -- Step 2. 3-tuple (u, v, w) * 2**e2
      !u = 4 * mf - 1 - asWord (m /= 0 || e <= 1)
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
        if vvIsTrailingZeros state || vuIsTrailingZeros state
           then pmap (\s -> calculate (not (acceptBounds v)
                                    || not (vuIsTrailingZeros s)) s)
                                      $ trimTrailing state
           else pmap (calculate True) $ trimNoTrailing state
      !e' = e10 + removed
   in FloatingDecimal output e'

{-# INLINE castDoubleToWord64 #-}
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 (D# d#) = W64# (stgDoubleToWord64 d#)

foreign import prim "stg_doubleToWord64zh"
    stgDoubleToWord64 :: Double# -> Word#

breakdown :: Double -> (Bool, Word64, Word64)
breakdown f =
  let bits = castDoubleToWord64 f
      sign = ((bits .>> (double_mantissa_bits + double_exponent_bits)) .&. 1) /= 0
      mantissa = bits .&. mask double_mantissa_bits
      expo = (bits .>> double_mantissa_bits) .&. mask double_exponent_bits
   in (sign, mantissa, expo)

{-# INLINE d2s' #-}
d2s' :: (Bool -> Word64 -> Int32 -> a) -> (Bool -> Bool -> Bool -> a) -> Double -> a
d2s' formatter specialFormatter d =
  let (sign, mantissa, expo) = breakdown d
   in if (expo == mask double_exponent_bits) || (expo == 0 && mantissa == 0)
         then specialFormatter sign (expo > 0) (mantissa > 0)
         else let v = unifySmallTrailing <$> d2dSmallInt mantissa (fromIntegral expo)
                  FloatingDecimal m e = fromMaybe (d2d mantissa (fromIntegral expo)) v
               in formatter sign m e

d2s :: Double -> Builder
d2s d = primBounded (d2s' toCharsScientific special d) ()

d2Intermediate :: Double -> FloatingDecimal
d2Intermediate = d2s' (const FloatingDecimal) (\_ _ _ -> FloatingDecimal 0 0)
