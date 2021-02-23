{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Data.ByteString.Builder.RealFloat.TableGenerator
  ( Word128(..)
  , float_pow5_inv_bitcount
  , float_pow5_bitcount
  , double_pow5_bitcount
  , double_pow5_inv_bitcount
  , float_max_split
  , float_max_inv_split
  , double_max_split
  , double_max_inv_split
  , finv
  , fnorm
  , gen_table_f
  , gen_table_d
  ) where

import Data.Array.Base
import Data.Bits ((.&.), shiftR, shiftL, shiftR)
import GHC.Exts
import GHC.ST (ST(..), runST)
import GHC.Word (Word64(..))
import Language.Haskell.TH

data Word128 = Word128
  { word128Hi64 :: !Word64
  , word128Lo64 :: !Word64
  }

instance Num Word128 where
  (+) = plus128
  (-) = minus128
  (*) = times128
  negate = negate128
  abs = id
  signum = signum128
  fromInteger = fromInteger128

{-# INLINABLE plus128 #-}
plus128 :: Word128 -> Word128 -> Word128
plus128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# s1) (W64# s0)
  where
    !(# c1, s0 #) = plusWord2# a0 b0
    s1a = plusWord# a1 b1
    s1 = plusWord# c1 s1a

{-# INLINABLE minus128 #-}
minus128 :: Word128 -> Word128 -> Word128
minus128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# d1) (W64# d0)
  where
    !(# d0, c1 #) = subWordC# a0 b0
    a1c = minusWord# a1 (int2Word# c1)
    d1 = minusWord# a1c b1

times128 :: Word128 -> Word128 -> Word128
times128 (Word128 (W64# a1) (W64# a0)) (Word128 (W64# b1) (W64# b0)) =
  Word128 (W64# p1) (W64# p0)
  where
    !(# c1, p0 #) = timesWord2# a0 b0
    p1a = timesWord# a1 b0
    p1b = timesWord# a0 b1
    p1c = plusWord# p1a p1b
    p1 = plusWord# p1c c1

{-# INLINABLE negate128 #-}
negate128 :: Word128 -> Word128
negate128 (Word128 (W64# a1) (W64# a0)) =
  case plusWord2# (not# a0) 1## of
    (# c, s #) -> Word128 (W64# (plusWord# (not# a1) c)) (W64# s)

{-# INLINABLE signum128 #-}
signum128 :: Word128 -> Word128
signum128 (Word128 (W64# 0##) (W64# 0##)) = Word128 0 0
signum128 _ = Word128 0 1

fromInteger128 :: Integer -> Word128
fromInteger128 i = Word128 (fromIntegral $ i `shiftR` 64) (fromIntegral i)

instance MArray (STUArray s) Word128 (ST s) where
  {-# INLINE getBounds #-}
  getBounds (STUArray l u _ _) = return (l,u)

  {-# INLINE getNumElements #-}
  getNumElements (STUArray _ _ n _) = return n

  {-# INLINE unsafeNewArray_ #-}
  unsafeNewArray_ (l,u) = unsafeNewArraySTUArray_ (l,u) (*# 16#)

  {-# INLINE unsafeRead #-}
  unsafeRead (STUArray _ _ _ marr) (I# i) = ST $ \s1 ->
    let !(# s2, w1 #) = readWord64Array# marr (i *# 2#) s1
        !(# _, w2 #) = readWord64Array# marr (i *# 2# +# 1#) s2
     in (# s2, Word128 (W64# w1) (W64# w2) #)

  {-# INLINE unsafeWrite #-}
  unsafeWrite (STUArray _ _ _ marr) (I# i) (Word128 (W64# w1) (W64# w2)) = ST $ \s1 ->
    let s2 = writeWord64Array# marr (i *# 2#) w1 s1
        s3 = writeWord64Array# marr (i *# 2# +# 1#) w2 s2
     in (# s3, () #)

instance IArray UArray Word128 where
  {-# INLINE bounds #-}
  bounds (UArray l u _ _) = (l,u)

  {-# INLINE numElements #-}
  numElements (UArray _ _ n _) = n

  {-# INLINE unsafeArray #-}
  unsafeArray lu ies = runST (unsafeArrayUArray lu ies 0)

  -- NB: don't actually use this anywhere but...
  {-# INLINE unsafeAt #-}
  unsafeAt (UArray _ _ _ arr) (I# i) =
    let w1 = indexWord64Array# arr (i *# 2#)
        w2 = indexWord64Array# arr (i *# 2# +# 1#)
     in Word128 (W64# w1) (W64# w2)


float_pow5_inv_bitcount :: Int
float_pow5_inv_bitcount = 59

float_pow5_bitcount :: Int
float_pow5_bitcount = 61

double_pow5_bitcount :: Int
double_pow5_bitcount = 125

double_pow5_inv_bitcount :: Int
double_pow5_inv_bitcount = 125

blen :: Integer -> Integer
blen 0 = 0
blen 1 = 1
blen n = 1 + blen (n `quot` 2)

finv :: Integer -> Integer -> Integer
finv bitcount i =
  let p = 5^i
   in (1 `shiftL` fromIntegral (blen p - 1 + bitcount)) `div` p + 1

fnorm :: Integer -> Integer -> Integer
fnorm bitcount i =
  let p = 5^i
      s = fromIntegral (blen p - bitcount)
   in if s < 0 then p `shiftL` (-s) else p `shiftR` s

gen_table_f :: (Integral a) => a -> (a -> Integer) -> Q Exp
gen_table_f n f = return $ ListE (fmap (LitE . IntegerL . f) [0..n])

gen_table_d :: forall a. (Integral a) => a -> (a -> Integer) -> Q Exp
gen_table_d n f = return $ ListE (fmap ff [0..n])
  where
    ff :: a -> Exp
    ff c = let r = f c
               hi = r `shiftR` 64
               lo = r .&. ((1 `shiftL` 64) - 1)
            in AppE (AppE (ConE 'Word128) (LitE . IntegerL $ hi)) (LitE . IntegerL $ lo)

get_range :: forall ff. (RealFloat ff) => ff -> (Integer, Integer)
get_range f =
  let (emin, emax) = floatRange f
      mantissaDigits = floatDigits f
      emin' = fromIntegral $ emin - mantissaDigits - 2
      emax' = fromIntegral $ emax - mantissaDigits - 2
      log10 :: ff -> ff
      log10 x = log x / log 10
   in ((-emin') - floor (fromIntegral (-emin') * log10 5), floor $ emax' * log10 2)

float_max_split :: Integer
float_max_inv_split :: Integer
(float_max_split, float_max_inv_split) = get_range (undefined :: Float)

-- we take a slightly different codepath s.t we need one extra entry
double_max_split :: Integer
double_max_inv_split :: Integer
(double_max_split, double_max_inv_split) =
    let (m, mi) = get_range (undefined :: Double)
     in (m + 1, mi)

