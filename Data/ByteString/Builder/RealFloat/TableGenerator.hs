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

import Data.Bits ((.&.), shiftR, shiftL, shiftR)
import GHC.Float (int2Double)
import GHC.Word (Word64(..))
import Language.Haskell.TH

-- | Representation of 128-bit integer used for lookup table generation
data Word128 = Word128
  { word128Hi64 :: !Word64
  , word128Lo64 :: !Word64
  }

-- The basic floating point conversion algorithm is as such:
--
-- Given floating point
--
--   f = (-1)^s * m_f * 2^e_f
--
-- which is IEEE encoded by `[s] [.. e ..] [.. m ..]`. `s` is the sign bit, `e`
-- is the biased exponent, and `m` is the mantissa, let
--
--       | e /= 0            | e == 0
--  -----+-------------------+-----------
--   m_f | 2^len(m) + m      | m
--   e_f | e - bias - len(m) | 1 - bias - len(m)
--
-- we compute the halfway points to the next smaller (`f-`) and larger (`f+`)
-- floating point numbers as
--
--  lower halfway point u * 2^e2, u = 4 * m_f - (if m == 0 then 1 else 2)
--                      v * 2^e2, v = 4 * m_f
--  upper halfway point w * 2^e2, u = 4 * m_f + 2
--  where e2 = ef - 2 (so u, v, w are integers)
--
--
-- Then we compute (a, b, c) * 10^e10 = (u, v, w) * 2^e2 which is split into
-- the case of
--
--   e2 >= 0   ==>    e10 = 0 , (a, b, c) = (u, v, w) * 2^e2
--   e2 <  0   ==>    e10 = e2, (a, b, c) = (u, v, w) * 5^-e2
--
-- And finally we find the shortest representation from integers d0 and e0 such
-- that
--
--  a * 10^e10 < d0 * 10^(e0+e10) < c * 10^e10
--
-- such that e0 is maximal (we allow equality to smaller or larger halfway
-- point depending on rounding mode). This is found through iteratively
-- dividing by 10 while a/10^j < c/10^j and doing some bookkeeping around
-- zeros.
--
--
--
--
-- The ryu algorithm removes the requirement for arbitrary precision arithmetic
-- and improves the runtime significantly by skipping most of the iterative
-- division by carefully selecting a point where certain invariants hold and
-- precomputing a few tables.
--
-- Specifically, define `q` such that the correspondings values of a/10^q <
-- c/10^q - 1. We can prove (not shown) that
--
--    if e2 >= 0, q = e2 * log_10(2)
--    if e2 <  0, q = -e2 * log_10(5)
--
-- Then we can compute (a, b, c) / 10^q. Starting from (u, v, w) we have
--
--      (a, b, c) / 10^q                  (a, b, c) / 10^q
--    = (u, v, w) * 2^e2 / 10^q    OR   = (u, v, w) * 5^-e2 / 10^q
--
-- And since q < e2,
--
--    = (u, v, w) * 2^e2-q / 5^q   OR   = (u, v, w) * 5^-e2-q / 2^q
--
-- While (u, v, w) are n-bit numbers, 5^q and whatnot are significantly larger,
-- but we only need the top-most n bits of the result so we can choose `k` that
-- reduce the number of bits required to ~2n. We then multiply by either
--
--    2^k / 5^q                    OR   5^-e2-q / 2^k
--
-- The required `k` is roughly linear in the exponent (we need more of the
-- multiplication to be precise) but the number of bits to store the
-- multiplicands above stays fixed.
--
-- Since the number of bits needed is relatively small for IEEE 32- and 64-bit
-- floating types, we can compute appropriate values for `k` for the
-- floating-point-type-specific bounds instead of each e2.
--
-- Finally, we need to do some final manual iterations potentially to do a
-- final fixup of the skipped state


-- | Bound for bits of 2^k / 5^q for floats
float_pow5_inv_bitcount :: Int
float_pow5_inv_bitcount = 59

-- | Bound for bits of 5^-e2-q / 2^k for floats
float_pow5_bitcount :: Int
float_pow5_bitcount = 61

-- | Bound for bits of 5^-e2-q / 2^k for doubles
double_pow5_bitcount :: Int
double_pow5_bitcount = 125

-- | Bound for bits of 2^k / 5^q for doubles
double_pow5_inv_bitcount :: Int
double_pow5_inv_bitcount = 125

-- | Number of bits in a positive integer
blen :: Integer -> Int
blen 0 = 0
blen 1 = 1
blen n = 1 + blen (n `quot` 2)

-- | Used for table generation of 2^k / 5^q + 1
finv :: Int -> Int -> Integer
finv bitcount i =
  let p = 5^i
   in (1 `shiftL` (blen p - 1 + bitcount)) `div` p + 1

-- | Used for table generation of 5^-e2-q / 2^k
fnorm :: Int -> Int -> Integer
fnorm bitcount i =
  let p = 5^i
      s = blen p - bitcount
   in if s < 0 then p `shiftL` (-s) else p `shiftR` s

-- | Generates a compile-time lookup table for floats as Word64
gen_table_f :: Int -> (Int -> Integer) -> Q Exp
gen_table_f n f = return $ ListE (fmap (LitE . IntegerL . f) [0..n])

-- | Generates a compile-time lookup table for doubles as Word128
gen_table_d :: Int -> (Int -> Integer) -> Q Exp
gen_table_d n f = return $ ListE (fmap ff [0..n])
  where
    ff :: Int -> Exp
    ff c = let r = f c
               hi = r `shiftR` 64
               lo = r .&. ((1 `shiftL` 64) - 1)
            in AppE (AppE (ConE 'Word128) (LitE . IntegerL $ hi)) (LitE . IntegerL $ lo)

-- Given a specific floating-point type, determine the range of q for the < 0
-- and >= 0 cases
get_range :: forall ff. (RealFloat ff) => ff -> (Int, Int)
get_range f =
  let (emin, emax) = floatRange f
      mantissaDigits = floatDigits f
      emin' = emin - mantissaDigits - 2
      emax' = emax - mantissaDigits - 2
   in ( (-emin') - floor (int2Double (-emin') * logBase 10 5)
      , floor (int2Double emax' * logBase 10 2))

float_max_split :: Int
float_max_inv_split :: Int
(float_max_split, float_max_inv_split) = get_range (undefined :: Float)

-- we take a slightly different codepath s.t we need one extra entry
double_max_split :: Int
double_max_inv_split :: Int
(double_max_split, double_max_inv_split) =
    let (m, mi) = get_range (undefined :: Double)
     in (m + 1, mi)

