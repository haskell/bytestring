{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : Data.ByteString.Builder.RealFloat.Internal
-- Copyright   : (c) Lawrence Wu 2021
-- License     : BSD-style
-- Maintainer  : lawrencejwu@gmail.com
--
-- Various floating-to-string conversion helpers that are somewhat
-- floating-size agnostic
--
-- This module includes
--
-- - Efficient formatting for scientific floating-to-string
-- - Trailing zero handling when converting to decimal power base
-- - Approximations for logarithms of powers
-- - Fast-division by reciprocal multiplication
-- - Prim-op bit-wise peek

module Data.ByteString.Builder.RealFloat.Internal
    ( mask
    , NonNumbersAndZero(..)
    , toCharsNonNumbersAndZero
    , SpecialStrings(..)
    , DecimalLength(..)
    , Mantissa
    , pow5bits
    , log10pow2
    , log10pow5
    , pow5_factor
    , multipleOfPowerOf5
    , multipleOfPowerOf2
    , acceptBounds
    , BoundsState(..)
    , trimTrailing
    , trimNoTrailing
    , closestCorrectlyRounded
    , toCharsScientific
    , asciiRaw
    -- hand-rolled division and remainder for f2s and d2s
    , fquot10
    , frem10
    , fquot5
    , frem5
    , dquot10
    , dquotRem10
    , dquot5
    , drem5
    , dquot100
    -- prim-op helpers
    , timesWord2
    , castDoubleToWord64
    , castFloatToWord32
    , getWord64At
    , getWord128At
    -- monomorphic conversions
    , boolToWord32
    , boolToWord64
    , int32ToInt
    , intToInt32
    , word32ToInt
    , word64ToInt
    , word32ToWord64
    , word64ToWord32
    -- joining Float and Double logic
    , FloatingDecimal(..)
    , MantissaWord

    , module Data.ByteString.Builder.RealFloat.TableGenerator
    ) where

import Control.Monad (foldM)
import Data.Bits (Bits(..), FiniteBits(..))
import Data.ByteString.Internal (c2w)
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boundedPrim)
import Data.ByteString.Builder.RealFloat.TableGenerator
import Data.ByteString.Utils.UnalignedWrite
import Data.Char (ord)
import Foreign.C.Types
import GHC.Int (Int(..), Int32(..))
import GHC.IO (IO(..), unIO)
import GHC.Prim
import GHC.Ptr (Ptr(..), plusPtr, castPtr)
import GHC.Types (isTrue#)
import GHC.Word (Word8, Word16(..), Word32(..), Word64(..))
import qualified Foreign.Storable as S (poke)

#include <ghcautoconf.h>
#include "MachDeps.h"

#if WORD_SIZE_IN_BITS < 64 && !MIN_VERSION_ghc_prim(0,8,0)
import GHC.IntWord64
#endif

import Data.ByteString.Builder.Prim.Internal.Floating
  (castFloatToWord32, castDoubleToWord64)

-- | Build a full bit-mask of specified length.
--
-- e.g
--
-- > showHex (mask 12) [] = "fff"
{-# INLINABLE mask #-}
mask :: (Bits a, Integral a) => Int -> a
mask = flip (-) 1 . unsafeShiftL 1

-- | Convert boolean false to 0 and true to 1
{-# INLINABLE boolToWord32 #-}
boolToWord32 :: Bool -> Word32
boolToWord32 = fromIntegral . fromEnum

-- | Convert boolean false to 0 and true to 1
{-# INLINABLE boolToWord64 #-}
boolToWord64 :: Bool -> Word64
boolToWord64 = fromIntegral . fromEnum

-- | Monomorphic conversion for @Int32 -> Int@
{-# INLINABLE int32ToInt #-}
int32ToInt :: Int32 -> Int
int32ToInt = fromIntegral

-- | Monomorphic conversion for @Int -> Int32@
{-# INLINABLE intToInt32 #-}
intToInt32 :: Int -> Int32
intToInt32 = fromIntegral

-- | Monomorphic conversion for @Word32 -> Int@
{-# INLINABLE word32ToInt #-}
word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

-- | Monomorphic conversion for @Word64 -> Int@
{-# INLINABLE word64ToInt #-}
word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral

-- | Monomorphic conversion for @Word32 -> Word64@
{-# INLINABLE word32ToWord64 #-}
word32ToWord64 :: Word32 -> Word64
word32ToWord64 = fromIntegral

-- | Monomorphic conversion for @Word64 -> Word32@
{-# INLINABLE word64ToWord32 #-}
word64ToWord32 :: Word64 -> Word32
word64ToWord32 = fromIntegral


-- | Returns the number of decimal digits in v, which must not contain more than 9 digits.
decimalLength9 :: Word32 -> Int
decimalLength9 v
  | v >= 100000000 = 9
  | v >= 10000000 = 8
  | v >= 1000000 = 7
  | v >= 100000 = 6
  | v >= 10000 = 5
  | v >= 1000 = 4
  | v >= 100 = 3
  | v >= 10 = 2
  | otherwise = 1

-- | Returns the number of decimal digits in v, which must not contain more than 17 digits.
decimalLength17 :: Word64 -> Int
decimalLength17 v
  | v >= 10000000000000000 = 17
  | v >= 1000000000000000 = 16
  | v >= 100000000000000 = 15
  | v >= 10000000000000 = 14
  | v >= 1000000000000 = 13
  | v >= 100000000000 = 12
  | v >= 10000000000 = 11
  | v >= 1000000000 = 10
  | v >= 100000000 = 9
  | v >= 10000000 = 8
  | v >= 1000000 = 7
  | v >= 100000 = 6
  | v >= 10000 = 5
  | v >= 1000 = 4
  | v >= 100 = 3
  | v >= 10 = 2
  | otherwise = 1

class DecimalLength a where decimalLength :: a -> Int
instance DecimalLength Word32 where decimalLength = decimalLength9
instance DecimalLength Word64 where decimalLength = decimalLength17

-- From 'In-and-Out Conversions' https://dl.acm.org/citation.cfm?id=362887, we
-- have that a conversion from a base-b n-digit number to a base-v m-digit
-- number such that the round-trip conversion is identity requires
--
--    v^(m-1) > b^n
--
-- Specifically for binary floating point to decimal conversion, we must have
--
--    10^(m-1) > 2^n
-- => log(10^(m-1)) > log(2^n)
-- => (m-1) * log(10) > n * log(2)
-- => m-1 > n * log(2) / log(10)
-- => m-1 >= ceil(n * log(2) / log(10))
-- => m >= ceil(n * log(2) / log(10)) + 1
--
-- And since 32 and 64-bit floats have 23 and 52 bits of mantissa (and then an
-- implicit leading-bit), we need
--
--    ceil(24 * log(2) / log(10)) + 1 => 9
--    ceil(53 * log(2) / log(10)) + 1 => 17
--
-- In addition, the exponent range from floats is [-45,38] and doubles is
-- [-324,308] (including subnormals) which are 3 and 4 digits respectively
--
-- Thus we have,
--
--    floats: 1 (sign) + 9 (mantissa) + 1 (.) + 1 (e) + 3 (exponent) = 15
--    doubles: 1 (sign) + 17 (mantissa) + 1 (.) + 1 (e) + 4 (exponent) = 24
--
maxEncodedLength :: Int
maxEncodedLength = 32

-- | Storable.poke a String into a Ptr Word8, converting through c2w
pokeAll :: String -> Ptr Word8 -> IO (Ptr Word8)
pokeAll s ptr = foldM pokeOne ptr s
  where pokeOne p c = S.poke p (c2w c) >> return (p `plusPtr` 1)

-- | Unsafe creation of a bounded primitive of String at most length
-- `maxEncodedLength`
boundString :: String -> BoundedPrim ()
boundString s = boundedPrim maxEncodedLength $ const (pokeAll s)

-- | Special rendering for NaN, positive\/negative 0, and positive\/negative
-- infinity. These are based on the IEEE representation of non-numbers.
--
-- Infinity
--
--   * sign = 0 for positive infinity, 1 for negative infinity.
--   * biased exponent = all 1 bits.
--   * fraction = all 0 bits.
--
-- NaN
--
--   * sign = either 0 or 1 (ignored)
--   * biased exponent = all 1 bits.
--   * fraction = anything except all 0 bits.
--
-- We also handle 0 specially here so that the exponent rendering is more
-- correct.
--
--   * sign = either 0 or 1.
--   * biased exponent = all 0 bits.
--   * fraction = all 0 bits.
data NonNumbersAndZero = NonNumbersAndZero
  { negative :: Bool
  , exponent_all_one :: Bool
  , mantissa_non_zero :: Bool
  }

-- | Renders NonNumbersAndZero into bounded primitive
toCharsNonNumbersAndZero :: SpecialStrings -> NonNumbersAndZero -> BoundedPrim ()
toCharsNonNumbersAndZero SpecialStrings{..} NonNumbersAndZero{..}
  | mantissa_non_zero = boundString nan
  | exponent_all_one = if negative then boundString negativeInfinity else boundString positiveInfinity
  | negative = boundString negativeZero
  | otherwise = boundString positiveZero

data SpecialStrings = SpecialStrings
  { nan :: String
  , positiveInfinity :: String
  , negativeInfinity :: String
  , positiveZero :: String
  , negativeZero :: String
  } deriving Show

-- | Part of the calculation on whether to round up the decimal representation.
-- This is currently a constant function to match behavior in Base `show` and
-- is implemented as
--
-- @
-- acceptBounds _ = False
-- @
--
-- For round-to-even and correct shortest, use
--
-- @
-- acceptBounds v = ((v \`quot\` 4) .&. 1) == 0
-- @
acceptBounds :: Mantissa a => a -> Bool
acceptBounds _ = False

-------------------------------------------------------------------------------
-- Logarithm Approximations
--
-- These are based on the same transformations.
--
-- e.g
--
--      log_2(5^e)                              goal function
--    = e * log_2(5)                            log exponenation
--   ~= e * floor(10^7 * log_2(5)) / 10^7       integer operations
--   ~= e * 1217359 / 2^19                      approximation into n / 2^m
--
-- These are verified in the unit tests for the given input ranges
-------------------------------------------------------------------------------

-- | Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
pow5bitsUnboxed :: Int# -> Int#
pow5bitsUnboxed e = (e *# 1217359#) `uncheckedIShiftRL#` 19# +# 1#

-- | Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
log10pow2Unboxed :: Int# -> Int#
log10pow2Unboxed e = (e *# 78913#) `uncheckedIShiftRL#` 18#

-- | Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
log10pow5Unboxed :: Int# -> Int#
log10pow5Unboxed e = (e *# 732923#) `uncheckedIShiftRL#` 20#

-- | Boxed versions of the functions above
pow5bits, log10pow2, log10pow5 :: Int -> Int
pow5bits  = wrapped pow5bitsUnboxed
log10pow2 = wrapped log10pow2Unboxed
log10pow5 = wrapped log10pow5Unboxed

-------------------------------------------------------------------------------
-- Fast Division
--
-- Division is slow. We leverage fixed-point arithmetic to calculate division
-- by a constant as multiplication by the inverse. This could potentially be
-- handled by an aggressive compiler, but to ensure that the optimization
-- happens, we hard-code the expected divisions / remainders by 5, 10, 100, etc
--
-- e.g
--
--     x / 5                                      goal function
--   = x * (1 / 5)                                reciprocal
--   = x * (4 / 5) / 4
--   = x * 0b0.110011001100.. / 4                 recurring binary representation
--  ~= x * (0xCCCCCCCD / 2^32) / 4                approximation with integers
--   = (x * 0xCCCCCCCD) >> 34
--
-- Look for `Reciprocal Multiplication, a tutorial` by Douglas W. Jones for a
-- more detailed explanation.
-------------------------------------------------------------------------------

-- | Returns @w / 10@
fquot10 :: Word32 -> Word32
fquot10 w = word64ToWord32 ((word32ToWord64 w * 0xCCCCCCCD) `unsafeShiftR` 35)

-- | Returns @w % 10@
frem10 :: Word32 -> Word32
frem10 w = w - fquot10 w * 10

-- | Returns @(w / 10, w % 10)@
fquotRem10 :: Word32 -> (Word32, Word32)
fquotRem10 w =
  let w' = fquot10 w
   in (w', w - fquot10 w * 10)

-- | Returns @w / 100@
fquot100 :: Word32 -> Word32
fquot100 w = word64ToWord32 ((word32ToWord64 w * 0x51EB851F) `unsafeShiftR` 37)

-- | Returns @(w / 10000, w % 10000)@
fquotRem10000 :: Word32 -> (Word32, Word32)
fquotRem10000 w =
  let w' = word64ToWord32 ((word32ToWord64 w * 0xD1B71759) `unsafeShiftR` 45)
    in (w', w - w' * 10000)

-- | Returns @w / 5@
fquot5 :: Word32 -> Word32
fquot5 w = word64ToWord32 ((word32ToWord64 w * 0xCCCCCCCD) `unsafeShiftR` 34)

-- | Returns @w % 5@
frem5 :: Word32 -> Word32
frem5 w = w - fquot5 w * 5

-- | Returns @w / 10@
dquot10 :: Word64 -> Word64
dquot10 w =
  let !(rdx, _) = w `timesWord2` 0xCCCCCCCCCCCCCCCD
    in rdx `unsafeShiftR` 3

-- | Returns @w / 100@
dquot100 :: Word64 -> Word64
dquot100 w =
  let !(rdx, _) = (w `unsafeShiftR` 2) `timesWord2` 0x28F5C28F5C28F5C3
    in rdx `unsafeShiftR` 2

-- | Returns @(w / 10000, w % 10000)@
dquotRem10000 :: Word64 -> (Word64, Word64)
dquotRem10000 w =
  let !(rdx, _) = w `timesWord2` 0x346DC5D63886594B
      w' = rdx `unsafeShiftR` 11
   in (w', w - w' * 10000)

-- | Returns @(w / 10, w % 10)@
dquotRem10 :: Word64 -> (Word64, Word64)
dquotRem10 w =
  let w' = dquot10 w
   in (w', w - w' * 10)

-- | Returns @w / 5@
dquot5 :: Word64 -> Word64
dquot5 w =
  let !(rdx, _) = w `timesWord2` 0xCCCCCCCCCCCCCCCD
    in rdx `unsafeShiftR` 2

-- | Returns @w % 5@
drem5 :: Word64 -> Word64
drem5 w = w - dquot5 w * 5

-- | Returns @(w / 5, w % 5)@
dquotRem5 :: Word64 -> (Word64, Word64)
dquotRem5 w =
  let w' = dquot5 w
   in (w', w - w' * 5)

-- | Wrap a unboxed function on Int# into the boxed equivalent
wrapped :: (Int# -> Int#) -> Int -> Int
wrapped f (I# w) = I# (f w)

#if WORD_SIZE_IN_BITS == 32
-- | Packs 2 32-bit system words (hi, lo) into a Word64
packWord64 :: Word# -> Word# -> Word64#
packWord64 hi lo =
#if defined(WORDS_BIGENDIAN)
    ((wordToWord64# lo) `uncheckedShiftL64#` 32#) `or64#` (wordToWord64# hi)
#else
    ((wordToWord64# hi) `uncheckedShiftL64#` 32#) `or64#` (wordToWord64# lo)
#endif

-- | Unpacks a Word64 into 2 32-bit words (hi, lo)
unpackWord64 :: Word64# -> (# Word#, Word# #)
unpackWord64 w =
#if defined(WORDS_BIGENDIAN)
    (# word64ToWord# w
     , word64ToWord# (w `uncheckedShiftRL64#` 32#)
     #)
#else
    (# word64ToWord# (w `uncheckedShiftRL64#` 32#)
     , word64ToWord# w
     #)
#endif

-- | Adds 2 Word64's with 32-bit addition and manual carrying
plusWord64 :: Word64# -> Word64# -> Word64#
plusWord64 x y =
  let !(# x_h, x_l #) = unpackWord64 x
      !(# y_h, y_l #) = unpackWord64 y
      lo = x_l `plusWord#` y_l
      carry = int2Word# (lo `ltWord#` x_l)
      hi = x_h `plusWord#` y_h `plusWord#` carry
   in packWord64 hi lo
#endif

-- | Boxed version of `timesWord2#` for 64 bits
timesWord2 :: Word64 -> Word64 -> (Word64, Word64)
timesWord2 a b =
  let ra = raw a
      rb = raw b
#if WORD_SIZE_IN_BITS >= 64
#if __GLASGOW_HASKELL__ < 903
      !(# hi, lo #) = ra `timesWord2#` rb
#else
      !(# hi_, lo_ #) = word64ToWord# ra `timesWord2#` word64ToWord# rb
      hi = wordToWord64# hi_
      lo = wordToWord64# lo_
#endif
#else
      !(# x_h, x_l #) = unpackWord64 ra
      !(# y_h, y_l #) = unpackWord64 rb

      !(# phh_h, phh_l #) = x_h `timesWord2#` y_h
      !(# phl_h, phl_l #) = x_h `timesWord2#` y_l
      !(# plh_h, plh_l #) = x_l `timesWord2#` y_h
      !(# pll_h, pll_l #) = x_l `timesWord2#` y_l

      --          x1 x0
      --  X       y1 y0
      --  -------------
      --             00  LOW PART
      --  -------------
      --          00
      --       10 10     MIDDLE PART
      --  +       01
      --  -------------
      --       01
      --  + 11 11        HIGH PART
      --  -------------

      phh = packWord64 phh_h phh_l
      phl = packWord64 phl_h phl_l

      !(# mh, ml #) = unpackWord64 (phl
        `plusWord64` (wordToWord64# pll_h)
        `plusWord64` (wordToWord64# plh_l))

      hi = phh
        `plusWord64` (wordToWord64# mh)
        `plusWord64` (wordToWord64# plh_h)

      lo = packWord64 ml pll_l
#endif
   in (W64# hi, W64# lo)

-- | #ifdef for 64-bit word that seems to work on both 32- and 64-bit platforms
type WORD64 =
#if WORD_SIZE_IN_BITS < 64 || __GLASGOW_HASKELL__ >= 903
  Word64#
#else
  Word#
#endif

-- | Returns the number of times @w@ is divisible by @5@
pow5_factor :: WORD64 -> Int# -> Int#
pow5_factor w count =
  let !(W64# q, W64# r) = dquotRem5 (W64# w)
#if WORD_SIZE_IN_BITS >= 64 && __GLASGOW_HASKELL__ < 903
   in case r `eqWord#` 0## of
#else
   in case r `eqWord64#` wordToWord64# 0## of
#endif
        0# -> count
        _  -> pow5_factor q (count +# 1#)

-- | Returns @True@ if value is divisible by @5^p@
multipleOfPowerOf5 :: Mantissa a => a -> Int -> Bool
multipleOfPowerOf5 value (I# p) = isTrue# (pow5_factor (raw value) 0# >=# p)

-- | Returns @True@ if value is divisible by @2^p@
multipleOfPowerOf2 :: Mantissa a => a -> Int -> Bool
multipleOfPowerOf2 value p = (value .&. mask p) == 0

-- | Wrapper for polymorphic handling of 32- and 64-bit floats
class (FiniteBits a, Integral a) => Mantissa a where
  -- NB: might truncate!
  -- Use this when we know the value fits in 32-bits
  unsafeRaw :: a -> Word#
  raw :: a -> WORD64

  boolToWord :: Bool -> a
  quotRem10 :: a -> (a, a)
  quot10  :: a -> a
  quot100 :: a -> a
  quotRem100 :: a -> (a, a)
  quotRem10000 :: a -> (a, a)

instance Mantissa Word32 where
#if __GLASGOW_HASKELL__ >= 902
  unsafeRaw (W32# w) = word32ToWord# w
#else
  unsafeRaw (W32# w) = w
#endif
#if WORD_SIZE_IN_BITS >= 64 && __GLASGOW_HASKELL__ < 903
  raw = unsafeRaw
#else
  raw w = wordToWord64# (unsafeRaw w)
#endif

  boolToWord = boolToWord32

  {-# INLINE quotRem10 #-}
  quotRem10 = fquotRem10

  {-# INLINE quot10 #-}
  quot10 = fquot10

  {-# INLINE quot100 #-}
  quot100 = fquot100

  quotRem100 w =
    let w' = fquot100 w
      in (w', (w - w' * 100))

  quotRem10000 = fquotRem10000

instance Mantissa Word64 where
#if WORD_SIZE_IN_BITS >= 64 && __GLASGOW_HASKELL__ < 903
  unsafeRaw (W64# w) = w
#else
  unsafeRaw (W64# w) = word64ToWord# w
#endif
  raw (W64# w) = w

  boolToWord = boolToWord64

  {-# INLINE quotRem10 #-}
  quotRem10 = dquotRem10

  {-# INLINE quot10 #-}
  quot10 = dquot10

  {-# INLINE quot100 #-}
  quot100 = dquot100

  quotRem100 w =
    let w' = dquot100 w
     in (w', (w - w' * 100))

  quotRem10000 = dquotRem10000

-- | Bookkeeping state for finding the shortest, correctly-rounded
-- representation. The same trimming algorithm is similar enough for 32- and
-- 64-bit floats
data BoundsState a = BoundsState
    { vu :: !a
    , vv :: !a
    , vw :: !a
    , lastRemovedDigit :: !a
    , vuIsTrailingZeros :: !Bool
    , vvIsTrailingZeros :: !Bool
    }

-- | Trim digits and update bookkeeping state when the table-computed
-- step results in trailing zeros (the general case, happens rarely)
--
-- NB: This function isn't actually necessary so long as acceptBounds is always
-- @False@ since we don't do anything different with the trailing-zero
-- information directly:
-- - vuIsTrailingZeros is always False.  We can see this by noting that in all
--   places where vuTrailing can possible be True, we must have acceptBounds be
--   True (accept_smaller)
-- - The final result doesn't change the lastRemovedDigit for rounding anyway
trimTrailing :: (Show a, Mantissa a) => BoundsState a -> (BoundsState a, Int32)
trimTrailing !initial = (res, r + r')
  where
    !(d', r) = trimTrailing' initial
    !(d'', r') = if vuIsTrailingZeros d' then trimTrailing'' d' else (d', 0)
    res = if vvIsTrailingZeros d'' && lastRemovedDigit d'' == 5 && vv d'' `rem` 2 == 0
             -- set `{ lastRemovedDigit = 4 }` to round-even
             then d''
             else d''

    trimTrailing' !d
      | vw' > vu' =
         fmap ((+) 1) . trimTrailing' $
          d { vu = vu'
            , vv = vv'
            , vw = vw'
            , lastRemovedDigit = vvRem
            , vuIsTrailingZeros = vuIsTrailingZeros d && vuRem == 0
            , vvIsTrailingZeros = vvIsTrailingZeros d && lastRemovedDigit d == 0
            }
      | otherwise = (d, 0)
      where
        !(vv', vvRem) = quotRem10 $ vv d
        !(vu', vuRem) = quotRem10 $ vu d
        !(vw', _    ) = quotRem10 $ vw d

    trimTrailing'' !d
      | vuRem == 0 =
         fmap ((+) 1) . trimTrailing'' $
          d { vu = vu'
            , vv = vv'
            , vw = vw'
            , lastRemovedDigit = vvRem
            , vvIsTrailingZeros = vvIsTrailingZeros d && lastRemovedDigit d == 0
            }
      | otherwise = (d, 0)
      where
        !(vu', vuRem) = quotRem10 $ vu d
        !(vv', vvRem) = quotRem10 $ vv d
        !(vw', _    ) = quotRem10 $ vw d


-- | Trim digits and update bookkeeping state when the table-computed
-- step results has no trailing zeros (common case)
trimNoTrailing :: Mantissa a => BoundsState a -> (BoundsState a, Int32)
trimNoTrailing !(BoundsState u v w ld _ _) =
  (BoundsState ru' rv' 0 ld' False False, c)
  where
    !(ru', rv', ld', c) = trimNoTrailing' u v w ld 0

    trimNoTrailing' u' v' w' lastRemoved count
      -- Loop iterations below (approximately), without div 100 optimization:
      -- 0: 0.03%, 1: 13.8%, 2: 70.6%, 3: 14.0%, 4: 1.40%, 5: 0.14%, 6+: 0.02%
      -- Loop iterations below (approximately), with div 100 optimization:
      -- 0: 70.6%, 1: 27.8%, 2: 1.40%, 3: 0.14%, 4+: 0.02%
      | vw' > vu' =
          trimNoTrailing'' vu' vv' vw' (quot10 (v' - (vv' * 100))) (count + 2)
      | otherwise =
          trimNoTrailing'' u' v' w' lastRemoved count
      where
        !vw' = quot100 w'
        !vu' = quot100 u'
        !vv' = quot100 v'

    trimNoTrailing'' u' v' w' lastRemoved count
      | vw' > vu' = trimNoTrailing' vu' vv' vw' lastRemoved' (count + 1)
      | otherwise = (u', v', lastRemoved, count)
      where
        !(vv', lastRemoved') = quotRem10 v'
        !vu' = quot10 u'
        !vw' = quot10 w'

-- | Returns the correctly rounded decimal representation mantissa based on if
-- we need to round up (next decimal place >= 5) or if we are outside the
-- bounds
{-# INLINE closestCorrectlyRounded #-}
closestCorrectlyRounded :: Mantissa a => Bool -> BoundsState a -> a
closestCorrectlyRounded acceptBound s = vv s + boolToWord roundUp
  where
    outsideBounds = not (vuIsTrailingZeros s) || not acceptBound
    roundUp = (vv s == vu s && outsideBounds) || lastRemovedDigit s >= 5

-- Wrappe around int2Word#
asciiRaw :: Int -> Word8#
asciiRaw (I# i) = wordToWord8# (int2Word# i)

asciiZero :: Int
asciiZero = ord '0'

asciiDot :: Int
asciiDot = ord '.'

asciiMinus :: Int
asciiMinus = ord '-'

-- | Convert a single-digit number to the ascii ordinal e.g '1' -> 0x31
toAscii :: Word# -> Word#
toAscii a = a `plusWord#` word8ToWord# (asciiRaw asciiZero)

-- | Index into the 64-bit word lookup table provided
{-# INLINE getWord64At #-}
getWord64At :: Ptr Word64 -> Int -> Word64
getWord64At (Ptr arr) (I# i) = W64# (indexWord64OffAddr# arr i)

-- | Index into the 128-bit word lookup table provided
-- Return (# high-64-bits , low-64-bits #)
--
-- NB: The lookup tables we use store the low 64 bits in
-- host-byte-order then the high 64 bits in host-byte-order
{-# INLINE getWord128At #-}
getWord128At :: Ptr Word64 -> Int -> (Word64, Word64)
getWord128At (Ptr arr) (I# i) = let
  !hi = W64# (indexWord64OffAddr# arr (i *# 2# +# 1#))
  !lo = W64# (indexWord64OffAddr# arr (i *# 2#))
  in (hi, lo)

-- | Packs 2 bytes [lsb, msb] into 16-bit word
packWord16 :: Word# -> Word# -> Word#
packWord16 l h =
#if defined(WORDS_BIGENDIAN)
    (h `uncheckedShiftL#` 8#) `or#` l
#else
    (l `uncheckedShiftL#` 8#) `or#` h
#endif

-- | Unpacks a 16-bit word into 2 bytes [lsb, msb]
unpackWord16 :: Word# -> (# Word8#, Word8# #)
unpackWord16 w =
#if defined(WORDS_BIGENDIAN)
    (# wordToWord8# (w `and#` 0xff##), wordToWord8# (w `uncheckedShiftRL#` 8#) #)
#else
    (# wordToWord8# (w `uncheckedShiftRL#` 8#), wordToWord8# (w `and#` 0xff##) #)
#endif


foreign import ccall "&hs_bytestring_digit_pairs_table"
  c_digit_pairs_table :: Ptr CChar

-- | Static array of 2-digit pairs 00..99 for faster ascii rendering
digit_table :: Ptr Word16
digit_table = castPtr c_digit_pairs_table

-- | Unsafe index a static array for the 16-bit word at the index
unsafeAt :: Ptr Word16 -> Int# -> Word#
unsafeAt (Ptr a) i =
#if __GLASGOW_HASKELL__ >= 902
    word16ToWord# (indexWord16OffAddr# a i)
#else
    indexWord16OffAddr# a i
#endif

-- | Write a 16-bit word into the given address
copyWord16 :: Word# -> Addr# -> State# RealWorld -> State# RealWorld
copyWord16 w a s = let
#if __GLASGOW_HASKELL__ >= 902
  w16 = wordToWord16# w
#else
  w16 = w
#endif
  in  case unIO (unalignedWriteU16 (W16# w16) (Ptr a)) s of
  (# s', _ #) -> s'

-- | Write an 8-bit word into the given address
poke :: Addr# -> Word8# -> State# d -> State# d
poke a w s =
#if __GLASGOW_HASKELL__ >= 902
    writeWord8OffAddr# a 0# w s
#else
    writeWord8OffAddr# a 0# (word8ToWord# w) s
#endif

-- | Write the mantissa into the given address. This function attempts to
-- optimize this by writing pairs of digits simultaneously when the mantissa is
-- large enough
{-# SPECIALIZE writeMantissa :: Addr# -> Int# -> Word32 -> State# RealWorld -> (# Addr#, State# RealWorld #) #-}
{-# SPECIALIZE writeMantissa :: Addr# -> Int# -> Word64 -> State# RealWorld -> (# Addr#, State# RealWorld #) #-}
writeMantissa :: forall a. (Mantissa a) => Addr# -> Int# -> a -> State# RealWorld -> (# Addr#, State# RealWorld #)
writeMantissa ptr olength = go (ptr `plusAddr#` olength)
  where
    go p mantissa s1
      | mantissa >= 10000 =
          let !(m', c) = quotRem10000 mantissa
              !(c1, c0) = quotRem100 c
              s2 = copyWord16 (digit_table `unsafeAt` word2Int# (unsafeRaw c0)) (p `plusAddr#` (-1#)) s1
              s3 = copyWord16 (digit_table `unsafeAt` word2Int# (unsafeRaw c1)) (p `plusAddr#` (-3#)) s2
           in go (p `plusAddr#` (-4#)) m' s3
      | mantissa >= 100 =
          let !(m', c) = quotRem100 mantissa
              s2 = copyWord16 (digit_table `unsafeAt` word2Int# (unsafeRaw c)) (p `plusAddr#` (-1#)) s1
           in finalize m' s2
      | otherwise = finalize mantissa s1
    finalize mantissa s1
      | mantissa >= 10 =
        let !bs = digit_table `unsafeAt` word2Int# (unsafeRaw mantissa)
            !(# lsb, msb #) = unpackWord16 bs
            s2 = poke (ptr `plusAddr#` 2#) lsb s1
            s3 = poke (ptr `plusAddr#` 1#) (asciiRaw asciiDot) s2
            s4 = poke ptr msb s3
           in (# ptr `plusAddr#` (olength +# 1#), s4 #)
      | (I# olength) > 1 =
          let s2 = copyWord16 (packWord16 (word8ToWord# (asciiRaw asciiDot)) (toAscii (unsafeRaw mantissa))) ptr s1
           in (# ptr `plusAddr#` (olength +# 1#), s2 #)
      | otherwise =
          let s2 = poke (ptr `plusAddr#` 2#) (asciiRaw asciiZero) s1
              s3 = poke (ptr `plusAddr#` 1#) (asciiRaw asciiDot) s2
              s4 = poke ptr (wordToWord8# (toAscii (unsafeRaw mantissa))) s3
           in (# ptr `plusAddr#` 3#, s4 #)

-- | Write the exponent into the given address.
writeExponent :: Addr# -> Int32 -> State# RealWorld -> (# Addr#, State# RealWorld #)
writeExponent ptr !expo s1
  | expo >= 100 =
      let !(e1, e0) = fquotRem10 (fromIntegral expo) -- TODO
          s2 = copyWord16 (digit_table `unsafeAt` word2Int# (unsafeRaw e1)) ptr s1
          s3 = poke (ptr `plusAddr#` 2#) (wordToWord8# (toAscii (unsafeRaw e0))) s2
       in (# ptr `plusAddr#` 3#, s3 #)
  | expo >= 10 =
      let s2 = copyWord16 (digit_table `unsafeAt` e) ptr s1
       in (# ptr `plusAddr#` 2#, s2 #)
  | otherwise =
      let s2 = poke ptr (wordToWord8# (toAscii (int2Word# e))) s1
       in (# ptr `plusAddr#` 1#, s2 #)
  where !(I# e) = int32ToInt expo

-- | Write the sign into the given address.
writeSign :: Addr# -> Bool -> State# d -> (# Addr#, State# d #)
writeSign ptr True s1 =
  let s2 = poke ptr (asciiRaw asciiMinus) s1
   in (# ptr `plusAddr#` 1#, s2 #)
writeSign ptr False s = (# ptr, s #)

-- | Returns the decimal representation of a floating point number in
-- scientific (exponential) notation
{-# INLINABLE toCharsScientific #-}
{-# SPECIALIZE toCharsScientific :: Word8# -> Bool -> Word32 -> Int32 -> BoundedPrim () #-}
{-# SPECIALIZE toCharsScientific :: Word8# -> Bool -> Word64 -> Int32 -> BoundedPrim () #-}
toCharsScientific :: (Mantissa a, DecimalLength a) => Word8# -> Bool -> a -> Int32 -> BoundedPrim ()
toCharsScientific !eE !sign !mantissa !expo = boundedPrim maxEncodedLength $ \_ !(Ptr p0)-> do
  let !olength@(I# ol) = decimalLength mantissa
      !expo' = expo + intToInt32 olength - 1
  IO $ \s1 ->
    let !(# p1, s2 #) = writeSign p0 sign s1
        !(# p2, s3 #) = writeMantissa p1 ol mantissa s2
        s4 = poke p2 eE s3
        !(# p3, s5 #) = writeSign (p2 `plusAddr#` 1#) (expo' < 0) s4
        !(# p4, s6 #) = writeExponent p3 (abs expo') s5
     in (# s6, (Ptr p4) #)

data FloatingDecimal a = FloatingDecimal
  { fmantissa :: !(MantissaWord a)
  , fexponent :: !Int32
  }
deriving instance Show (MantissaWord a) => Show (FloatingDecimal a)
deriving instance Eq (MantissaWord a) => Eq (FloatingDecimal a)

type family MantissaWord a
type instance MantissaWord Float = Word32
type instance MantissaWord Double = Word64

