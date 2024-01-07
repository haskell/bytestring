{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module      : Data.ByteString.Builder.RealFloat
-- Copyright   : (c) Lawrence Wu 2021
-- License     : BSD-style
-- Maintainer  : lawrencejwu@gmail.com
--
-- Floating point formatting for @Bytestring.Builder@
--
-- This module primarily exposes `floatDec` and `doubleDec` which do the
-- equivalent of converting through @'Data.ByteString.Builder.string7' . 'show'@.
--
-- It also exposes `formatFloat` and `formatDouble` with a similar API as
-- `GHC.Float.formatRealFloat`.
--
-- NB: The float-to-string conversions exposed by this module match `show`'s
-- output (specifically with respect to default rounding and length). In
-- particular, there are boundary cases where the closest and \'shortest\'
-- string representations are not used.  Mentions of \'shortest\' in the docs
-- below are with this caveat.
--
-- For example, for fidelity, we match `show` on the output below.
--
-- >>> show (1.0e23 :: Float)
-- "1.0e23"
-- >>> show (1.0e23 :: Double)
-- "9.999999999999999e22"
-- >>> floatDec 1.0e23
-- "1.0e23"
-- >>> doubleDec 1.0e23
-- "9.999999999999999e22"
--
-- Simplifying, we can build a shorter, lossless representation by just using
-- @"1.0e23"@ since the floating point values that are 1 ULP away are
--
-- >>> showHex (castDoubleToWord64 1.0e23) []
-- "44b52d02c7e14af6"
-- >>> castWord64ToDouble 0x44b52d02c7e14af5
-- 9.999999999999997e22
-- >>> castWord64ToDouble 0x44b52d02c7e14af6
-- 9.999999999999999e22
-- >>> castWord64ToDouble 0x44b52d02c7e14af7
-- 1.0000000000000001e23
--
-- In particular, we could use the exact boundary if it is the shortest
-- representation and the original floating number is even. To experiment with
-- the shorter rounding, refer to
-- `Data.ByteString.Builder.RealFloat.Internal.acceptBounds`. This will give us
--
-- >>> floatDec 1.0e23
-- "1.0e23"
-- >>> doubleDec 1.0e23
-- "1.0e23"
--
-- For more details, please refer to the
-- <https://dl.acm.org/doi/10.1145/3192366.3192369 Ryu paper>.
--
-- @since 0.11.2.0

module Data.ByteString.Builder.RealFloat
  ( floatDec
  , doubleDec

  -- * Custom formatting
  , formatFloat
  , formatDouble
  , FloatFormat
  , standard
  , standardDefaultPrecision
  , scientific
  , generic
  ) where

import Data.ByteString.Builder.Internal (Builder)
import qualified Data.ByteString.Builder.RealFloat.Internal as R
import qualified Data.ByteString.Builder.RealFloat.F2S as RF
import qualified Data.ByteString.Builder.RealFloat.D2S as RD
import qualified Data.ByteString.Builder.Prim as BP
import GHC.Float (roundTo)
import GHC.Word (Word32, Word64)
import GHC.Int (Int32)
import GHC.Show (intToDigit)
import Data.Char (ord)
import GHC.Prim (Word8#)

-- | Returns a rendered Float. Matches `show` in displaying in standard or
-- scientific notation
--
-- @
-- floatDec = 'formatFloat' 'generic'
-- @
{-# INLINABLE floatDec #-}
floatDec :: Float -> Builder
floatDec = formatFloat generic

-- | Returns a rendered Double. Matches `show` in displaying in standard or
-- scientific notation
--
-- @
-- doubleDec = 'formatDouble' 'generic'
-- @
{-# INLINABLE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = formatDouble generic

-- | Format type for use with `formatFloat` and `formatDouble`.
--
-- @since 0.11.2.0
data FloatFormat
  = FScientific Word8# R.SpecialStrings -- ^ scientific notation
  | FStandard (Maybe Int)       -- ^ standard notation with `Maybe Int` digits after the decimal
  | FGeneric Word8# (Maybe Int) (Int,Int)       -- ^ dispatches to scientific or standard notation based on the exponent
  deriving Show
fScientific eE = FScientific (R.asciiRaw $ ord eE)
fGeneric eE = FGeneric (R.asciiRaw $ ord eE)

-- | Standard notation with `n` decimal places
--
-- @since 0.11.2.0
standard :: Int -> FloatFormat
standard n = FStandard (Just n)

-- | Standard notation with the \'default precision\' (decimal places matching `show`)
--
-- @since 0.11.2.0
standardDefaultPrecision :: FloatFormat
standardDefaultPrecision = FStandard Nothing

-- | Scientific notation with \'default precision\' (decimal places matching `show`)
--
-- @since 0.11.2.0
scientific :: FloatFormat
scientific = fScientific 'e' scientificSpecialStrings

scientificSpecialStrings :: R.SpecialStrings
scientificSpecialStrings = R.SpecialStrings
  { R.nan = "NaN"
  , R.positiveInfinity = "Infinity"
  , R.negativeInfinity = "-Infinity"
  , R.positiveZero = "0.0e0"
  , R.negativeZero = "-0.0e0"
  }

-- | Standard or scientific notation depending on the exponent. Matches `show`
--
-- @since 0.11.2.0
generic :: FloatFormat
generic = fGeneric 'e' Nothing (0,7)

-- TODO: support precision argument for FGeneric and FScientific
-- | Returns a rendered Float. Returns the \'shortest\' representation in
-- scientific notation and takes an optional precision argument in standard
-- notation. Also see `floatDec`.
--
-- With standard notation, the precision argument is used to truncate (or
-- extend with 0s) the \'shortest\' rendered Float. The \'default precision\' does
-- no such modifications and will return as many decimal places as the
-- representation demands.
--
-- e.g
--
-- >>> formatFloat (standard 1) 1.2345e-2
-- "0.0"
-- >>> formatFloat (standard 10) 1.2345e-2
-- "0.0123450000"
-- >>> formatFloat standardDefaultPrecision 1.2345e-2
-- "0.01234"
-- >>> formatFloat scientific 12.345
-- "1.2345e1"
-- >>> formatFloat generic 12.345
-- "12.345"
--
-- @since 0.11.2.0
{-# INLINABLE formatFloat #-}
formatFloat :: FloatFormat -> Float -> Builder
formatFloat = formatFloating

-- TODO: support precision argument for FGeneric and FScientific
-- | Returns a rendered Double. Returns the \'shortest\' representation in
-- scientific notation and takes an optional precision argument in standard
-- notation. Also see `doubleDec`.
--
-- With standard notation, the precision argument is used to truncate (or
-- extend with 0s) the \'shortest\' rendered Float. The \'default precision\'
-- does no such modifications and will return as many decimal places as the
-- representation demands.
--
-- e.g
--
-- >>> formatDouble (standard 1) 1.2345e-2
-- "0.0"
-- >>> formatDouble (standard 10) 1.2345e-2
-- "0.0123450000"
-- >>> formatDouble standardDefaultPrecision 1.2345e-2
-- "0.01234"
-- >>> formatDouble scientific 12.345
-- "1.2345e1"
-- >>> formatDouble generic 12.345
-- "12.345"
--
-- @since 0.11.2.0
{-# INLINABLE formatDouble #-}
formatDouble :: FloatFormat -> Double -> Builder
formatDouble = formatFloating

formatFloating ::
  -- a
  ( ToS a
  , Num a
  , Ord a
  , RealFloat a
  , Intermediate a
  -- mantissa
  , mw ~ R.MantissaWord a
  , R.Mantissa mw
  , ToWord64 mw
  , R.DecimalLength mw
  ) => FloatFormat -> a -> Builder
formatFloating fmt f =
  let (R.FloatingDecimal m e) = intermediate f
      e' = toInt e + R.decimalLength m in
  case fmt of
    FGeneric eE prec (minExpo,maxExpo) ->
      case specialStr f of
        Just b -> b
        Nothing ->
          if e' >= minExpo && e' <= maxExpo
             then sign f `mappend` showStandard (toWord64 m) e' prec
             else BP.primBounded (R.toCharsScientific eE (f < 0) m e) ()
    FScientific eE ss -> toS eE ss f
    FStandard prec ->
      case specialStr f of
        Just b -> b
        Nothing -> sign f `mappend` showStandard (toWord64 m) e' prec

class Intermediate a where intermediate :: a -> R.FloatingDecimal a
instance Intermediate Float where intermediate = RF.f2Intermediate
instance Intermediate Double where intermediate = RD.d2Intermediate

class ToInt a where toInt :: a -> Int
instance ToInt Int32 where toInt = R.int32ToInt

class ToWord64 a where toWord64 :: a -> Word64
instance ToWord64 Word32 where toWord64 = R.word32ToWord64
instance ToWord64 Word64 where toWord64 = id

class ToS a where toS :: Word8# -> R.SpecialStrings -> a -> Builder
instance ToS Float where toS = RF.f2s
instance ToS Double where toS = RD.d2s

-- | Char7 encode a 'Char'.
{-# INLINE char7 #-}
char7 :: Char -> Builder
char7 = BP.primFixed BP.char7

-- | Char7 encode a 'String'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = BP.primMapListFixed BP.char7

-- | Encodes a `-` if input is negative
sign :: RealFloat a => a -> Builder
sign f = if f < 0 then char7 '-' else mempty

-- | Special rendering for Nan, Infinity, and 0. See
-- RealFloat.Internal.NonNumbersAndZero
specialStr :: RealFloat a => a -> Maybe Builder
specialStr f
  | isNaN f          = Just $ string7 "NaN"
  | isInfinite f     = Just $ sign f `mappend` string7 "Infinity"
  | isNegativeZero f = Just $ string7 "-0.0"
  | f == 0           = Just $ string7 "0.0"
  | otherwise        = Nothing

-- | Returns a list of decimal digits in a Word64
digits :: Word64 -> [Int]
digits w = go [] w
  where go ds 0 = ds
        go ds c = let (q, r) = R.dquotRem10 c
                   in go ((R.word64ToInt r) : ds) q

-- | Show a floating point value in standard notation. Based on GHC.Float.showFloat
showStandard :: Word64 -> Int -> Maybe Int -> Builder
showStandard m e prec =
  case prec of
    Nothing
      | e <= 0 -> char7 '0'
               `mappend` char7 '.'
               `mappend` string7 (replicate (-e) '0')
               `mappend` mconcat (digitsToBuilder ds)
      | otherwise ->
          let f 0 s     rs = mk0 (reverse s) `mappend` char7 '.' `mappend` mk0 rs
              f n s     [] = f (n-1) (char7 '0':s) []
              f n s (r:rs) = f (n-1) (r:s) rs
           in f e [] (digitsToBuilder ds)
    Just p
      | e >= 0 ->
          let (ei, is') = roundTo 10 (p' + e) ds
              (ls, rs) = splitAt (e + ei) (digitsToBuilder is')
           in mk0 ls `mappend` mkDot rs
      | otherwise ->
          let (ei, is') = roundTo 10 p' (replicate (-e) 0 ++ ds)
              -- ds' should always be non-empty but use redundant pattern
              -- matching to silence warning
              ds' = if ei > 0 then is' else 0:is'
              (ls, rs) = splitAt 1 $ digitsToBuilder ds'
           in mk0 ls `mappend` mkDot rs
          where p' = max p 0
  where
    mk0 ls = case ls of [] -> char7 '0'; _ -> mconcat ls
    mkDot rs = if null rs then mempty else char7 '.' `mappend` mconcat rs
    ds = digits m
    digitsToBuilder = fmap (char7 . intToDigit)

