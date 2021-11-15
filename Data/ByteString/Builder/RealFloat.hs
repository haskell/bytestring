-- |
-- Module      : Data.ByteString.Builder.RealFloat
-- Copyright   : (c) Lawrence Wu 2021
-- License     : BSD-style
-- Maintainer  : lawrencejwu@gmail.com
--
-- Floating point formatting for Bytestring.Builder
--
-- This module primarily exposes `floatDec` and `doubleDec` which do the
-- equivalent of converting through `string7 . show`.
--
-- It also exposes `formatFloat` and `formatDouble` with a similar API as
-- `GHC.Float.formatRealFloat`.
--
-- NB: this implementation matches `show`'s output (specifically with respect
-- to default rounding and length). In particular, there are boundary cases
-- where the closest and 'shortest' string representations are not used.
-- Mentions of 'shortest' in the docs below are with this caveat.

module Data.ByteString.Builder.RealFloat
  ( FloatFormat
  , floatDec
  , doubleDec
  , fixed
  , fixedDefaultPrecision
  , scientific
  , generic
  , formatFloat
  , formatDouble
  ) where

import Data.ByteString.Builder.Internal (Builder)
import qualified Data.ByteString.Builder.RealFloat.Internal as R
import qualified Data.ByteString.Builder.RealFloat.F2S as RF
import qualified Data.ByteString.Builder.RealFloat.D2S as RD
import qualified Data.ByteString.Builder.Prim as BP
import GHC.Float (roundTo)
import GHC.Word (Word64)
import GHC.Show (intToDigit)

-- | Returns a rendered Float. Matches `show` in displaying in fixed or
-- scientific notation
{-# INLINABLE floatDec #-}
floatDec :: Float -> Builder
floatDec = formatFloat generic

-- | Returns a rendered Double. Matches `show` in displaying in fixed or
-- scientific notation
{-# INLINABLE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = formatDouble generic

-- | An opaque wrapper around `FloatFormat'` and an optional precision argument
-- used by `formatFloat` and `formatDouble`.
data FloatFormat = MkFloatFormat
  { floatFormat' :: FloatFormat'
  , precision :: Maybe Int
  }

-- | Standard notation with `n` decimal places
fixed :: Int -> FloatFormat
fixed n = MkFloatFormat FFixed (Just n)

-- | Standard notation with the default precision (number of decimal places)
fixedDefaultPrecision :: FloatFormat
fixedDefaultPrecision = MkFloatFormat FFixed Nothing

-- | Scientific notation with default precision
scientific :: FloatFormat
scientific = MkFloatFormat FScientific Nothing

-- | Standard or scientific notation depending on the exponent. Matches `show`
generic :: FloatFormat
generic = MkFloatFormat FGeneric Nothing

-- | ByteString float-to-string format
data FloatFormat'
  = FScientific     -- ^ scientific notation
  | FFixed          -- ^ standard notation with `Maybe Int` digits after the decimal
  | FGeneric        -- ^ dispatches to scientific or standard notation based on the exponent
  deriving Show

-- TODO: support precision argument for FGeneric and FScientific
-- | Returns a rendered Float. Matches the API of `formatRealFloat` but does
-- not currently handle the precision argument in scientific notation.
--
-- The precision argument is used to truncate (or extend with 0s) the
-- 'shortest' rendered Float. A precision of 'Nothing' does no such
-- modifications and will return as many decimal places as the representation
-- demands.
--
-- e.g
--
-- >>> formatFloat (fixed 1) 1.2345e-2
-- "0.0"
-- >>> formatFloat (fixed 5) 1.2345e-2
-- "0.01234"
-- >>> formatFloat (fixed 10) 1.2345e-2
-- "0.0123450000"
-- >>> formatFloat fixedDefaultPrecision 1.2345e-2
-- "0.01234"
-- >>> formatFloat scientific 12.345
-- "1.2345e1"
-- >>> formatFloat generic 12.345
-- "12.345"
{-# INLINABLE formatFloat #-}
formatFloat :: FloatFormat -> Float -> Builder
formatFloat (MkFloatFormat fmt prec) f =
  case fmt of
    FGeneric ->
      case specialStr f of
        Just b -> b
        Nothing ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed (R.word32ToWord64 m) e' prec
             else BP.primBounded (R.toCharsScientific (f < 0) m e) ()
    FScientific -> RF.f2s f
    FFixed ->
      case specialStr f of
        Just b -> b
        Nothing -> sign f `mappend` showFixed (R.word32ToWord64 m) e' prec
  where (RF.FloatingDecimal m e) = RF.f2Intermediate f
        e' = R.int32ToInt e + R.decimalLength9 m

-- TODO: support precision argument for FGeneric and FScientific
-- | Returns a rendered Double. Matches the API of `formatRealFloat` but does
-- not currently handle the precision argument in scientific notation
--
-- The precision argument is used to truncate (or extend with 0s) the
-- 'shortest' rendered Double. A precision of 'Nothing' does no such
-- modifications and will return as many decimal places as the representation
-- demands.
--
-- e.g
--
-- >>> formatDouble (fixed 1) 1.2345e-2
-- "0.0"
-- >>> formatDouble (fixed 5) 1.2345e-2
-- "0.01234"
-- >>> formatDouble (fixed 10) 1.2345e-2
-- "0.0123450000"
-- >>> formatDouble fixedDefaultPrecision 1.2345e-2
-- "0.01234"
-- >>> formatDouble scientific 12.345
-- "1.2345e1"
-- >>> formatDouble generic 12.345
-- "12.345"
{-# INLINABLE formatDouble #-}
formatDouble :: FloatFormat -> Double -> Builder
formatDouble (MkFloatFormat fmt prec) f =
  case fmt of
    FGeneric ->
      case specialStr f of
        Just b -> b
        Nothing ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed m e' prec
             else BP.primBounded (R.toCharsScientific (f < 0) m e) ()
    FScientific -> RD.d2s f
    FFixed ->
      case specialStr f of
        Just b -> b
        Nothing -> sign f `mappend` showFixed m e' prec
  where (RD.FloatingDecimal m e) = RD.d2Intermediate f
        e' = R.int32ToInt e + R.decimalLength17 m

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

-- | Show a floating point value in fixed point. Based on GHC.Float.showFloat
showFixed :: Word64 -> Int -> Maybe Int -> Builder
showFixed m e prec =
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
              (b:bs) = digitsToBuilder (if ei > 0 then is' else 0:is')
           in b `mappend` mkDot bs
          where p' = max p 0
  where
    mk0 ls = case ls of [] -> char7 '0'; _ -> mconcat ls
    mkDot rs = if null rs then mempty else char7 '.' `mappend` mconcat rs
    ds = digits m
    digitsToBuilder = fmap (char7 . intToDigit)

