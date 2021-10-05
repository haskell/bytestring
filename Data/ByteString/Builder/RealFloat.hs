-- | Floating point formatting for Bytestring.Builder
--
-- This module primarily exposes `floatDec` and `doubleDec` which do the
-- equivalent of converting through `string7 . show`.
--
-- Experimentally, it also exposes `formatFloat` and `formatDouble` which
-- accept the same arguments as `GHC.Float.formatRealFloat`. Currently,
-- precision is not supported for the general and scientific notation versions
-- of this API.
--

module Data.ByteString.Builder.RealFloat
  ( FloatFormat(..)
  , floatDec
  , doubleDec
  , formatFloat
  , formatDouble
  ) where

import Data.ByteString.Builder.Internal (Builder)
import qualified Data.ByteString.Builder.RealFloat.Internal as R
import qualified Data.ByteString.Builder.RealFloat.F2S as RF
import qualified Data.ByteString.Builder.RealFloat.D2S as RD
import qualified Data.ByteString.Builder.Prim as BP
import GHC.Float (roundTo)
import GHC.Word (Word32, Word64)
import GHC.Show (intToDigit)

-- | Returns a rendered Float. Matches `show` in displaying in fixed or
-- scientific notation
{-# INLINABLE floatDec #-}
floatDec :: Float -> Builder
floatDec = formatFloat FGeneric Nothing

-- | Returns a rendered Double. Matches `show` in displaying in fixed or
-- scientific notation
{-# INLINABLE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = formatDouble FGeneric Nothing

-- | ByteString float-to-string format
data FloatFormat
  = FExponent       -- ^ scientific notation
  | FFixed          -- ^ fixed precision with `Maybe Int` digits after the decimal
  | FGeneric        -- ^ dispatches to fixed or exponent based on the exponent
  deriving Show

-- TODO: support precision argument for FGeneric and FExponent
-- | Returns a rendered Float. Matches the API of `formatRealFloat` but does
-- not currently handle the precision argument in case of `FGeneric` and
-- `FExponent`
--
-- e.g
--
-- > formatFloat FFixed (Just 2) 12.345 = "12.34"
-- > formatFloat FFixed (Just 5) 12.345 = "12.34500"
-- > formatFloat FGeneric Nothing 12.345 = "12.345"
{-# INLINABLE formatFloat #-}
formatFloat :: FloatFormat-> Maybe Int -> Float -> Builder
formatFloat fmt prec f =
  case fmt of
    FGeneric ->
      case specialStr f of
        Just b -> b
        Nothing ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed (word32ToWord64 m) e' prec
             else BP.primBounded (R.toCharsScientific (f < 0) m e) ()
      where (RF.FloatingDecimal m e) = RF.f2Intermediate f
            e' = R.int32ToInt e + R.decimalLength9 m
            word32ToWord64 :: Word32 -> Word64
            word32ToWord64 = fromIntegral
    FExponent -> RF.f2s f
    FFixed -> d2Fixed (realToFrac f) prec

-- TODO: support precision argument for FGeneric and FExponent
-- | Returns a rendered Double. Matches the API of `formatRealFloat` but does
-- not currently handle the precision argument in case of `FGeneric` and
-- `FExponent`
--
-- e.g
--
-- > formatDouble FFixed (Just 2) 12.345 = "12.34"
-- > formatDouble FFixed (Just 5) 12.345 = "12.34500"
-- > formatDouble FGeneric Nothing 12.345 = "12.345"
{-# INLINABLE formatDouble #-}
formatDouble :: FloatFormat-> Maybe Int -> Double -> Builder
formatDouble fmt prec f =
  case fmt of
    FGeneric ->
      case specialStr f of
        Just b -> b
        Nothing ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed m e' prec
             else BP.primBounded (R.toCharsScientific (f < 0) m e) ()
      where (RD.FloatingDecimal m e) = RD.d2Intermediate f
            e' = R.int32ToInt e + R.decimalLength17 m
    FExponent -> RD.d2s f
    FFixed -> d2Fixed f prec

-- | Show fixed floating point matching show / formatRealFloat output by
-- dropping digits after exponentiation precision
d2Fixed :: Double -> Maybe Int -> Builder
d2Fixed f prec =
  case specialStr f of
    Just b -> b
    Nothing -> sign f `mappend` showFixed m e' prec
  where (RD.FloatingDecimal m e) = RD.d2Intermediate f
        -- NB: exponent in exponential format is e' - 1
        e' = R.int32ToInt e + R.decimalLength17 m :: Int

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
        go ds c = let (q, r) = R.dquotRem10Boxed c
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

