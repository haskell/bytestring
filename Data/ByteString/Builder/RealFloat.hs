
module Data.ByteString.Builder.RealFloat
  ( FFFormat(..)
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
import GHC.Float (FFFormat(..), roundTo)
import GHC.Word (Word64(..))
import GHC.Show (intToDigit)

{-# INLINABLE floatDec #-}
floatDec :: Float -> Builder
floatDec = formatFloat FFGeneric Nothing

{-# INLINABLE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = formatDouble FFGeneric Nothing

{-# INLINABLE formatFloat #-}
-- TODO precision for general and exponent formats
formatFloat :: FFFormat -> Maybe Int -> Float -> Builder
formatFloat fmt prec f =
  case fmt of
    FFGeneric ->
      case specialStr f of
        Just b -> b
        Nothing ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed (fromIntegral m) e' prec
             else BP.primBounded (R.toCharsScientific (f < 0) m e) ()
      where (RF.FloatingDecimal m e) = RF.f2Intermediate f
            e' = fromIntegral e + R.decimalLength9 m
    FFExponent -> RF.f2s f
    FFFixed -> d2Fixed (realToFrac f) prec

{-# INLINABLE formatDouble #-}
formatDouble :: FFFormat -> Maybe Int -> Double -> Builder
-- TODO precision for general and exponent formats
formatDouble fmt prec f =
  case fmt of
    FFGeneric ->
      case specialStr f of
        Just b -> b
        Nothing ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed m e' prec
             else BP.primBounded (R.toCharsScientific (f < 0) m e) ()
      where (RD.FloatingDecimal m e) = RD.d2Intermediate f
            e' = fromIntegral e + R.decimalLength17 m
    FFExponent -> RD.d2s f
    FFFixed -> d2Fixed f prec

-- show fixed floating point matching show / showFFloat output by dropping
-- digits after exponentiation precision
d2Fixed :: Double -> Maybe Int -> Builder
d2Fixed f prec =
  case specialStr f of
    Just b -> b
    Nothing -> sign f `mappend` showFixed m e' prec
  where (RD.FloatingDecimal m e) = RD.d2Intermediate f
        olength = R.decimalLength17 m
        -- NB: exponent in exponential format is e' - 1
        e' = fromIntegral e + olength

-- | Char7 encode a 'Char'.
{-# INLINE char7 #-}
char7 :: Char -> Builder
char7 = BP.primFixed BP.char7

-- | Char7 encode a 'String'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = BP.primMapListFixed BP.char7

sign :: RealFloat a => a -> Builder
sign f = if f < 0 then char7 '-' else mempty

specialStr :: RealFloat a => a -> Maybe Builder
specialStr f
  | isNaN f          = Just $ string7 "NaN"
  | isInfinite f     = Just $ sign f `mappend` string7 "Infinity"
  | isNegativeZero f = Just $ string7 "-0.0"
  | f == 0           = Just $ string7 "0.0"
  | otherwise        = Nothing

digits :: Word64 -> [Int]
digits w = go [] w
  where go ds 0 = ds
        go ds c = let (q, r) = R.dquotRem10Boxed c
                   in go (fromIntegral r:ds) q

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

