{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-- |
-- Module:    Data.ByteString.Builder.RealFloat
-- Copyright: (c) 2016 Winter Han
-- License:   BSD3-style (see LICENSE)
--
-- Write a floating point value to a 'Builder'.

module Data.ByteString.Builder.RealFloat
    ( FFFormat(..) -- TODO: add document to GHC.Float
    , formatFloat
    , formatDouble
    , floatDec
    , doubleDec
    , grisu3_sp
    , grisu3
    ) where

import GHC.Float (FFFormat(..), floatToDigits, roundTo)
import GHC.Show (intToDigit)
import Foreign.Marshal (peekArray, alloca, allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Foreign.C.Types
import Data.Word (Word8)
import qualified Data.ByteString.Builder.Prim  as P
import Data.ByteString.Builder.ASCII (intDec, string7, char7)
import Data.ByteString.Builder.Internal (Builder, empty, append)

#if MIN_VERSION_base(4,4,0)
import System.IO.Unsafe (unsafeDupablePerformIO)
#else
import           GHC.IO (unsafeDupablePerformIO)
#endif

-- Floating point numbers
-------------------------

-- | Decimal encoding of an IEEE 'Float'.
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
{-# INLINE floatDec #-}
floatDec :: Float -> Builder
floatDec = formatFloat FFGeneric Nothing

-- | Decimal encoding of an IEEE 'Double'.
-- Using standard decimal notation for arguments whose absolute value lies
-- between @0.1@ and @9,999,999@, and scientific notation otherwise.
--
{-# INLINE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = formatDouble FFGeneric Nothing

-- | Format single-precision float using drisu3 with dragon4 fallback.
--
{-# INLINE formatFloat #-}
formatFloat :: FFFormat
            -> Maybe Int  -- ^ Number of decimal places to render.
            -> Float
            -> Builder
formatFloat fmt decs x
    | isNaN x                   = string7 "NaN"
    | isInfinite x              = if x < 0 then string7 "-Infinity" else string7 "Infinity"
    | x < 0                     = char7 '-' `append` doFmt fmt decs (digits (-x))
    | isNegativeZero x          = string7 "-0.0"
    | x == 0                    = string7 "0.0"
    | otherwise                 = doFmt fmt decs (digits x) -- Grisu only handles strictly positive finite numbers.
  where
    digits y = case grisu3_sp y of Just r  -> r
                                   Nothing -> floatToDigits 10 y

-- | Format double-precision float using drisu3 with dragon4 fallback.
--
{-# INLINE formatDouble #-}
formatDouble :: FFFormat
             -> Maybe Int  -- ^ Number of decimal places to render.
             -> Double
             -> Builder
formatDouble fmt decs x
    | isNaN x                   = string7 "NaN"
    | isInfinite x              = if x < 0 then string7 "-Infinity" else string7 "Infinity"
    | x < 0                     = char7 '-' `append` doFmt fmt decs (digits (-x))
    | isNegativeZero x          = string7 "-0.0"
    | x == 0                    = string7 "0.0"
    | otherwise                 = doFmt fmt decs (digits x) -- Grisu only handles strictly positive finite numbers.
  where
    digits y = case grisu3 y of Just r  -> r
                                Nothing -> floatToDigits 10 y

{-# INLINE doFmt #-}
doFmt :: FFFormat -> Maybe Int -> ([Int], Int) -> Builder
doFmt format decs (is, e) =
    let ds = map intToDigit is
    in case format of
        FFGeneric ->
            doFmt (if e < 0 || e > 7 then FFExponent else FFFixed) decs (is,e)
        FFExponent ->
            case decs of
                Nothing ->
                    let show_e' = intDec (e-1)
                    in case ds of
                        "0"     -> string7 "0.0e0"
                        [d]     -> char7 d `append` string7 ".0e" `append` show_e'
                        (d:ds') -> char7 d `append` char7 '.' `append`
                                    string7 ds' `append` char7 'e' `append` show_e'
                        []      -> error "doFmt/Exponent: []"
                Just dec ->
                    let dec' = max dec 1 in
                    case is of
                        [0] -> char7 '0' `append` char7 '.' `append`
                                string7 (replicate dec' '0') `append` char7 'e' `append` char7 '0'
                        _ ->
                            let (ei,is') = roundTo 10 (dec'+1) is
                                (d:ds') = map intToDigit (if ei > 0 then init is' else is')
                            in char7 d `append` char7 '.' `append`
                                string7 ds' `append` char7 'e' `append` intDec (e-1+ei)
        FFFixed ->
            let mk0 ls = case ls of { "" -> char7 '0' ; _ -> string7 ls}
            in case decs of
                Nothing
                    | e <= 0    -> char7 '0' `append` char7 '.' `append`
                                    string7 (replicate (-e) '0') `append` string7 ds
                    | otherwise ->
                        let f 0 s    rs  = mk0 (reverse s) `append` char7 '.' `append` mk0 rs
                            f n s    ""  = f (n-1) ('0':s) ""
                            f n s (r:rs) = f (n-1) (r:s) rs
                        in f e "" ds
                Just dec ->
                    let dec' = max dec 0
                    in if e >= 0
                        then
                            let (ei,is') = roundTo 10 (dec' + e) is
                                (ls,rs)  = splitAt (e+ei) (map intToDigit is')
                            in mk0 ls `append`
                                (if null rs then empty else char7 '.' `append` string7 rs)
                        else
                            let (ei,is') = roundTo 10 dec' (replicate (-e) 0 ++ is)
                                d:ds' = map intToDigit (if ei > 0 then is' else 0:is')
                            in char7 d `append`
                                (if null ds' then empty else char7 '.' `append` string7 ds')

------------------------------------------------------------------------------
-- Conversion of 'Float's and 'Double's to ASCII in decimal using Grisu3
------------------------------------------------------------------------

#define GRISU3_SINGLE_BUF_LEN 10
#define GRISU3_DOUBLE_BUF_LEN 18

foreign import ccall unsafe "static grisu3" c_grisu3
    :: CDouble -> Ptr Word8 -> Ptr CInt -> Ptr CInt -> IO CInt

-- | Decimal encoding of a 'Double'.
{-# INLINE grisu3 #-}
grisu3 :: Double -> Maybe ([Int], Int)
grisu3 d = unsafeDupablePerformIO $
    allocaBytes GRISU3_DOUBLE_BUF_LEN $ \ pBuf ->
        alloca $ \ pLen ->
            alloca $ \ pE -> do
                success <- c_grisu3 (realToFrac d) pBuf pLen pE
                if success == 0 -- grisu3 fail
                    then return Nothing
                    else do
                        len <- peek pLen
                        e <- peek pE
                        buf <- map fromIntegral `fmap` peekArray (fromIntegral len) pBuf
                        let e' = fromIntegral (e + len)
                        e' `seq` return $ Just (buf, e')

foreign import ccall unsafe "static grisu3_sp" c_grisu3_sp
    :: CFloat -> Ptr Word8 -> Ptr CInt -> Ptr CInt -> IO CInt

-- | Decimal encoding of a 'Float'.
{-# INLINE grisu3_sp #-}
grisu3_sp :: Float -> Maybe ([Int], Int)
grisu3_sp d = unsafeDupablePerformIO $
    allocaBytes GRISU3_SINGLE_BUF_LEN $ \ pBuf ->
        alloca $ \ pLen ->
            alloca $ \ pE -> do
                success <- c_grisu3_sp (realToFrac d) pBuf pLen pE
                if success == 0 -- grisu3 fail
                    then return Nothing
                    else do
                        len <- peek pLen
                        e <- peek pE
                        buf <- map fromIntegral `fmap` peekArray (fromIntegral len) pBuf
                        let e' = fromIntegral (e + len)
                        e' `seq` return $ Just (buf, e')
