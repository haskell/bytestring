{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

module Data.ByteString.Builder.RealFloat
  ( FFFormat(..)
  , floatDec
  , doubleDec
  , formatFloat
  , formatDouble
  ) where

import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Data.ByteString.Builder.Internal (Builder, byteString)
import qualified Data.ByteString.Builder.Prim  as P

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
import Control.Applicative ((<$>))
#endif

import Foreign.C.Types (CFloat, CDouble, CInt, CUInt, CULong, CUChar)

#if MIN_VERSION_base(4,5,0) || __GLASGOW_HASKELL__ >= 703
import Foreign.C.Types (CFloat(..), CDouble(..), CInt(..), CUInt(..), CULong(..), CUChar(..))
#else
import Foreign.C.Types (CFloat, CDouble, CInt, CUInt, CULong, CUChar)
#endif

import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import GHC.Word (Word8, Word32, Word64(..))
import GHC.Int (Int32)
import GHC.Float (FFFormat(..), roundTo)
import GHC.Prim
import GHC.Show (intToDigit)

#if MIN_VERSION_base(4,4,0)
import System.IO.Unsafe (unsafeDupablePerformIO)
#else
import           GHC.IO (unsafeDupablePerformIO)
#endif

{-# INLINABLE floatDec #-}
floatDec :: Float -> Builder
floatDec = formatFloat FFGeneric Nothing

{-# INLINABLE doubleDec #-}
doubleDec :: Double -> Builder
doubleDec = formatDouble FFGeneric Nothing

{-# INLINABLE formatFloat #-}
formatFloat :: FFFormat -> Maybe Int -> Float -> Builder
formatFloat fmt prec f =
  case fmt of
      FFGeneric
        | Just b <- specialStr f -> b
        | otherwise ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed (fromIntegral m) e' prec
             else byteString $ ryu_f2s_to_chars m e (f < 0)
        where (FD32 m e) = ryu_f2s_fd f
              e' = fromIntegral e + decimalLength9 m
      FFExponent -> byteString $ ryu_f2s f
      FFFixed -> ryu_d2fixed (realToFrac f) prec

{-# INLINABLE formatDouble #-}
formatDouble :: FFFormat -> Maybe Int -> Double -> Builder
formatDouble fmt prec f =
  case fmt of
      FFGeneric
        | Just b <- specialStr f -> b
        | otherwise ->
          if e' >= 0 && e' <= 7
             then sign f `mappend` showFixed m e' prec
             else byteString $ ryu_d2s_to_chars m e (f < 0)
        where (FD64 m e) = ryu_d2s_fd f
              e' = fromIntegral e + decimalLength17 m
      FFExponent -> byteString $ ryu_d2s f
      FFFixed -> ryu_d2fixed f prec


-- C calls and wrappers
foreign import ccall unsafe "static f2s_buffered_n"
    c_ryu_f2s :: CFloat -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "static d2s_buffered_n"
    c_ryu_d2s :: CDouble -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "static f2s_floating_decimal"
    c_ryu_f2s_fd :: CFloat -> Ptr Word32 -> Ptr Int32 -> IO ()

foreign import ccall unsafe "static d2s_floating_decimal"
    c_ryu_d2s_fd :: CDouble -> Ptr Word64 -> Ptr Int32 -> IO ()

foreign import ccall unsafe "static f2s_to_chars"
    c_ryu_f2s_to_chars :: CUInt -> CInt -> CUChar -> Ptr Word8 -> IO CInt

foreign import ccall unsafe "static d2s_to_chars"
    c_ryu_d2s_to_chars :: CULong -> CInt -> CUChar -> Ptr Word8 -> IO CInt

#include "ryu.h"

{-# INLINABLE f2s_max_digits #-}
f2s_max_digits :: Int
f2s_max_digits = #const F2S_MAX_DIGITS

{-# INLINABLE d2s_max_digits #-}
d2s_max_digits :: Int
d2s_max_digits = #const D2S_MAX_DIGITS

{-# INLINE ryu_f2s #-}
ryu_f2s :: Float -> ByteString
ryu_f2s f = unsafeDupablePerformIO $ do
    fp <- mallocByteString f2s_max_digits :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p ->
      PS fp 0 . fromIntegral <$> c_ryu_f2s (realToFrac f) p

{-# INLINE ryu_d2s #-}
ryu_d2s :: Double -> ByteString
ryu_d2s f = unsafeDupablePerformIO $ do
    fp <- mallocByteString d2s_max_digits :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p ->
      PS fp 0 . fromIntegral <$> c_ryu_d2s (realToFrac f) p

data FloatingDecimal64 = FD64 !Word64 !Int32
data FloatingDecimal32 = FD32 !Word32 !Int32

instance Show FloatingDecimal64 where
  showsPrec p (FD64 m e) = showsPrec p m `mappend` showsPrec p '.' `mappend` showsPrec p e

instance Show FloatingDecimal32 where
  showsPrec p (FD32 m e) = showsPrec p m `mappend` showsPrec p '.' `mappend` showsPrec p e

-- extracts base-10 converted mantissa and exponent for floats using ryu
-- algorithm
-- NB: only valid if not NaN, +/-0, or +/-Inf. In practice, all calls should
--     guarded by `specialStr`
{-# INLINE ryu_f2s_fd #-}
ryu_f2s_fd :: Float -> FloatingDecimal32
ryu_f2s_fd f = unsafeDupablePerformIO $
    alloca $ \mOut -> do
      alloca $ \eOut -> do
        c_ryu_f2s_fd (realToFrac f) mOut eOut
        m <- peek mOut
        e <- peek eOut
        return $ FD32 m e

-- extracts base-10 converted mantissa and exponent for doubles using ryu
-- algorithm
-- NB: only valid if not NaN, +/-0, or +/-Inf. In practice, all calls should
--     guarded by `specialStr`
{-# INLINE ryu_d2s_fd #-}
ryu_d2s_fd :: Double -> FloatingDecimal64
ryu_d2s_fd f = unsafeDupablePerformIO $
    alloca $ \mOut -> do
      alloca $ \eOut -> do
        c_ryu_d2s_fd (realToFrac f) mOut eOut
        m <- peek mOut
        e <- peek eOut
        return $ FD64 m e

asCBool :: Bool -> CUChar
asCBool x = if x then 1 else 0

{-# INLINE ryu_f2s_to_chars #-}
ryu_f2s_to_chars :: Word32 -> Int32 -> Bool -> ByteString
ryu_f2s_to_chars m e s = unsafeDupablePerformIO $ do
    fp <- mallocByteString f2s_max_digits :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p ->
      PS fp 0 . fromIntegral <$> c_ryu_f2s_to_chars (fromIntegral m) (fromIntegral e) (asCBool s) p

{-# INLINE ryu_d2s_to_chars #-}
ryu_d2s_to_chars :: Word64 -> Int32 -> Bool -> ByteString
ryu_d2s_to_chars m e s = unsafeDupablePerformIO $ do
    fp <- mallocByteString d2s_max_digits :: IO (ForeignPtr Word8)
    withForeignPtr fp $ \p ->
      PS fp 0 . fromIntegral <$> c_ryu_d2s_to_chars (fromIntegral m) (fromIntegral e) (asCBool s) p


-- auxiliary fixed format printing functions
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

-- | Char7 encode a 'Char'.
{-# INLINE char7 #-}
char7 :: Char -> Builder
char7 = P.primFixed P.char7

-- | Char7 encode a 'String'.
{-# INLINE string7 #-}
string7 :: String -> Builder
string7 = P.primMapListFixed P.char7

sign :: RealFloat a => a -> Builder
sign f = if f < 0 then char7 '-' else mempty

specialStr :: RealFloat a => a -> Maybe Builder
specialStr f
  | isNaN f          = Just $ string7 "NaN"
  | isInfinite f     = Just $ sign f `mappend` string7 "Infinity"
  | isNegativeZero f = Just $ string7 "-0.0"
  | f == 0           = Just $ string7 "0.0"
  | otherwise        = Nothing

-- show fixed floating point matching show / showFFloat output by dropping
-- digits after exponentiation precision
ryu_d2fixed :: Double -> Maybe Int -> Builder
ryu_d2fixed f prec
  | Just b <- specialStr f = b
  | otherwise = sign f `mappend` showFixed m e' prec
  where (FD64 m e) = ryu_d2s_fd f
        olength = decimalLength17 m
        -- NB: exponent in exponential format is e' - 1
        e' = fromIntegral e + olength

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

#if MIN_VERSION_base(4,6,0) && !defined(RYU_32_BIT_PLATFORM)
dquot10 :: Word## -> Word##
dquot10 w
  = let !(## rdx, _ ##) = w `timesWord2##` 0xCCCCCCCCCCCCCCCD####
     in rdx `uncheckedShiftRL##` 3##

dquotRem10 :: Word## -> (## Word##, Word## ##)
dquotRem10 w = let w' = dquot10 w
               in (## w', w `minusWord##` (w' `timesWord##` 10####) ##)

dquotRem10Boxed :: Word64 -> (Word64, Word64)
dquotRem10Boxed (W64## w) = let !(## q, r ##) = dquotRem10 w in (W64## q, W64## r)
#else
dquotRem10Boxed :: Word64 -> (Word64, Word64)
dquotRem10Boxed w = w `quotRem` 10
#endif

digits :: Word64 -> [Int]
digits w = go [] w
  where go ds 0 = ds
        go ds c = let (q, r) = dquotRem10Boxed c
                   in go (fromIntegral r:ds) q
