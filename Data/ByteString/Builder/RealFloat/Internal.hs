{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}

module Data.ByteString.Builder.RealFloat.Internal
    ( (.>>)
    , (.<<)
    , mask
    , asWord
    , pmap
    , (...)
    , special
    , decimalLength9
    , decimalLength17
    , pow5bitsUnboxed
    , log10pow2Unboxed
    , log10pow5Unboxed
    , pow5_factor
    , multipleOfPowerOf5_Unboxed
    , multipleOfPowerOf5_UnboxedB
    , multipleOfPowerOf2Unboxed
    , acceptBoundsUnboxed
    , toCharsScientific
    , fcoerceToWord
    , dcoerceToWord
    -- hand-rolled division and remainder for f2s and d2s
    , fquot10
    , frem10
    , fquotRem10
    , fquotRem10Boxed
    , fquot5
    , frem5
    , fquotRem5
    , fwrapped
    , dquot10
    , drem10
    , dquotRem10
    , dquotRem10Boxed
    , dquot5
    , drem5
    , dquotRem5
    , dquot100
    , dwrapped
    -- prim-op helpers
    , boxToBool
    , box
    , unbox
    ) where

import Control.Monad (foldM)
import Data.Array.Unboxed (UArray, IArray(..), listArray)
import Data.Array.Base (unsafeAt, STUArray(..), MArray(..), castSTUArray, readArray)
import Data.Bits (Bits(..), FiniteBits(..))
import Data.ByteString.Internal (c2w)
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boundedPrim)
import Data.Char (ord)
import GHC.Int (Int(..), Int32)
import GHC.Exts
import GHC.Word (Word8, Word16, Word32(..), Word64(..))
import GHC.ST (ST(..), runST)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (poke)

{-# INLINABLE (.>>) #-}
(.>>) :: (Bits a, Integral b) => a -> b -> a
a .>> s = unsafeShiftR a (fromIntegral s)

{-# INLINABLE (.<<) #-}
(.<<) :: (Bits a, Integral b) => a -> b -> a
a .<< s = unsafeShiftL a (fromIntegral s)

{-# INLINABLE mask #-}
mask :: (Bits a, Integral a) => a -> a
mask = flip (-) 1 . (.<<) 1

{-# INLINABLE asWord #-}
asWord :: Integral w => Bool -> w
asWord = fromIntegral . fromEnum

pmap :: (a -> c) -> (a, b) -> (c, b)
pmap f (a, b) = (f a, b)

-- Returns the number of decimal digits in v, which must not contain more than 9 digits.
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

-- Returns the number of decimal digits in v, which must not contain more than 17 digits.
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

infixr 9 `dot`
dot :: (a -> b) -> (c -> d -> a) -> c -> d -> b
dot = (.) . (.)

infixr 9 ...
(...) :: (a -> b) -> (c -> d -> e -> a) -> c -> d -> e -> b
(...) = dot . (.)

-- TODO encode proof in types?
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

-- TODO TH?
pokeAll :: String -> Ptr Word8 -> IO (Ptr Word8)
pokeAll s ptr = foldM pokeOne ptr s
  where pokeOne p c = poke p (c2w c) >> return (p `plusPtr` 1)

boundString :: String -> BoundedPrim ()
boundString s = boundedPrim maxEncodedLength $ const (pokeAll s)

--         Sign -> Exp  -> Mantissa
special :: Bool -> Bool -> Bool -> BoundedPrim ()
special    _       _       True =  boundString "NaN"
special    True    False   _    =  boundString "-0.0e0"
special    False   False   _    =  boundString "0.0e0"
special    True    True    _    =  boundString "-Infinity"
special    False   True    _    =  boundString "Infinity"

-- Returns e == 0 ? 1 : ceil(log_2(5^e)); requires 0 <= e <= 3528.
pow5bitsUnboxed :: Int# -> Int#
pow5bitsUnboxed e = (e *# 1217359#) `uncheckedIShiftRL#` 19# +# 1#

-- Returns floor(log_10(2^e)); requires 0 <= e <= 1650.
log10pow2Unboxed :: Int# -> Int#
log10pow2Unboxed e = (e *# 78913#) `uncheckedIShiftRL#` 18#

-- Returns floor(log_10(5^e)); requires 0 <= e <= 2620.
log10pow5Unboxed :: Int# -> Int#
log10pow5Unboxed e = (e *# 732928#) `uncheckedIShiftRL#` 20#

acceptBoundsUnboxed :: Word# -> Int#
acceptBoundsUnboxed _ = 0#
-- for round-to-even and correct shortest
-- acceptBoundsUnboxed v = ((v `uncheckedShiftRL#` 2#) `and#` 1##) `eqWord#` 0##

fcoerceToWord :: Float -> Word32
fcoerceToWord !x = runST (cast x)

dcoerceToWord :: Double -> Word64
dcoerceToWord !x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

fquot10 :: Word# -> Word#
fquot10 w = (w `timesWord#` 0xCCCCCCCD##) `uncheckedShiftRL#` 35#

frem10 :: Word# -> Word#
frem10 w =
  let w' = fquot10 w
   in w `minusWord#` (w' `timesWord#` 10##)

fquotRem10 :: Word# -> (# Word#, Word# #)
fquotRem10 w =
  let w' = fquot10 w
   in (# w', w `minusWord#` (w' `timesWord#` 10##) #)

fquot5 :: Word# -> Word#
fquot5 w = (w `timesWord#` 0xCCCCCCCD##) `uncheckedShiftRL#` 34#

frem5 :: Word# -> Word#
frem5 w =
  let w' = fquot5 w
   in w `minusWord#` (w' `timesWord#` 5##)

fquotRem5 :: Word# -> (# Word#, Word# #)
fquotRem5 w =
  let w' = fquot5 w
   in (# w', w `minusWord#` (w' `timesWord#` 5##) #)

fquotRem10Boxed :: Word32 -> (Word32, Word32)
fquotRem10Boxed (W32# w) = let !(# q, r #) = fquotRem10 w in (W32# q, W32# r)

fwrapped :: (Word# -> Word#) -> Word32 -> Word32
fwrapped f (W32# w) = W32# (f w)

dquot10 :: Word# -> Word#
dquot10 w =
  let !(# rdx, _ #) = w `timesWord2#` 0xCCCCCCCCCCCCCCCD##
    in rdx `uncheckedShiftRL#` 3#

dquot100 :: Word# -> Word#
dquot100 w =
  let !(# rdx, _ #) = (w `uncheckedShiftRL#` 2#) `timesWord2#` 0x28F5C28F5C28F5C3##
    in rdx `uncheckedShiftRL#` 2#

drem10 :: Word# -> Word#
drem10 w =
  let w' = dquot10 w
   in w `minusWord#` (w' `timesWord#` 10##)

dquotRem10 :: Word# -> (# Word#, Word# #)
dquotRem10 w =
  let w' = dquot10 w
   in (# w', w `minusWord#` (w' `timesWord#` 10##) #)

dquot5 :: Word# -> Word#
dquot5 w =
  let !(# rdx, _ #) = w `timesWord2#` 0xCCCCCCCCCCCCCCCD##
    in rdx `uncheckedShiftRL#` 2#

drem5 :: Word# -> Word#
drem5 w =
  let w' = dquot5 w
   in w `minusWord#` (w' `timesWord#` 5##)

dquotRem5 :: Word# -> (# Word#, Word# #)
dquotRem5 w =
  let w' = dquot5 w
   in (# w', w `minusWord#` (w' `timesWord#` 5##) #)

dquotRem10Boxed :: Word64 -> (Word64, Word64)
dquotRem10Boxed (W64# w) = let !(# q, r #) = dquotRem10 w in (W64# q, W64# r)

dwrapped :: (Word# -> Word#) -> Word64 -> Word64
dwrapped f (W64# w) = W64# (f w)

boxToBool :: Int# -> Bool
boxToBool i = case i of
                1# -> True
                0# -> False

box :: Int# -> Int
box i = I# i

unbox :: Int -> Int#
unbox (I# i) = i

pow5_factor :: Word# -> Int# -> Int#
pow5_factor w count =
  let !(# q, r #) = dquotRem5 w
   in case r `eqWord#` 0## of
        0# -> count
        1# -> pow5_factor q (count +# 1#)

multipleOfPowerOf5_Unboxed :: Word# -> Word# -> Int#
multipleOfPowerOf5_Unboxed value p = pow5_factor value 0# >=# word2Int# p

multipleOfPowerOf5_UnboxedB :: Word# -> Word# -> Bool
multipleOfPowerOf5_UnboxedB value p = boxToBool (multipleOfPowerOf5_Unboxed value p)

multipleOfPowerOf2Unboxed :: Word# -> Word# -> Int#
multipleOfPowerOf2Unboxed value p = (value `and#` ((1## `uncheckedShiftL#` word2Int# p) `minusWord#` 1##)) `eqWord#` 0##

class (IArray UArray a, FiniteBits a, Integral a) => Mantissa a where
  decimalLength :: a -> Int
  quotRem100 :: a -> (a, a)
  quotRem10000 :: a -> (a, a)

instance Mantissa Word32 where
  decimalLength = decimalLength9
  quotRem100 (W32# w) =
    let w' = (w `timesWord#` 0x51EB851F##) `uncheckedShiftRL#` 37#
      in (W32# w', W32# (w `minusWord#` (w' `timesWord#` 100##)))
  quotRem10000 (W32# w) =
    let w' = (w `timesWord#` 0xD1B71759##) `uncheckedShiftRL#` 45#
      in (W32# w', W32# (w `minusWord#` (w' `timesWord#` 10000##)))

instance Mantissa Word64 where
    decimalLength = decimalLength17
    quotRem100 (W64# w) =
      let w' = dquot100 w
       in (W64# w', W64# (w `minusWord#` (w' `timesWord#` 100##)))
    quotRem10000 (W64# w) =
      let !(# rdx, _ #) = w `timesWord2#` 0x346DC5D63886594B##
          w' = rdx `uncheckedShiftRL#` 11#
       in (W64# w', W64# (w `minusWord#` (w' `timesWord#` 10000##)))

type DigitStore = Word16

toAscii :: (Integral a, Integral b) => a -> b
toAscii = fromIntegral . (+) (fromIntegral $ ord '0')

digit_table :: UArray Int32 DigitStore
digit_table = listArray (0, 99) [ (toAscii b .<< (8 :: Word16)) .|. toAscii a | a <- [0..9 :: Word16], b <- [0..9 :: Word16] ]

copy :: DigitStore -> Ptr Word8 -> IO ()
copy d p = poke (castPtr p) d

first :: DigitStore -> Word8
first = fromIntegral . flip (.>>) (8 :: Word16)

second :: DigitStore -> Word8
second = fromIntegral

-- for loop recursively...
{-# SPECIALIZE writeMantissa :: Ptr Word8 -> Int -> Word32 -> IO (Ptr Word8) #-}
{-# SPECIALIZE writeMantissa :: Ptr Word8 -> Int -> Word64 -> IO (Ptr Word8) #-}
writeMantissa :: (Mantissa a) => Ptr Word8 -> Int -> a -> IO (Ptr Word8)
writeMantissa !ptr !olength = go (ptr `plusPtr` olength)
  where
    go !p !mantissa
      | mantissa >= 10000 = do
          let !(m', c) = quotRem10000 mantissa
              !(c1, c0) = quotRem100 c
          copy (digit_table `unsafeAt` fromIntegral c0) (p `plusPtr` (-1))
          copy (digit_table `unsafeAt` fromIntegral c1) (p `plusPtr` (-3))
          go (p `plusPtr` (-4)) m'
      | mantissa >= 100 = do
          let !(m', c) = quotRem100 mantissa
          copy (digit_table `unsafeAt` fromIntegral c) (p `plusPtr` (-1))
          finalize m'
      | otherwise = finalize mantissa
    finalize mantissa
      | mantissa >= 10 = do
          let !bs = digit_table `unsafeAt` fromIntegral mantissa
          poke (ptr `plusPtr` 2) (first bs)
          poke (ptr `plusPtr` 1) (c2w '.')
          poke ptr (second bs)
          return (ptr `plusPtr` (olength + 1))
      | olength > 1 = do
          copy ((fromIntegral (c2w '.') .<< (8 :: Word16)) .|. toAscii mantissa) ptr
          return $ ptr `plusPtr` (olength + 1)
      | otherwise = do
          poke (ptr `plusPtr` 2) (c2w '0')
          poke (ptr `plusPtr` 1) (c2w '.')
          poke ptr (toAscii mantissa)
          return (ptr `plusPtr` 3)

writeExponent :: Ptr Word8 -> Int32 -> IO (Ptr Word8)
writeExponent !ptr !expo
  | expo >= 100 = do
      let !(e1, e0) = fquotRem10Boxed (fromIntegral expo)
      copy (digit_table `unsafeAt` fromIntegral e1) ptr
      poke (ptr `plusPtr` 2) (toAscii e0 :: Word8)
      return $ ptr `plusPtr` 3
  | expo >= 10 = do
      copy (digit_table `unsafeAt` fromIntegral expo) ptr
      return $ ptr `plusPtr` 2
  | otherwise = do
      poke ptr (toAscii expo)
      return $ ptr `plusPtr` 1

writeSign :: Ptr Word8 -> Bool -> IO (Ptr Word8)
writeSign ptr True = do
  poke ptr (c2w '-')
  return $ ptr `plusPtr` 1
writeSign ptr False = return ptr

{-# INLINABLE toCharsScientific #-}
{-# SPECIALIZE toCharsScientific :: Bool -> Word32 -> Int32 -> BoundedPrim () #-}
{-# SPECIALIZE toCharsScientific :: Bool -> Word64 -> Int32 -> BoundedPrim () #-}
toCharsScientific :: (Mantissa a) => Bool -> a -> Int32 -> BoundedPrim ()
toCharsScientific !sign !mantissa !expo = boundedPrim maxEncodedLength $ \_ !p0 -> do
  let !olength = decimalLength mantissa
      !expo' = expo + fromIntegral olength - 1
  p1 <- writeSign p0 sign
  p2 <- writeMantissa p1 olength mantissa
  poke p2 (c2w 'e')
  p3 <- writeSign (p2 `plusPtr` 1) (expo' < 0)
  writeExponent p3 (abs expo')
