{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Data.ByteString.Builder.RealFloat.Internal
    ( (.>>)
    , (.<<)
    , mask
    , asWord
    , NonNumbersAndZero(..)
    , toCharsNonNumbersAndZero
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
    , unbox
    , ByteArray(..)
    , castDoubleToWord64
    , castFloatToWord32
    ) where

import Control.Monad (foldM)
import Data.Bits (Bits(..), FiniteBits(..))
import Data.ByteString.Internal (c2w)
import Data.ByteString.Builder.Prim.Internal (BoundedPrim, boundedPrim)
import Data.Char (ord)
import GHC.Int (Int32(..))
import GHC.Exts
import GHC.ST (ST(..), runST)
import GHC.Word (Word8, Word32(..), Word64(..))
import Foreign.Ptr (plusPtr)
import qualified Foreign.Storable as S (poke)

#if __GLASGOW_HASKELL__ >= 804
import GHC.Float (castFloatToWord32, castDoubleToWord64)
#else
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (peek)

-- | Interpret a 'Float' as a 'Word32' as if through a bit-for-bit copy.
-- (fallback if not availble through GHC.Float)
--
-- e.g
--
-- > showHex (castFloatToWord32 1.0) [] = "3f800000"
{-# NOINLINE castFloatToWord32 #-}
castFloatToWord32 :: Float -> Word32
castFloatToWord32 x = unsafePerformIO (with x (peek . castPtr))

-- | Interpret a 'Double' as a 'Word64' as if through a bit-for-bit copy.
-- (fallback if not availble through GHC.Float)
--
-- e.g
--
-- > showHex (castDoubleToWord64 1.0) [] = "3ff0000000000000"
{-# NOINLINE castDoubleToWord64 #-}
castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 x = unsafePerformIO (with x (peek . castPtr))
#endif

-- | Unsafe shiftR by specified number of bits. The second argument must be
-- non-negative and smaller than the number of bits in the type.
{-# INLINABLE (.>>) #-}
(.>>) :: (Bits a, Integral a) => a -> a -> a
a .>> s = unsafeShiftR a (fromIntegral s)

-- | Unsafe shiftL by specified number of bits. The second argument must be
-- non-negative and smaller than the number of bits in the type.
{-# INLINABLE (.<<) #-}
(.<<) :: (Bits a, Integral a) => a -> a -> a
a .<< s = unsafeShiftL a (fromIntegral s)

-- | Build a full bit-mask of specified length.
--
-- e.g
--
-- > showHex (mask (12 :: Word32)) [] = "fff"
{-# INLINABLE mask #-}
mask :: (Bits a, Integral a) => a -> a
mask = flip (-) 1 . (.<<) 1

-- | Convert Boolean False to 0 and True to 1
{-# INLINABLE asWord #-}
asWord :: Integral w => Bool -> w
asWord = fromIntegral . fromEnum

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

-- Special rendering for NaN, positive/negative 0, and positive/negative
-- infinity. These are based on the IEEE representation of non-numbers.
--
-- Infinity
--   sign = 0 for positive infinity, 1 for negative infinity.
--   biased exponent = all 1 bits.
--   fraction = all 0 bits.
--
-- NaN
--   sign = either 0 or 1 (ignored)
--   biased exponent = all 1 bits.
--   fraction = anything except all 0 bits.
--
-- We also handle 0 specially here so that the exponent rendering is more
-- correct.
--   sign = either 0 or 1.
--   biased exponent = all 0 bits.
--   fraction = all 0 bits.
data NonNumbersAndZero = NonNumbersAndZero
  { negative :: Bool
  , exponent_all_one :: Bool
  , mantissa_non_zero :: Bool
  }

-- | Renders NonNumbersAndZero into bounded primitive
toCharsNonNumbersAndZero :: NonNumbersAndZero -> BoundedPrim ()
toCharsNonNumbersAndZero NonNumbersAndZero{..}
  | mantissa_non_zero = boundString "NaN"
  | exponent_all_one = boundString $ signStr ++ "Infinity"
  | otherwise = boundString $ signStr ++ "0.0e0"
  where signStr = if negative then "-" else ""

-- | Part of the calculation on whether to round up the decimal representation.
-- This is currently a constant function to match behavior in Base `show`.
--
-- For round-to-even and correct shortest
-- acceptBoundsUnboxed v = ((v `uncheckedShiftRL#` 2#) `and#` 1##) `eqWord#` 0##
acceptBoundsUnboxed :: Word# -> Int#
acceptBoundsUnboxed _ = 0#

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

-- | Returns w / 10
fquot10 :: Word# -> Word#
fquot10 w = (w `timesWord#` 0xCCCCCCCD##) `uncheckedShiftRL#` 35#

-- | Returns w % 10
frem10 :: Word# -> Word#
frem10 w =
  let w' = fquot10 w
   in w `minusWord#` (w' `timesWord#` 10##)

-- | Returns (w / 10, w % 10)
fquotRem10 :: Word# -> (# Word#, Word# #)
fquotRem10 w =
  let w' = fquot10 w
   in (# w', w `minusWord#` (w' `timesWord#` 10##) #)

-- | Returns w / 5
fquot5 :: Word# -> Word#
fquot5 w = (w `timesWord#` 0xCCCCCCCD##) `uncheckedShiftRL#` 34#

-- | Returns w % 5
frem5 :: Word# -> Word#
frem5 w =
  let w' = fquot5 w
   in w `minusWord#` (w' `timesWord#` 5##)

-- | Returns (w / 5, w % 5)
fquotRem5 :: Word# -> (# Word#, Word# #)
fquotRem5 w =
  let w' = fquot5 w
   in (# w', w `minusWord#` (w' `timesWord#` 5##) #)

-- | Returns (w / 10, w % 10)
fquotRem10Boxed :: Word32 -> (Word32, Word32)
fquotRem10Boxed (W32# w) = let !(# q, r #) = fquotRem10 w in (W32# q, W32# r)

-- | Wrap a unboxed function on 32-bit floats into the boxed equivalent
fwrapped :: (Word# -> Word#) -> Word32 -> Word32
fwrapped f (W32# w) = W32# (f w)

-- | Returns w / 10
dquot10 :: Word# -> Word#
dquot10 w =
  let !(# rdx, _ #) = w `timesWord2#` 0xCCCCCCCCCCCCCCCD##
    in rdx `uncheckedShiftRL#` 3#

-- | Returns w / 100
dquot100 :: Word# -> Word#
dquot100 w =
  let !(# rdx, _ #) = (w `uncheckedShiftRL#` 2#) `timesWord2#` 0x28F5C28F5C28F5C3##
    in rdx `uncheckedShiftRL#` 2#

-- | Returns w % 10
drem10 :: Word# -> Word#
drem10 w =
  let w' = dquot10 w
   in w `minusWord#` (w' `timesWord#` 10##)

-- | Returns (w / 10, w % 10)
dquotRem10 :: Word# -> (# Word#, Word# #)
dquotRem10 w =
  let w' = dquot10 w
   in (# w', w `minusWord#` (w' `timesWord#` 10##) #)

-- | Returns w / 5
dquot5 :: Word# -> Word#
dquot5 w =
  let !(# rdx, _ #) = w `timesWord2#` 0xCCCCCCCCCCCCCCCD##
    in rdx `uncheckedShiftRL#` 2#

-- | Returns w % 5
drem5 :: Word# -> Word#
drem5 w =
  let w' = dquot5 w
   in w `minusWord#` (w' `timesWord#` 5##)

-- | Returns (w / 5, w % 5)
dquotRem5 :: Word# -> (# Word#, Word# #)
dquotRem5 w =
  let w' = dquot5 w
   in (# w', w `minusWord#` (w' `timesWord#` 5##) #)

-- | Returns (w / 10, w % 10)
dquotRem10Boxed :: Word64 -> (Word64, Word64)
dquotRem10Boxed (W64# w) = let !(# q, r #) = dquotRem10 w in (W64# q, W64# r)

-- | Wrap a unboxed function on 64-bit floats into the boxed equivalent
dwrapped :: (Word# -> Word#) -> Word64 -> Word64
dwrapped f (W64# w) = W64# (f w)

-- | Unbox an Int
unbox :: Int -> Int#
unbox (I# i) = i

-- | Returns the number of times w is divisible by 5
pow5_factor :: Word# -> Int# -> Int#
pow5_factor w count =
  let !(# q, r #) = dquotRem5 w
   in case r `eqWord#` 0## of
        0# -> count
        _  -> pow5_factor q (count +# 1#)

-- | Returns 1# if value is divisible by 5^p
multipleOfPowerOf5_Unboxed :: Word# -> Word# -> Int#
multipleOfPowerOf5_Unboxed value p = pow5_factor value 0# >=# word2Int# p

-- | Returns True if value is divisible by 5^p
multipleOfPowerOf5_UnboxedB :: Word# -> Word# -> Bool
multipleOfPowerOf5_UnboxedB value p = isTrue# (multipleOfPowerOf5_Unboxed value p)

-- | Returns 1# if value is divisible by 2^p
multipleOfPowerOf2Unboxed :: Word# -> Word# -> Int#
multipleOfPowerOf2Unboxed value p = (value `and#` ((1## `uncheckedShiftL#` word2Int# p) `minusWord#` 1##)) `eqWord#` 0##

-- | Wrapper for polymorphic handling of 32- and 64-bit floats
class (FiniteBits a, Integral a) => Mantissa a where
  decimalLength :: a -> Int
  raw :: a -> Word#
  wrap :: Word# -> a
  quotRem100 :: a -> (# Word#, Word# #)
  quotRem10000 :: a -> (# Word#, Word# #)

instance Mantissa Word32 where
  decimalLength = decimalLength9
  raw (W32# w) = w
  wrap w = (W32# w)
  quotRem100 (W32# w) =
    let w' = (w `timesWord#` 0x51EB851F##) `uncheckedShiftRL#` 37#
      in (# w', (w `minusWord#` (w' `timesWord#` 100##)) #)
  quotRem10000 (W32# w) =
    let w' = (w `timesWord#` 0xD1B71759##) `uncheckedShiftRL#` 45#
      in (# w', (w `minusWord#` (w' `timesWord#` 10000##)) #)

instance Mantissa Word64 where
    decimalLength = decimalLength17
    raw (W64# w) = w
    wrap w = (W64# w)
    quotRem100 (W64# w) =
      let w' = dquot100 w
       in (# w', (w `minusWord#` (w' `timesWord#` 100##)) #)
    quotRem10000 (W64# w) =
      let !(# rdx, _ #) = w `timesWord2#` 0x346DC5D63886594B##
          w' = rdx `uncheckedShiftRL#` 11#
       in (# w', (w `minusWord#` (w' `timesWord#` 10000##)) #)

-- Wrappe around int2Word#
asciiRaw :: Int -> Word#
asciiRaw (I# i) = int2Word# i

asciiZero :: Int
asciiZero = ord '0'

asciiDot :: Int
asciiDot = ord '.'

asciiMinus :: Int
asciiMinus = ord '-'

ascii_e :: Int
ascii_e = ord 'e'

-- | Convert a single-digit number to the ascii ordinal e.g '1' -> 0x31
toAscii :: Word# -> Word#
toAscii a = a `plusWord#` asciiRaw asciiZero

data ByteArray = ByteArray ByteArray#

-- | ByteArray of 2-digit pairs 00..99 for faster ascii rendering
digit_table :: ByteArray
digit_table = runST (ST $ \s1 ->
  let !(# s2, marr #) = newByteArray# 200# s1
      go (I# y) r = \i s ->
        let !(# h, l #) = fquotRem10 (int2Word# y)
            e' = (toAscii l `uncheckedShiftL#` 8#) `or#` toAscii h
            s' = writeWord16Array# marr i e' s
         in if isTrue# (i ==# 99#) then s' else r (i +# 1#) s'
      !(# s3, bs #) = unsafeFreezeByteArray# marr (foldr go (\_ s -> s) [0..99] 0# s2)
   in (# s3, ByteArray bs #))

-- | Unsafe index a ByteArray for the 16-bit word at the index
unsafeAt :: ByteArray -> Int# -> Word#
unsafeAt (ByteArray bs) i = indexWord16Array# bs i

-- | Write a 16-bit word into the given address
copyWord16 :: Word# -> Addr# -> State# d -> State# d
copyWord16 w a s = writeWord16OffAddr# a 0# w s

-- | Write an 8-bit word into the given address
poke :: Addr# -> Word# -> State# d -> State# d
poke a w s = writeWord8OffAddr# a 0# w s

-- | Write the mantissa into the given address. This function attempts to
-- optimize this by writing pairs of digits simultaneously when the mantissa is
-- large enough
{-# SPECIALIZE writeMantissa :: Addr# -> Int# -> Word32 -> State# d -> (# Addr#, State# d #) #-}
{-# SPECIALIZE writeMantissa :: Addr# -> Int# -> Word64 -> State# d -> (# Addr#, State# d #) #-}
writeMantissa :: forall a d. (Mantissa a) => Addr# -> Int# -> a -> State# d -> (# Addr#, State# d #)
writeMantissa ptr olength = go (ptr `plusAddr#` olength)
  where
    go p mantissa s1
      | mantissa >= 10000 =
          let !(# m', c #) = quotRem10000 mantissa
              !(# c1, c0 #) = quotRem100 (wrap c :: a)
              s2 = copyWord16 (digit_table `unsafeAt` word2Int# c0) (p `plusAddr#` (-1#)) s1
              s3 = copyWord16 (digit_table `unsafeAt` word2Int# c1) (p `plusAddr#` (-3#)) s2
           in go (p `plusAddr#` (-4#)) (wrap m') s3
      | mantissa >= 100 =
          let !(# m', c #) = quotRem100 mantissa
              s2 = copyWord16 (digit_table `unsafeAt` word2Int# c) (p `plusAddr#` (-1#)) s1
           in finalize (wrap m' :: a) s2
      | otherwise = finalize mantissa s1
    finalize mantissa s1
      | mantissa >= 10 =
        let !bs = digit_table `unsafeAt` word2Int# (raw mantissa)
            s2 = poke (ptr `plusAddr#` 2#) (bs `uncheckedShiftRL64#` 8#) s1
            s3 = poke (ptr `plusAddr#` 1#) (asciiRaw asciiDot) s2
            s4 = poke ptr (bs `and#` 0xff##) s3
           in (# ptr `plusAddr#` (olength +# 1#), s4 #)
      | (I# olength) > 1 =
          let s2 = copyWord16 (((asciiRaw asciiDot) `uncheckedShiftL#` 8#) `or#` toAscii (raw mantissa)) ptr s1
           in (# ptr `plusAddr#` (olength +# 1#), s2 #)
      | otherwise =
          let s2 = poke (ptr `plusAddr#` 2#) (asciiRaw asciiZero) s1
              s3 = poke (ptr `plusAddr#` 1#) (asciiRaw asciiDot) s2
              s4 = poke ptr (toAscii (raw mantissa)) s3
           in (# ptr `plusAddr#` 3#, s4 #)

-- | Write the exponent into the given address.
writeExponent :: Addr# -> Int -> State# d -> (# Addr#, State# d #)
writeExponent ptr !expo@(I# e) s1
  | expo >= 100 =
      let !(# e1, e0 #) = fquotRem10 (int2Word# e)
          s2 = copyWord16 (digit_table `unsafeAt` word2Int# e1) ptr s1
          s3 = poke (ptr `plusAddr#` 2#) (toAscii e0) s2
       in (# ptr `plusAddr#` 3#, s3 #)
  | expo >= 10 =
      let s2 = copyWord16 (digit_table `unsafeAt` e) ptr s1
       in (# ptr `plusAddr#` 2#, s2 #)
  | otherwise =
      let s2 = poke ptr (toAscii (int2Word# e)) s1
       in (# ptr `plusAddr#` 1#, s2 #)

-- | Write the sign into the given address.
writeSign :: Addr# -> Bool -> State# d -> (# Addr#, State# d #)
writeSign ptr True s1 =
  let s2 = poke ptr (asciiRaw asciiMinus) s1
   in (# ptr `plusAddr#` 1#, s2 #)
writeSign ptr False s = (# ptr, s #)

-- | Returns the decimal representation of a floating point number in
-- scientific (exponential) notation
{-# INLINABLE toCharsScientific #-}
{-# SPECIALIZE toCharsScientific :: Bool -> Word32 -> Int32 -> BoundedPrim () #-}
{-# SPECIALIZE toCharsScientific :: Bool -> Word64 -> Int32 -> BoundedPrim () #-}
toCharsScientific :: (Mantissa a) => Bool -> a -> Int32 -> BoundedPrim ()
toCharsScientific !sign !mantissa !expo = boundedPrim maxEncodedLength $ \_ !(Ptr p0)-> do
  let !olength@(I# ol) = decimalLength mantissa
      !expo' = expo + fromIntegral olength - 1
  return $ runST (ST $ \s1 ->
    let !(# p1, s2 #) = writeSign p0 sign s1
        !(# p2, s3 #) = writeMantissa p1 ol mantissa s2
        s4 = poke p2 (asciiRaw ascii_e) s3
        !(# p3, s5 #) = writeSign (p2 `plusAddr#` 1#) (expo' < 0) s4
        !(# p4, s6 #) = writeExponent p3 (fromIntegral $ abs expo') s5
     in (# s6, (Ptr p4) #))
