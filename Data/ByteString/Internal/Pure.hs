{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

-- Enable yields to make `isValidUtf8` safe to use on large inputs.
{-# OPTIONS_GHC -fno-omit-yields #-}

-- | Haskell implementation of C bits
module Data.ByteString.Internal.Pure
  ( -- * standard string.h functions
    strlen
  , memchr
  , memcmp
    -- * fpstring.c
  , intersperse
  , countOcc
  , countOccBA
  , reverseBytes
  , findMaximum
  , findMinimum
  , quickSort
  , elemIndex
  , isValidUtf8
  , isValidUtf8BA
  -- * itoa.c
  , encodeSignedDec
  , encodeUnsignedDec
  , encodeUnsignedDecPadded
  , encodeUnsignedHex
  -- * static tables (unaligned!)
  , lower_hex_table
  , digit_pairs_table
  )
where

import Prelude

import GHC.Exts                 (Ptr(..), ByteArray#, indexWord8Array#, Word8#, Int#, indexWord8OffAddr#)
import GHC.Types                (Int (..))
import GHC.Word                 (Word8(..))
import GHC.Int                  (Int8(..))

import Data.Bits                (Bits(..), shiftR, (.&.))
import Data.Word
import Foreign.Ptr              (plusPtr, nullPtr)
import Foreign.Storable         (Storable(..))
import Control.Monad            (when)
import Control.Exception        (assert)

import Data.ByteString.Utils.ByteOrder
import Data.ByteString.Utils.UnalignedAccess

----------------------------------------------------------------
-- Haskell versions of standard functions in string.h
----------------------------------------------------------------

strlen :: Ptr Word8 -> IO Int
strlen = go 0 where
  go :: Int -> Ptr Word8 -> IO Int
  go !acc !p = do
    c <- peek p
    if | c == 0 -> pure acc
       | nextAcc <- acc + 1
       , nextAcc >= 0 -> go nextAcc (p `plusPtr` 1)
       | otherwise -> errorWithoutStackTrace
           "bytestring: strlen: String length does not fit in a Haskell Int"

memchr :: Ptr Word8 -> Word8 -> Int -> IO (Ptr Word8)
memchr !p !target !len
  | len == 0 = pure nullPtr
  | otherwise = assert (len > 0) $ do
      c <- peek p
      if c == target
        then pure p
        else memchr (p `plusPtr` 1) target (len - 1)

memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int
memcmp !p1 !p2 !len
  | len >= 8 = do
      w1 <- unalignedReadU64 p1
      w2 <- unalignedReadU64 p2
      let toBigEndian = whenLittleEndian byteSwap64
      if | w1 == w2
           -> memcmp (p1 `plusPtr` 8) (p2 `plusPtr` 8) (len - 8)
         | toBigEndian w1 < toBigEndian w2
           -> pure (0-1)
         | otherwise -> pure 1
  | otherwise = memcmp1 p1 p2 len

-- | Like 'memcmp', but definitely scans one byte at a time
memcmp1 :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int
memcmp1 !p1 !p2 !len
  | len == 0 = pure 0
  | otherwise = assert (len > 0) $ do
      c1 <- peek p1
      c2 <- peek p2
      if | c1 == c2 -> memcmp1 (p1 `plusPtr` 1) (p2 `plusPtr` 1) (len - 1)
         | c1 < c2   -> pure (0-1)
         | otherwise -> pure 1


----------------------------------------------------------------
-- Haskell versions of functions in fpstring.c
----------------------------------------------------------------

-- | duplicate a string, interspersing the character through the elements of the
-- duplicated string
intersperse :: Ptr Word8 -> Ptr Word8 -> Int -> Word8 -> IO ()
intersperse !dst !src !len !w = case len of
  0 -> pure ()
  1 -> do
    -- copy last char
    c <- peekByteOff src 0
    pokeByteOff dst 0 (c :: Word8)
  _ -> do
    c <- peekByteOff src 0
    pokeByteOff dst 0 (c :: Word8)
    pokeByteOff dst 1 w
    intersperse (plusPtr dst 2) (plusPtr src 1) (len-1) w

countOccBA :: ByteArray# -> Int -> Word8 -> IO Int
countOccBA ba len w = pure (go 0 0)
  where
    go !n !i@(I# i#)
      | i == len = n
      | W8# (indexWord8Array# ba i#) == w = go (n+1) (i+1)
      | otherwise = go n (i+1)

countOcc :: Ptr Word8 -> Int -> Word8 -> IO Int
countOcc p len w = go 0 0
  where
    go !n !i
      | i == len  = pure n
      | otherwise = do
          c <- peekByteOff p i
          if c == w
            then go (n+1) (i+1)
            else go n     (i+1)

-- | Haskell equivalent of C `sbs_elem_index`
elemIndex :: ByteArray# -> Word8 -> Int -> IO Int
elemIndex !ba !w !len = pure (go 0)
  where
    go !i@(I# i#)
      | i == len  = -1
      | W8# (indexWord8Array# ba i#) == w = i
      | otherwise = go (i+1)

-- | Reverse n-bytes from the second pointer into the first
reverseBytes :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
reverseBytes !dst !src !n
  | n == 0    = pure ()
  | otherwise = reverse_bytes dst (plusPtr dst (n - 1)) src

-- | Note that reverse_bytes reverses at least one byte.
-- Then it loops if necessary until the destination buffer is full
reverse_bytes :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
reverse_bytes orig_dst dst src = do
  c <- peekByteOff src 0
  pokeByteOff dst 0 (c :: Word8)
  if orig_dst == dst
    then pure ()
    else reverse_bytes orig_dst (plusPtr dst (-1)) (plusPtr src 1)


findMaximum :: Ptr Word8 -> Int -> IO Word8
findMaximum !p !n = assert (n > 0) $ find_maximum minBound p (plusPtr p (n - 1))

find_maximum :: Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word8
find_maximum !m !p !plast = do
  c <- peekByteOff p 0
  let !c' = if c > m then c else m
  if p == plast
    then pure c'
    else find_maximum c' (plusPtr p 1) plast

findMinimum :: Ptr Word8 -> Int -> IO Word8
findMinimum !p !n = assert (n > 0) $ find_minimum maxBound p (plusPtr p (n - 1))

find_minimum :: Word8 -> Ptr Word8 -> Ptr Word8 -> IO Word8
find_minimum !m !p !plast = do
  c <- peekByteOff p 0
  let !c' = if c < m then c else m
  if p == plast
    then pure c'
    else find_minimum c' (plusPtr p 1) plast


quickSort :: Ptr Word8 -> Int -> IO ()
quickSort !p !n
  | n <= 0    = pure ()
  | otherwise = quick_sort p 0 (n - 1)

quick_sort :: Ptr Word8 -> Int -> Int -> IO ()
quick_sort !p !low !high
  | low >= high = pure ()
  | otherwise   = do
    pivot_index <- partition p low high
    quick_sort p low (pivot_index-1)
    quick_sort p (pivot_index+1) high


partition :: Ptr Word8 -> Int -> Int -> IO Int
partition !p !low !high = do
  -- choose the rightmost element as the pivot
  pivot <- peekByteOff p high :: IO Word8
  -- traverse through all elements.
  -- swap element smaller than pivot at index j with leftmost element at
  -- index i greater than pivot (can be itself if no greater element read yet)
  let go !i !j
        | j > high  = pure (i-1)
        | otherwise = do
          jv <- peekByteOff p j
          if (jv <= pivot)
            then do
              when (i /= j) $ do
                -- swap values
                iv <- peekByteOff p i :: IO Word8
                pokeByteOff p j iv
                pokeByteOff p i jv
              go (i+1) (j+1)
            else
              go i (j+1)
  go low low

isValidUtf8BA :: ByteArray# -> Int -> IO Bool
isValidUtf8BA !ba !len' = isValidUtf8' (indexWord8Array# ba) len'

isValidUtf8 :: Ptr Word8 -> Int -> IO Bool
isValidUtf8 !(Ptr a) !len' = isValidUtf8' (indexWord8OffAddr# a) len'

isValidUtf8' :: (Int# -> Word8#) -> Int -> IO Bool
isValidUtf8' idx !len = go 0
  where
    indexWord8 (I# i) = W8# (idx i)

    indexIsCont :: Int -> Bool
    indexIsCont i =
        -- We use a signed comparison to avoid an extra comparison with 0x80,
        -- since _signed_ 0x80 is -128.
        let
           v :: Int8
           v = fromIntegral (indexWord8 i)
        in v <= (fromIntegral (0xBF :: Word8))

    go !i
      | i >= len  = pure True -- done
      | otherwise = do
            let !b0 = indexWord8 i
            if | b0 <= 0x7F -> go (i+1) -- ASCII
               | b0 >= 0xC2 && b0 <= 0xDF -> go2 (i+1)
               | b0 >= 0xE0 && b0 <= 0xEF -> go3 (i+1) b0
               | otherwise                -> go4 (i+1) b0

    go2 !i
      | i >= len  = pure False
      | indexIsCont i
      = go (i+1)
      | otherwise
      = pure False

    go3 !i !b0
      | i >= len - 1  = pure False -- Be careful: i+1 might overflow!
      | indexIsCont i
      , indexIsCont (i+1)
      , b1 <- indexWord8 i
      ,    (b0 == 0xE0 && b1 >= 0xA0)  -- E0, A0..BF, 80..BF
        || (b0 >= 0xE1 && b0 <= 0xEC)  -- E1..EC, 80..BF, 80..BF
        || (b0 == 0xED && b1 <= 0x9F)  -- ED, 80..9F, 80..BF
        || (b0 >= 0xEE && b0 <= 0xEF)  -- EE..EF, 80..BF, 80..BF
      = go (i+2)
      | otherwise
      = pure False

    go4 !i !b0
      | i >= len - 2  = pure False -- Be careful: i+2 might overflow!
      | indexIsCont i
      , indexIsCont (i+1)
      , indexIsCont (i+2)
      , b1 <- indexWord8 i
      ,    (b0 == 0xF0 && b1 >= 0x90) -- F0, 90..BF, 80..BF, 80..BF
        || (b0 >= 0xF1 && b0 <= 0xF3) -- F1..F3, 80..BF, 80..BF, 80..BF
        || (b0 == 0xF4 && b1 <= 0x8F) -- F4, 80..8F, 80..BF, 80..BF
      = go (i+3)

      | otherwise
      = pure False


----------------------------------------------------------------
-- Haskell versions of functions in itoa.c
----------------------------------------------------------------


getDigit :: Int -> Word8
getDigit (I# i) = W8# (indexWord8OffAddr# digits i)
  where
    !digits = "0123456789abcdef"#

putDigit :: Ptr a -> Int -> Int -> IO ()
putDigit !addr !off !i = pokeByteOff addr off (getDigit i)

-- | Reverse bytes in the given memory range (inclusive)
reverseBytesInplace :: Ptr Word8 -> Ptr Word8 -> IO ()
reverseBytesInplace !p1 !p2
  | p1 < p2 = do
    c1 <- peekByteOff p1 0
    c2 <- peekByteOff p2 0
    pokeByteOff p1 0 (c2 :: Word8)
    pokeByteOff p2 0 (c1 :: Word8)
    reverseBytesInplace (plusPtr p1 1) (plusPtr p2 (-1))
  | otherwise = pure ()

-- | Encode signed number as decimal
encodeSignedDec :: (Eq a, Num a, Integral a) => a -> Ptr Word8 -> IO (Ptr Word8)
{-# INLINABLE encodeSignedDec #-} -- for specialization
encodeSignedDec !x !buf
  | x >= 0    = encodeUnsignedDec x buf
  | otherwise = do
    -- we cannot negate directly as  0 - (minBound :: Int) = minBound
    -- So we write the sign and the first digit.
    pokeByteOff buf 0 '-'
    let !(q,r) = quotRem x (-10)
    putDigit buf 1 (fromIntegral (abs r))
    case q of
      0 -> pure (plusPtr buf 2)
      _ -> encodeUnsignedDec' q (plusPtr buf 1) (plusPtr buf 2)


-- | Encode positive number as decimal
encodeUnsignedDec :: (Eq a, Num a, Integral a) => a -> Ptr Word8 -> IO (Ptr Word8)
{-# INLINABLE encodeUnsignedDec #-} -- for specialization
encodeUnsignedDec !v !next_ptr = encodeUnsignedDec' v next_ptr next_ptr

-- | Encode positive number as little-endian decimal, then reverse it.
--
-- Take two pointers (orig_ptr, next_ptr) to support already encoded digits
-- (e.g. used by encodeSignedDec to avoid overflows)
--
encodeUnsignedDec' :: (Eq a, Num a, Integral a) => a -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
{-# INLINABLE encodeUnsignedDec' #-} -- for specialization
encodeUnsignedDec' !v !orig_ptr !next_ptr = do
  let !(q,r) = divMod v 10
  putDigit next_ptr 0 (fromIntegral r)
  case q of
    0 -> do
      -- reverse written digits
      reverseBytesInplace orig_ptr next_ptr
      -- return pointer after our digits
      pure (plusPtr next_ptr 1)
    _ -> encodeUnsignedDec' q orig_ptr (plusPtr next_ptr 1)

encodeUnsignedDecPadded :: (Eq a, Num a, Integral a) => Int -> a -> Ptr Word8 -> IO ()
{-# INLINABLE encodeUnsignedDecPadded #-} -- for specialization
encodeUnsignedDecPadded !max_width !v !buf = assert (max_width > 0) $ do
  let !(q,r) = divMod v 10
  putDigit buf (max_width - 1) (fromIntegral r)
  case q of
    0 -> do
        -- pad beginning
        let pad 0 = pure ()
            pad n = putDigit buf (n - 1) 0 >> pad (n - 1)
        pad (max_width - 1)
    _ -> encodeUnsignedDecPadded (max_width - 1) q buf



-- | Encode positive number as hexadecimal
encodeUnsignedHex :: (Eq a, Num a, Integral a, Bits a) => a -> Ptr Word8 -> IO (Ptr Word8)
{-# INLINABLE encodeUnsignedHex #-} -- for specialization
encodeUnsignedHex !v !next_ptr = encodeUnsignedHex' v next_ptr next_ptr

-- | Encode positive number as little-endian hexdecimal, then reverse it.
--
-- Take two pointers (orig_ptr, next_ptr) to support already encoded digits
encodeUnsignedHex' :: (Eq a, Num a, Integral a, Bits a) => a -> Ptr Word8 -> Ptr Word8 -> IO (Ptr Word8)
{-# INLINABLE encodeUnsignedHex' #-} -- for specialization
encodeUnsignedHex' !v !orig_ptr !next_ptr = do
  -- (q,r) = divMod v 16, but faster
  let !q = v `shiftR` 4
  let !r = v .&. 0x0F
  putDigit next_ptr 0 (fromIntegral r)
  case q of
    0 -> do
      -- reverse written digits
      reverseBytesInplace orig_ptr next_ptr
      -- return pointer after our digits
      pure (plusPtr next_ptr 1)
    _ -> encodeUnsignedHex' q orig_ptr (plusPtr next_ptr 1)


lower_hex_table :: Ptr Word16
lower_hex_table = Ptr
  "000102030405060708090a0b0c0d0e0f\
  \101112131415161718191a1b1c1d1e1f\
  \202122232425262728292a2b2c2d2e2f\
  \303132333435363738393a3b3c3d3e3f\
  \404142434445464748494a4b4c4d4e4f\
  \505152535455565758595a5b5c5d5e5f\
  \606162636465666768696a6b6c6d6e6f\
  \707172737475767778797a7b7c7d7e7f\
  \808182838485868788898a8b8c8d8e8f\
  \909192939495969798999a9b9c9d9e9f\
  \a0a1a2a3a4a5a6a7a8a9aaabacadaeaf\
  \b0b1b2b3b4b5b6b7b8b9babbbcbdbebf\
  \c0c1c2c3c4c5c6c7c8c9cacbcccdcecf\
  \d0d1d2d3d4d5d6d7d8d9dadbdcdddedf\
  \e0e1e2e3e4e5e6e7e8e9eaebecedeeef\
  \f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"#

digit_pairs_table :: Ptr Word16
digit_pairs_table = Ptr
  "00010203040506070809\
  \10111213141516171819\
  \20212223242526272829\
  \30313233343536373839\
  \40414243444546474849\
  \50515253545556575859\
  \60616263646566676869\
  \70717273747576777879\
  \80818283848586878889\
  \90919293949596979899"#
