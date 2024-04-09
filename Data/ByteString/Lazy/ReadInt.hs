{-# LANGUAGE CPP #-}

-- This file is also included by "Data.ByteString.ReadInt", after defining
-- "BYTESTRING_STRICT".  The two modules share much of their code, but
-- the lazy version adds an outer loop over the chunks.

#ifdef BYTESTRING_STRICT
module Data.ByteString.ReadInt
#else
module Data.ByteString.Lazy.ReadInt
#endif
    ( readInt
    , readInt8
    , readInt16
    , readInt32
    , readWord
    , readWord8
    , readWord16
    , readWord32
    , readInt64
    , readWord64
    ) where

import qualified Data.ByteString.Internal as BI
#ifdef BYTESTRING_STRICT
import Data.ByteString
#else
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Internal
#endif
import Data.Bits (FiniteBits, isSigned)
import Data.ByteString.Internal (pattern BS, plusForeignPtr)
import Data.Int
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (Storable(..))

----- Public API

-- | Try to read a signed 'Int' value from the 'ByteString', returning
-- @Just (val, str)@ on success, where @val@ is the value read and @str@ is the
-- rest of the input string.  If the sequence of digits decodes to a value
-- larger than can be represented by an 'Int', the returned value will be
-- 'Nothing'.
--
-- 'readInt' does not ignore leading whitespace, the value must start
-- immediately at the beginning of the input string.
--
-- ==== __Examples__
-- >>> readInt "-1729 sum of cubes"
-- Just (-1729," sum of cubes")
-- >>> readInt "+1: readInt also accepts a leading '+'"
-- Just (1, ": readInt also accepts a leading '+'")
-- >>> readInt "not a decimal number"
-- Nothing
-- >>> readInt "12345678901234567890 overflows maxBound"
-- Nothing
-- >>> readInt "-12345678901234567890 underflows minBound"
-- Nothing
--
readInt :: ByteString -> Maybe (Int, ByteString)
readInt = _read

-- | A variant of 'readInt' specialised to 'Int32'.
readInt32 :: ByteString -> Maybe (Int32, ByteString)
readInt32 = _read

-- | A variant of 'readInt' specialised to 'Int16'.
readInt16 :: ByteString -> Maybe (Int16, ByteString)
readInt16 = _read

-- | A variant of 'readInt' specialised to 'Int8'.
readInt8 :: ByteString -> Maybe (Int8, ByteString)
readInt8 = _read

-- | Try to read a 'Word' value from the 'ByteString', returning
-- @Just (val, str)@ on success, where @val@ is the value read and @str@ is the
-- rest of the input string.  If the sequence of digits decodes to a value
-- larger than can be represented by a 'Word', the returned value will be
-- 'Nothing'.
--
-- 'readWord' does not ignore leading whitespace, the value must start with a
-- decimal digit immediately at the beginning of the input string.  Leading @+@
-- signs are not accepted.
--
-- ==== __Examples__
-- >>> readWord "1729 sum of cubes"
-- Just (1729," sum of cubes")
-- >>> readWord "+1729 has an explicit sign"
-- Nothing
-- >>> readWord "not a decimal number"
-- Nothing
-- >>> readWord "98765432109876543210 overflows maxBound"
-- Nothing
--
readWord :: ByteString -> Maybe (Word, ByteString)
readWord = _read

-- | A variant of 'readWord' specialised to 'Word32'.
readWord32 :: ByteString -> Maybe (Word32, ByteString)
readWord32 = _read

-- | A variant of 'readWord' specialised to 'Word16'.
readWord16 :: ByteString -> Maybe (Word16, ByteString)
readWord16 = _read

-- | A variant of 'readWord' specialised to 'Word8'.
readWord8 :: ByteString -> Maybe (Word8, ByteString)
readWord8 = _read

-- | A variant of 'readInt' specialised to 'Int64'.
readInt64 :: ByteString -> Maybe (Int64, ByteString)
readInt64 = _read

-- | A variant of 'readWord' specialised to 'Word64'.
readWord64 :: ByteString -> Maybe (Word64, ByteString)
readWord64 = _read

-- | Polymorphic Int*/Word* reader
_read :: forall a. (Integral a, FiniteBits a, Bounded a)
      => ByteString  -> Maybe (a, ByteString)
{-# INLINE _read #-}
_read
    | isSigned @a 0
      = \ bs -> signed bs >>= \ (r, s, d1) -> _readDecimal r s d1
    | otherwise
      -- When the input is @16^n-1@, as is the case with 'maxBound' for
      -- all the Word* types, the last decimal digit of 'maxBound' is 5.
      = \ bs -> unsigned 5 bs >>= \ (r, s, d1) -> _readDecimal r s d1
  where
    -- Returns:
    --  * Mod 10 min/max bound remainder
    --  * 2nd and later digits
    --  * 1st digit
    --
    -- When the input is @8*16^n-1@, as is the case with 'maxBound' for
    -- all the Int* types, the last decimal digit of 'maxBound' is 7.
    --
    signed :: ByteString -> Maybe (Word64, ByteString, Word64)
    signed bs = do
        (w, s) <- uncons bs
        let d1 = fromDigit w
        if | d1 <= 9   -> Just (7, s, d1) -- leading digit
           | w == 0x2d -> unsigned 8 s    -- minus sign
           | w == 0x2b -> unsigned 7 s    -- plus sign
           | otherwise -> Nothing         -- not a number

    unsigned :: Word64 -> ByteString -> Maybe (Word64, ByteString, Word64)
    unsigned r bs = do
        (w, s) <- uncons bs
        let d1 = fromDigit w
        if | d1 <= 9   -> Just (r, s, d1) -- leading digit
           | otherwise -> Nothing         -- not a number

----- Fixed-width unsigned reader

-- | Intermediate result from scanning a chunk, final output is
-- converted to the requested type once all chunks are processed.
--
data Result = Overflow
            | Result !Int    -- number of bytes (digits) read
                     !Word64 -- accumulator value

_readDecimal :: forall a. (Integral a, Bounded a)
             => Word64     -- ^ abs(maxBound/minBound) `mod` 10
             -> ByteString -- ^ Input string
             -> Word64     -- ^ First digit value
             -> Maybe (a, ByteString)
{-# INLINE _readDecimal #-}
_readDecimal !r = consume
  where
    consume :: ByteString -> Word64 -> Maybe (a, ByteString)
#ifdef BYTESTRING_STRICT
    consume (BS fp len) a = case _digits q r fp len a of
        Result used acc
            | used == len
              -> convert acc empty
            | otherwise
              -> convert acc $ BS (fp `plusForeignPtr` used) (len - used)
        _   -> Nothing
#else
    -- All done
    consume Empty acc = convert acc Empty
    -- Process next chunk
    consume (Chunk (BS fp len) cs) acc
        = case _digits q r fp len acc of
            Result used acc'
                | used == len
                  -- process remaining chunks
                  -> consume cs acc'
                | otherwise
                  -- ran into a non-digit
                  -> convert acc' $
                     Chunk (BS (fp `plusForeignPtr` used) (len - used)) cs
            _     -> Nothing
#endif
    convert :: Word64 -> ByteString -> Maybe (a, ByteString)
    convert !acc rest =
        let !i = case r of
                -- minBound @Int* `mod` 10 == 8
                8 -> negate $ fromIntegral @Word64 @a acc
                _ -> fromIntegral @Word64 @a acc
         in Just (i, rest)

    -- The quotient of 'maxBound' divided by 10 is needed for
    -- overflow checks, once the accumulator exceeds this value
    -- no further digits can be added.  If equal, the last digit
    -- must not exceed the `r` value (max/min bound `mod` 10).
    --
    q = fromIntegral @a @Word64 maxBound `div` 10

----- Per chunk decoder

-- | Process as many digits as we can, returning the additional
-- number of digits found and the updated accumulator.  If the
-- accumulator would overflow return 'Overflow'.
--
_digits :: Word64           -- ^ maximum non-overflow value `div` 10
        -> Word64           -- ^ maximum non-overflow vavlue `mod` 10
        -> ForeignPtr Word8 -- ^ Input buffer
        -> Int              -- ^ Input length
        -> Word64           -- ^ Accumulated value of leading digits
        -> Result           -- ^ Bytes read and final accumulator,
                            -- or else overflow indication
{-# INLINE _digits #-}
_digits !q !r fp len a = BI.accursedUnutterablePerformIO $
    BI.unsafeWithForeignPtr fp $ \ ptr -> do
        let end = ptr `plusPtr` len
        go ptr end ptr a
  where
    go !start !end = loop
      where
        loop !ptr !acc = getDigit >>= \ !d ->
            if | d > 9
                 -> return $ Result (ptr `minusPtr` start) acc
               | acc < q || acc == q && d <= r
                 -> loop (ptr `plusPtr` 1) (acc * 10 + d)
               | otherwise
                 -> return Overflow
          where
            getDigit :: IO Word64
            getDigit
                | ptr /= end = fromDigit <$> peek ptr
                | otherwise  = pure 10  -- End of input
            {-# NOINLINE getDigit #-}
            -- 'getDigit' makes it possible to implement a single success
            -- exit point from the loop.  If instead we return 'Result'
            -- from multiple places, when '_digits' is inlined we get (at
            -- least GHC 8.10 through 9.2) for each exit path a separate
            -- join point implementing the continuation code.  GHC ticket
            -- <https://gitlab.haskell.org/ghc/ghc/-/issues/20739>.
            --
            -- The NOINLINE pragma is required to avoid inlining branches
            -- that would restore multiple exit points.

fromDigit :: Word8 -> Word64
{-# INLINE fromDigit #-}
fromDigit = \ !w -> fromIntegral w - 0x30 -- i.e. w - '0'
