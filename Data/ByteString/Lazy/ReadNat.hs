{-# LANGUAGE CPP #-}

-- This file is included by "Data.ByteString.ReadInt", after defining
-- "BYTESTRING_STRICT".  The two modules are largely identical, except for the
-- choice of ByteString type and the loops in `readNatural`, where the lazy
-- version needs to nest the inner loop inside a loop over the constituent
-- chunks.

#ifdef BYTESTRING_STRICT
module Data.ByteString.ReadNat
#else
module Data.ByteString.Lazy.ReadNat
#endif
    ( readInteger
    , readNatural
    ) where

import qualified Data.ByteString.Internal as BI
#ifdef BYTESTRING_STRICT
import Data.ByteString
#else
import Data.ByteString.Lazy
import Data.ByteString.Lazy.Internal
#endif
import Data.Bits (finiteBitSize)
import Data.ByteString.Internal (pattern BS, plusForeignPtr)
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Numeric.Natural (Natural)

----- Public API

-- | 'readInteger' reads an 'Integer' from the beginning of the 'ByteString'.
-- If there is no 'Integer' at the beginning of the string, it returns
-- 'Nothing', otherwise it just returns the 'Integer' read, and the rest of
-- the string.
--
-- 'readInteger' does not ignore leading whitespace, the value must start
-- immediately at the beginning of the input string.
--
-- ==== __Examples__
-- >>> readInteger "-000111222333444555666777888999 all done"
-- Just (-111222333444555666777888999," all done")
-- >>> readInteger "+1: readInteger also accepts a leading '+'"
-- Just (1, ": readInteger also accepts a leading '+'")
-- >>> readInteger "not a decimal number"
-- Nothing
--
readInteger :: ByteString -> Maybe (Integer, ByteString)
readInteger = \ bs -> do
    (w, s) <- uncons bs
    let d = fromDigit w
    if | d <=    9 -> unsigned d s -- leading digit
       | w == 0x2d -> negative s   -- minus sign
       | w == 0x2b -> positive s   -- plus sign
       | otherwise -> Nothing      -- not a number
  where
    unsigned :: Word -> ByteString -> Maybe (Integer, ByteString)
    unsigned d s =
         let (!n, rest) = _readDecimal d s
             !i = toInteger n
          in Just (i, rest)

    positive :: ByteString -> Maybe (Integer, ByteString)
    positive bs = do
        (w, s) <- uncons bs
        let d = fromDigit w
        if | d <=    9 -> unsigned d s
           | otherwise -> Nothing

    negative :: ByteString -> Maybe (Integer, ByteString)
    negative bs = do
        (w, s) <- uncons bs
        let d = fromDigit w
        if | d >     9 -> Nothing
           | otherwise -> let (n, rest) = _readDecimal d s
                              !i = negate $ toInteger n
                           in Just (i, rest)

-- | 'readNatural' reads a 'Natural' number from the beginning of the
-- 'ByteString'.  If there is no 'Natural' number at the beginning of the
-- string, it returns 'Nothing', otherwise it just returns the number read, and
-- the rest of the string.
--
-- 'readNatural' does not ignore leading whitespace, the value must start with
-- a decimal digit immediately at the beginning of the input string.  Leading
-- @+@ signs are not accepted.
--
-- ==== __Examples__
-- >>> readNatural "000111222333444555666777888999 all done"
-- Just (111222333444555666777888999," all done")
-- >>> readNatural "+000111222333444555666777888999 explicit sign"
-- Nothing
-- >>> readNatural "not a decimal number"
-- Nothing
--
readNatural :: ByteString -> Maybe (Natural, ByteString)
readNatural bs = do
    (w, s) <- uncons bs
    let d = fromDigit w
    if | d <=    9 -> Just $! _readDecimal d s
       | otherwise -> Nothing

----- Internal implementation

-- | Intermediate result from scanning a chunk, final output is
-- obtained via `convert` after all the chunks are processed.
--
data Result = Result !Int      -- Bytes consumed
                     !Word     -- Value of LSW
                     !Int      -- Digits in LSW
                     [Natural] -- Little endian MSW list

_readDecimal :: Word -> ByteString -> (Natural, ByteString)
_readDecimal =
    -- Having read one digit, we're about to read the 2nd So the digit count
    -- up to 'safeLog' starts at 2.
    consume [] 2
  where
    consume :: [Natural] -> Int -> Word -> ByteString
            -> (Natural, ByteString)
#ifdef BYTESTRING_STRICT
    consume ns cnt acc (BS fp len) =
        -- Having read one digit, we're about to read the 2nd
        -- So the digit count up to 'safeLog' starts at 2.
        case natdigits fp len acc cnt ns of
            Result used acc' cnt' ns'
                | used == len
                  -> convert acc' cnt' ns' $ empty
                | otherwise
                  -> convert acc' cnt' ns' $
                     BS (fp `plusForeignPtr` used) (len - used)
#else
    -- All done
    consume ns cnt acc Empty = convert acc cnt ns Empty
    -- Process next chunk
    consume ns cnt acc (Chunk (BS fp len) cs)
        = case natdigits fp len acc cnt ns of
            Result used acc' cnt' ns'
                | used == len -- process more chunks
                  -> consume ns' cnt' acc' cs
                | otherwise   -- ran into a non-digit
                  -> let c = Chunk (BS (fp `plusForeignPtr` used) (len - used)) cs
                      in convert acc' cnt' ns' c
#endif
    convert !acc !cnt !ns rest =
        let !n = combine acc cnt ns
         in (n, rest)

    -- | Merge least-significant word with reduction of of little-endian tail.
    --
    -- The input is:
    --
    -- * Least significant digits as a 'Word' (LSW)
    -- * The number of digits that went into the LSW
    -- * All the remaining digit groups ('safeLog' digits each),
    --   in little-endian order
    --
    -- The result is obtained by pairwise recursive combining of all the
    -- full size digit groups, followed by multiplication by @10^cnt@ and
    -- addition of the LSW.
    combine :: Word      -- ^ value of LSW
            -> Int       -- ^ count of digits in LSW
            -> [Natural] -- ^ tail elements (base @10^'safeLog'@)
            -> Natural
    {-# INLINE combine #-}
    combine !acc !_   [] = wordToNatural acc
    combine !acc !cnt ns =
        wordToNatural (10^cnt) * combine1 safeBase ns + wordToNatural acc

    -- | Recursive reduction of little-endian sequence of 'Natural'-valued
    -- /digits/ in base @base@ (a power of 10).  The base is squared after
    -- each round.  This shows better asymptotic performance than one word
    -- at a time multiply-add folds.  See:
    -- <https://gmplib.org/manual/Multiplication-Algorithms>
    --
    combine1 :: Natural -> [Natural] -> Natural
    combine1 _    [n] = n
    combine1 base ns  = combine1 (base * base) (combine2 base ns)

    -- | One round pairwise merge of numbers in base @base@.
    combine2 :: Natural -> [Natural] -> [Natural]
    combine2 base (n:m:ns) = let !t = m * base + n in t : combine2 base ns
    combine2 _    ns       = ns

-- The intermediate representation is a little-endian sequence in base
-- @10^'safeLog'@, prefixed by an initial element in base @10^cnt@ for some
-- @cnt@ between 1 and 'safeLog'.  The final result is obtained by recursive
-- pairwise merging of the tail followed by a final multiplication by @10^cnt@
-- and addition of the head.
--
natdigits :: ForeignPtr Word8 -- ^ Input chunk
          -> Int              -- ^ Chunk length
          -> Word             -- ^ accumulated element
          -> Int              -- ^ partial digit count
          -> [Natural]        -- ^ accumulated MSB elements
          -> Result
{-# INLINE natdigits #-}
natdigits fp len = \ acc cnt ns ->
    BI.accursedUnutterablePerformIO $
        BI.unsafeWithForeignPtr fp $ \ ptr -> do
            let end = ptr `plusPtr` len
            go ptr end acc cnt ns ptr
  where
    go !start !end = loop
      where
        loop :: Word -> Int -> [Natural] -> Ptr Word8 -> IO Result
        loop !acc !cnt ns !ptr = getDigit >>= \ !d ->
            if | d > 9
                 -> return $ Result (ptr `minusPtr` start) acc cnt ns
               | cnt < safeLog
                 -> loop (10*acc + d) (cnt+1) ns $ ptr `plusPtr` 1
               | otherwise
                 -> let !acc' = wordToNatural acc
                     in loop d 1 (acc' : ns) $ ptr `plusPtr` 1
          where
            getDigit | ptr /= end = fromDigit <$> peek ptr
                     | otherwise  = pure 10  -- End of input
            {-# NOINLINE getDigit #-}
            -- 'getDigit' makes it possible to implement a single success
            -- exit point from the loop.  If instead we return 'Result'
            -- from multiple places, when 'natdigits' is inlined we get (at
            -- least GHC 8.10 through 9.2) for each exit path a separate
            -- join point implementing the continuation code.  GHC ticket
            -- <https://gitlab.haskell.org/ghc/ghc/-/issues/20739>.
            --
            -- The NOINLINE pragma is required to avoid inlining branches
            -- that would restore multiple exit points.

----- Misc functions

-- | Largest decimal digit count that never overflows the accumulator
-- The base 10 logarithm of 2 is ~0.30103, therefore 2^n has at least
-- @1 + floor (0.3 n)@ decimal digits.  Therefore @floor (0.3 n)@,
-- digits cannot overflow the upper bound of an @n-bit@ word.
--
safeLog :: Int
safeLog = 3 * finiteBitSize @Word 0 `div` 10

-- | 10-power base for little-endian sequence of ~Word-sized "digits"
safeBase :: Natural
safeBase = 10 ^ safeLog

fromDigit :: Word8 -> Word
{-# INLINE fromDigit #-}
fromDigit = \ !w -> fromIntegral w - 0x30 -- i.e. w - '0'

wordToNatural :: Word -> Natural
{-# INLINE wordToNatural #-}
wordToNatural  = fromIntegral
