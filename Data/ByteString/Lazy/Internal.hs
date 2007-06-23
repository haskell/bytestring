{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-- |
-- Module      : Data.ByteString.Lazy.Internal
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au, duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
-- 
-- A module containing semi-public 'ByteString' internals. This exposes
-- the 'ByteString' representation and low level construction functions.
-- Modules which extend the 'ByteString' system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.ByteString.Lazy.Internal (

        -- * The lazy @ByteString@ type and representation
        ByteString(..),     -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Data type invariant and abstraction function
        invariant,
        checkInvariant,
        abstr,

        -- * Chunk allocation sizes
        defaultChunkSize,
        smallChunkSize,
        chunkOverhead

  ) where

import qualified Data.ByteString as S
import qualified Data.List       as L

import Foreign.Storable (sizeOf)

#if defined(__GLASGOW_HASKELL__)
import Data.Generics            (Data(..), Typeable(..))
#endif

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
newtype ByteString = LPS { unLPS :: [S.ByteString] } -- LPS for lazy packed string
    deriving (Show, Read
#if defined(__GLASGOW_HASKELL__)
                        ,Data, Typeable
#endif
             )
--
-- hmm, what about getting the PS constructor unpacked into the cons cell?
--
-- data List = Nil | Cons {-# UNPACK #-} !S.ByteString List
--
-- Would avoid one indirection per chunk.
--

------------------------------------------------------------------------

-- | The data type invariant:
-- Every ByteString is either empty or consists of non-null ByteStrings.
-- All functions must preserve this, and the QC properties must check this.
--
invariant :: ByteString -> Bool
invariant (LPS []) = True
invariant (LPS xs) = L.all (not . S.null) xs

-- | In a form useful for QC testing
checkInvariant :: ByteString -> ByteString
checkInvariant lps
    | invariant lps = lps
    | otherwise     = error ("Data.ByteString.Lazy: invariant violation:" ++ show lps)

-- | The data abstraction function
--
abstr :: ByteString -> S.ByteString
abstr (LPS []) = S.empty
abstr (LPS xs) = S.concat xs

------------------------------------------------------------------------

-- The representation uses lists of packed chunks. When we have to convert from
-- a lazy list to the chunked representation, then by default we use this
-- chunk size. Some functions give you more control over the chunk size.
--
-- Measurements here:
--  http://www.cse.unsw.edu.au/~dons/tmp/chunksize_v_cache.png
--
-- indicate that a value around 0.5 to 1 x your L2 cache is best.
-- The following value assumes people have something greater than 128k,
-- and need to share the cache with other programs.

-- | Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

-- | Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
   where k = 1024

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)

------------------------------------------------------------------------

instance Eq  ByteString
    where (==)    = eq

instance Ord ByteString
    where compare = compareBytes

eq :: ByteString -> ByteString -> Bool
eq (LPS xs) (LPS ys) = eq' xs ys
  where eq' [] [] = True
        eq' [] _  = False
        eq' _  [] = False
        eq' (a:as) (b:bs) =
          case compare (S.length a) (S.length b) of
            LT -> a == (S.take (S.length a) b) && eq' as (S.drop (S.length a) b : bs)
            EQ -> a == b                       && eq' as bs
            GT -> (S.take (S.length b) a) == b && eq' (S.drop (S.length b) a : as) bs

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (LPS xs) (LPS ys) = cmp xs ys
  where cmp [] [] = EQ
        cmp [] _  = LT
        cmp _  [] = GT
        cmp (a:as) (b:bs) =
          case compare (S.length a) (S.length b) of
            LT -> case compare a (S.take (S.length a) b) of
                    EQ     -> cmp as (S.drop (S.length a) b : bs)
                    result -> result
            EQ -> case compare a b of
                    EQ     -> cmp as bs
                    result -> result
            GT -> case compare (S.take (S.length b) a) b of
                    EQ     -> cmp (S.drop (S.length b) a : as) bs
                    result -> result
