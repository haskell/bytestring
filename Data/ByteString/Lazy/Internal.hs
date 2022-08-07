{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Lazy.Internal
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- A module containing semi-public 'ByteString' internals. This exposes
-- the 'ByteString' representation and low level construction functions.
-- Modules which extend the 'ByteString' system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.ByteString.Lazy.Internal (

        -- * The lazy @ByteString@ type and representation
        ByteString(..),
        LazyByteString,
        chunk,
        foldrChunks,
        foldlChunks,

        -- * Data type invariant and abstraction function
        invariant,
        checkInvariant,

        -- * Chunk allocation sizes
        defaultChunkSize,
        smallChunkSize,
        chunkOverhead,

        -- * Conversion with lists: packing and unpacking
        packBytes, packChars,
        unpackBytes, unpackChars,
        -- * Conversions with strict ByteString
        fromStrict, toStrict,

  ) where

import Prelude hiding (concat)

import qualified Data.ByteString.Internal as S

import Data.Word (Word8)
import Foreign.Storable (Storable(sizeOf))

#if MIN_VERSION_base(4,13,0)
import Data.Semigroup   (Semigroup (sconcat, stimes))
#else
import Data.Semigroup   (Semigroup ((<>), sconcat, stimes))
#endif
import Data.List.NonEmpty (NonEmpty ((:|)))
import Control.DeepSeq  (NFData, rnf)

import Data.String      (IsString(..))

import Data.Typeable            (Typeable)
import Data.Data                (Data(..), mkNoRepType)

import GHC.Exts                 (IsList(..))

import qualified Language.Haskell.TH.Syntax as TH

-- | A space-efficient representation of a 'Word8' vector, supporting many
-- efficient operations.
--
-- A lazy 'ByteString' contains 8-bit bytes, or by using the operations
-- from "Data.ByteString.Lazy.Char8" it can be interpreted as containing
-- 8-bit characters.
--
data ByteString = Empty | Chunk {-# UNPACK #-} !S.ByteString ByteString
    deriving (Typeable, TH.Lift)
-- See 'invariant' function later in this module for internal invariants.

-- | Type synonym for the lazy flavour of 'ByteString'.
--
-- @since 0.11.2.0
type LazyByteString = ByteString

instance Eq  ByteString where
    (==)    = eq

instance Ord ByteString where
    compare = cmp

instance Semigroup ByteString where
    (<>)    = append
    sconcat (b:|bs) = concat (b:bs)
    stimes  = times

instance Monoid ByteString where
    mempty  = Empty
    mappend = (<>)
    mconcat = concat

instance NFData ByteString where
    rnf Empty       = ()
    rnf (Chunk _ b) = rnf b

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackChars ps) r

instance Read ByteString where
    readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

-- | @since 0.10.12.0
instance IsList ByteString where
  type Item ByteString = Word8
  fromList = packBytes
  toList   = unpackBytes

-- | Beware: 'fromString' truncates multi-byte characters to octets.
-- e.g. "枯朶に烏のとまりけり秋の暮" becomes �6k�nh~�Q��n�
instance IsString ByteString where
    fromString = packChars

instance Data ByteString where
  gfoldl f z txt = z packBytes `f` unpackBytes txt
  toConstr _     = error "Data.ByteString.Lazy.ByteString.toConstr"
  gunfold _ _    = error "Data.ByteString.Lazy.ByteString.gunfold"
  dataTypeOf _   = mkNoRepType "Data.ByteString.Lazy.ByteString"

------------------------------------------------------------------------
-- Packing and unpacking from lists

packBytes :: [Word8] -> ByteString
packBytes cs0 =
    packChunks 32 cs0
  where
    packChunks n cs = case S.packUptoLenBytes n cs of
      (bs, [])  -> chunk bs Empty
      (bs, cs') -> Chunk bs (packChunks (min (n * 2) smallChunkSize) cs')

packChars :: [Char] -> ByteString
packChars cs0 = packChunks 32 cs0
  where
    packChunks n cs = case S.packUptoLenChars n cs of
      (bs, [])  -> chunk bs Empty
      (bs, cs') -> Chunk bs (packChunks (min (n * 2) smallChunkSize) cs')

unpackBytes :: ByteString -> [Word8]
unpackBytes Empty        = []
unpackBytes (Chunk c cs) = S.unpackAppendBytesLazy c (unpackBytes cs)

unpackChars :: ByteString -> [Char]
unpackChars Empty        = []
unpackChars (Chunk c cs) = S.unpackAppendCharsLazy c (unpackChars cs)

------------------------------------------------------------------------

-- | The data type invariant:
-- Every ByteString is either 'Empty' or consists of non-null 'S.ByteString's.
-- All functions must preserve this, and the QC properties must check this.
--
invariant :: ByteString -> Bool
invariant Empty                     = True
invariant (Chunk (S.BS _ len) cs) = len > 0 && invariant cs

-- | In a form that checks the invariant lazily.
checkInvariant :: ByteString -> ByteString
checkInvariant Empty = Empty
checkInvariant (Chunk c@(S.BS _ len) cs)
    | len > 0   = Chunk c (checkInvariant cs)
    | otherwise = error $ "Data.ByteString.Lazy: invariant violation:"
               ++ show (Chunk c cs)

------------------------------------------------------------------------

-- | Smart constructor for 'Chunk'. Guarantees the data type invariant.
chunk :: S.ByteString -> ByteString -> ByteString
chunk c@(S.BS _ len) cs | len == 0  = cs
                        | otherwise = Chunk c cs
{-# INLINE chunk #-}

-- | Consume the chunks of a lazy ByteString with a natural right fold.
foldrChunks :: (S.ByteString -> a -> a) -> a -> ByteString -> a
foldrChunks f z = go
  where go Empty        = z
        go (Chunk c cs) = f c (go cs)
{-# INLINE foldrChunks #-}

-- | Consume the chunks of a lazy ByteString with a strict, tail-recursive,
-- accumulating left fold.
foldlChunks :: (a -> S.ByteString -> a) -> a -> ByteString -> a
foldlChunks f = go
  where go !a Empty        = a
        go !a (Chunk c cs) = go (f a c) cs
{-# INLINE foldlChunks #-}

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

-- | The chunk size used for I\/O. Currently set to 32k, less the memory management overhead
defaultChunkSize :: Int
defaultChunkSize = 32 * k - chunkOverhead
   where k = 1024

-- | The recommended chunk size. Currently set to 4k, less the memory management overhead
smallChunkSize :: Int
smallChunkSize = 4 * k - chunkOverhead
   where k = 1024

-- | The memory management overhead. Currently this is tuned for GHC only.
chunkOverhead :: Int
chunkOverhead = 2 * sizeOf (undefined :: Int)

------------------------------------------------------------------------
-- Implementations for Eq, Ord and Monoid instances

eq :: ByteString -> ByteString -> Bool
eq Empty Empty = True
eq Empty _     = False
eq _     Empty = False
eq (Chunk a@(S.BS ap al) as) (Chunk b@(S.BS bp bl) bs) =
  case compare al bl of
    LT -> a == S.BS bp al && eq as (Chunk (S.BS (S.plusForeignPtr bp al) (bl - al)) bs)
    EQ -> a == b && eq as bs
    GT -> S.BS ap bl == b && eq (Chunk (S.BS (S.plusForeignPtr ap bl) (al - bl)) as) bs

cmp :: ByteString -> ByteString -> Ordering
cmp Empty Empty = EQ
cmp Empty _     = LT
cmp _     Empty = GT
cmp (Chunk a@(S.BS ap al) as) (Chunk b@(S.BS bp bl) bs) =
  case compare al bl of
    LT -> case compare a (S.BS bp al) of
            EQ     -> cmp as (Chunk (S.BS (S.plusForeignPtr bp al) (bl - al)) bs)
            result -> result
    EQ -> case compare a b of
            EQ     -> cmp as bs
            result -> result
    GT -> case compare (S.BS ap bl) b of
            EQ     -> cmp (Chunk (S.BS (S.plusForeignPtr ap bl) (al - bl)) as) bs
            result -> result

append :: ByteString -> ByteString -> ByteString
append xs ys = foldrChunks Chunk ys xs

concat :: [ByteString] -> ByteString
concat = to
  where
    go Empty        css = to css
    go (Chunk c cs) css = Chunk c (go cs css)
    to []               = Empty
    to (cs:css)         = go cs css

-- | Repeats the given ByteString n times.
times :: Integral a => a -> ByteString -> ByteString
times 0 _ = Empty
times n lbs0
  | n < 0 = error "stimes: non-negative multiplier expected"
  | otherwise = case lbs0 of
    Empty -> Empty
    Chunk bs lbs -> Chunk bs (go lbs)
  where
    go Empty = times (n-1) lbs0
    go (Chunk c cs) = Chunk c (go cs)

------------------------------------------------------------------------
-- Conversions

-- |/O(1)/ Convert a strict 'ByteString' into a lazy 'ByteString'.
fromStrict :: S.ByteString -> ByteString
fromStrict (S.BS _ 0) = Empty
fromStrict bs = Chunk bs Empty

-- |/O(n)/ Convert a lazy 'ByteString' into a strict 'ByteString'.
--
-- Note that this is an /expensive/ operation that forces the whole lazy
-- ByteString into memory and then copies all the data. If possible, try to
-- avoid converting back and forth between strict and lazy bytestrings.
--
toStrict :: ByteString -> S.ByteString
toStrict = \cs -> goLen0 cs cs
    -- We pass the original [ByteString] (bss0) through as an argument through
    -- goLen0, goLen1, and goLen since we will need it again in goCopy. Passing
    -- it as an explicit argument avoids capturing it in these functions'
    -- closures which would result in unnecessary closure allocation.
  where
    -- It's still possible that the result is empty
    goLen0 _   Empty                 = S.BS S.nullForeignPtr 0
    goLen0 cs0 (Chunk (S.BS _ 0) cs) = goLen0 cs0 cs
    goLen0 cs0 (Chunk c cs)          = goLen1 cs0 c cs

    -- It's still possible that the result is a single chunk
    goLen1 _   bs Empty = bs
    goLen1 cs0 bs (Chunk (S.BS _ 0) cs) = goLen1 cs0 bs cs
    goLen1 cs0 (S.BS _ bl) (Chunk (S.BS _ cl) cs) =
        goLen cs0 (S.checkedAdd "Lazy.toStrict" bl cl) cs

    -- General case, just find the total length we'll need
    goLen cs0 !total (Chunk (S.BS _ cl) cs) =
      goLen cs0 (S.checkedAdd "Lazy.toStrict" total cl) cs
    goLen cs0 total Empty =
      S.unsafeCreatef total $ \ptr -> goCopy cs0 ptr

    -- Copy the data
    goCopy Empty                    !_   = return ()
    goCopy (Chunk (S.BS _  0  ) cs) !ptr = goCopy cs ptr
    goCopy (Chunk (S.BS fp len) cs) !ptr = do
      S.memcpyf ptr fp len
      goCopy cs (ptr `S.plusForeignPtr` len)
-- See the comment on Data.ByteString.Internal.concat for some background on
-- this implementation.
