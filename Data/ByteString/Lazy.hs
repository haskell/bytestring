{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Data.ByteString.Lazy
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time and space-efficient implementation of lazy byte vectors
-- using lists of packed 'Word8' arrays, suitable for high performance
-- use, both in terms of large data quantities, or high speed
-- requirements. Lazy ByteStrings are encoded as lazy lists of strict chunks
-- of bytes.
--
-- A key feature of lazy ByteStrings is the means to manipulate large or
-- unbounded streams of data without requiring the entire sequence to be
-- resident in memory. To take advantage of this you have to write your
-- functions in a lazy streaming style, e.g. classic pipeline composition. The
-- default I\/O chunk size is 32k, which should be good in most circumstances.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons', have
-- better complexity than their "Data.ByteString" equivalents, due to
-- optimisations resulting from the list spine structure. For other
-- operations lazy ByteStrings are usually within a few percent of
-- strict ones.
--
-- The recomended way to assemble lazy ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'Foreign.ForeignPtr.ForeignPtr'
-- by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
-- Lazy variant by Duncan Coutts and Don Stewart.
--

module Data.ByteString.Lazy (

        -- * Lazy @ByteString@
        ByteString,
        LazyByteString,

        -- * Introducing and eliminating 'ByteString's
        empty,
        singleton,
        pack,
        unpack,
        fromStrict,
        toStrict,
        fromChunks,
        toChunks,
        foldrChunks,
        foldlChunks,

        -- * Basic interface
        cons,
        cons',
        snoc,
        append,
        head,
        uncons,
        unsnoc,
        last,
        tail,
        init,
        null,
        length,

        -- * Transforming ByteStrings
        map,
        reverse,
        intersperse,
        intercalate,
        transpose,

        -- * Reducing 'ByteString's (folds)
        foldl,
        foldl',
        foldl1,
        foldl1',
        foldr,
        foldr',
        foldr1,
        foldr1',

        -- ** Special folds
        concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,
        compareLength,

        -- * Building ByteStrings
        -- ** Scans
        scanl,
        scanl1,
        scanr,
        scanr1,

        -- ** Accumulating maps
        mapAccumL,
        mapAccumR,

        -- ** Infinite ByteStrings
        repeat,
        replicate,
        cycle,
        iterate,

        -- ** Unfolding ByteStrings
        unfoldr,

        -- * Substrings

        -- ** Breaking strings
        take,
        takeEnd,
        drop,
        dropEnd,
        splitAt,
        takeWhile,
        takeWhileEnd,
        dropWhile,
        dropWhileEnd,
        span,
        spanEnd,
        break,
        breakEnd,
        group,
        groupBy,
        inits,
        tails,
        initsNE,
        tailsNE,
        stripPrefix,
        stripSuffix,

        -- ** Breaking into many substrings
        split,
        splitWith,

        -- * Predicates
        isPrefixOf,
        isSuffixOf,
--        isInfixOf,

        -- ** Search for arbitrary substrings
--        isSubstringOf,

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,
        notElem,

        -- ** Searching with a predicate
        find,
        filter,
        partition,

        -- * Indexing ByteStrings
        index,
        indexMaybe,
        (!?),
        elemIndex,
        elemIndexEnd,
        elemIndices,
        findIndex,
        findIndexEnd,
        findIndices,
        count,

        -- * Zipping and unzipping ByteStrings
        zip,
        zipWith,
        packZipWith,
        unzip,

        -- * Ordered ByteStrings
--        sort,

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,
--        defrag,

        -- * I\/O with 'ByteString's
        -- $IOChunk

        -- ** Standard input and output
        getContents,
        putStr,
        interact,

        -- ** Files
        readFile,
        writeFile,
        appendFile,

        -- ** I\/O with Handles
        hGetContents,
        hGet,
        hGetNonBlocking,
        hPut,
        hPutNonBlocking,
        hPutStr,

  ) where

import Prelude hiding
    (reverse,head,tail,last,init,null,length,map,lines,foldl,foldr,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,elem,filter,maximum
    ,minimum,all,concatMap,foldl1,foldr1,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)

import qualified Data.List              as List
import qualified Data.List.NonEmpty     as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Bifunctor         as BF
import qualified Data.ByteString        as P  (ByteString) -- type name only
import qualified Data.ByteString        as S  -- S for strict (hmm...)
import qualified Data.ByteString.Internal.Type as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.ByteString.Lazy.Internal.Deque as D
import Data.ByteString.Lazy.Internal

import Control.Monad            (mplus)
import Data.Word                (Word8)
import Data.Int                 (Int64)
import GHC.Stack.Types          (HasCallStack)
import System.IO                (Handle,openBinaryFile,stdin,stdout,withBinaryFile,IOMode(..)
                                ,hClose)
import System.IO.Error          (mkIOError, illegalOperationErrorType)
import System.IO.Unsafe

import Foreign.Ptr
import Foreign.Storable


-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
singleton w = Chunk (S.singleton w) Empty
{-# INLINE singleton #-}

-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'.
pack :: [Word8] -> ByteString
pack = packBytes

-- | /O(n)/ Converts a 'ByteString' to a '[Word8]'.
unpack :: ByteString -> [Word8]
unpack = unpackBytes

-- | /O(c)/ Convert a list of strict 'ByteString' into a lazy 'ByteString'
fromChunks :: [P.ByteString] -> ByteString
fromChunks = List.foldr chunk Empty

-- | /O(c)/ Convert a lazy 'ByteString' into a list of strict 'ByteString'
toChunks :: ByteString -> [P.ByteString]
toChunks = foldrChunks (:) []

------------------------------------------------------------------------

{-
-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = LPS $ L.map (P.packWith k) (chunk defaultChunkSize str)
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith k (LPS ss) = L.concatMap (S.unpackWith k) ss
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}
-}

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null Empty = True
null _     = False
{-# INLINE null #-}

-- | /O(c)/ 'length' returns the length of a ByteString as an 'Int64'
length :: ByteString -> Int64
length = foldlChunks (\n c -> n + fromIntegral (S.length c)) 0
{-# INLINE [1] length #-}

infixr 5 `cons`, `cons'` --same as list (:)
infixl 5 `snoc`

-- | /O(1)/ 'cons' is analogous to '(Prelude.:)' for lists.
--
cons :: Word8 -> ByteString -> ByteString
cons c = Chunk (S.singleton c)
{-# INLINE cons #-}

-- | /O(1)/ Unlike 'cons', 'cons'' is
-- strict in the ByteString that we are consing onto. More precisely, it forces
-- the head and the first chunk. It does this because, for space efficiency, it
-- may coalesce the new byte onto the first \'chunk\' rather than starting a
-- new \'chunk\'.
--
-- So that means you can't use a lazy recursive contruction like this:
--
-- > let xs = cons' c xs in xs
--
-- You can however use 'cons', as well as 'repeat' and 'cycle', to build
-- infinite lazy ByteStrings.
--
cons' :: Word8 -> ByteString -> ByteString
cons' w (Chunk c cs) | S.length c < 16 = Chunk (S.cons w c) cs
cons' w cs                             = Chunk (S.singleton w) cs
{-# INLINE cons' #-}

-- | /O(n\/c)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc cs w = foldrChunks Chunk (singleton w) cs
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
--
-- This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => ByteString -> Word8
head Empty       = errorEmptyList "head"
head (Chunk c _) = S.unsafeHead c
{-# INLINE head #-}

-- | /O(1)/ Extract the 'head' and 'tail' of a ByteString, returning 'Nothing'
-- if it is empty.
uncons :: ByteString -> Maybe (Word8, ByteString)
uncons Empty = Nothing
uncons (Chunk c cs) = case S.length c of
  -- Don't move this test inside of the Just or (,).
  -- We don't want to allocate a thunk to put inside of the tuple!
  -- And if "let !tl = ... in Just (..., tl)" seems more appealing,
  -- remember that this function must remain lazy in cs.
  1 -> Just (S.unsafeHead c, cs)
  _ -> Just (S.unsafeHead c, Chunk (S.unsafeTail c) cs)
{-# INLINE uncons #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be
-- non-empty.
--
-- This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => ByteString -> ByteString
tail Empty          = errorEmptyList "tail"
tail (Chunk c cs)
  | S.length c == 1 = cs
  | otherwise       = Chunk (S.unsafeTail c) cs
{-# INLINE tail #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite
-- and non-empty.
--
-- This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => ByteString -> Word8
last Empty          = errorEmptyList "last"
last (Chunk c0 cs0) = go c0 cs0
  where go c Empty        = S.unsafeLast c
        go _ (Chunk c cs) = go c cs
-- XXX Don't inline this. Something breaks with 6.8.2 (haven't investigated yet)

-- | /O(n\/c)/ Returns all the elements of a 'ByteString' except the last one.
--
-- This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => ByteString -> ByteString
init Empty          = errorEmptyList "init"
init (Chunk c0 cs0) = go c0 cs0
  where go c Empty | S.length c == 1 = Empty
                   | otherwise       = Chunk (S.unsafeInit c) Empty
        go c (Chunk c' cs)           = Chunk c (go c' cs)

-- | /O(n\/c)/ Extract the 'init' and 'last' of a ByteString, returning 'Nothing'
-- if it is empty.
--
-- * It is no faster than using 'init' and 'last'
unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc Empty        = Nothing
unsnoc (Chunk c cs) = Just (init (Chunk c cs), last (Chunk c cs))

-- | /O(n\/c)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append = mappend
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> ByteString -> ByteString
map f = go
    where
        go Empty        = Empty
        go (Chunk x xs) = Chunk y ys
            where
                y  = S.map f x
                ys = go xs
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk c cs) = rev (Chunk (S.reverse c) a) cs
{-# INLINE reverse #-}

-- | The 'intersperse' function takes a 'Word8' and a 'ByteString' and
-- \`intersperses\' that byte between the elements of the 'ByteString'.
-- It is analogous to the intersperse function on Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse _ Empty        = Empty
intersperse w (Chunk c cs) = Chunk (S.intersperse w c)
                                   (foldrChunks (Chunk . intersperse') Empty cs)
  where intersperse' :: P.ByteString -> P.ByteString
        intersperse' (S.BS fp l) =
          S.unsafeCreateFp (2*l) $ \fp' ->
            S.unsafeWithForeignPtr fp' $ \p' ->
              S.unsafeWithForeignPtr fp $ \p -> do
                poke p' w
                S.c_intersperse (p' `plusPtr` 1) p (fromIntegral l) w

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose css = List.map (\ss -> Chunk (S.pack ss) Empty)
                      (List.transpose (List.map unpack css))
--TODO: make this fast

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f = go
  where go a Empty        = a
        go a (Chunk c cs) = go (S.foldl f a c) cs
{-# INLINE foldl #-}

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f = go
  where go !a Empty        = a
        go !a (Chunk c cs) = go (S.foldl' f a c) cs
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k = foldrChunks (flip (S.foldr k))
{-# INLINE foldr #-}

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
--
-- @since 0.11.2.0
foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' f a = go
  where
    go Empty = a
    go (Chunk c cs) = S.foldr' f (foldr' f a cs) c
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteString's.
foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 _ Empty        = errorEmptyList "foldl1"
foldl1 f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go v x xs = let v' = S.foldl f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' _ Empty        = errorEmptyList "foldl1'"
foldl1' f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go !v x xs = let v' = S.foldl' f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 _ Empty          = errorEmptyList "foldr1"
foldr1 f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1 f c
        go c (Chunk c' cs) = S.foldr  f (go c' cs) c

-- | 'foldr1'' is like 'foldr1', but strict in the accumulator.
--
-- @since 0.11.2.0
foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' _ Empty          = errorEmptyList "foldr1'"
foldr1' f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1' f c
        go c (Chunk c' cs) = S.foldr'  f (go c' cs) c

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat = mconcat

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap _ Empty        = Empty
concatMap f (Chunk c0 cs0) = to c0 cs0
  where
    go :: ByteString -> P.ByteString -> ByteString -> ByteString
    go Empty        c' cs' = to c' cs'
    go (Chunk c cs) c' cs' = Chunk c (go cs c' cs')

    to :: P.ByteString -> ByteString -> ByteString
    to c cs | S.null c  = case cs of
        Empty          -> Empty
        (Chunk c' cs') -> to c' cs'
            | otherwise = go (f (S.unsafeHead c)) (S.unsafeTail c) cs

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any f = foldrChunks (\c rest -> S.any f c || rest) False
{-# INLINE any #-}

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all f = foldrChunks (\c rest -> S.all f c && rest) True
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
maximum :: HasCallStack => ByteString -> Word8
maximum Empty        = errorEmptyList "maximum"
maximum (Chunk c cs) = foldlChunks (\n c' -> n `max` S.maximum c')
                                   (S.maximum c) cs
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
minimum :: HasCallStack => ByteString -> Word8
minimum Empty        = errorEmptyList "minimum"
minimum (Chunk c cs) = foldlChunks (\n c' -> n `min` S.minimum c')
                                     (S.minimum c) cs
{-# INLINE minimum #-}

-- | /O(c)/ 'compareLength' compares the length of a 'ByteString'
-- to an 'Int64'
--
-- @since 0.11.1.0
compareLength :: ByteString -> Int64 -> Ordering
compareLength _ toCmp | toCmp < 0 = GT
compareLength Empty toCmp         = compare 0 toCmp
compareLength (Chunk c cs) toCmp  = compareLength cs (toCmp - fromIntegral (S.length c))
{-# INLINE compareLength #-}

{-# RULES
"ByteString.Lazy length/compareN -> compareLength" [~1] forall t n.
  compare (length t) n = compareLength t n
"ByteString.Lazy compareN/length -> compareLength" [~1] forall t n.
  -- compare EQ LT = GT and vice versa
  compare n (length t) = compare EQ $ compareLength t n
"ByteString.Lazy length/==N -> compareLength/==EQ" [~1] forall t n.
   length t == n = compareLength t n == EQ
"ByteString.Lazy N==/length -> compareLength/==EQ" [~1] forall t n.
   n == length t = compareLength t n == EQ
"ByteString.Lazy length//=N -> compareLength//=EQ" [~1] forall t n.
   length t /= n = compareLength t n /= EQ
"ByteString.Lazy N/=/length -> compareLength//=EQ" [~1] forall t n.
   n /= length t = compareLength t n /= EQ
"ByteString.Lazy length/<N -> compareLength/==LT" [~1] forall t n.
   length t < n = compareLength t n == LT
"ByteString.Lazy >N/length -> compareLength/==LT" [~1] forall t n.
   n > length t = compareLength t n == LT
"ByteString.Lazy length/<=N -> compareLength//=GT" [~1] forall t n.
   length t <= n = compareLength t n /= GT
"ByteString.Lazy <=N/length -> compareLength//=GT" [~1] forall t n.
   n >= length t = compareLength t n /= GT
"ByteString.Lazy length/>N -> compareLength/==GT" [~1] forall t n.
   length t > n = compareLength t n == GT
"ByteString.Lazy <N/length -> compareLength/==GT" [~1] forall t n.
   n < length t = compareLength t n == GT
"ByteString.Lazy length/>=N -> compareLength//=LT" [~1] forall t n.
   length t >= n = compareLength t n /= LT
"ByteString.Lazy >=N/length -> compareLength//=LT" [~1] forall t n.
   n <= length t = compareLength t n /= LT
  #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s',  c')  = S.mapAccumL f s c
              (s'', cs') = go s' cs

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s'', c') = S.mapAccumR f s' c
              (s', cs') = go s cs

-- ---------------------------------------------------------------------
-- Building ByteStrings

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > head (scanl f z xs) == z
-- > last (scanl f z xs) == foldl f z xs
--
scanl
    :: (Word8 -> Word8 -> Word8)
    -- ^ accumulator -> element -> new accumulator
    -> Word8
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanl function = fmap (uncurry (flip snoc)) . mapAccumL (\x y -> (function x y, x))
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
-- @since 0.11.2.0
scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 function byteStream = case uncons byteStream of
  Nothing -> Empty
  Just (firstByte, remainingBytes) -> scanl function firstByte remainingBytes

-- | 'scanr' is similar to 'foldr', but returns a list of successive
-- reduced values from the right.
--
-- > scanr f z [..., x{n-1}, xn] == [..., x{n-1} `f` (xn `f` z), xn `f` z, z]
--
-- Note that
--
-- > head (scanr f z xs) == foldr f z xs
-- > last (scanr f z xs) == z
--
-- @since 0.11.2.0
scanr
    :: (Word8 -> Word8 -> Word8)
    -- ^ element -> accumulator -> new accumulator
    -> Word8
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanr function = fmap (uncurry cons) . mapAccumR (\x y -> (function y x, x))

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
--
-- @since 0.11.2.0
scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 function byteStream = case unsnoc byteStream of
  Nothing -> Empty
  Just (initialBytes, lastByte) -> scanr function lastByte initialBytes

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
iterate :: (Word8 -> Word8) -> Word8 -> ByteString
iterate f = unfoldr (\x -> case f x of !x' -> Just (x', x'))

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Word8 -> ByteString
repeat w = cs where cs = Chunk (S.replicate smallChunkSize w) cs

-- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- the value of every element.
--
replicate :: Int64 -> Word8 -> ByteString
replicate n w
    | n <= 0             = Empty
    | n < fromIntegral smallChunkSize = Chunk (S.replicate (fromIntegral n) w) Empty
    | r == 0             = cs -- preserve invariant
    | otherwise          = Chunk (S.unsafeTake (fromIntegral r) c) cs
 where
    c      = S.replicate smallChunkSize w
    cs     = nChunks q
    (q, r) = quotRem n (fromIntegral smallChunkSize)
    nChunks 0 = Empty
    nChunks m = Chunk c (nChunks (m-1))

-- | 'cycle' ties a finite ByteString into a circular one, or equivalently,
-- the infinite repetition of the original ByteString.
--
cycle :: HasCallStack => ByteString -> ByteString
cycle Empty = errorEmptyList "cycle"
cycle cs    = cs' where cs' = foldrChunks Chunk cs' cs

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = unfoldChunk 32
  where unfoldChunk n x =
          case S.unfoldrN n f x of
            (c, Nothing)
              | S.null c  -> Empty
              | otherwise -> Chunk c Empty
            (c, Just x')  -> Chunk c (unfoldChunk (n*2) x')

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n\/c)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int64 -> ByteString -> ByteString
take i _ | i <= 0 = Empty
take i cs0         = take' i cs0
  where take' 0 _            = Empty
        take' _ Empty        = Empty
        take' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.take (fromIntegral n) c) Empty
            else Chunk c (take' (n - fromIntegral (S.length c)) cs)

-- | /O(c)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
-- >>> takeEnd 0 "abcdefg"
-- ""
-- >>> takeEnd 4 "abc"
-- "abc"
--
-- @since 0.11.2.0
takeEnd :: Int64 -> ByteString -> ByteString
takeEnd i _ | i <= 0 = Empty
takeEnd i cs0        = takeEnd' i cs0
  where takeEnd' 0 _         = Empty
        takeEnd' n cs        =
            snd $ foldrChunks takeTuple (n,Empty) cs
        takeTuple _ (0, cs)  = (0, cs)
        takeTuple c (n, cs)
            | n > fromIntegral (S.length c) = (n - fromIntegral (S.length c), Chunk c cs)
            | otherwise      = (0, Chunk (S.takeEnd (fromIntegral n) c) cs)

-- | /O(n\/c)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or 'empty' if @n > 'length' xs@.
drop  :: Int64 -> ByteString -> ByteString
drop i p | i <= 0 = p
drop i cs0 = drop' i cs0
  where drop' 0 cs           = cs
        drop' _ Empty        = Empty
        drop' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.drop (fromIntegral n) c) cs
            else drop' (n - fromIntegral (S.length c)) cs

-- | /O(n)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
--
-- @since 0.11.2.0
dropEnd :: Int64 -> ByteString -> ByteString
dropEnd i p | i <= 0 = p
dropEnd i p          = go D.empty p
  where go :: D.Deque -> ByteString -> ByteString
        go deque (Chunk c cs)
            | D.byteLength deque < i = go (D.snoc c deque) cs
            | otherwise              =
                  let (output, deque') = getOutput empty (D.snoc c deque)
                    in foldrChunks Chunk (go deque' cs) output
        go deque Empty               = fromDeque $ dropEndBytes deque i

        len c = fromIntegral (S.length c)

        -- get a `ByteString` from all the front chunks of the accumulating deque
        -- for which we know they won't be dropped
        getOutput :: ByteString -> D.Deque -> (ByteString, D.Deque)
        getOutput out deque = case D.popFront deque of
            Nothing                       -> (reverseChunks out, deque)
            Just (x, deque') | D.byteLength deque' >= i ->
                            getOutput (Chunk x out) deque'
            _ -> (reverseChunks out, deque)

        -- reverse a `ByteString`s chunks, keeping all internal `S.ByteString`s
        -- unchanged
        reverseChunks = foldlChunks (flip Chunk) empty

        -- drop n elements from the rear of the accumulating `deque`
        dropEndBytes :: D.Deque -> Int64 -> D.Deque
        dropEndBytes deque n = case D.popRear deque of
            Nothing                       -> deque
            Just (deque', x) | len x <= n -> dropEndBytes deque' (n - len x)
                             | otherwise  ->
                                D.snoc (S.dropEnd (fromIntegral n) x) deque'

        -- build a lazy ByteString from an accumulating `deque`
        fromDeque :: D.Deque -> ByteString
        fromDeque deque =
            List.foldr chunk Empty (D.front deque) `append`
            List.foldl' (flip chunk) Empty (D.rear deque)

-- | /O(n\/c)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int64 -> ByteString -> (ByteString, ByteString)
splitAt i cs0 | i <= 0 = (Empty, cs0)
splitAt i cs0 = splitAt' i cs0
  where splitAt' 0 cs           = (Empty, cs)
        splitAt' _ Empty        = (Empty, Empty)
        splitAt' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then (Chunk (S.take (fromIntegral n) c) Empty
                 ,Chunk (S.drop (fromIntegral n) c) cs)
            else let (cs', cs'') = splitAt' (n - fromIntegral (S.length c)) cs
                   in (Chunk c cs', cs'')


-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f = takeWhile'
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk c cs) =
          case S.findIndexOrLength (not . f) c of
            0                  -> Empty
            n | n < S.length c -> Chunk (S.take n c) Empty
              | otherwise      -> Chunk c (takeWhile' cs)

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- >>> {-# LANGUAGE OverloadedLists #-)
-- >>> takeWhileEnd even [1,2,3,4,6]
-- [4,6]
--
-- @since 0.11.2.0
takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd f = takeWhileEnd'
  where takeWhileEnd' Empty = Empty
        takeWhileEnd' cs    =
            snd $ foldrChunks takeTuple (True,Empty) cs
        takeTuple _ (False, bs) = (False,bs)
        takeTuple c (True,bs)   =
           case S.takeWhileEnd f c of
                c' | S.length c' == S.length c -> (True, Chunk c bs)
                   | otherwise                 -> (False, fromStrict c' `append` bs)

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f = dropWhile'
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk c cs) =
          case S.findIndexOrLength (not . f) c of
            n | n < S.length c -> Chunk (S.drop n c) cs
              | otherwise      -> dropWhile' cs

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- >>> {-# LANGUAGE OverloadedLists #-)
-- >>> dropWhileEnd even [1,2,3,4,6]
-- [1,2,3]
--
-- @since 0.11.2.0
dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd f = go []
  where go acc (Chunk c cs)
            | f (S.last c) = go (c : acc) cs
            | otherwise    = List.foldl (flip Chunk) (go [] cs) (c : acc)
        go acc Empty       = dropEndBytes acc
        dropEndBytes []         = Empty
        dropEndBytes (x : xs)   =
            case S.dropWhileEnd f x of
                 x' | S.null x' -> dropEndBytes xs
                    | otherwise -> List.foldl' (flip Chunk) Empty (x' : xs)

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break f = break'
  where break' Empty        = (Empty, Empty)
        break' (Chunk c cs) =
          case S.findIndexOrLength f c of
            0                  -> (Empty, Chunk c cs)
            n | n < S.length c -> (Chunk (S.take n c) Empty
                                  ,Chunk (S.drop n c) cs)
              | otherwise      -> let (cs', cs'') = break' cs
                                   in (Chunk c cs', cs'')


-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
--
-- @since 0.11.2.0
breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd  f = go []
  where go acc (Chunk c cs)
            | f (S.last c) = List.foldl (flip $ BF.first . Chunk) (go [] cs) (c : acc)
            | otherwise = go (c : acc) cs
        go acc Empty = dropEndBytes acc
        dropEndBytes [] = (Empty, Empty)
        dropEndBytes (x : xs) =
            case S.breakEnd f x of
                 (x', x'') | S.null x' -> let (y, y') = dropEndBytes xs
                                           in (y, y' `append` fromStrict x)
                           | otherwise ->
                                List.foldl' (flip $ BF.first . Chunk) (fromStrict x', fromStrict x'') xs


--
-- TODO
--
-- Add rules
--

{-
-- | 'breakByte' breaks its ByteString argument at the first occurrence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (==99) "abcd" == breakByte 99 "abcd" -- fromEnum 'c' == 99
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c (LPS ps) = case (breakByte' ps) of (a,b) -> (LPS a, LPS b)
  where breakByte' []     = ([], [])
        breakByte' (x:xs) =
          case P.elemIndex c x of
            Just 0  -> ([], x : xs)
            Just n  -> (P.take n x : [], P.drop n x : xs)
            Nothing -> let (xs', xs'') = breakByte' xs
                        in (x : xs', xs'')

-- | 'spanByte' breaks its ByteString argument at the first
-- occurrence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (==99) "abcd" == spanByte 99 "abcd" -- fromEnum 'c' == 99
--
spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c (LPS ps) = case (spanByte' ps) of (a,b) -> (LPS a, LPS b)
  where spanByte' []     = ([], [])
        spanByte' (x:xs) =
          case P.spanByte c x of
            (x', x'') | P.null x'  -> ([], x : xs)
                      | P.null x'' -> let (xs', xs'') = spanByte' xs
                                       in (x : xs', xs'')
                      | otherwise  -> (x' : [], x'' : xs)
-}

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'spanEnd' @p@ is equivalent to @'breakEnd' (not . p)@ and to @('takeWhileEnd' p &&& 'dropWhileEnd' p)@.
--
-- We have
--
-- > spanEnd (not . isSpace) "x y z" == ("x y ", "z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse ps) in (reverse y, reverse x)
--
-- @since 0.11.2.0
spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p = breakEnd (not . p)

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ Empty          = []
splitWith p (Chunk c0 cs0) = comb [] (S.splitWith p c0) cs0

  where comb :: [P.ByteString] -> [P.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.splitWith p c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteString's that
-- are slices of the original.
--
split :: Word8 -> ByteString -> [ByteString]
split _ Empty     = []
split w (Chunk c0 cs0) = comb [] (S.split w c0) cs0

  where comb :: [P.ByteString] -> [P.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.split w c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
{-# INLINE split #-}

-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each string in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test.
group :: ByteString -> [ByteString]
group = go
  where
    go Empty        = []
    go (Chunk c cs)
      | S.length c == 1  = to [c] (S.unsafeHead c) cs
      | otherwise        = to [S.unsafeTake 1 c] (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)

    to acc !_ Empty        = [revNonEmptyChunks acc]
    to acc !w (Chunk c cs) =
      case S.findIndexOrLength (/= w) c of
        0                    -> revNonEmptyChunks acc
                              : go (Chunk c cs)
        n | n == S.length c  -> to (S.unsafeTake n c : acc) w cs
          | otherwise        -> revNonEmptyChunks (S.unsafeTake n c : acc)
                              : go (Chunk (S.unsafeDrop n c) cs)

-- | The 'groupBy' function is the non-overloaded version of 'group'.
--
groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k = go
  where
    go Empty        = []
    go (Chunk c cs)
      | S.length c == 1  = to [c] (S.unsafeHead c) cs
      | otherwise        = to [S.unsafeTake 1 c] (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)

    to acc !_ Empty        = [revNonEmptyChunks acc]
    to acc !w (Chunk c cs) =
      case S.findIndexOrLength (not . k w) c of
        0                    -> revNonEmptyChunks acc
                              : go (Chunk c cs)
        n | n == S.length c  -> to (S.unsafeTake n c : acc) w cs
          | otherwise        -> revNonEmptyChunks (S.unsafeTake n c : acc)
                              : go (Chunk (S.unsafeDrop n c) cs)

-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: ByteString -> [ByteString] -> ByteString
intercalate s = concat . List.intersperse s

-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(c)/ 'ByteString' index (subscript) operator, starting from 0.
--
-- This is a partial function, consider using 'indexMaybe' instead.
index :: HasCallStack => ByteString -> Int64 -> Word8
index _  i | i < 0  = moduleError "index" ("negative index: " ++ show i)
index cs0 i         = index' cs0 i
  where index' Empty     n = moduleError "index" ("index too large: " ++ show n)
        index' (Chunk c cs) n
          | n >= fromIntegral (S.length c) =
              index' cs (n - fromIntegral (S.length c))
          | otherwise       = S.unsafeIndex c (fromIntegral n)

-- | /O(c)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ByteString -> Int64 -> Maybe Word8
indexMaybe _ i | i < 0 = Nothing
indexMaybe cs0 i       = index' cs0 i
  where index' Empty _ = Nothing
        index' (Chunk c cs) n
          | n >= fromIntegral (S.length c) =
              index' cs (n - fromIntegral (S.length c))
          | otherwise       = Just $! S.unsafeIndex c (fromIntegral n)

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ByteString -> Int64 -> Maybe Word8
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int64
elemIndex w = elemIndex' 0
  where elemIndex' _ Empty        = Nothing
        elemIndex' n (Chunk c cs) =
          case S.elemIndex w c of
            Nothing -> elemIndex' (n + fromIntegral (S.length c)) cs
            Just i  -> Just (n + fromIntegral i)

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs = case elemIndex c (reverse xs) of
-- >   Nothing -> Nothing
-- >   Just i  -> Just (length xs - 1 - i)
--
-- @since 0.10.6.0
elemIndexEnd :: Word8 -> ByteString -> Maybe Int64
elemIndexEnd = findIndexEnd . (==)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int64]
elemIndices w = elemIndices' 0
  where elemIndices' _ Empty        = []
        elemIndices' n (Chunk c cs) = List.map ((+n).fromIntegral) (S.elemIndices w c)
                             ++ elemIndices' (n + fromIntegral (S.length c)) cs

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int64
count w = foldlChunks (\n c -> n + fromIntegral (S.count w c)) 0

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndex k = findIndex' 0
  where findIndex' _ Empty        = Nothing
        findIndex' n (Chunk c cs) =
          case S.findIndex k c of
            Nothing -> findIndex' (n + fromIntegral (S.length c)) cs
            Just i  -> Just (n + fromIntegral i)
{-# INLINE findIndex #-}

-- | The 'findIndexEnd' function takes a predicate and a 'ByteString' and
-- returns the index of the last element in the ByteString
-- satisfying the predicate.
--
-- @since 0.10.12.0
findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndexEnd k = findIndexEnd' 0
  where
    findIndexEnd' _ Empty = Nothing
    findIndexEnd' n (Chunk c cs) =
      let !n' = n + S.length c
          !i  = fromIntegral . (n +) <$> S.findIndexEnd k c
      in findIndexEnd' n' cs `mplus` i
{-# INLINE findIndexEnd #-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f = find'
  where find' Empty        = Nothing
        find' (Chunk c cs) = case S.find f c of
            Nothing -> find' cs
            Just w  -> Just w
{-# INLINE find #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]
findIndices k = findIndices' 0
  where findIndices' _ Empty        = []
        findIndices' n (Chunk c cs) = List.map ((+n).fromIntegral) (S.findIndices k c)
                             ++ findIndices' (n + fromIntegral (S.length c)) cs
{-# INLINE findIndices #-}

-- ---------------------------------------------------------------------
-- Searching ByteStrings

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem w cs = case elemIndex w cs of Nothing -> False ; _ -> True

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem w cs = not (w `elem` cs)

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter p = go
    where
        go Empty        = Empty
        go (Chunk x xs) = chunk (S.filter p x) (go xs)
{-# INLINE filter #-}

{-
-- | /O(n)/ and /O(n\/c) space/ A first order equivalent of /filter .
-- (==)/, for the common case of filtering a single byte. It is more
-- efficient to use /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w
{-# INLINE filterByte #-}

{-# RULES
"ByteString specialise filter (== x)" forall x.
  filter ((==) x) = filterByte x

"ByteString specialise filter (== x)" forall x.
 filter (== x) = filterByte x
  #-}
-}

{-
-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single byte out of a list. It is more efficient
-- to use /filterNotByte/ in this case.
--
-- > filterNotByte == filter . (/=)
--
-- filterNotByte is around 2x faster than its filter equivalent.
filterNotByte :: Word8 -> ByteString -> ByteString
filterNotByte w (LPS xs) = LPS (filterMap (P.filterNotByte w) xs)
-}

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition _ Empty = (Empty, Empty)
partition p (Chunk x xs) = (chunk t ts, chunk f fs)
  where
    (t,   f) = S.partition p x
    (ts, fs) = partition   p xs

-- ---------------------------------------------------------------------
-- Searching for substrings

-- | /O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf Empty _  = True
isPrefixOf _ Empty  = False
isPrefixOf (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = x == y  && isPrefixOf xs ys
    | S.length x <  S.length y = x == yh && isPrefixOf xs (Chunk yt ys)
    | otherwise                = xh == y && isPrefixOf (Chunk xt xs) ys
  where (xh,xt) = S.splitAt (S.length y) x
        (yh,yt) = S.splitAt (S.length x) y

-- | /O(n)/ The 'stripPrefix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 0.10.8.0
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix Empty bs  = Just bs
stripPrefix _ Empty  = Nothing
stripPrefix (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = if x == y then stripPrefix xs ys else Nothing
    | S.length x <  S.length y = do yt <- S.stripPrefix x y
                                    stripPrefix xs (Chunk yt ys)
    | otherwise                = do xt <- S.stripPrefix y x
                                    stripPrefix (Chunk xt xs) ys

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y
--TODO: a better implementation

-- | /O(n)/ The 'stripSuffix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix x y = reverse <$> stripPrefix (reverse x) (reverse y)
--TODO: a better implementation

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of bytes. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip = zipWith (,)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- corresponding sums.
zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith _ Empty     _  = []
zipWith _ _      Empty = []
zipWith f (Chunk a as) (Chunk b bs) = go a as b bs
  where
    go x xs y ys = f (S.unsafeHead x) (S.unsafeHead y)
                 : to (S.unsafeTail x) xs (S.unsafeTail y) ys

    to x Empty         _ _             | S.null x       = []
    to _ _             y Empty         | S.null y       = []
    to x xs            y ys            | not (S.null x)
                                      && not (S.null y) = go x  xs y  ys
    to x xs            _ (Chunk y' ys) | not (S.null x) = go x  xs y' ys
    to _ (Chunk x' xs) y ys            | not (S.null y) = go x' xs y  ys
    to _ (Chunk x' xs) _ (Chunk y' ys)                  = go x' xs y' ys

-- | A specialised version of `zipWith` for the common case of a
-- simultaneous map over two ByteStrings, to build a 3rd.
--
-- @since 0.11.1.0
packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith _ Empty _ = Empty
packZipWith _ _ Empty = Empty
packZipWith f (Chunk a@(S.BS _ al) as) (Chunk b@(S.BS _ bl) bs) = Chunk (S.packZipWith f a b) $
    case compare al bl of
        LT -> packZipWith f as $ Chunk (S.drop al b) bs
        EQ -> packZipWith f as bs
        GT -> packZipWith f (Chunk (S.drop bl a) as) bs
{-# INLINE packZipWith #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip ls = (pack (List.map fst ls), pack (List.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Special lists

-- | Returns all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
inits bs = NE.toList $! initsNE bs

-- | Returns all initial segments of the given 'ByteString', shortest first.
--
-- @since 0.11.4.0
initsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
initsNE = (Empty :|) . inits' id
  where
    inits' :: (ByteString -> ByteString) -> ByteString -> [ByteString]
    -- inits' f bs === map f (tail (inits bs))
    inits' _ Empty = []
    inits' f (Chunk c@(S.BS x len) cs)
      = [f (S.BS x n `Chunk` Empty) | n <- [1..len]]
      ++ inits' (f . Chunk c) cs

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
tails bs = NE.toList $! tailsNE bs

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
--
-- @since 0.11.4.0
tailsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
tailsNE bs = case uncons bs of
  Nothing -> Empty :| []
  Just (_, tl) -> bs :| tails tl


-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
--   This is mainly useful to allow the rest of the data pointed
--   to by the 'ByteString' to be garbage collected, for example
--   if a large string has been read in, and only a small part of it
--   is needed in the rest of the program.
copy :: ByteString -> ByteString
copy = foldrChunks (Chunk . S.copy) Empty
--TODO, we could coalese small blocks here
--FIXME: probably not strict enough, if we're doing this to avoid retaining
-- the parent blocks then we'd better copy strictly.

-- ---------------------------------------------------------------------

-- TODO defrag func that concatenates block together that are below a threshold
-- defrag :: ByteString -> ByteString

-- ---------------------------------------------------------------------
-- Lazy ByteString IO
--
-- Rule for when to close: is it expected to read the whole file?
-- If so, close when done.
--

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, in at most @k@-sized chunks. It does not block
-- waiting for a whole @k@-sized chunk, so if less than @k@ bytes are
-- available then they will be returned immediately as a smaller chunk.
--
-- The handle is closed on EOF.
--
hGetContentsN :: Int -> Handle -> IO ByteString
hGetContentsN k h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h k -- only blocks if there is no data available
        if S.null c
          then hClose h >> return Empty
          else Chunk c <$> lazyRead

-- | Read @n@ bytes into a 'ByteString', directly from the
-- specified 'Handle', in chunks of size @k@.
--
hGetN :: Int -> Handle -> Int -> IO ByteString
hGetN k h n | n > 0 = readChunks n
  where
    readChunks !i = do
        c <- S.hGet h (min k i)
        case S.length c of
            0 -> return Empty
            m -> do cs <- readChunks (i - m)
                    return (Chunk c cs)

hGetN _ _ 0 = return Empty
hGetN _ h n = illegalBufferSize h "hGet" n

-- | hGetNonBlockingN is similar to 'hGetContentsN', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available. Chunks are read on demand, in @k@-sized chunks.
--
hGetNonBlockingN :: Int -> Handle -> Int -> IO ByteString
hGetNonBlockingN k h n | n > 0= readChunks n
  where
    readChunks !i = do
        c <- S.hGetNonBlocking h (min k i)
        case S.length c of
            0 -> return Empty
            m -> do cs <- readChunks (i - m)
                    return (Chunk c cs)

hGetNonBlockingN _ _ 0 = return Empty
hGetNonBlockingN _ h n = illegalBufferSize h "hGetNonBlocking" n

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, using the default chunk size.
--
-- File handles are closed on EOF if all the file is read, or through
-- garbage collection otherwise.
--
hGetContents :: Handle -> IO ByteString
hGetContents = hGetContentsN defaultChunkSize

-- | Read @n@ bytes into a 'ByteString', directly from the specified 'Handle'.
--
hGet :: Handle -> Int -> IO ByteString
hGet = hGetN defaultChunkSize

-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.  If there is no data available to be read, 'hGetNonBlocking'
-- returns 'empty'.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hGet'.
--
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking = hGetNonBlockingN defaultChunkSize

-- | Read an entire file /lazily/ into a 'ByteString'.
--
-- The 'Handle' will be held open until EOF is encountered.
--
-- Note that this function's implementation relies on 'hGetContents'.
-- The reader is advised to read its documentation.
--
readFile :: FilePath -> IO ByteString
readFile f = openBinaryFile f ReadMode >>= hGetContents

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

-- | Write a 'ByteString' to a file.
--
writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

-- | Append a 'ByteString' to a file.
--
appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode

-- | getContents. Equivalent to hGetContents stdin. Will read /lazily/
--
getContents :: IO ByteString
getContents = hGetContents stdin

-- | Outputs a 'ByteString' to the specified 'Handle'.
--
-- The chunks will be
-- written one at a time. Other threads might write to the 'Handle' in between,
-- and hence 'hPut' alone is not suitable for concurrent writes.
--
hPut :: Handle -> ByteString -> IO ()
hPut h = foldrChunks (\c rest -> S.hPut h c >> rest) (return ())

-- | Similar to 'hPut' except that it will never block. Instead it returns
-- any tail that did not get written. This tail may be 'empty' in the case that
-- the whole string was written, or the whole original string if nothing was
-- written. Partial writes are also possible.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hPut'.
--
hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking _ Empty           = return Empty
hPutNonBlocking h bs@(Chunk c cs) = do
  c' <- S.hPutNonBlocking h c
  case S.length c' of
    l' | l' == S.length c -> hPutNonBlocking h cs
    0                     -> return bs
    _                     -> return (Chunk c' cs)

-- | A synonym for 'hPut', for compatibility
--
hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

-- | Write a ByteString to 'stdout'.
--
-- The chunks will be
-- written one at a time. Other threads might write to the 'stdout' in between,
-- and hence 'putStr' alone is not suitable for concurrent writes.
--
putStr :: ByteString -> IO ()
putStr = hPut stdout

-- | The interact function takes a function of type @ByteString -> ByteString@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device.
--
interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun = moduleError fun "empty ByteString"
{-# NOINLINE errorEmptyList #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error ("Data.ByteString.Lazy." ++ fun ++ ':':' ':msg)
{-# NOINLINE moduleError #-}


-- reverse a list of non-empty chunks into a lazy ByteString
revNonEmptyChunks :: [P.ByteString] -> ByteString
revNonEmptyChunks = List.foldl' (flip Chunk) Empty

-- reverse a list of possibly-empty chunks into a lazy ByteString
revChunks :: [P.ByteString] -> ByteString
revChunks = List.foldl' (flip chunk) Empty

-- $IOChunk
--
-- ⚠ Using lazy I\/O functions like 'readFile' or 'hGetContents'
-- means that the order of operations such as closing the file handle
-- is left at the discretion of the RTS.
-- Hence, the developer can face some issues when:
--
-- * The program reads a file and writes the same file. This means that the file
--   may be locked because the handler has not been released when 'writeFile' is executed.
-- * The program reads thousands of files, but due to lazy evaluation, the OS's file descriptor
--   limit is reached before the handlers can be released.
--
-- === Why?
--
-- Consider the following program:
--
-- > import qualified Data.ByteString.Lazy as BL
-- > main = do
-- >   _ <- BL.readFile "foo.txt"
-- >   BL.writeFile "foo.txt" mempty
--
-- Generally, in the 'IO' monad side effects happen
-- sequentially and in full. Therefore, one might reasonably expect that
-- reading the whole file via 'readFile' executes all three actions
-- (open the file handle, read its content, close the file handle) before
-- control moves to the following 'writeFile' action. This expectation holds
-- for the strict "Data.ByteString" API. However, the above lazy 'ByteString' variant
-- of the program fails with @openBinaryFile: resource busy (file is locked)@.
--
-- The reason for this is that "Data.ByteString.Lazy" is specifically designed
-- to handle large or unbounded streams of data incrementally, without requiring all the data
-- to be resident in memory at the same time. Incremental processing would not be possible
-- if 'readFile' were to follow the usual rules of 'IO': evaluating all side effects
-- would require reading the file in full and closing its handle before returning from 'readFile'. This is why
-- 'readFile' (and 'hGetContents' in general) is implemented
-- via 'unsafeInterleaveIO', which allows 'IO' side effects to be delayed and
-- interleaved with subsequent processing of the return value.
-- That's exactly what happens
-- in the example above: 'readFile' opens a file handle, but since the content
-- is not fully consumed, the file handle remains open, allowing the content to
-- read __on demand__ (never in this case, since the return value is ignored).
-- So when 'writeFile' is executed next, @foo.txt@ is still open for reading and
-- the RTS takes care to avoid simultaneously opening it for writing, instead
-- returning the error shown above.
--
-- === How to enforce the order of effects?
--
-- If the content is small enough to fit in memory,
-- consider using strict 'Data.ByteString.readFile',
-- potentially applying 'fromStrict' afterwards. E. g.,
--
-- > import qualified Data.ByteString as BS
-- > import qualified Data.ByteString.Lazy as BL
-- > main = do
-- >   _ <- BS.readFile "foo.txt"
-- >   BL.writeFile "foo.txt" mempty
--
-- If you are dealing with large or unbounded data streams,
-- consider reaching out for a specialised package, such as
-- <http://hackage.haskell.org/package/conduit conduit>,
-- <http://hackage.haskell.org/package/machines-bytestring machines-bytestring>,
-- <http://hackage.haskell.org/package/pipes-bytestring pipes-bytestring>,
-- <http://hackage.haskell.org/package/streaming-bytestring streaming-bytestring>,
-- <http://hackage.haskell.org/package/streamly-bytestring streamly-bytestring>,
-- etc.
