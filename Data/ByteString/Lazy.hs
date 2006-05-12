{-# OPTIONS_GHC -cpp -fffi -fglasgow-exts #-}
--
-- Module      : ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005
--               (c) Don Stewart 2005-2006
--               (c) Bjorn Bringert 2006
--               (c) Duncan Coutts 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
-- 

--
-- | A time and space-efficient implementation of lazy byte vectors
-- using lists of packed Word8 arrays, suitable for high performance
-- use, both in terms of large data quantities, or high speed
-- requirements. Byte vectors are encoded as lazy lists of strict Word8
-- arrays of bytes, held in a ForeignPtr, and can be passed between C
-- and Haskell with little effort. They provide a means to manipulate
-- large byte vectors without requiring the entire vector be resident in
-- memory.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- > import qualified Data.ByteString.Lazy as B
--
-- Original GHC implementation by Bryan O\'Sullivan. Rewritten to use
-- UArray by Simon Marlow. Rewritten to support slices and use
-- ForeignPtr by David Roundy. Polished and extended by Don Stewart.
-- Lazy variant by Duncan Coutts.
--

module Data.ByteString.Lazy (

        -- * The @ByteString@ type
        ByteString(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        packByte,               -- :: Word8   -> ByteString
        pack,                   -- :: [Word8] -> ByteString
        unpack,                 -- :: ByteString -> [Word8]
        packWith,               -- :: (a -> Word8) -> [a] -> ByteString
        unpackWith,             -- :: (Word8 -> a) -> ByteString -> [a]

        -- * Basic interface
        cons,                   -- :: Word8 -> ByteString -> ByteString
        snoc,                   -- :: Word8 -> ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int
        head,                   -- :: ByteString -> Word8
        tail,                   -- :: ByteString -> ByteString
        last,                   -- :: ByteString -> Word8
        init,                   -- :: ByteString -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString

        -- * Special ByteStrings
--      inits,                  -- :: ByteString -> [ByteString]
--      tails,                  -- :: ByteString -> [ByteString]
--      elems,                  -- :: ByteString -> [ByteString]

        -- * Transformating ByteStrings
        map,                    -- :: (Word8 -> Word8) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
        intersperse,            -- :: Word8 -> ByteString -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing 'ByteString's
        foldl,                  -- :: (a -> Word8 -> a) -> a -> ByteString -> a
        foldr,                  -- :: (Word8 -> a -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
        foldr1,                 -- :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8

        -- ** Special folds
        concat,                 -- :: [ByteString] -> ByteString
        concatMap,              -- :: (Word8 -> ByteString) -> ByteString -> ByteString
        any,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        all,                    -- :: (Word8 -> Bool) -> ByteString -> Bool
        maximum,                -- :: ByteString -> Word8
        minimum,                -- :: ByteString -> Word8
        mapIndexed,             -- :: (Int -> Word8 -> Word8) -> ByteString -> ByteString

        -- * Generating and unfolding ByteStrings
        replicate,              -- :: Int -> Word8 -> ByteString
        unfoldrN,               -- :: (Word8 -> Maybe (Word8, Word8)) -> Word8 -> ByteString

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> ByteString -> ByteString
        drop,                   -- :: Int -> ByteString -> ByteString
        splitAt,                -- :: Int -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        break,                  -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        span,                   -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
--      spanEnd,                -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

        -- ** Breaking and dropping on specific bytes
        breakByte,              -- :: Word8 -> ByteString -> (ByteString, ByteString)
        spanByte,               -- :: Word8 -> ByteString -> (ByteString, ByteString)
        breakFirst,             -- :: Word8 -> ByteString -> Maybe (ByteString,ByteString)
        breakLast,              -- :: Word8 -> ByteString -> Maybe (ByteString,ByteString)

        -- ** Breaking into many substrings
        split,                  -- :: Word8 -> ByteString -> [ByteString]
        splitWith,              -- :: (Word8 -> Bool) -> ByteString -> [ByteString]
        tokens,                 -- :: (Word8 -> Bool) -> ByteString -> [ByteString]
        group,                  -- :: ByteString -> [ByteString]
        groupBy,                -- :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]

        -- ** Joining strings
        join,                   -- :: ByteString -> [ByteString] -> ByteString
        joinWithByte,           -- :: Word8 -> ByteString -> ByteString -> ByteString

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int -> Word8
        elemIndex,              -- :: Word8 -> ByteString -> Maybe Int
        elemIndices,            -- :: Word8 -> ByteString -> [Int]
--      elemIndexLast,          -- :: Word8 -> ByteString -> Maybe Int
        findIndex,              -- :: (Word8 -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Word8 -> Bool) -> ByteString -> [Int]
        count,                  -- :: Word8 -> ByteString -> Int

        -- * Ordered ByteStrings
--        sort,                   -- :: ByteString -> ByteString

        -- * Searching ByteStrings

        -- ** Searching by equality
        -- | These functions use memchr(3) to efficiently search the ByteString

        elem,                   -- :: Word8 -> ByteString -> Bool
        notElem,                -- :: Word8 -> ByteString -> Bool
        filterByte,             -- :: Word8 -> ByteString -> ByteString
        filterNotByte,          -- :: Word8 -> ByteString -> ByteString

        -- ** Searching with a predicate
        filter,                 -- :: (Word8 -> Bool) -> ByteString -> ByteString
        find,                   -- :: (Word8 -> Bool) -> ByteString -> Maybe Word8
{-
        -- ** Prefixes and suffixes
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool

        -- ** Search for arbitrary substrings
        isSubstringOf,          -- :: ByteString -> ByteString -> Bool
        findSubstring,          -- :: ByteString -> ByteString -> Maybe Int
        findSubstrings,         -- :: ByteString -> ByteString -> [Int]

        -- * Zipping and unzipping ByteStrings
        zip,                    -- :: ByteString -> ByteString -> [(Word8,Word8)]
        zipWith,                -- :: (Word8 -> Word8 -> c) -> ByteString -> ByteString -> [c]
        unzip,                  -- :: [(Word8,Word8)] -> (ByteString,ByteString)

        -- * Unchecked access
        unsafeHead,             -- :: ByteString -> Word8
        unsafeTail,             -- :: ByteString -> ByteString
        unsafeIndex,            -- :: ByteString -> Int -> Word8
-}

        -- * I\/O with @ByteString@s

        -- ** I\/O with Handles
        hGetContentsN,          -- :: Int -> Handle -> IO ByteString
        hGetN,                  -- :: Int -> Handle -> Int -> IO ByteString
        hGetContents,           -- :: Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()

        -- ** Standard input and output
        getContents,            -- :: IO ByteString
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()

  ) where

import qualified Prelude
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,maximum
                                ,minimum,all,concatMap,foldl1,foldr1
                                ,readFile,writeFile,replicate
                                ,getContents,getLine,putStr,putStrLn
                                ,zip,zipWith,unzip,notElem)


import qualified Data.List as L        -- L for list/lazy
import qualified Data.ByteString as P  -- P for packed

import Data.Word                (Word8)
import Data.Int                 (Int64)
import System.IO (Handle,stdin,stdout,openBinaryFile,IOMode(..),hClose)
import System.IO.Unsafe
import Control.Exception        (bracket)

#if defined(__GLASGOW_HASKELL__)
import Data.Generics            (Data(..), Typeable(..))
#endif

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
newtype ByteString = LPS [P.ByteString] -- LPS for lazy packed string
    deriving (Show,Read
#if defined(__GLASGOW_HASKELL__)
                        ,Data, Typeable
#endif
             )

instance Eq  ByteString
    where (==)    = eq

instance Ord ByteString
    where compare = compareBytes

------------------------------------------------------------------------

-- The data type invariant:
-- the list is either empty or consists of non-null ByteStrings
--
invariant :: ByteString -> Bool
invariant (LPS []) = True
invariant (LPS xs) = L.all (not . P.null) xs

-- The Data abstraction function
--
abstr :: ByteString -> P.ByteString
abstr (LPS []) = P.empty
abstr (LPS xs) = P.concat xs

-- The representation uses lists of packed chunks. When we have to convert from
-- a lazy list to the chunked representation, then by default we'll use this
-- chunk size. Some functions give you more control over the chunk size.
defaultChunkSize :: Int
defaultChunkSize = 4096

------------------------------------------------------------------------

eq :: ByteString -> ByteString -> Bool
eq (LPS xs) (LPS ys) = eq' xs ys
  where eq' [] [] = True
        eq' [] _  = False
        eq' _  [] = False
        eq' (a:as) (b:bs) =
          case compare (P.length a) (P.length b) of
            LT -> a == (P.take (P.length a) b) && eq' as (P.drop (P.length a) b : bs)
            EQ -> a == b                       && eq' as bs
            GT -> (P.take (P.length b) a) == b && eq' (P.drop (P.length b) a : as) bs

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (LPS xs) (LPS ys) = cmp xs ys
  where cmp [] [] = EQ
        cmp [] _  = LT
        cmp _  [] = GT
        cmp (a:as) (b:bs) =
          case compare (P.length a) (P.length b) of
            LT -> case compare a (P.take (P.length a) b) of
                    EQ     -> cmp as (P.drop (P.length a) b : bs)
                    result -> result
            EQ -> case compare a b of
                    EQ     -> cmp as bs
                    result -> result
            GT -> case compare (P.take (P.length b) a) b of
                    EQ     -> cmp (P.drop (P.length b) a : as) bs
                    result -> result

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = LPS []
{-# NOINLINE empty #-}

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
packByte :: Word8 -> ByteString
packByte c = LPS [P.packByte c]
{-# NOINLINE packByte #-}

-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'. 
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck. In such cases, consider using packAddress (GHC only).
pack :: [Word8] -> ByteString
pack str = LPS $ L.map P.pack (chunk defaultChunkSize str)

-- ?
chunk :: Int -> [a] -> [[a]]
chunk _    [] = []
chunk size xs = case L.splitAt size xs of (xs', xs'') -> xs' : chunk size xs''

-- | /O(n)/ Converts a 'ByteString' to a '[Word8]'.
unpack :: ByteString -> [Word8]
unpack (LPS ss) = L.concatMap P.unpack ss
{-# INLINE unpack #-}

------------------------------------------------------------------------

-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = LPS $ L.map (P.packWith k) (chunk defaultChunkSize str)
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith k (LPS ss) = L.concatMap (P.unpackWith k) ss
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null (LPS []) = True
null (_)      = False  -- TODO: guarantee this invariant is maintained
{-# INLINE null #-}

-- | /O(n/c)/ 'length' returns the length of a ByteString as an 'Int64'
length :: ByteString -> Int64
length (LPS ss) = L.sum (L.map (fromIntegral.P.length) ss)

-- avoid the intermediate list?
-- length (LPS ss) = L.foldl lengthF 0 ss
--     where lengthF n s = let m = n + fromIntegral (P.length s) in m `seq` m
{-# INLINE length #-}

-- | /O(1)/ 'cons' is analogous to (:) for lists
cons :: Word8 -> ByteString -> ByteString
cons c (LPS ss) = LPS (P.packByte c : ss)   -- TODO: coalesing and O(1) amortised time
{-# INLINE cons #-}

-- | /O(n/c)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (LPS ss) c = LPS (ss ++ [P.packByte c])
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Word8
head (LPS [])    = errorEmptyList "head"
head (LPS (x:_)) = P.unsafeHead x
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
tail :: ByteString -> ByteString
tail (LPS [])     = errorEmptyList "tail"
tail (LPS (x:xs))
  | P.length x == 1 = LPS xs
  | otherwise       = LPS (P.unsafeTail x : xs)
{-# INLINE tail #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite and non-empty.
last :: ByteString -> Word8
last (LPS []) = errorEmptyList "last"
last (LPS xs) = P.last (L.last xs)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'ByteString' except the last one.
init :: ByteString -> ByteString
init (LPS []) = errorEmptyList "init"
init (LPS as) = LPS (init' as)
  where init' (x:[]) = P.init x : []
        init' (x:xs) = x : init' xs
{-# INLINE init #-}

-- | /O(n)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append (LPS []) (LPS ys) = LPS ys
append (LPS xs) (LPS []) = LPS xs
append (LPS xs) (LPS ys) = LPS (xs ++ ys)
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> ByteString -> ByteString
map f (LPS xs) = LPS (L.map (P.map f) xs)
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse (LPS xs) = LPS (L.reverse $ L.map P.reverse xs)

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'ByteString' and \`intersperses\' that byte between the elements of
-- the 'ByteString'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse = error "FIXME: not yet implemented"
{-intersperse c (LPS [])     = LPS []
intersperse c (LPS (x:xs)) = LPS (P.intersperse c x : L.map intersperse')
  where intersperse' c ps@(PS x s l) =
          P.create (2*l) $ \p -> withForeignPtr x $ \f ->
                poke p c
                c_intersperse (p `plusPtr` 1) (f `plusPtr` s) l c
-}
-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose s = L.map (\ss -> LPS [P.pack ss]) (L.transpose (L.map unpack s))

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
-- TODO: Is this function is subject to list and array fusion?
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z (LPS xs) = L.foldl (P.foldl f) z xs
{-# INLINE foldl #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z (LPS xs) = L.foldr (flip (P.foldr k)) z xs
{-# INLINE foldr #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
-- This function is subject to array fusion.
foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 _ (LPS []) = errorEmptyList "foldl1"
foldl1 f (LPS (x:xs)) = foldl f (P.unsafeHead x) (LPS (P.unsafeTail x : xs))

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 _ (LPS []) = errorEmptyList "foldr1"
foldr1 f (LPS xs) = foldr1' xs
  where foldr1' (x:[]) = P.foldr1 f x
        foldr1' (x:xs) = P.foldr  f (foldr1' xs) x

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat lpss = LPS (L.concatMap (\(LPS xs) -> xs) lpss)

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f (LPS lps) = LPS (L.map (P.concatMap (\w -> case f w of LPS xs -> P.concat xs)) lps)
-- TODO: above seems overly complex and I'm not sure of the chunking behaviour

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any f (LPS xs) = L.or (L.map (P.any f) xs)
-- todo fuse

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all f (LPS xs) = L.and (L.map (P.all f) xs)
-- todo fuse

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Word8
maximum (LPS []) = errorEmptyList "maximum"
maximum (LPS xs) = L.maximum (L.map P.maximum xs)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Word8
minimum (LPS []) = errorEmptyList "minimum"
minimum (LPS xs) = L.minimum (L.map P.minimum xs)
{-# INLINE minimum #-}

-- | /O(n)/ map Word8 functions, provided with the index at each position
mapIndexed :: (Int -> Word8 -> Word8) -> ByteString -> ByteString
mapIndexed k (LPS xs) = LPS (snd (L.mapAccumL mapIndexedChunk 0 xs))
  where mapIndexedChunk :: Int -> P.ByteString -> (Int, P.ByteString)
        mapIndexedChunk i x = (i + P.length x, P.mapIndexed (\i' -> k (i+i')) x)
        --TODO: the data flow is a bit nasty, we're passing in a differnt
        -- function each time to mapIndexed

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Word8 -> ByteString
replicate w c = LPS [P.replicate w c]

-- | /O(n)/ The 'unfoldrN' function is analogous to the List \'unfoldr\'.
-- 'unfoldrN' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
--
-- The int parameter gives a chunk size.
--
unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldrN = error "FIXME: not yet implemented"
{-unfoldrN c f = LPS . unfoldr
  where unfoldr s = case unfoldrN' c f s of
                      (ps, Just s')
                        | P.null ps ->      unfoldr s'
                        | otherwise -> ps : unfoldr s'
                      (ps, Nothing)
                        | P.null ps ->      []
                        | otherwise -> ps : []

        unfoldrN' :: Int -> (a -> Maybe (Word8, a)) -> a -> (P.ByteString, Maybe a)
        unfoldrN' i f w = P.create' i $ \p -> go p w 0
            where
                STRICT3(go)
                go q c n | n == i    = return (n,Just c)      -- stop if we reach `i'
                         | otherwise = case f c of
                                           Nothing        -> return (n, Nothing)
                                           Just (a,new_c) -> do
                                                poke q a
                                                go (q `plusPtr` 1) new_c (n+1)
-}

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n\/c)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take n _ | n < 0 = empty     -- FOR NOW.
take i (LPS ps)  = LPS (take' i ps)
  where take' _ []     = []
        take' 0 _      = []
        take' n (x:xs) =
          if n < P.length x
            then P.take n x : []
            else x : take' (n - P.length x) xs

-- | /O(n\/c)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop i (LPS ps) = LPS (drop' i ps)
  where drop' _ []     = []
        drop' 0 xs     = xs
        drop' n (x:xs) =
          if n < P.length x
            then P.drop n x : xs
            else drop' (n - P.length x) xs

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt  i (LPS ps) = case splitAt' i ps of (a,b) -> (LPS a, LPS b)
  where splitAt' _ []     = ([], [])
        splitAt' 0 xs     = ([], xs)
        splitAt' n (x:xs) =
          if n < P.length x
            then (P.take n x : [], P.drop n x : xs)
            else let (xs', xs'') = splitAt' (n - P.length x) xs
                   in (x:xs', xs'')


-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f (LPS ps) = LPS (takeWhile' ps)
  where takeWhile' []     = []
        takeWhile' (x:xs) =
          case P.findIndexOrEnd (not . f) x of
            0                  -> []
            n | n < P.length x -> P.take n x : []
              | otherwise      -> x : takeWhile' xs

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f (LPS ps) = LPS (dropWhile' ps)
  where dropWhile' []     = []
        dropWhile' (x:xs) =
          case P.findIndexOrEnd (not . f) x of
            n | n < P.length x -> P.drop n x : xs
              | otherwise      -> dropWhile' xs

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break f (LPS ps) = case (break' ps) of (a,b) -> (LPS a, LPS b)
  where break' []     = ([], [])
        break' (x:xs) =
          case P.findIndexOrEnd f x of
            0                  -> ([], x : xs)
            n | n < P.length x -> (P.take n x : [], P.drop n x : xs)
              | otherwise      -> let (xs', xs'') = break' xs
                                   in (x : xs', xs'')

-- | 'breakByte' breaks its ByteString argument at the first occurence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakByte 'c' "abcd"
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
-- occurence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
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


-- | /O(n)/ 'breakFirst' breaks the given ByteString on the first
-- occurence of @w@. It behaves like 'break', except the delimiter is
-- not returned, and @Nothing@ is returned if the delimiter is not in
-- the ByteString. I.e.
--
-- > breakFirst 'b' "aabbcc" == Just ("aa","bcc")
--
-- > breakFirst c xs ==
-- > let (x,y) = break (== c) xs 
-- > in if null y then Nothing else Just (x, drop 1 y))
--
breakFirst :: Word8 -> ByteString -> Maybe (ByteString,ByteString)
--breakFirst c p = case P.elemIndex c p of
--   Nothing -> Nothing
--   Just n -> Just (take n p, drop (n+1) p)

breakFirst c (LPS ps) = breakByte' [] ps
  where breakByte' _   []     = Nothing
        breakByte' acc (x:xs) =
          case P.elemIndex c x of
            Just n -> let acc' | n == 0    = acc
                               | otherwise = P.take n x : acc

                          xs'  | n+1 >= P.length x = xs
                               | otherwise         = P.drop (n+1) x : xs
                       in Just (LPS (L.reverse acc'), LPS xs')
            Nothing -> breakByte' (x:acc) xs


-- | /O(n)/ 'breakLast' behaves like breakFirst, but from the end of the
-- ByteString.
--
-- > breakLast ('b') (pack "aabbcc") == Just ("aab","cc")
--
-- and the following are equivalent:
--
-- > breakLast 'c' "abcdef"
-- > let (x,y) = break (=='c') (reverse "abcdef") 
-- > in if null x then Nothing else Just (reverse (drop 1 y), reverse x)
--
breakLast :: Word8 -> ByteString -> Maybe (ByteString,ByteString)
breakLast _c _p = error "not implemented"

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith (=='a') []        == []
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith = error "Not implemented"
{-
--
-- Wrong. Problem is that we dont' coalesce chunks that we should, so:
--
--   List.concatMap (Data.ByteString.splitWith (==97)) xs 
--   ["","e","ehg","",""]
--
--   Data.ByteString.splitWith (==97)) t 
--   ["","eehg",""]
--
--  So we have to combine chunks that start with the same delimiter? Or
--  is it more complex..
--
splitWith f (LPS xs) =
    L.map (\x -> if P.null x then LPS [] else LPS [x])
  $ L.concatMap (P.splitWith f) xs
-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
-- > split 'x'  "x"          == ["",""]
-- 
-- and
--
-- > join [c] . split c == id
-- > split == splitWith . (==)
-- 
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteStrings' that
-- are slices of the original.
--
split :: Word8 -> ByteString -> [ByteString]
split c (LPS [])     = []
split c (LPS (x:xs)) = comb' [] (P.split c x) xs
  where comb' :: [P.ByteString] -> [P.ByteString] -> [P.ByteString] -> [ByteString]
        comb' acc (s:[]) []      
                    | P.null s  = LPS (L.reverse    acc ) : []
                    | otherwise = LPS (L.reverse (s:acc)) : []
        comb' acc (s:[]) (x:xs)
                    | P.null s  = comb'    acc  (P.split c x) xs
                    | otherwise = comb' (s:acc) (P.split c x) xs
        comb' []  (s:ss) xs     
                    | P.null s  = LPS []  : comb' [] ss xs
                    | otherwise = LPS [s] : comb' [] ss xs
        comb' acc (s:ss) xs
                    | P.null s  = LPS (L.reverse    acc ) : comb' [] ss xs
                    | otherwise = LPS (L.reverse (s:acc)) : comb' [] ss xs
        
-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Word8 -> Bool) -> ByteString -> [ByteString]
tokens f = L.filter (not.null) . splitWith f

-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each sublist in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test. It is about 40% faster than 
-- /groupBy (==)/
group :: ByteString -> [ByteString]
group (LPS ps) = group' [] ps
  where group' :: [P.ByteString] -> [P.ByteString] -> [ByteString]
        group' []     []     = []
        group' []     (x:xs) = group' (P.group x) xs
        group' (s:[]) (x:xs) =
          case P.group x of
           (s':ss')
             | P.unsafeHead s'
            == P.unsafeHead s -> LPS [s,s']         : group' ss' xs
             | otherwise      -> LPS [s] : LPS [s'] : group' ss' xs
        group' (s:ss) xs = LPS [s] : group' ss xs

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k (LPS ps) = group' [] ps
  where group' :: [P.ByteString] -> [P.ByteString] -> [ByteString]
        group' []     []     = []
        group' []     (x:xs) = group' (P.groupBy k x) xs
        group' (s:[]) (x:xs) =
          case P.group x of
           (s':ss')
             | k (P.unsafeHead s') (P.unsafeHead s ) -> LPS [s,s']         : group' ss' xs
             | otherwise                             -> LPS [s] : LPS [s'] : group' ss' xs
        group' (s:ss) xs = LPS [s] : group' ss xs

-- | /O(n)/ The 'join' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
join :: ByteString -> [ByteString] -> ByteString
join filler pss = concat (splice pss)
  where splice []  = []
        splice [x] = [x]
        splice (x:y:xs) = x:filler:splice (y:xs)

-- | /O(n)/ joinWithByte. An efficient way to join to two ByteStrings
-- with a char.
--
joinWithByte :: Word8 -> ByteString -> ByteString -> ByteString
joinWithByte c x y = append x (cons c y)


-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(c)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Word8
index _        i | i < 0 = moduleError "index" ("negative index: " ++ show i)
index (LPS ps) i         = index' ps i
  where index' []     n = moduleError "index" ("index too large: " ++ show n)
        index' (x:xs) n
          | n >= P.length x = index' xs (n - P.length x)
          | otherwise       = P.unsafeIndex x n

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. 
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex c (LPS ps) = elemIndex' 0 ps
  where elemIndex' _ []     = Nothing
        elemIndex' n (x:xs) =
          case P.elemIndex c x of
            Nothing -> elemIndex' (n + P.length x) xs
            Just i  -> Just (n+i)

{-
-- | /O(n)/ The 'elemIndexLast' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexLast c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexLast :: Word8 -> ByteString -> Maybe Int
elemIndexLast ch (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p ->
    go (p `plusPtr` s) (l-1)
  where
    STRICT2(go)
    go p i | i < 0     = return Nothing
           | otherwise = do ch' <- peekByteOff p i
                            if ch == ch'
                                then return $ Just i
                                else go p (i-1)
-}
-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int]
elemIndices c (LPS ps) = elemIndices' 0 ps
  where elemIndices' _ []     = []
        elemIndices' n (x:xs) = L.map (+n) (P.elemIndices c x)
                             ++ elemIndices' (n + P.length x) xs

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int
count w (LPS xs) = L.sum (L.map (P.count w) xs)

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex k (LPS ps) = findIndex' 0 ps
  where findIndex' _ []     = Nothing
        findIndex' n (x:xs) =
          case P.findIndex k x of
            Nothing -> findIndex' (n + P.length x) xs
            Just i  -> Just (n+i)

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices k (LPS ps) = findIndices' 0 ps
  where findIndices' _ []     = []
        findIndices' n (x:xs) = L.map (+n) (P.findIndices k x)
                             ++ findIndices' (n + P.length x) xs

-- ---------------------------------------------------------------------
-- Searching ByteStrings

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem c ps = not (elem c ps)

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter f (LPS xs) = LPS (L.filter (not . P.null) $ L.map (P.filter f) xs)

-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single byte. It is more efficient to use
-- /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
filterByte :: Word8 -> ByteString -> ByteString
filterByte w (LPS xs) = LPS (L.filter (not . P.null) $ L.map (P.filterByte w) xs)

-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single byte out of a list. It is more efficient
-- to use /filterNotByte/ in this case.
--
-- > filterNotByte == filter . (/=)
--
-- filterNotByte is around 2x faster than its filter equivalent.
filterNotByte :: Word8 -> ByteString -> ByteString
filterNotByte w (LPS xs) = LPS (L.filter (not . P.null) $ L.map (P.filterNotByte w) xs)

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f (LPS xs) = find' xs
  where find' []     = Nothing
        find' (x:xs) = case P.find f x of
            Nothing -> find' xs
            Just w  -> Just w

-- ---------------------------------------------------------------------
-- Searching for substrings

-- | /O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf = error "not yet implemented"

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
-- 
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implemenation uses memcmp to compare the end of the
-- string only, with no reverse required..
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf = error "not yet implemented"

-- | Check whether one string is a substring of another. @isSubstringOf
-- p s@ is equivalent to @not (null (findSubstrings p s))@.
isSubstringOf :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to search in.
              -> Bool
isSubstringOf p s = error "not yet implemented"

-- | Get the first index of a substring in another string,
--   or 'Nothing' if the string is not found.
--   @findSubstring p s@ is equivalent to @listToMaybe (findSubstrings p s)@.
findSubstring :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to seach in.
              -> Maybe Int
findSubstring = error "not yet implemented"

-- | Find the indexes of all (possibly overlapping) occurances of a
-- substring in a string.  This function uses the Knuth-Morris-Pratt
-- string matching algorithm.
findSubstrings :: ByteString -- ^ String to search for.
               -> ByteString -- ^ String to seach in.
               -> [Int]
findSubstrings = error "not yet implemented"

-- ---------------------------------------------------------------------

-- TODO defrag func that concatenates block together that are below a threshold
-- defrag :: Int -> ByteString -> ByteString

-- ---------------------------------------------------------------------
-- Lazy ByteString IO

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, in @k@-sized chunks.
hGetContentsN :: Int -> Handle -> IO ByteString
hGetContentsN k h = lazyRead >>= return . LPS
  where
    lazyRead = unsafeInterleaveIO $ do
        ps <- P.hGet h k
        case P.length ps of
            0         -> return []
            n | n < k -> return [ps]
            _         -> do pss <- lazyRead
                            return (ps : pss)

-- | Read @n@ bytes /lazily/ into a 'ByteString', directly from the
-- specified 'Handle', in chunks of size @k@.
hGetN :: Int -> Handle -> Int -> IO ByteString
hGetN _ _ 0 = return empty
hGetN k h n = lazyRead n >>= return . LPS
  where
    lazyRead i = unsafeInterleaveIO $ do
        ps <- P.hGet h (min k i)
        case P.length ps of
            0          -> return []
            m | m == n -> return [ps]
            m          -> do pss <- lazyRead (i - m)
                             return (ps : pss)

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, using the default chunk size.
hGetContents :: Handle -> IO ByteString
hGetContents = hGetContentsN defaultChunkSize

-- | Read @n@ bytes /lazily/ into a 'ByteString', directly from the specified 'Handle'.
hGet :: Handle -> Int -> IO ByteString
hGet = hGetN defaultChunkSize

-- | Read an entire file /lazily/ into a 'ByteString'.
readFile :: FilePath -> IO ByteString
readFile f = openBinaryFile f ReadMode >>= hGetContents

-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeFile :: FilePath -> ByteString -> IO ()
writeFile f txt = bracket (openBinaryFile f WriteMode) hClose
    (\hdl -> hPut hdl txt)

-- | getContents. Equivalent to hGetContents stdin. Will read /lazily/
getContents :: IO ByteString
getContents = hGetContents stdin

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> ByteString -> IO ()
hPut h (LPS xs) = mapM_ (P.hPut h) xs

-- | Write a ByteString to stdout
putStr :: ByteString -> IO ()
putStr = hPut stdout

-- | Write a ByteString to stdout, appending a newline byte
putStrLn :: ByteString -> IO ()
putStrLn ps = hPut stdout ps >> hPut stdout (packByte 0x0a)

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = moduleError fun "empty ByteString"

moduleError :: String -> String -> a
moduleError fun msg = error ("Data.ByteString.Lazy." ++ fun ++ ':':' ':msg)
