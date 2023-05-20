{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.ByteString.Lazy.Char8
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- Manipulate /lazy/ 'ByteString's using 'Char' operations. All Chars will
-- be truncated to 8 bits. It can be expected that these functions will
-- run at identical speeds to their 'Data.Word.Word8' equivalents in
-- "Data.ByteString.Lazy".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy.Char8 as C
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the ByteString type, enabling you to use string literals, and
-- have them implicitly packed to ByteStrings.
-- Use @{-\# LANGUAGE OverloadedStrings \#-}@ to enable this.
--

module Data.ByteString.Lazy.Char8 (

        -- * The @ByteString@ type
        ByteString,

        -- * Introducing and eliminating 'ByteString's
        empty,
        singleton,
        pack,
        unpack,
        fromChunks,
        toChunks,
        fromStrict,
        toStrict,

        -- * Basic interface
        cons,
        cons',
        snoc,
        append,
        head,
        uncons,
        last,
        tail,
        unsnoc,
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

        -- ** Breaking into lines and words
        lines,
        words,
        unlines,
        unwords,

        -- * Predicates
        isPrefixOf,
        isSuffixOf,

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

        -- * Reading from ByteStrings
        -- | Note that a lazy 'ByteString' may hold an unbounded stream of
        -- @\'0\'@ digits, in which case the functions below may never return.
        -- If that's a concern, you can use 'take' to first truncate the input
        -- to an acceptable length.  Non-termination is also possible when
        -- reading arbitrary precision numbers via 'readInteger' or
        -- 'readNatural', if the input is an unbounded stream of arbitrary
        -- decimal digits.
        --
        readInt,
        readInt64,
        readInt32,
        readInt16,
        readInt8,

        readWord,
        readWord64,
        readWord32,
        readWord16,
        readWord8,

        readInteger,
        readNatural,

        -- * I\/O with 'ByteString's
        -- | ByteString I/O uses binary mode, without any character decoding
        -- or newline conversion. The fact that it does not respect the Handle
        -- newline mode is considered a flaw and may be changed in a future version.

        -- ** Standard input and output
        getContents,
        putStr,
        putStrLn,
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
        hPutStrLn,

  ) where

-- Functions transparently exported
import Data.ByteString.Lazy
        (fromChunks, toChunks
        ,empty,null,length,tail,init,append,reverse,transpose,cycle
        ,concat,take,takeEnd,drop,dropEnd,splitAt,intercalate
        ,isPrefixOf,isSuffixOf,group,inits,tails,initsNE,tailsNE,copy
        ,stripPrefix,stripSuffix
        ,hGetContents, hGet, hPut, getContents
        ,hGetNonBlocking, hPutNonBlocking
        ,putStr, hPutStr, interact
        ,readFile,writeFile,appendFile,compareLength)

-- Functions we need to wrap.
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S (ByteString) -- typename only
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.List.NonEmpty (NonEmpty(..))
import Data.ByteString.Lazy.Internal
import Data.ByteString.Lazy.ReadInt
import Data.ByteString.Lazy.ReadNat

import Data.ByteString.Internal (c2w,w2c,isSpaceWord8)

import Data.Int (Int64)
import qualified Data.List as List

import Prelude hiding
        (reverse,head,tail,last,init,Foldable(..),map,lines,unlines
        ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,filter
        ,unwords,words,all,concatMap,scanl,scanl1,scanr,scanr1
        ,readFile,writeFile,appendFile,replicate,getContents,getLine,putStr,putStrLn
        ,zip,zipWith,unzip,notElem,repeat,iterate,interact,cycle)

import System.IO            (Handle, stdout)

------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'ByteString'
singleton :: Char -> ByteString
singleton = L.singleton . c2w
{-# INLINE singleton #-}

-- | /O(n)/ Convert a 'String' into a 'ByteString'.
pack :: [Char] -> ByteString
pack = packChars

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> [Char]
unpack = unpackChars

infixr 5 `cons`, `cons'` --same as list (:)
infixl 5 `snoc`

-- | /O(1)/ 'cons' is analogous to '(Prelude.:)' for lists.
cons :: Char -> ByteString -> ByteString
cons = L.cons . c2w
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
cons' :: Char -> ByteString -> ByteString
cons' = L.cons' . c2w
{-# INLINE cons' #-}

-- | /O(n)/ Append a Char to the end of a 'ByteString'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: ByteString -> Char -> ByteString
snoc p = L.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Char
head = w2c . L.head
{-# INLINE head #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ByteString -> Maybe (Char, ByteString)
uncons bs = case L.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')
{-# INLINE uncons #-}

-- | /O(n\/c)/ Extract the 'init' and 'last' of a ByteString, returning Nothing
-- if it is empty.
unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc bs = case L.unsnoc bs of
                  Nothing -> Nothing
                  Just (bs', w) -> Just (bs', w2c w)
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the last element of a packed string, which must be non-empty.
last :: ByteString -> Char
last = w2c . L.last
{-# INLINE last #-}

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> ByteString -> ByteString
map f = L.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'ByteString'
-- and \`intersperses\' that Char between the elements of the
-- 'ByteString'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> ByteString -> ByteString
intersperse = L.intersperse . c2w
{-# INLINE intersperse #-}

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = L.foldl (\a c -> f a (w2c c))
{-# INLINE foldl #-}

-- | 'foldl'' is like foldl, but strict in the accumulator.
foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = L.foldl' (\a c -> f a (w2c c))
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = L.foldr (f . w2c)
{-# INLINE foldr #-}

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
--
-- @since 0.11.2.0
foldr' :: (Char -> a -> a) -> a -> ByteString -> a
foldr' f = L.foldr' (f . w2c)

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteString's.
foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (L.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1 #-}

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
foldl1' :: (Char -> Char -> Char) -> ByteString -> Char
foldl1' f ps = w2c (L.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (L.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1 #-}

-- | 'foldr1'' is like 'foldr1', but strict in the accumulator.
--
-- @since 0.11.2.0
foldr1' :: (Char -> Char -> Char) -> ByteString -> Char
foldr1' f ps = w2c (L.foldr1' (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = L.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Char -> Bool) -> ByteString -> Bool
any f = L.any (f . w2c)
{-# INLINE any #-}

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f = L.all (f . w2c)
{-# INLINE all #-}

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Char
maximum = w2c . L.maximum
{-# INLINE maximum #-}

-- | 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Char
minimum = w2c . L.minimum
{-# INLINE minimum #-}

-- ---------------------------------------------------------------------
-- Building ByteStrings

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z = L.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
-- @since 0.11.2.0
scanl1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanl1 f = L.scanl1 f'
  where f' accumulator value = c2w (f (w2c accumulator) (w2c value))

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
    :: (Char -> Char -> Char)
    -- ^ element -> accumulator -> new accumulator
    -> Char
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanr f = L.scanr f' . c2w
  where f' accumulator value = c2w (f (w2c accumulator) (w2c value))

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
--
-- @since 0.11.2.0
scanr1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanr1 f = L.scanr1 f'
  where f' accumulator value = c2w (f (w2c accumulator) (w2c value))

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = L.mapAccumL (\a w -> case f a (w2c w) of (a',c) -> (a', c2w c))

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = L.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

------------------------------------------------------------------------
-- Generating and unfolding ByteStrings

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
iterate :: (Char -> Char) -> Char -> ByteString
iterate f = L.iterate (c2w . f . w2c) . c2w

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Char -> ByteString
repeat = L.repeat . c2w

-- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- the value of every element.
--
replicate :: Int64 -> Char -> ByteString
replicate w c = L.replicate w (c2w c)

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString
unfoldr f = L.unfoldr $ \a -> case f a of
                                    Nothing      -> Nothing
                                    Just (c, a') -> Just (c2w c, a')

------------------------------------------------------------------------

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = L.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- @since 0.11.2.0
takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd f = L.takeWhileEnd (f . w2c)
{-# INLINE takeWhileEnd #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = L.dropWhile (f . w2c)
{-# INLINE dropWhile #-}

-- | Similar to 'P.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.11.2.0
dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd f = L.dropWhileEnd (f . w2c)
{-# INLINE dropWhileEnd #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = L.break (f . w2c)
{-# INLINE break #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'ByteString'
--
-- breakEnd p == spanEnd (not.p)
--
-- @since 0.11.2.0
breakEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd f = L.breakEnd (f . w2c)
{-# INLINE breakEnd #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = L.span (f . w2c)
{-# INLINE span #-}

-- | 'spanEnd' behaves like 'span' but from the end of the 'ByteString'.
-- We have
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x)
--
-- @since 0.11.2.0
spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd f = L.spanEnd (f . w2c)
{-# INLINE spanEnd #-}

{-
-- | 'breakChar' breaks its ByteString argument at the first occurrence
-- of the specified Char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar = L.breakByte . c2w
{-# INLINE breakChar #-}

-- | 'spanChar' breaks its ByteString argument at the first
-- occurrence of a Char other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (=='c') "abcd" == spanByte 'c' "abcd"
--
spanChar :: Char -> ByteString -> (ByteString, ByteString)
spanChar = L.spanByte . c2w
{-# INLINE spanChar #-}
-}

--
-- TODO, more rules for breakChar*
--

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
-- > split 'x'  "x"          == ["",""]
-- > split undefined ""      == []  -- and not [""]
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
split :: Char -> ByteString -> [ByteString]
split = L.split . c2w
{-# INLINE split #-}

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
-- > splitWith undefined ""      == []  -- and not [""]
--
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = L.splitWith (f . w2c)
{-# INLINE splitWith #-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy k = L.groupBy (\a b -> k (w2c a) (w2c b))

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int64 -> Char
index = (w2c .) . L.index
{-# INLINE index #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ByteString -> Int64 -> Maybe Char
indexMaybe = (fmap w2c .) . L.indexMaybe
{-# INLINE indexMaybe #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ByteString -> Int64 -> Maybe Char
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> ByteString -> Maybe Int64
elemIndex = L.elemIndex . c2w
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs = case elemIndex c (reverse xs) of
-- >   Nothing -> Nothing
-- >   Just i  -> Just (length xs - 1 - i)
--
-- @since 0.11.1.0
elemIndexEnd :: Char -> ByteString -> Maybe Int64
elemIndexEnd = L.elemIndexEnd . c2w
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> ByteString -> [Int64]
elemIndices = L.elemIndices . c2w
{-# INLINE elemIndices #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString satisfying the predicate.
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int64
findIndex f = L.findIndex (f . w2c)
{-# INLINE findIndex #-}

-- | The 'findIndexEnd' function takes a predicate and a 'ByteString' and
-- returns the index of the last element in the ByteString
-- satisfying the predicate.
--
-- @since 0.11.1.0
findIndexEnd :: (Char -> Bool) -> ByteString -> Maybe Int64
findIndexEnd f = L.findIndexEnd (f . w2c)
{-# INLINE findIndexEnd #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> ByteString -> [Int64]
findIndices f = L.findIndices (f . w2c)
{-# INLINE findIndices #-}

-- | count returns the number of times its argument appears in the ByteString
--
-- > count      == length . elemIndices
-- > count '\n' == length . lines
--
-- But more efficiently than using length on the intermediate list.
count :: Char -> ByteString -> Int64
count c = L.count (c2w c)

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> ByteString -> Bool
elem c = L.elem (c2w c)
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> ByteString -> Bool
notElem c = L.notElem (c2w c)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = L.filter (f . w2c)
{-# INLINE filter #-}

-- | @since 0.10.12.0
partition :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
partition f = L.partition (f . w2c)
{-# INLINE partition #-}

{-
-- | /O(n)/ and /O(n\/c) space/ A first order equivalent of /filter .
-- (==)/, for the common case of filtering a single Char. It is more
-- efficient to use /filterChar/ in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> ByteString -> ByteString
filterChar c ps = replicate (count c ps) c
{-# INLINE filterChar #-}

{-# RULES
  "ByteString specialise filter (== x)" forall x.
      filter ((==) x) = filterChar x
  #-}

{-# RULES
  "ByteString specialise filter (== x)" forall x.
     filter (== x) = filterChar x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = w2c `fmap` L.find (f . w2c) ps
{-# INLINE find #-}

{-
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single Char. It is more efficient to use
-- filterChar in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> ByteString -> ByteString
filterChar c = L.filterByte (c2w c)
{-# INLINE filterChar #-}

-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single Char out of a list. It is more efficient
-- to use /filterNotChar/ in this case.
--
-- > filterNotChar == filter . (/=)
--
-- filterNotChar is around 3x faster, and uses much less space, than its
-- filter equivalent
--
filterNotChar :: Char -> ByteString -> ByteString
filterNotChar c = L.filterNotByte (c2w c)
{-# INLINE filterNotChar #-}
-}

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of Chars. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations, and so space
-- usage may be large for multi-megabyte ByteStrings
zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs
    | L.null ps || L.null qs = []
    | otherwise = (head ps, head qs) : zip (L.tail ps) (L.tail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = L.zipWith ((. w2c) . f . w2c)

-- | A specialised version of `zipWith` for the common case of a
-- simultaneous map over two ByteStrings, to build a 3rd.
--
-- @since 0.11.1.0
packZipWith :: (Char -> Char -> Char) -> ByteString -> ByteString -> ByteString
packZipWith f = L.packZipWith f'
    where
        f' c1 c2 = c2w $ f (w2c c1) (w2c c2)
{-# INLINE packZipWith #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of chars into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
--
-- @since 0.11.1.0
unzip :: [(Char, Char)] -> (ByteString, ByteString)
unzip ls = (pack (fmap fst ls), pack (fmap snd ls))
{-# INLINE unzip #-}

-- | 'lines' lazily splits a ByteString into a list of ByteStrings at
-- newline Chars (@'\\n'@). The resulting strings do not contain newlines.
-- The first chunk of the result is only strict in the first chunk of the
-- input.
--
-- Note that it __does not__ regard CR (@'\\r'@) as a newline character.
--
lines :: ByteString -> [ByteString]
lines Empty          = []
lines (Chunk c0 cs0) = unNE $! go c0 cs0
  where
    -- Natural NonEmpty -> List
    unNE :: NonEmpty a -> [a]
    unNE (a :| b) = a : b

    -- Strict in the first argument, lazy in the second.
    consNE :: ByteString -> NonEmpty ByteString -> NonEmpty ByteString
    consNE !a b = a :| (unNE $! b)

    -- Note invariant: The initial chunk is non-empty on input, and we
    -- need to be sure to maintain this in internal recursive calls.
    go :: S.ByteString -> ByteString -> NonEmpty ByteString
    go c cs = case B.elemIndex (c2w '\n') c of
        Just n
            | n1 <- n + 1
            , n1 < B.length c -> consNE c' $ go (B.unsafeDrop n1 c) cs
              -- 'c' was a multi-line chunk
            | otherwise       -> c' :| lines cs
              -- 'c' was a single-line chunk
          where
            !c' = chunk (B.unsafeTake n c) Empty

        -- Initial chunk with no new line becomes first chunk of
        -- first line of result, with the rest of the result lazy!
        -- In particular, we don't strictly pattern match on 'cs'.
        --
        -- We can form `Chunk c ...` because the invariant is maintained
        -- here and also by using `chunk` in the defintion of `c'` above.
        Nothing -> let ~(l:|ls) = lazyRest cs
                    in  Chunk c l :| ls
          where
            lazyRest :: ByteString -> NonEmpty ByteString
            lazyRest (Chunk c' cs') = go c' cs'
            lazyRest Empty          = Empty :| []

-- | 'unlines' joins lines, appending a terminating newline after each.
--
-- Equivalent to
--     @'concat' . Data.List.concatMap (\\x -> [x, 'singleton' \'\\n'])@.
unlines :: [ByteString] -> ByteString
unlines = List.foldr (\x t -> x `append` cons '\n' t) Empty

-- | 'words' breaks a ByteString up into a list of words, which
-- were delimited by Chars representing white space. And
--
-- > tokens isSpace = words
--
words :: ByteString -> [ByteString]
words = List.filter (not . L.null) . L.splitWith isSpaceWord8
{-# INLINE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [ByteString] -> ByteString
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | Write a ByteString to a handle, appending a newline byte.
--
-- The chunks will be
-- written one at a time, followed by a newline.
-- Other threads might write to the 'Handle' in between,
-- and hence 'hPutStrLn' alone is not suitable for concurrent writes.
--
hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h ps = hPut h ps >> hPut h (L.singleton 0x0a)

-- | Write a ByteString to 'stdout', appending a newline byte.
--
-- The chunks will be
-- written one at a time, followed by a newline.
-- Other threads might write to the 'stdout' in between,
-- and hence 'putStrLn' alone is not suitable for concurrent writes.
--
putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout
