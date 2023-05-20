{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Trustworthy #-}

-- |
-- Module      : Data.ByteString.Char8
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- Manipulate 'ByteString's using 'Char' operations. All Chars will be
-- truncated to 8 bits. It can be expected that these functions will run
-- at identical speeds to their 'Word8' equivalents in "Data.ByteString".
--
-- More specifically these byte strings are taken to be in the
-- subset of Unicode covered by code points 0-255. This covers
-- Unicode Basic Latin, Latin-1 Supplement and C0+C1 Controls.
--
-- See:
--
--  * <http://www.unicode.org/charts/>
--
--  * <http://www.unicode.org/charts/PDF/U0000.pdf>
--
--  * <http://www.unicode.org/charts/PDF/U0080.pdf>
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Char8 as C
--
-- The Char8 interface to bytestrings provides an instance of IsString
-- for the ByteString type, enabling you to use string literals, and
-- have them implicitly packed to ByteStrings.
-- Use @{-\# LANGUAGE OverloadedStrings \#-}@ to enable this.
--

module Data.ByteString.Char8 (

        -- * The @ByteString@ type
        ByteString,

        -- * Introducing and eliminating 'ByteString's
        empty,
        singleton,
        pack,
        unpack,
        B.fromStrict,
        B.toStrict,

        -- * Basic interface
        cons,
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

        -- * Building ByteStrings
        -- ** Scans
        scanl,
        scanl1,
        scanr,
        scanr1,

        -- ** Accumulating maps
        mapAccumL,
        mapAccumR,

        -- ** Generating and unfolding ByteStrings
        replicate,
        unfoldr,
        unfoldrN,

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
        dropSpace,
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
        strip,
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
        isInfixOf,

        -- ** Search for arbitrary substrings
        breakSubstring,

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
        elemIndices,
        elemIndexEnd,
        findIndex,
        findIndices,
        findIndexEnd,
        count,

        -- * Zipping and unzipping ByteStrings
        zip,
        zipWith,
        packZipWith,
        unzip,

        -- * Ordered ByteStrings
        sort,

        -- * Reading from ByteStrings
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

        -- * Low level CString conversions

        -- ** Copying ByteStrings
        copy,

        -- ** Packing CStrings and pointers
        packCString,
        packCStringLen,

        -- ** Using ByteStrings as CStrings
        useAsCString,
        useAsCStringLen,

        -- * I\/O with 'ByteString's
        -- | ByteString I/O uses binary mode, without any character decoding
        -- or newline conversion. The fact that it does not respect the Handle
        -- newline mode is considered a flaw and may be changed in a future version.

        -- ** Standard input and output
        getLine,
        getContents,
        putStr,
        putStrLn,
        interact,

        -- ** Files
        readFile,
        writeFile,
        appendFile,
--      mmapFile,

        -- ** I\/O with Handles
        hGetLine,
        hGetContents,
        hGet,
        hGetSome,
        hGetNonBlocking,
        hPut,
        hPutNonBlocking,
        hPutStr,
        hPutStrLn,

  ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,Foldable(..)
                                ,map,lines,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,filter,unwords
                                ,words,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,appendFile,readFile,writeFile
                                ,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

-- Listy functions transparently exported
import Data.ByteString (null,length,tail,init,append
                       ,inits,tails,initsNE,tailsNE,reverse,transpose
                       ,concat,take,takeEnd,drop,dropEnd,splitAt
                       ,intercalate,sort,isPrefixOf,isSuffixOf
                       ,isInfixOf,stripPrefix,stripSuffix
                       ,breakSubstring,copy,group

                       ,getLine, getContents, putStr, interact
                       ,readFile, writeFile, appendFile
                       ,hGetContents, hGet, hGetSome, hPut, hPutStr
                       ,hGetLine, hGetNonBlocking, hPutNonBlocking
                       ,packCString,packCStringLen
                       ,useAsCString,useAsCStringLen
                       )

import Data.ByteString.Internal.Type
import Data.ByteString.ReadInt
import Data.ByteString.ReadNat

import Data.Char    ( isSpace )
-- See bytestring #70
import GHC.Char (eqChar)
import qualified Data.List as List

import System.IO    (Handle,stdout)
import Foreign


------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'ByteString'
singleton :: Char -> ByteString
singleton = B.singleton . c2w
{-# INLINE singleton #-}

-- | /O(n)/ Convert a 'String' into a 'ByteString'
--
-- For applications with large numbers of string literals, pack can be a
-- bottleneck.
pack :: String -> ByteString
pack = packChars
{-# INLINE pack #-}

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> [Char]
unpack = B.unpackChars
{-# INLINE unpack #-}

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Char -> ByteString -> ByteString
cons = B.cons . c2w
{-# INLINE cons #-}

-- | /O(n)/ Append a Char to the end of a 'ByteString'. Similar to
-- 'cons', this function performs a memcpy.
snoc :: ByteString -> Char -> ByteString
snoc p = B.snoc p . c2w
{-# INLINE snoc #-}

-- | /O(1)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
uncons :: ByteString -> Maybe (Char, ByteString)
uncons bs = case B.uncons bs of
                  Nothing -> Nothing
                  Just (w, bs') -> Just (w2c w, bs')
{-# INLINE uncons #-}

-- | /O(1)/ Extract the 'init' and 'last' of a ByteString, returning Nothing
-- if it is empty.
unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc bs = case B.unsnoc bs of
                  Nothing -> Nothing
                  Just (bs', w) -> Just (bs', w2c w)
{-# INLINE unsnoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Char
head = w2c . B.head
{-# INLINE head #-}

-- | /O(1)/ Extract the last element of a packed string, which must be non-empty.
last :: ByteString -> Char
last = w2c . B.last
{-# INLINE last #-}

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> ByteString -> ByteString
map f = B.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'ByteString'
-- and \`intersperses\' that Char between the elements of the
-- 'ByteString'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> ByteString -> ByteString
intersperse = B.intersperse . c2w
{-# INLINE intersperse #-}

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = B.foldl (\a c -> f a (w2c c))
{-# INLINE foldl #-}

-- | 'foldl'' is like foldl, but strict in the accumulator.
foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = B.foldl' (\a c -> f a (w2c c))
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = B.foldr (f . w2c)
{-# INLINE foldr #-}

-- | 'foldr'' is a strict variant of foldr
foldr' :: (Char -> a -> a) -> a -> ByteString -> a
foldr' f = B.foldr' (f . w2c)
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteString's.
foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1 #-}

-- | A strict version of 'foldl1'
foldl1' :: (Char -> Char -> Char) -> ByteString -> Char
foldl1' f ps = w2c (B.foldl1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (B.foldr1 (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1 #-}

-- | A strict variant of foldr1
foldr1' :: (Char -> Char -> Char) -> ByteString -> Char
foldr1' f ps = w2c (B.foldr1' (\x y -> c2w (f (w2c x) (w2c y))) ps)
{-# INLINE foldr1' #-}

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = B.concatMap (f . w2c)
{-# INLINE concatMap #-}

-- | Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Char -> Bool) -> ByteString -> Bool
any f = B.any (f . w2c)
{-# INLINE any #-}

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f = B.all (f . w2c)
{-# INLINE all #-}

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Char
maximum = w2c . B.maximum
{-# INLINE maximum #-}

-- | 'minimum' returns the minimum value from a 'ByteString'
minimum :: ByteString -> Char
minimum = w2c . B.minimum
{-# INLINE minimum #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = B.mapAccumL (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Char -> (acc, Char)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = B.mapAccumR (\acc w -> case f acc (w2c w) of (acc', c) -> (acc', c2w c))

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanl f z = B.scanl (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanl1 f = B.scanl1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | scanr is the right-to-left dual of scanl.
scanr :: (Char -> Char -> Char) -> Char -> ByteString -> ByteString
scanr f z = B.scanr (\a b -> c2w (f (w2c a) (w2c b))) (c2w z)

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Char -> Char -> Char) -> ByteString -> ByteString
scanr1 f = B.scanr1 (\a b -> c2w (f (w2c a) (w2c b)))

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implementation uses @memset(3)@
replicate :: Int -> Char -> ByteString
replicate n = B.replicate n . c2w
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next character in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- > unfoldr (\x -> if x <= '9' then Just (x, succ x) else Nothing) '0' == "0123456789"
unfoldr :: (a -> Maybe (Char, a)) -> a -> ByteString
unfoldr f = B.unfoldr (fmap k . f)
    where k (i, j) = (c2w i, j)

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > unfoldrN n f s == take n (unfoldr f s)
unfoldrN :: Int -> (a -> Maybe (Char, a)) -> a -> (ByteString, Maybe a)
unfoldrN n f = B.unfoldrN n ((k `fmap`) . f)
    where k (i,j) = (c2w i, j)
{-# INLINE unfoldrN #-}

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = B.takeWhile (f . w2c)
{-# INLINE takeWhile #-}

-- | 'takeWhileEnd', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest suffix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
--
-- @since 0.10.12.0
takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd f = B.takeWhileEnd (f . w2c)
{-# INLINE takeWhileEnd #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = B.dropWhile (f . w2c)
{-# INLINE [1] dropWhile #-}

{-# RULES
"ByteString specialise dropWhile isSpace -> dropSpace"
    dropWhile isSpace = dropSpace
  #-}

-- | 'dropWhileEnd' @p xs@ returns the prefix remaining after 'takeWhileEnd' @p
-- xs@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd f = B.dropWhileEnd (f . w2c)
{-# INLINE dropWhileEnd #-}

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = B.break (f . w2c)
{-# INLINE [1] break #-}

-- See bytestring #70
{-# RULES
"ByteString specialise break (x==)" forall x.
    break (x `eqChar`) = breakChar x
"ByteString specialise break (==x)" forall x.
    break (`eqChar` x) = breakChar x
  #-}

-- INTERNAL:

-- | 'breakChar' breaks its ByteString argument at the first occurrence
-- of the specified char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (B.unsafeTake n p, B.unsafeDrop n p)
{-# INLINE breakChar #-}

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = B.span (f . w2c)
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
spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd f = B.spanEnd (f . w2c)
{-# INLINE spanEnd #-}

-- | 'breakEnd' behaves like 'break' but from the end of the 'ByteString'
--
-- breakEnd p == spanEnd (not.p)
breakEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd f = B.breakEnd (f . w2c)
{-# INLINE breakEnd #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
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
split = B.split . c2w
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
splitWith f = B.splitWith (f . w2c)
{-# INLINE splitWith #-}
-- the inline makes a big difference here.

{-
-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
--
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> ByteString -> [ByteString]
tokens f = B.tokens (f . w2c)
{-# INLINE tokens #-}
-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> ByteString -> [ByteString]
groupBy k = B.groupBy (\a b -> k (w2c a) (w2c b))

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Char
index = (w2c .) . B.index
{-# INLINE index #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ByteString -> Int -> Maybe Char
indexMaybe = (fmap w2c .) . B.indexMaybe
{-# INLINE indexMaybe #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ByteString -> Int -> Maybe Char
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> ByteString -> Maybe Int
elemIndex = B.elemIndex . c2w
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
elemIndexEnd :: Char -> ByteString -> Maybe Int
elemIndexEnd = B.elemIndexEnd . c2w
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> ByteString -> [Int]
elemIndices = B.elemIndices . c2w
{-# INLINE elemIndices #-}

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString satisfying the predicate.
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex f = B.findIndex (f . w2c)
{-# INLINE [1] findIndex #-}

-- | /O(n)/ The 'findIndexEnd' function takes a predicate and a 'ByteString' and
-- returns the index of the last element in the ByteString
-- satisfying the predicate.
--
-- @since 0.11.1.0
findIndexEnd :: (Char -> Bool) -> ByteString -> Maybe Int
findIndexEnd f = B.findIndexEnd (f . w2c)
{-# INLINE [1] findIndexEnd #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices f = B.findIndices (f . w2c)
{-# INLINE [1] findIndices #-}

{-# RULES
"ByteString specialise findIndex (x==)" forall x.
    findIndex (x `eqChar`) = elemIndex x
"ByteString specialise findIndex (==x)" forall x.
    findIndex (`eqChar` x) = elemIndex x
"ByteString specialise findIndices (x==)" forall x.
    findIndices (x `eqChar`) = elemIndices x
"ByteString specialise findIndices (==x)" forall x.
    findIndices (`eqChar` x) = elemIndices x
  #-}


-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- Also
--
-- > count '\n' == length . lines
--
-- But more efficiently than using length on the intermediate list.
count :: Char -> ByteString -> Int
count c = B.count (c2w c)

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> ByteString -> Bool
elem    c = B.elem (c2w c)
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> ByteString -> Bool
notElem c = B.notElem (c2w c)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = B.filter (f . w2c)
{-# INLINE filter #-}

-- | @since 0.10.12.0
partition :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
partition f = B.partition (f . w2c)
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
"ByteString specialise filter (== x)" forall x.
    filter (== x) = filterChar x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = w2c `fmap` B.find (f . w2c) ps
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
filterChar c = B.filterByte (c2w c)
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
filterNotChar c = B.filterNotByte (c2w c)
{-# INLINE filterNotChar #-}
-}

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of Chars. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations, and so space
-- usage may be large for multi-megabyte ByteStrings
zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> (psH, qsH) : zip psT qsT

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = B.zipWith ((. w2c) . f . w2c)

-- | A specialised version of `zipWith` for the common case of a
-- simultaneous map over two ByteStrings, to build a 3rd.
--
-- @since 0.11.1.0
packZipWith :: (Char -> Char -> Char) -> ByteString -> ByteString -> ByteString
packZipWith f = B.packZipWith f'
    where
        f' c1 c2 = c2w $ f (w2c c1) (w2c c2)
{-# INLINE packZipWith #-}

-- | 'unzip' transforms a list of pairs of Chars into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Char,Char)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Things that depend on the encoding

{-# RULES
"ByteString specialise break -> breakSpace"
    break isSpace = breakSpace
  #-}

-- | 'breakSpace' returns the pair of ByteStrings when the argument is
-- broken at the first whitespace byte. I.e.
--
-- > break isSpace == breakSpace
--
breakSpace :: ByteString -> (ByteString,ByteString)
breakSpace (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    i <- firstspace p 0 l
    return $! case () of {_
        | i == 0    -> (empty, BS x l)
        | i == l    -> (BS x l, empty)
        | otherwise -> (BS x i, BS (plusForeignPtr x i) (l-i))
    }
{-# INLINE breakSpace #-}

firstspace :: Ptr Word8 -> Int -> Int -> IO Int
firstspace !ptr !n !m
    | n >= m    = return n
    | otherwise = do w <- peekByteOff ptr n
                     if (not . isSpaceWord8) w then firstspace ptr (n+1) m else return n

-- | 'dropSpace' efficiently returns the 'ByteString' argument with
-- white space Chars removed from the front. It is more efficient than
-- calling dropWhile for removing whitespace. I.e.
--
-- > dropWhile isSpace == dropSpace
--
-- @since 0.10.12.0
dropSpace :: ByteString -> ByteString
dropSpace (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    i <- firstnonspace p 0 l
    return $! if i == l then empty else BS (plusForeignPtr x i) (l-i)
{-# INLINE dropSpace #-}

firstnonspace :: Ptr Word8 -> Int -> Int -> IO Int
firstnonspace !ptr !n !m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then firstnonspace ptr (n+1) m else return n

-- | Remove leading and trailing white space from a 'ByteString'.
--
-- @since 0.10.12.0
strip :: ByteString -> ByteString
strip = dropWhile isSpace . dropWhileEnd isSpace

{-
-- | 'dropSpaceEnd' efficiently returns the 'ByteString' argument with
-- white space removed from the end. I.e.
--
-- > reverse . (dropWhile isSpace) . reverse == dropSpaceEnd
--
-- but it is more efficient than using multiple reverses.
--
dropSpaceEnd :: ByteString -> ByteString
dropSpaceEnd (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    i <- lastnonspace p (l-1)
    return $! if i == (-1) then empty else BS x (i+1)
{-# INLINE dropSpaceEnd #-}

lastnonspace :: Ptr Word8 -> Int -> IO Int
lastnonspace ptr n
    | n < 0     = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then lastnonspace ptr (n-1) else return n
-}

-- | 'lines' breaks a ByteString up into a list of ByteStrings at
-- newline Chars (@'\\n'@). The resulting strings do not contain newlines.
--
-- Note that it __does not__ regard CR (@'\\r'@) as a newline character.
--
lines :: ByteString -> [ByteString]
lines ps
    | null ps = []
    | otherwise = case search ps of
             Nothing -> [ps]
             Just n  -> take n ps : lines (drop (n+1) ps)
    where search = elemIndex '\n'

{-
-- Could be faster, now passes tests...
lines (BS _ 0) = []
lines (BS x l) = go x l
  where
    nl = c2w '\n'
    -- It is important to remain lazy in the tail of the list.  The caller
    -- might only want the first few lines.
    go !f !len = accursedUnutterablePerformIO $ unsafeWithForeignPtr f $ \p -> do
        q <- memchr p nl $! fromIntegral len
        if q == nullPtr
            then return [BS f len]
            else do
                let !i = q `minusPtr` p
                    !j = i + 1
                if j < len
                    then return $ BS f i : go (plusForeignPtr f j) (len - j)
                    else return [BS f i]
-}

-- | 'unlines' joins lines, appending a terminating newline after each.
--
-- Equivalent to
--     @'concat' . Data.List.concatMap (\\x -> [x, 'singleton' \'\\n'])@.
unlines :: [ByteString] -> ByteString
unlines = \li -> let
  totLen = List.foldl' (\acc s -> acc +! length s +! 1) 0 li
  (+!) = checkedAdd "Char8.unlines"

  go [] _ = pure ()
  go (BS src len : srcs) dest = do
    memcpyFp dest src len
    pokeFpByteOff dest len (c2w '\n')
    go srcs $ dest `plusForeignPtr` (len + 1)
  in  unsafeCreateFp totLen (go li)

-- | 'words' breaks a ByteString up into a list of words, which
-- were delimited by Chars representing white space.
words :: ByteString -> [ByteString]
words = P.filter (not . B.null) . B.splitWith isSpaceWord8
{-# INLINE words #-}

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [ByteString] -> ByteString
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

------------------------------------------------------------------------
-- For non-binary text processing:

-- | Write a ByteString to a handle, appending a newline byte.
--
-- Unlike 'hPutStr', this is not atomic: other threads might write
-- to the handle between writing of the bytestring and the newline.
--
hPutStrLn :: Handle -> ByteString -> IO ()
hPutStrLn h ps
    | length ps < 1024 = hPut h (ps `B.snoc` 0x0a)
    | otherwise        = hPut h ps >> hPut h (B.singleton 0x0a) -- don't copy

-- | Write a ByteString to 'stdout', appending a newline byte.
--
-- Unlike 'putStr', this is not atomic: other threads might write
-- to 'stdout' between writing of the bytestring and the newline.
--
putStrLn :: ByteString -> IO ()
putStrLn = hPutStrLn stdout
