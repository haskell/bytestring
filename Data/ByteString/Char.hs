{-# OPTIONS -cpp -O -fglasgow-exts -funbox-strict-fields #-}
--
-- Module      : Data.ByteString.Char
-- Copyright   : (c) Don Stewart 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
-- 

--
-- | Manipulate 'ByteString's using Char operations. All Chars will be
-- truncated to 8 bits. Operations on Chars will be marginally slower
-- than the corresponding operations on Word8s alone.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- > import qualified Data.ByteString.Char as P
--

module Data.ByteString.Char (

        module Data.ByteString, -- All of the list interface

        -- * Introducing and eliminating 'ByteString's
        packChar,               -- :: Char   -> ByteString
        pack,                   -- :: String -> ByteString
        unpack,                 -- :: ByteString -> String

        -- * Basic interface
        cons,                   -- :: Char -> ByteString -> ByteString
        snoc,                   -- :: Char -> ByteString -> ByteString
        head,                   -- :: ByteString -> Char
        last,                   -- :: ByteString -> Char

        -- * Transformating ByteStrings
        map,                    -- :: (Char -> Char) -> ByteString -> ByteString
        intersperse,            -- :: Char -> ByteString -> ByteString

        -- * Reducing 'ByteString's
        foldl,                  -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldr,                  -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldr1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char

        -- ** Special folds
        concatMap,              -- :: (Char -> ByteString) -> ByteString -> ByteString
        any,                    -- :: (Char -> Bool) -> ByteString -> Bool
        all,                    -- :: (Char -> Bool) -> ByteString -> Bool
        maximum,                -- :: ByteString -> Char
        minimum,                -- :: ByteString -> Char
        mapIndexed,             -- :: (Int -> Char -> Char) -> ByteString -> ByteString

        -- * Generating and unfolding ByteStrings
        replicate,              -- :: Int -> Char -> ByteString
        unfoldrN,               -- :: (Char -> Maybe (Char, Char)) -> Char -> ByteString

        -- * Substrings

        -- ** Breaking strings
        takeWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        break,                  -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        span,                   -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        spanEnd,                -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)

        -- ** Breaking and dropping on specific bytes
        breakChar,              -- :: Char -> ByteString -> (ByteString, ByteString)
        breakFirst,             -- :: Char -> ByteString -> Maybe (ByteString,ByteString)
        breakLast,              -- :: Char -> ByteString -> Maybe (ByteString,ByteString)

        -- ** Breaking into many substrings
        split,                  -- :: Char -> ByteString -> [ByteString]
        splitWith,              -- :: (Char -> Bool) -> ByteString -> [ByteString]
        tokens,                 -- :: (Char -> Bool) -> ByteString -> [ByteString]

        -- ** Joining strings
        joinWithChar,           -- :: Char -> ByteString -> ByteString -> ByteString

        -- * Indexing ByteStrings
        index,                  -- :: ByteString -> Int -> Char
        elemIndex,              -- :: Char -> ByteString -> Maybe Int
        elemIndexLast,          -- :: Char -> ByteString -> Maybe Int
        elemIndices,            -- :: Char -> ByteString -> [Int]
        findIndex,              -- :: (Char -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Char -> Bool) -> ByteString -> [Int]

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,                   -- :: Char -> ByteString -> Bool
        notElem,                -- :: Char -> ByteString -> Bool
        filterChar,             -- :: Char -> ByteString -> ByteString
        filterNotChar,          -- :: Char -> ByteString -> ByteString

        -- ** Searching with a predicate
        filter,                 -- :: (Char -> Bool) -> ByteString -> ByteString
        find,                   -- :: (Char -> Bool) -> ByteString -> Maybe Char

        -- * Zipping and unzipping ByteString
        zip,                    -- :: ByteString -> ByteString -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> ByteString -> ByteString -> [c]
        unzip,                  -- :: [(Char,Char)] -> (ByteString,ByteString)

        -- * Unchecked access
        unsafeHead,             -- :: ByteString -> Char
        unsafeIndex,            -- :: ByteString -> Int -> Char
        w2c,                    -- :: Word8 -> Char
        c2w,                    -- :: Char  -> Word8

    ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,null
                                ,length,map,lines,foldl,foldr,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,elem,filter,unwords
                                ,words,maximum,minimum,all,concatMap
                                ,foldl1,foldr1,readFile,writeFile,replicate
                                ,getContents,getLine,putStr,putStrLn
                                ,zip,zipWith,unzip,notElem)

import qualified Data.ByteString as B

-- Listy functions transparently exported
import Data.ByteString (ByteString
                       ,empty,null,length,tail,init,append
                       ,inits,tails,elems,reverse,transpose
                       ,concat,hash,take,drop,splitAt,breakSpace,
                       ,dropSpace,dropSpaceEnd,lines,words,unlines
                       ,unwords,lines',unlines',linesCRLF',unlinesCRLF'
                       ,words',unwords',lineIndices,betweenLines,join
                       ,sort,isPrefixOf,isSuffixOf,isSubstringOf,findSubstring
                       ,findSubstrings,unsafeTail,readInt,unsafeReadInt,copy

                       ,getContents, putStr, putStrLn,
                       ,readFile, mmapFile, writeFile,
                       ,hGetContents, hGet, hPut, hGetNonBlocking,
#if defined(__GLASGOW_HASKELL__)
                       ,getLine, getArgs, hGetLine
#endif
                       )

import Data.Word
import Data.Char (ord)

#if defined(__GLASGOW_HASKELL__)
import GHC.Base (unsafeChr,unpackCString#)
#endif

#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined

------------------------------------------------------------------------

-- | /O(1)/ Convert a 'Char' into a 'ByteString'
packChar :: Char -> ByteString
packChar = B.packByte . c2w

-- | /O(n)/ Convert a 'String' into a 'ByteString'
pack :: String -> ByteString
pack = B.packWith c2w

{-# RULES
"pack/packAddress" forall s# .
                   pack (unpackCString# s#) = B.packAddress s#
 #-}

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> [Char]
unpack = B.unpackWith w2c

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Char -> ByteString -> ByteString
cons c p = B.cons (c2w c) p

-- | /O(n)/ Append a Char to the end of a 'ByteString'
snoc :: ByteString -> Char -> ByteString
snoc p c = B.snoc p (c2w c)

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Char
head = w2c . B.head

-- | /O(1)/ Extract the last element of a packed string, which must be finite and non-empty.
last :: ByteString -> Char
last = w2c . B.last

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> ByteString -> ByteString
map f = B.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'ByteString'
-- and \`intersperses\' that Char between the elements of the
-- 'ByteString'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> ByteString -> ByteString
intersperse c = B.intersperse (c2w c)

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f = B.foldl (\a c -> f a (w2c c))

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr f = B.foldr (\c a -> f (w2c c) a)

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = B.concatMap (f . w2c)

-- | Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Char -> Bool) -> ByteString -> Bool
any f = B.any (f . w2c)

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f = B.all (f . w2c)

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Char
maximum = w2c . B.maximum

-- | 'minimum' returns the maximum value from a 'ByteString'
minimum :: ByteString -> Char
minimum = w2c . B.minimum

-- | /O(n)/ map Char functions, provided with the index at each position
mapIndexed :: (Int -> Char -> Char) -> ByteString -> ByteString
mapIndexed f ps = B.mapIndexed (\i c -> c2w (f i (w2c c))) ps

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Char -> ByteString
replicate w c = B.replicate w (c2w c)

-- | /O(n)/ The 'unfoldrN' function is analogous to the List \'unfoldr\'.
-- 'unfoldrN' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
--
-- To preven unfoldrN having /O(n^2)/ complexity (as prepending a
-- character to a ByteString is /O(n)/, this unfoldr requires a maximum
-- final size of the ByteString as an argument. 'cons' can then be
-- implemented in /O(1)/ (i.e.  a 'poke'), and the unfoldr itself has
-- linear complexity. The depth of the recursion is limited to this
-- size, but may be less. For lazy, infinite unfoldr, use
-- 'Data.List.unfoldr' (from 'Data.List').
--
-- Examples:
--
-- > unfoldrN 10 (\x -> Just (x, chr (ord x + 1))) '0' == "0123456789"
--
-- The following equation connects the depth-limited unfoldr to the List unfoldr:
--
-- > unfoldrN n == take n $ List.unfoldr
--
unfoldrN :: Int -> (Char -> Maybe (Char, Char)) -> Char -> ByteString
unfoldrN n f w = B.unfoldrN n f' (c2w w)
    where f' c = case f (w2c c) of
                    Nothing    -> Nothing
                    Just (i,j) -> Just ((c2w i),(c2w j))

-- | 'takeWhile', applied to a predicate @p@ and a ByteString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f = B.takeWhile (f . w2c)

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f = B.dropWhile (f . w2c)

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break f = B.break (f . w2c)

-- | 'span' @p xs@ breaks the ByteString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span f = B.span (f . w2c)

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

-- | 'breakChar' breaks its ByteString argument at the first occurence
-- of the specified Char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> ByteString -> (ByteString, ByteString)
breakChar c = B.breakByte (c2w c)

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
breakFirst :: Char -> ByteString -> Maybe (ByteString,ByteString)
breakFirst c = B.breakFirst (c2w c)

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
breakLast :: Char -> ByteString -> Maybe (ByteString,ByteString)
breakLast  c = B.breakLast (c2w c)

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
split :: Char -> ByteString -> [ByteString]
split c = B.split (c2w c)

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith f = B.splitWith (f . w2c)

-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> ByteString -> [ByteString]
tokens f = B.tokens (f . w2c)

-- | /O(n)/ joinWithChar. An efficient way to join to two ByteStrings with a
-- char. Around 4 times faster than the generalised join.
--
joinWithChar :: Char -> ByteString -> ByteString -> ByteString
joinWithChar c = B.joinWithByte (c2w c)

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Char
index p i = w2c (B.index p i)

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> ByteString -> Maybe Int
elemIndex c = B.elemIndex (c2w c)

-- | /O(n)/ The 'elemIndexLast' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexLast c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexLast :: Char -> ByteString -> Maybe Int
elemIndexLast c = B.elemIndexLast (c2w c)

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> ByteString -> [Int]
elemIndices c = B.elemIndices (c2w c)

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString satisfying the predicate.
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex f = B.findIndex (f . w2c)

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices f = B.findIndices (f . w2c)

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> ByteString -> Bool
elem    c = B.elem (c2w c)

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> ByteString -> Bool
notElem c = B.notElem (c2w c)

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> ByteString -> ByteString
filter f = B.filter (f . w2c)

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> ByteString -> Maybe Char
find f ps = case B.find (f . w2c) ps of
                Nothing -> Nothing
                Just w  -> Just (w2c w)

-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single Char. It is more efficient to use
-- filterChar in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 3x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> ByteString -> ByteString
filterChar    c = B.filterByte (c2w c)

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

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of Chars. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs
    | B.null ps || B.null qs = []
    | otherwise = (unsafeHead ps, unsafeHead qs) : zip (B.unsafeTail ps) (B.unsafeTail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f = B.zipWith (\c d -> f (w2c c) (w2c d))

-- | 'unzip' transforms a list of pairs of Chars into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Char,Char)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits
-- the check for the empty case, which is good for performance, but
-- there is an obligation on the programmer to provide a proof that the
-- ByteString is non-empty.
unsafeHead :: ByteString -> Char
unsafeHead  = w2c . B.unsafeHead

-- | Unsafe 'ByteString' index (subscript) operator, starting from 0, returning a Char.
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: ByteString -> Int -> Char
unsafeIndex p i = w2c (B.unsafeIndex p i)

-- | Conversion between 'Word8' and 'Char'
w2c :: Word8 -> Char
#if !defined(__GLASGOW_HASKELL__)
w2c = chr . fromIntegral
#else
w2c = unsafeChr . fromIntegral
#endif
{-# INLINE w2c #-}

-- | Conversion between 'Word8' and 'Char'
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}
