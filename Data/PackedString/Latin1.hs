{-# OPTIONS -cpp -O -fglasgow-exts #-}
--
-- Module      : Latin1.hs
-- Copyright   : (c) Don Stewart 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
-- 

--
-- Latin1 strings represented as packed vectors of 8-bit characters.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- > import qualified Data.PackedString.Latin1 as P
--

module Data.PackedString.Latin1 (

        -- * The @PackedString@ type
        PackedString,           -- instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'PackedString's
        empty,                  -- :: PackedString
        packChar,               -- :: Char   -> PackedString
        pack,                   -- :: String -> PackedString
        unpack,                 -- :: PackedString -> String

        -- * Basic interface
        cons,                   -- :: Char -> PackedString -> PackedString
        snoc,                   -- :: Char -> PackedString -> PackedString
        null,                   -- :: PackedString -> Bool
        length,                 -- :: PackedString -> Int
        head,                   -- :: PackedString -> Char
        tail,                   -- :: PackedString -> PackedString
        last,                   -- :: PackedString -> Char
        init,                   -- :: PackedString -> PackedString
        append,                 -- :: PackedString -> PackedString -> PackedString

        -- * Special PackedStrings
        inits,                  -- :: PackedString -> [PackedString]
        tails,                  -- :: PackedString -> [PackedString]
        elems,                  -- :: PackedString -> [PackedString]

        -- * Transformating PackedStrings
        map,                    -- :: (Char -> Char) -> PackedString -> PackedString
        reverse,                -- :: PackedString -> PackedString
        intersperse,            -- :: Char -> PackedString -> PackedString
        transpose,              -- :: [PackedString] -> [PackedString]

        -- * Reducing 'PackedString's
        foldl,                  -- :: (a -> Char -> a) -> a -> PackedString -> a
        foldr,                  -- :: (Char -> a -> a) -> a -> PackedString -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> PackedString -> Char
        foldr1,                 -- :: (Char -> Char -> Char) -> PackedString -> Char

        -- ** Special folds
        concat,                 -- :: [PackedString] -> PackedString
        concatMap,              -- :: (Char -> PackedString) -> PackedString -> PackedString
        any,                    -- :: (Char -> Bool) -> PackedString -> Bool
        all,                    -- :: (Char -> Bool) -> PackedString -> Bool
        maximum,                -- :: PackedString -> Char
        minimum,                -- :: PackedString -> Char
        mapIndexed,             -- :: (Int -> Char -> Char) -> PackedString -> PackedString
        hash,                   -- :: PackedString -> Int32

        -- * Generating and unfolding PackedStrings
        replicate,              -- :: Int -> Char -> PackedString
        unfoldrN,               -- :: (Char -> Maybe (Char, Char)) -> Char -> PackedString

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> PackedString -> PackedString
        drop,                   -- :: Int -> PackedString -> PackedString
        splitAt,                -- :: Int -> PackedString -> (PackedString, PackedString)
        takeWhile,              -- :: (Char -> Bool) -> PackedString -> PackedString
        dropWhile,              -- :: (Char -> Bool) -> PackedString -> PackedString
        break,                  -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
        span,                   -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
        spanEnd,                -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)

        -- ** Breaking and dropping on specific bytes
        breakChar,              -- :: Char -> PackedString -> (PackedString, PackedString)
        breakFirst,             -- :: Char -> PackedString -> Maybe (PackedString,PackedString)
        breakLast,              -- :: Char -> PackedString -> Maybe (PackedString,PackedString)
        breakSpace,             -- :: PackedString -> Maybe (PackedString,PackedString)
        dropSpace,              -- :: PackedString -> PackedString
        dropSpaceEnd,           -- :: PackedString -> PackedString

        -- ** Breaking into many substrings
        split,                  -- :: Char -> PackedString -> [PackedString]
        splitWith,              -- :: (Char -> Bool) -> PackedString -> [PackedString]
        tokens,                 -- :: (Char -> Bool) -> PackedString -> [PackedString]

        -- ** Breaking into lines and words
        lines,                  -- :: PackedString -> [PackedString]
        words,                  -- :: PackedString -> [PackedString]
        unlines,                -- :: [PackedString] -> PackedString
        unwords,                -- :: PackedString -> [PackedString]

        lines',                 -- :: PackedString -> [PackedString]
        unlines',               -- :: [PackedString] -> PackedString
        linesCRLF',             -- :: PackedString -> [PackedString]
        unlinesCRLF',           -- :: [PackedString] -> PackedString
        words',                 -- :: PackedString -> [PackedString]
        unwords',               -- :: PackedString -> [PackedString]

        lineIndices,            -- :: PackedString -> [Int]
        betweenLines,           -- :: PackedString -> PackedString -> PackedString 
                                -- -> Maybe (PackedString)

        -- ** Joining strings
        join,                   -- :: PackedString -> [PackedString] -> PackedString
        joinWithChar,           -- :: Char -> PackedString -> PackedString -> PackedString

        -- * Indexing PackedStrings
        index,                  -- :: PackedString -> Int -> Char
        elemIndex,              -- :: Char -> PackedString -> Maybe Int
        elemIndexLast,          -- :: Char -> PackedString -> Maybe Int
        elemIndices,            -- :: Char -> PackedString -> [Int]
        findIndex,              -- :: (Char -> Bool) -> PackedString -> Maybe Int
        findIndices,            -- :: (Char -> Bool) -> PackedString -> [Int]

        -- * Ordered PackedStrings
        sort,                   -- :: PackedString -> PackedString

        -- * Searching PackedStrings

        -- ** Searching by equality
        elem,                   -- :: Char -> PackedString -> Bool
        notElem,                -- :: Char -> PackedString -> Bool

        -- ** Searching with a predicate
        filter,                 -- :: (Char -> Bool) -> PackedString -> PackedString
        find,                   -- :: (Char -> Bool) -> PackedString -> Maybe Word8
        filterChar,             -- :: Char -> PackedString -> PackedString
        filterNotChar,          -- :: Char -> PackedString -> PackedString

        -- ** Searching for substrings
        isPrefixOf,             -- :: PackedString -> PackedString -> Bool
        isSuffixOf,             -- :: PackedString -> PackedString -> Bool
        isSubstringOf,          -- :: PackedString -> PackedString -> Bool
        findSubstring,          -- :: PackedString -> PackedString -> Maybe Int
        findSubstrings,         -- :: PackedString -> PackedString -> [Int]

        -- * Zipping and unzipping PackedString
        zip,                    -- :: PackedString -> PackedString -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> PackedString -> PackedString -> [c]
        unzip,                  -- :: [(Char,Char)] -> (PackedString,PackedString)

        -- * Unchecked access
        unsafeHead,             -- :: PackedString -> Char
        unsafeTail,             -- :: PackedString -> PackedString
        unsafeIndex,            -- :: PackedString -> Int -> Char
        w2c,                    -- :: Word8 -> Char
        c2w,                    -- :: Char  -> Word8

        -- * Reading from PackedStrings
        readInt,                -- :: PackedString -> Maybe Int
        unsafeReadInt,          -- :: PackedString -> Maybe Int

        -- * Copying PackedStrings
        copy,                   -- :: PackedString -> PackedString

        -- * I\/O with @ByteString@s
#if defined(__GLASGOW_HASKELL__)
        getLine,                -- :: IO PackedString
#endif
        getContents,            -- :: IO PackedString
        readFile,               -- :: FilePath -> IO PackedString
        mmapFile,               -- :: FilePath -> IO PackedString
        putStr,                 -- :: PackedString -> IO ()
        putStrLn,               -- :: PackedString -> IO ()
        writeFile,              -- :: FilePath -> PackedString -> IO ()

        -- ** I\/O with Handles
#if defined(__GLASGOW_HASKELL__)
        getArgs,                -- :: IO [PackedString]
        hGetLine,               -- :: Handle -> IO PackedString
#endif
        hGetContents,           -- :: Handle -> IO PackedString
        hGet,                   -- :: Handle -> Int -> IO PackedString
        hPut,                   -- :: Handle -> PackedString -> IO ()
        hGetNonBlocking,        -- :: Handle -> Int -> IO PackedString

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

-- Everything we want transparently exposed
import Data.ByteString (ByteString
                       ,empty,null,length,tail,init,append,reverse,transpose
                       ,concat,hash,take,drop,splitAt,breakSpace,dropSpace,dropSpaceEnd
                       ,lines,words,unlines,unwords,lines',unlines',linesCRLF'
                       ,unlinesCRLF',words',unwords', lineIndices,betweenLines
                       ,join,sort,isPrefixOf,isSuffixOf,isSubstringOf,findSubstring
                       ,findSubstrings,unsafeTail,readInt,unsafeReadInt,copy
                       ,inits,tails,elems
                       ,getContents,readFile,mmapFile,putStr,putStrLn,writeFile
                       ,hGetContents,hGet,hPut,hGetNonBlocking
#if defined(__GLASGOW_HASKELL__)
                       ,getLine, hGetLine, getArgs
#endif
                       )

import Data.Word
import Data.Char (ord)

#if defined(__GLASGOW_HASKELL__)
import GHC.Base (unsafeChr,unpackCString#)
#endif

#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined

------------------------------------------------------------------------
--
-- Transparently a ByteString
--
type PackedString = ByteString

------------------------------------------------------------------------

-- | /O(n)/ Convert a 'Word8' into a 'PackedString'
packChar :: Char -> PackedString
packChar = B.packByte . c2w

-- | /O(n)/ Convert a 'String' into a 'PackedString'
pack :: String -> PackedString
pack = B.packWith c2w

{-# RULES
"pack/packAddress" forall s# .
                   pack (unpackCString# s#) = B.packAddress s#
 #-}

-- | /O(n)/ Converts a 'PackedString' to a '[Word8]'.
unpack :: PackedString -> [Char]
unpack = B.unpackWith w2c

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires a memcpy.
cons :: Char -> PackedString -> PackedString
cons c p = B.cons (c2w c) p

-- | /O(n)/ Append a Char to the end of a 'PackedString'
snoc :: PackedString -> Char -> PackedString
snoc p c = B.snoc p (c2w c)

-- | /O(1)/ Extract the first element of a PackedString, which must be non-empty.
head :: PackedString -> Char
head = w2c . B.head

-- | /O(1)/ Extract the last element of a packed string, which must be finite and non-empty.
last :: PackedString -> Char
last = w2c . B.last

-- | /O(n)/ 'map' @f xs@ is the PackedString obtained by applying @f@ to each element of @xs@
map :: (Char -> Char) -> PackedString -> PackedString
map f = B.map (c2w . f . w2c)
{-# INLINE map #-}

-- | /O(n)/ The 'intersperse' function takes a Char and a 'PackedString'
-- and \`intersperses\' that Char between the elements of the
-- 'PackedString'.  It is analogous to the intersperse function on Lists.
intersperse :: Char -> PackedString -> PackedString
intersperse c = B.intersperse (c2w c)

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a PackedString, reduces the
-- PackedString using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> PackedString -> a
foldl f = B.foldl (\a c -> f a (w2c c))

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> PackedString -> a
foldr f = B.foldr (\c a -> f (w2c c) a)

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'PackedStrings'.
foldl1 :: (Char -> Char -> Char) -> PackedString -> Char
foldl1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'PackedString's
foldr1 :: (Char -> Char -> Char) -> PackedString -> Char
foldr1 f ps = w2c (B.foldl1 (\x y -> c2w (f (w2c x) (w2c y))) ps)

-- | Map a function over a 'PackedString' and concatenate the results
concatMap :: (Char -> PackedString) -> PackedString -> PackedString
concatMap f = B.concatMap (f . w2c)

-- | Applied to a predicate and a PackedString, 'any' determines if
-- any element of the 'PackedString' satisfies the predicate.
any :: (Char -> Bool) -> PackedString -> Bool
any f = B.any (f . w2c)

-- | Applied to a predicate and a 'PackedString', 'all' determines if
-- all elements of the 'PackedString' satisfy the predicate.
all :: (Char -> Bool) -> PackedString -> Bool
all f = B.all (f . w2c)

-- | 'maximum' returns the maximum value from a 'PackedString'
maximum :: PackedString -> Char
maximum = w2c . B.maximum

-- | 'minimum' returns the maximum value from a 'PackedString'
minimum :: PackedString -> Char
minimum = w2c . B.minimum

-- | /O(n)/ map Char functions, provided with the index at each position
mapIndexed :: (Int -> Char -> Char) -> PackedString -> PackedString
mapIndexed f ps = B.mapIndexed (\i c -> c2w (f i (w2c c))) ps

-- | /O(n)/ 'replicate' @n x@ is a PackedString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
replicate :: Int -> Char -> PackedString
replicate w c = B.replicate w (c2w c)

-- | /O(n)/ The 'unfoldrN' function is analogous to the List \'unfoldr\'.
-- 'unfoldrN' builds a PackedString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- PackedString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the PackedString and @b@ is used as the next element in a
-- recursive call.
--
-- To preven unfoldrN having /O(n^2)/ complexity (as prepending a
-- character to a PackedString is /O(n)/, this unfoldr requires a maximum
-- final size of the PackedString as an argument. 'cons' can then be
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
unfoldrN :: Int -> (Char -> Maybe (Char, Char)) -> Char -> PackedString
unfoldrN n f w = B.unfoldrN n f' (c2w w)
    where f' c = case f (w2c c) of
                    Nothing    -> Nothing
                    Just (i,j) -> Just ((c2w i),(c2w j))

-- | 'takeWhile', applied to a predicate @p@ and a PackedString @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> PackedString -> PackedString
takeWhile f = B.takeWhile (f . w2c)

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> PackedString -> PackedString
dropWhile f = B.dropWhile (f . w2c)

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
break f = B.break (f . w2c)

-- | 'span' @p xs@ breaks the PackedString into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
span f = B.span (f . w2c)

-- | 'spanEnd' behaves like 'span' but from the end of the 'PackedString'.
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
spanEnd :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanEnd f = B.spanEnd (f . w2c)

-- | 'breakChar' breaks its PackedString argument at the first occurence
-- of the specified Char. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakChar 'c' "abcd"
--
breakChar :: Char -> PackedString -> (PackedString, PackedString)
breakChar c = B.breakByte (c2w c)

-- | /O(n)/ 'breakFirst' breaks the given PackedString on the first
-- occurence of @w@. It behaves like 'break', except the delimiter is
-- not returned, and @Nothing@ is returned if the delimiter is not in
-- the PackedString. I.e.
--
-- > breakFirst 'b' "aabbcc" == Just ("aa","bcc")
--
-- > breakFirst c xs ==
-- > let (x,y) = break (== c) xs 
-- > in if null y then Nothing else Just (x, drop 1 y))
--
breakFirst :: Char -> PackedString -> Maybe (PackedString,PackedString)
breakFirst c = B.breakFirst (c2w c)

-- | /O(n)/ 'breakLast' behaves like breakFirst, but from the end of the
-- PackedString.
--
-- > breakLast ('b') (pack "aabbcc") == Just ("aab","cc")
--
-- and the following are equivalent:
--
-- > breakLast 'c' "abcdef"
-- > let (x,y) = break (=='c') (reverse "abcdef") 
-- > in if null x then Nothing else Just (reverse (drop 1 y), reverse x)
--
breakLast :: Char -> PackedString -> Maybe (PackedString,PackedString)
breakLast  c = B.breakLast (c2w c)

-- | /O(n)/ Break a 'PackedString' into pieces separated by the byte
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
-- not copy the substrings, it just constructs new 'PackedStrings' that
-- are slices of the original.
--
split :: Char -> PackedString -> [PackedString]
split c = B.split (c2w c)

-- | /O(n)/ Splits a 'PackedString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> PackedString -> [PackedString]
splitWith f = B.splitWith (f . w2c)

-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> PackedString -> [PackedString]
tokens f = B.tokens (f . w2c)

-- | /O(n)/ joinWithChar. An efficient way to join to two PackedStrings with a
-- char. Around 4 times faster than the generalised join.
--
joinWithChar :: Char -> PackedString -> PackedString -> PackedString
joinWithChar c = B.joinWithByte (c2w c)

-- | /O(1)/ 'PackedString' index (subscript) operator, starting from 0.
index :: PackedString -> Int -> Char
index p i = w2c (B.index p i)

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'PackedString' which is equal (by memchr) to the
-- query element, or 'Nothing' if there is no such element.
elemIndex :: Char -> PackedString -> Maybe Int
elemIndex c = B.elemIndex (c2w c)

-- | /O(n)/ The 'elemIndexLast' function returns the last index of the
-- element in the given 'PackedString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexLast c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexLast :: Char -> PackedString -> Maybe Int
elemIndexLast c = B.elemIndexLast (c2w c)

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> PackedString -> [Int]
elemIndices c = B.elemIndices (c2w c)

-- | The 'findIndex' function takes a predicate and a 'PackedString' and
-- returns the index of the first element in the PackedString satisfying the predicate.
findIndex :: (Char -> Bool) -> PackedString -> Maybe Int
findIndex f = B.findIndex (f . w2c)

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> PackedString -> [Int]
findIndices f = B.findIndices (f . w2c)

-- | /O(n)/ 'elem' is the 'PackedString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> PackedString -> Bool
elem    c = B.elem (c2w c)

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> PackedString -> Bool
notElem c = B.notElem (c2w c)

-- | /O(n)/ 'filter', applied to a predicate and a PackedString,
-- returns a PackedString containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> PackedString -> PackedString
filter f = B.filter (f . w2c)

-- | /O(n)/ The 'find' function takes a predicate and a PackedString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> PackedString -> Maybe Char
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
filterChar :: Char -> PackedString -> PackedString
filterChar    c = B.filterByte (c2w c)

-- | /O(n)/ A first order equivalent of /filter . (/=)/, for the common
-- case of filtering a single Char out of a list. It is more efficient
-- to use /filterNotChar/ in this case.
--
-- > filterNotChar == filter . (/=)
--
-- filterNotChar is around 3x faster, and uses much less space, than its
-- filter equivalent
--
filterNotChar :: Char -> PackedString -> PackedString
filterNotChar c = B.filterNotByte (c2w c)

-- | /O(n)/ 'zip' takes two PackedStrings and returns a list of
-- corresponding pairs of Chars. If one input PackedString is short,
-- excess elements of the longer PackedString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: PackedString -> PackedString -> [(Char,Char)]
zip ps qs
    | B.null ps || B.null qs = []
    | otherwise = (unsafeHead ps, unsafeHead qs) : zip (unsafeTail ps) (unsafeTail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two PackedStrings to produce the list
-- of corresponding sums.
zipWith :: (Char -> Char -> a) -> PackedString -> PackedString -> [a]
zipWith f = B.zipWith (\c d -> f (w2c c) (w2c d))

-- | 'unzip' transforms a list of pairs of Chars into a pair of
-- PackedStrings. Note that this performs two 'pack' operations.
unzip :: [(Char,Char)] -> (PackedString,PackedString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- | A variety of 'head' for non-empty PackedStrings. 'unsafeHead' omits
-- the check for the empty case, which is good for performance, but
-- there is an obligation on the programmer to provide a proof that the
-- PackedString is non-empty.
unsafeHead :: PackedString -> Char
unsafeHead  = w2c . B.unsafeHead

-- | Unsafe 'PackedString' index (subscript) operator, starting from 0, returning a Char.
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: PackedString -> Int -> Char
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
