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
-- | A time and space-efficient implementation of lazy byte vectors using
-- lists of packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities, or high speed requirements. Byte vectors
-- are encoded as lazy lists of strict Word8 arrays of bytes, held in a ForeignPtr,
-- and can be passed between C and Haskell with little effort.
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
--        inits,                  -- :: ByteString -> [ByteString]
--        tails,                  -- :: ByteString -> [ByteString]
--        elems,                  -- :: ByteString -> [ByteString]

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
{-        gloop,                  -- :: (acc -> Word8 -> (Maybe Word8, acc)
                                -- -> acc -> ByteString -> (ByteString, acc)

        -- * Substrings

        -- ** Breaking strings
        take,                   -- :: Int -> ByteString -> ByteString
        drop,                   -- :: Int -> ByteString -> ByteString
        splitAt,                -- :: Int -> ByteString -> (ByteString, ByteString)
        takeWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Word8 -> Bool) -> ByteString -> ByteString
        break,                  -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        span,                   -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
        spanEnd,                -- :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)

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
        elemIndexLast,          -- :: Word8 -> ByteString -> Maybe Int
        findIndex,              -- :: (Word8 -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Word8 -> Bool) -> ByteString -> [Int]
        count,                  -- :: Word8 -> ByteString -> Int

        -- * Ordered ByteStrings
        sort,                   -- :: ByteString -> ByteString

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

        -- ** Prefixes and suffixes
        -- | These functions use memcmp(3) to efficiently compare substrings
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

        -- * Low level introduction and elimination
        generate,               -- :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> ByteString
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)
        skipIndex,              -- :: ByteString -> Int

        -- ** Packing CStrings and pointers
        packCString,            -- :: CString -> ByteString
        packCStringLen,         -- :: CString -> ByteString
        packMallocCString,      -- :: CString -> ByteString

#if defined(__GLASGOW_HASKELL__)
        packCStringFinalizer,   -- :: Ptr Word8 -> Int -> IO () -> IO ByteString
        packAddress,            -- :: Addr# -> ByteString
        unsafePackAddress,      -- :: Int -> Addr# -> ByteString
        unsafeFinalize,         -- :: ByteString -> IO ()
#endif

        -- ** Using ByteStrings as CStrings
        useAsCString,           -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCString,     -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCStringLen,  -- :: ByteString -> (CStringLen -> IO a) -> IO a

        -- ** Copying ByteStrings
        -- | These functions perform memcpy(3) operations
        copy,                   -- :: ByteString -> ByteString
        copyCString,            -- :: CString -> ByteString
        copyCStringLen,         -- :: CStringLen -> ByteString

        -- * I\/O with @ByteString@s

        -- ** Standard input and output

#if defined(__GLASGOW_HASKELL__)
        getLine,                -- :: IO ByteString
#endif
        getContents,            -- :: IO ByteString
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()

        -- ** Files
        readFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()
--      mmapFile,               -- :: FilePath -> IO ByteString

        -- ** I\/O with Handles
#if defined(__GLASGOW_HASKELL__)
        getArgs,                -- :: IO [ByteString]
        hGetLine,               -- :: Handle -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int -> IO ByteString
#endif
        hGetContents,           -- :: Handle -> IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()

#if defined(__GLASGOW_HASKELL__)
        -- * Miscellaneous
        unpackList, -- eek, otherwise it gets thrown away by the simplifier
#endif
-}
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

import Data.Word                (Word8)

#if !defined(__GLASGOW_HASKELL__)
import System.IO.Unsafe
#endif

#if defined(__GLASGOW_HASKELL__)

import Data.Generics            (Data(..), Typeable(..))

#endif

import qualified Data.List as L        -- L for list/lazy
import qualified Data.ByteString as P  -- P for packed

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
data ByteString = LPS [P.ByteString] -- LPS for lazy packed string

#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

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

-- -----------------------------------------------------------------------------

instance Eq  ByteString
    where (==)    = eq

instance Ord ByteString
    where compare = compareBytes

--instance Show ByteString where
--    showsPrec p ps r = showsPrec p (unpackWith w2c ps) r

--instance Read ByteString where
--    readsPrec p str = [ (packWith c2w x, y) | (x, y) <- readsPrec p str ]


eq :: ByteString -> ByteString -> Bool
eq (LPS as) (LPS bs) = eq' as bs
  where eq' [] [] = True
        eq' [] _  = False
        eq' _  [] = False
        eq' (a:as) (b:bs) =
          case compare (P.length a) (P.length b) of
            LT -> a == (P.take (P.length a) b) && eq' as (P.drop (P.length a) b : bs)
            EQ -> a == b                       && eq' as bs
            GT -> (P.take (P.length b) a) == b && eq' (P.drop (P.length b) a : as) bs


compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (LPS as) (LPS bs) = cmp as bs
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

-- | /O(n/c)/ 'length' returns the length of a ByteString as an 'Int'.
length :: ByteString -> Int
length (LPS ss) = L.sum (L.map P.length ss)
{-# INLINE length #-}

-- | /O(1)/ 'cons' is analogous to (:) for lists
cons :: Word8 -> ByteString -> ByteString
cons c (LPS ss) = LPS (P.packByte c : ss)   -- TODO: coalesing and O(1) amortised time
{-# INLINE cons #-}

-- todo fuse

-- | /O(n/c)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (LPS ss) c = LPS (ss ++ [P.packByte c])
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
head :: ByteString -> Word8
head (LPS [])    = errorEmptyList "head"
head (LPS (x:_)) = P.head x
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
tail :: ByteString -> ByteString
tail (LPS [])     = errorEmptyList "tail"
tail (LPS (x:xs))
  | P.length x == 1 = LPS xs
  | otherwise       = LPS (P.tail x : xs)
{-# INLINE tail #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite and non-empty.
last :: ByteString -> Word8
last (LPS []) = errorEmptyList "last"
last (LPS xs) = P.last (L.last xs)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'ByteString' except the last one.
init :: ByteString -> ByteString
init (LPS []) = errorEmptyList "init"
init (LPS xs) = LPS (init' xs)
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
reverse (LPS xs) = LPS (L.map P.reverse xs)

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'ByteString' and \`intersperses\' that byte between the elements of
-- the 'ByteString'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse c ls = error "FIXME: not yet implemented"

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose s = error "FIXME: not yet implemented"

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
-- This function is subject to array fusion.
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z = error "FIXME: not yet implemented"
{-# INLINE foldl #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z lps = error "FIXME: not yet implemented"

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
-- This function is subject to array fusion.
foldl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 f lps = error "FIXME: not yet implemented"

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 f lps = error "FIXME: not yet implemented"

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat lpss = LPS (L.concat [ lps | (LPS lps@(_:_)) <- lpss ])

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
mapIndexed k (LPS xs) = error "FIXME: not yet implemented"

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
unfoldrN :: Int -> (Word8 -> Maybe (Word8, Word8)) -> Word8 -> ByteString
unfoldrN i f w = error "FIXME: not yet implemented"



-- TODO defrag func that concatenates block together that are below a threshold
-- defrag :: Int -> ByteString -> ByteString

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = error ("Data.ByteString.Lazy." ++ fun ++ ": empty ByteString")
