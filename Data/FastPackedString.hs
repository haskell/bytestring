{-# OPTIONS -fglasgow-exts -cpp #-}
--
-- Module      : FastPackedString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Don Stewart 2005
--               (c) Bjorn Bringert 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
-- 

--
-- | A time and space-efficient implementation of strings as packed
-- byte arrays.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- >  import qualified Data.FastPackedString as P
--
-- Original GHC implementation by Bryan O\'Sullivan. Rewritten to use
-- UArray by Simon Marlow. Rewritten to support slices and use
-- ForeignPtr by David Roundy. Polished and extended by Don Stewart.
--

module Data.FastPackedString (

        -- * The @FastString@ type
        FastString(..), -- abstract, instances: Eq, Ord, Show, Typeable

        -- * Introducing and eliminating 'FastString's
        empty,        -- :: FastString
        pack,         -- :: String -> FastString
        packChar,     -- :: String -> FastString
        unpack,       -- :: FastString -> String
        packWords,    -- :: [Word8] -> FastString
        unpackWords,  -- :: FastString -> [Word8]

        -- * Basic list-like interface
        cons,         -- :: Char -> FastString -> FastString
        snoc,         -- :: FastString -> Char -> FastString
        append,       -- :: FastString -> FastString -> FastString
        head,         -- :: FastString -> Char
        tail,         -- :: FastString -> FastString
        last,         -- :: FastString -> Char
        init,         -- :: FastString -> FastString
        null,         -- :: FastString -> Bool
        length,       -- :: FastString -> Int
        inits,        -- :: FastString -> [FastString]
        tails,        -- :: FastString -> [FastString]

        idx,          -- :: FastString -> Int
        lineIdxs,     -- :: FastString -> [Int]

        -- * List transformations
        map,          -- :: (Char -> Char) -> FastString -> FastString
        mapIndexed,   -- :: (Int -> Char -> Char) -> FastString -> FastString
        mapWords,     -- :: (Word8 -> Word8) -> FastString -> FastString
        mapIndexedWords,-- :: (Int -> Word8 -> Word8) -> FastString -> FastString
        reverse,      -- :: FastString -> FastString
        intersperse,  -- :: Char -> FastString -> FastString
        transpose,    -- :: [FastString] -> [FastString]
        join,         -- :: FastString -> [FastString] -> FastString

        -- * Reducing 'FastString's
        foldl,        -- :: (a -> Char -> a) -> a -> FastString -> a
        foldr,        -- :: (Char -> a -> a) -> a -> FastString -> a
        foldl1,       -- :: (Char -> Char -> Char) -> FastString -> Char
        foldr1,       -- :: (Char -> Char -> Char) -> FastString -> Char

        -- ** Special folds
        concat,       -- :: [FastString] -> FastString
        concatMap,    -- :: (Char -> FastString) -> FastString -> FastString
        any,          -- :: (Char -> Bool) -> FastString -> Bool
        all,          -- :: (Char -> Bool) -> FastString -> Bool
        maximum,      -- :: FastString -> Char
        minimum,      -- :: FastString -> Char

        -- ** Infinite lists
        replicate,    -- :: Int -> Char -> P.FastString

        -- ** Unfolding
        unfoldr,      -- :: (Char -> Maybe (Char, Char)) -> Char -> FastString

        -- * Substrings
        take,         -- :: Int -> FastString -> FastString
        drop,         -- :: Int -> FastString -> FastString
        splitAt,      -- :: Int -> FastString -> (FastString, FastString)

        takeWhile,    -- :: (Char -> Bool) -> FastString -> FastString
        dropWhile,    -- :: (Char -> Bool) -> FastString -> FastString
        span,         -- :: (Char -> Bool) -> FastString -> (FastString, FastString)
        break,        -- :: (Char -> Bool) -> FastString -> (FastString, FastString)

        -- * Searching 'FastString's

        -- ** Searching by equality
        elem,         -- :: Char -> FastString -> Bool

        -- ** Searching with a predicate
        filter,       -- :: (Char -> Bool) -> FastString -> FastString
        find,         -- :: (Char -> Bool) -> FastString -> Maybe Char

        -- ** Searching for substrings
        isSubstringOf, -- :: FastString -> FastString -> Bool
        findSubstring, -- :: FastString -> FastString -> Maybe Int
        findSubstrings, -- :: FastString -> FastString -> [Int]

        -- * Indexing 'FastString's
        index,        -- :: FastString -> Int -> Char
        indexWord8,   -- :: FastString -> Int -> Word8
        elemIndex,    -- :: Char -> FastString -> Maybe Int
        elemIndexWord8, -- :: Word8 -> FastString -> Maybe Int
        elemIndices,  -- :: Char -> FastString -> [Int]

        findIndex,    -- :: (Char -> Bool) -> FastString -> Maybe Int
        findIndices,  -- :: (Char -> Bool) -> FastString -> [Int]
        isPrefixOf,   -- :: FastString -> FastString -> Bool
        isSuffixOf,   -- :: FastString -> FastString -> Bool

        -- * Special 'FastString's
        elems,        -- :: FastString -> [FastString]

        -- ** Lines and words
        lines,        -- :: FastString -> [FastString]
        words,        -- :: FastString -> [FastString]
        unlines,      -- :: [FastString] -> FastString
        unwords,      -- :: FastString -> [FastString]

        -- ** Ordered 'FastString's
        sort,         -- :: FastString -> FastString

        -- * Extensions to the list interface
        breakOn,      -- :: Char -> FastString -> (FastString, FastString)
        breakSpace,   -- :: FastString -> Maybe (FastString,FastString)
        breakAll,     -- :: (Char -> Bool) -> FastString -> [FastString]
        breakFirst,   -- :: Char -> FastString -> Maybe (FastString,FastString)
        breakLast,    -- :: Char -> FastString -> Maybe (FastString,FastString)
        dropSpace,    -- :: FastString -> FastString
        dropSpaceEnd, -- :: FastString -> FastString
        spanEnd,      -- :: (Char -> Bool) -> FastString -> (FastString, FastString)
        split,        -- :: Char -> FastString -> [FastString]
        tokens,       -- :: (Char -> Bool) -> FastString -> [FastString]
        hash,         -- :: FastString -> Int32
        elemIndexLast,-- :: Char -> FastString -> Maybe Int
        elemIndexLastWord8,-- :: Char -> FastString -> Maybe Int
        betweenLines, -- :: FastString -> FastString -> FastString -> Maybe (FastString)
        lines',       -- :: FastString -> [FastString]
        unlines',     -- :: [FastString] -> FastString
        linesCRLF',   -- :: FastString -> [FastString]
        unlinesCRLF', -- :: [FastString] -> FastString
        words',       -- :: FastString -> [FastString]
        unwords',     -- :: FastString -> [FastString]
        unsafeHead,   -- :: FastString -> Char
        unsafeTail,   -- :: FastString -> FastString

        ------------------------------------------------------------------------

        -- * I\/O with @FastString@s
        hGet,                 -- :: Handle -> Int -> IO FastString
        hGetNonBlocking,      -- :: Handle -> Int -> IO FastString
        hPut,                 -- :: Handle -> FastString -> IO ()
        hGetContents,         -- :: Handle -> IO FastString
        readFile,             -- :: FilePath -> IO FastString
        writeFile,            -- :: FilePath -> FastString -> IO ()
        mmapFile,             -- :: FilePath -> IO FastString

        -- * Lower-level constructors
        generate,             -- :: Int -> (Ptr Word8 -> Int -> IO Int) -> IO FastString
#if defined(__GLASGOW_HASKELL__)
        construct,            -- :: (Ptr Word8) -> Int -> IO () -> IO FastString
        packAddress,          -- :: Addr# -> FastString
        unsafePackAddress,    -- :: Int -> Addr# -> FastString
        unsafeFinalize,       -- :: FastString -> IO ()
#endif
        packMallocCString,    -- :: CString -> FastString
        packCString,          -- :: CString -> FastString
        packCStringLen,       -- :: CString -> FastString
        useAsCString,         -- :: FastString -> (CString -> IO a) -> IO a
        unsafeUseAsCString,   -- :: FastString -> (CString -> IO a) -> IO a
        unsafeUseAsCStringLen,-- :: FastString -> (CStringLen -> IO a) -> IO a
        unpackFromUTF8,       -- :: FastString -> String

        copy,                 -- :: FastString -> FastString

        fromForeignPtr,       -- :: ForeignPtr Word8 -> Int -> FastString
        toForeignPtr,         -- :: FastString -> (ForeignPtr Word8, Int, Int)

        -- * Extensions to the I\/O interface
        LazyFile(..),
        readFileLazily,         -- :: FilePath -> IO LazyFile
#if defined(USE_ZLIB)
        gzReadFile,             -- :: FilePath -> IO FastString
        gzWriteFile,            -- :: FilePath -> FastString -> IO ()
        gzReadFileLazily,       -- :: FilePath -> IO LazyFile
        gzWriteFilePSs,         -- :: FilePath -> [FastString] -> IO ()
#endif

   ) where

import qualified Prelude
import Prelude hiding (reverse,head,tail,last,init,null,
                       length,map,lines,foldl,foldr,unlines,
                       concat,any,take,drop,splitAt,takeWhile,
                       dropWhile,span,break,elem,filter,unwords,
                       words,maximum,minimum,all,concatMap,
                       foldl1,foldr1,readFile,writeFile,replicate)

import qualified Data.List as List (intersperse,transpose)

import Data.Array               (listArray)
import qualified Data.Array as Array ((!))
import Data.Bits                (rotateL)
import Data.Char                (chr, ord, String, isSpace)
import Data.Int                 (Int32)
import Data.Word                (Word8)
import Data.Maybe               (listToMaybe)

import Control.Monad            (when, liftM)
import Control.Exception        (bracket)

import System.IO    hiding      (hGetContents,readFile,writeFile)
import System.IO.Unsafe         (unsafePerformIO)
import System.Mem               (performGC)

import Foreign.Ptr              (Ptr, FunPtr, plusPtr, nullPtr, minusPtr, castPtr)
import Foreign.ForeignPtr       (newForeignPtr, newForeignPtr_, withForeignPtr, 
                                 finalizeForeignPtr, mallocForeignPtrArray, ForeignPtr)
import Foreign.Storable         (peekByteOff, peek, poke)
import Foreign.C.String         (CString, CStringLen)
import Foreign.C.Types          (CSize, CInt)
import Foreign.Marshal.Alloc    (free)
import Foreign.Marshal.Array

#if defined(__GLASGOW_HASKELL__)
import qualified Foreign.Concurrent as FC (newForeignPtr)

#if defined(USE_MMAP)
import System.Posix             (handleToFd)
#endif
#endif

#if !defined(USE_MMAP)
import System.IO.Unsafe         (unsafeInterleaveIO)
#endif

#if defined(__GLASGOW_HASKELL__)
import Data.Generics

import GHC.Base (unsafeChr, unpackCString#)

import GHC.Ptr  (Ptr(..))
import GHC.ST
import GHC.Prim
import GHC.Base (Int(..), Char(..))
import Control.Monad.ST
#endif

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a 'String', which supports various
-- efficient operations.  A 'FastString' contains 8-bit characters only.
--
data FastString = PS {-# UNPACK #-} !(ForeignPtr Word8) !Int !Int
#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

------------------------------------------------------------------------

instance Eq  FastString 
    where (==)    = eqPS

instance Ord FastString 
    where compare = comparePS

instance Show FastString where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read FastString where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

------------------------------------------------------------------------

-- | /O(n)/ Equality on the 'FastString' type. This implementation
-- uses @memcmp(3)@.
eqPS :: FastString -> FastString -> Bool
eqPS a b = (comparePS a b) == EQ
{-# INLINE eqPS #-}

-- | /O(n)/ 'comparePS' provides an 'Ordering' for 'FastStrings' supporting slices. 
-- This implementation uses @memcmp(3)@
comparePS :: FastString -> FastString -> Ordering
comparePS (PS _ _ 0) (PS _ _ 0) = EQ    -- short cut for empty strings
comparePS (PS x1 s1 l1) (PS x2 s2 l2) = unsafePerformIO $ 
    withForeignPtr x1 $ \p1 -> 
        withForeignPtr x2 $ \p2 -> do 
            i <- c_memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (min l1 l2)
            return $ case i `compare` 0 of
                EQ  -> l1 `compare` l2
                x   -> x

-- -----------------------------------------------------------------------------
-- Constructing and destructing packed strings

-- | /O(1)/ The empty 'FastString'
empty :: FastString
empty = unsafePerformIO $ mallocForeignPtr 1 >>= \fp -> return $ PS fp 0 0
{-# NOINLINE empty #-}

-- | /O(n)/ Convert a 'Char' into a 'FastString'
packChar :: Char -> FastString
packChar c = unsafePerformIO $ mallocForeignPtr 2 >>= \fp -> do
    withForeignPtr fp $ \p -> poke p (toEnum (ord c))
    return $ PS fp 0 1
{-# NOINLINE packChar #-}

-- | /O(n)/ Convert a 'String' into a 'FastString'
pack :: String -> FastString
#if !defined(__GLASGOW_HASKELL__)

pack str = createPS (Prelude.length str) $ \p -> go p str
    where
        go _ []     = return ()    
        go p (x:xs) = poke p (c2w x) >> go (p `plusPtr` 1) xs -- less space than pokeElemOff

#else /* hack away */

pack str = createPS (Prelude.length str) $ \(Ptr p) -> stToIO (go p 0# str)
    where
        go _ _ []        = return ()
        go p i (C# c:cs) 
            | C# c > '\255' = error ("Data.FastPackedString.pack: "
                                     ++ "character out of range")
            | otherwise     = writeByte p i c >> go p (i +# 1#) cs

        writeByte p i c = ST $ \s# -> 
            case writeCharOffAddr# p i c s# of s2# -> (# s2#, () #)
{-# RULES
"pack/packAddress" forall s# .
                   pack (unpackCString# s#) = packAddress s#
 #-}

#endif

-- | /O(n)/ Convert a 'FastString' into a 'String'
unpack :: FastString -> String
unpack (PS _  _ 0) = []
unpack (PS ps s l) = unsafePerformIO $ withForeignPtr ps $ \p -> 
        go (p `plusPtr` s) (l - 1) []
    where
        go p 0 acc = liftM w2c (peekByteOff p 0) >>= \e -> return (e : acc)
        go p n acc = liftM w2c (peekByteOff p n) >>= \e -> go p (n-1) (e : acc)

-- | /O(n)/ Convert a '[Word8]' into a 'FastString'
packWords :: [Word8] -> FastString
packWords s = createPS (Prelude.length s) $ \p -> pokeArray p s

-- | /O(n)/ Convert a 'FastString' to a '[Word8]'
unpackWords :: FastString -> [Word8]
unpackWords ps@(PS x s _)
    | null ps     = []
    | otherwise     =
        (unsafePerformIO $ withForeignPtr x $ \p -> peekByteOff p s) 
            : unpackWords (unsafeTail ps)

-- -----------------------------------------------------------------------------
-- List-like functions for FastStrings

-- | /O(n)/ 'cons' is analogous to (:) for lists. Requires a memcpy.
cons :: Char -> FastString -> FastString
cons c (PS x s l) = createPS (l+1) $ \p -> withForeignPtr x $ \f -> do
        c_memcpy (p `plusPtr` 1) (f `plusPtr` s) l  -- 99% less space
        poke p (c2w c)

-- | /O(n)/ Append a character to the end of a 'FastString'
snoc :: FastString -> Char -> FastString
snoc (PS x s l) c = createPS (l+1) $ \p -> withForeignPtr x $ \f -> do
        c_memcpy p (f `plusPtr` s) l
        poke (p `plusPtr` l) (c2w c)

-- | /O(1)/ Extract the first element of a packed string, which must be non-empty.
head :: FastString -> Char
head ps@(PS x s _)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | null ps   = errorEmptyList "head"
  | otherwise = w2c $ unsafePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a packed string, which must be non-empty.
tail :: FastString -> FastString
tail (PS p s l) 
    | l <= 0    = errorEmptyList "tail"
--  | l == 1    = empty                                                                    
    | otherwise = PS p (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the last element of a packed string, which must be finite and non-empty.
last :: FastString -> Char
last ps@(PS x s l)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | null ps   = errorEmptyList "last"
  | otherwise = w2c $ unsafePerformIO $ 
        withForeignPtr x $ \p -> peekByteOff p (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'FastString' except the last one.
init :: FastString -> FastString
init (PS p s l) 
    | l <= 0    = errorEmptyList "init"
    | l == 1    = empty                                                                    
    | otherwise = PS p s (l-1)                                                          
{-# INLINE init #-}

-- | /O(1)/ Test whether a packed string is empty.
null :: FastString -> Bool
null (PS _ _ l) = l == 0
{-# INLINE null #-}

-- | /O(1)/ 'length' returns the length of a packed string as an 'Int'.
length :: FastString -> Int
length (PS _ _ l) = l
{-# INLINE length #-}

-- | Return all initial segments of the given 'FastString', shortest first.
inits :: FastString -> [FastString]
inits (PS x s l) = [PS x s n | n <- [0..l]]

-- | Return all final segments of the given 'FastString', longest first.
tails :: FastString -> [FastString]
tails (PS x s l) = [PS x (s+n) (l-n) | n <- [0..l]]

-- | /O(1)/ 'idx' returns the skipped index as an 'Int'.
idx :: FastString -> Int
idx (PS _ s _) = s
{-# INLINE idx #-}

-- a set of positions where newline occurs
lineIdxs :: FastString -> [Int]
lineIdxs ps 
    | null ps = []
    | otherwise = case elemIndexWord8 0x0A ps of
             Nothing -> []
             Just n  -> (n + idx ps:lineIdxs (drop (n+1) ps))

-- | /O(n)/ Append two packed strings
append :: FastString -> FastString -> FastString
append xs ys
    | null xs = ys
    | null ys = xs
    | otherwise  = concat [xs,ys]
{-# INLINE append #-}

-- | /O(n)/ 'map' @f xs@ is the packed string obtained by applying @f@ to each
-- element of @xs@, i.e.,
map :: (Char -> Char) -> FastString -> FastString
map k = mapWords (c2w . k . w2c)

mapIndexed :: (Int -> Char -> Char) -> FastString -> FastString
mapIndexed k = mapIndexedWords (\i w -> c2w (k i (w2c w)))

mapWords :: (Word8 -> Word8) -> FastString -> FastString
mapWords k
    = mapIndexedWords (const k)

mapIndexedWords :: (Int -> Word8 -> Word8) -> FastString -> FastString
mapIndexedWords k (PS ps s l)
    = createPS l $ \p -> withForeignPtr ps $ \f -> 
      go 0 (f `plusPtr` s) p (f `plusPtr` s `plusPtr` l)
    where 
        go :: Int -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
        go n f t p | f == p    = return ()
                   | otherwise = do w <- peek f
                                    ((poke t) . k n) w
                                    go (n+1) (f `plusPtr` 1) (t `plusPtr` 1) p

-- | /O(n)/ 'filter', applied to a predicate and a packed string,
-- returns a packed string containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> FastString -> FastString
filter k ps@(PS x s l)
    | null ps   = ps
    | otherwise = unsafePerformIO $ generate l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p l
        return (t `minusPtr` p) -- actual length
    where
        go _ t 0 = return t
        go f t e = do w <- peek f
                      if k (w2c w)
                        then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) (e - 1)
                        else             go (f `plusPtr` 1) t               (e - 1)
    -- Almost as good: pack $ foldl (\xs c -> if f c then c : xs else xs) [] ps

-- | /O(n)/ The 'find' function takes a predicate and a packed string
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> FastString -> Maybe Char
find p ps = case filter p ps of
        p' | null p' -> Nothing
           | otherwise -> Just (unsafeHead p')

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a packed string, reduces the
-- packed string using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> FastString -> a
foldl f v (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        lgo v (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        lgo z p q | p == q    = return z
                  | otherwise = do c <- liftM w2c $ peek p
                                   lgo (f z c) (p `plusPtr` 1) q

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> FastString -> a
foldr k z (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        go p q | p == q    = return z
               | otherwise = do c  <- liftM w2c $ peek p
                                ws <- go (p `plusPtr` 1) q
                                return $ c `k` ws

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'FastStrings'.
foldl1 :: (Char -> Char -> Char) -> FastString -> Char
foldl1 f ps
    | null ps   = errorEmptyList "foldl1"
    | otherwise = foldl f (unsafeHead ps) (unsafeTail ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'FastString's
foldr1 :: (Char -> Char -> Char) -> FastString -> Char
foldr1 f ps
    | null ps        = errorEmptyList "foldr1"
    | length ps == 1 = unsafeHead ps
    | otherwise      = f (unsafeHead ps) (foldr1 f (unsafeTail ps))

-- | 'replicate' @n x@ is a packed string of length @n@ with @x@ the
-- value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
replicate :: Int -> Char -> FastString
replicate w c = unsafePerformIO $ generate w $ \ptr -> go ptr w
    where 
        x = fromIntegral . ord $ c
        go _   0 = return w
        go ptr n = poke ptr x >> go (ptr `plusPtr` 1) (n-1)


-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a FastString from a seed value.  The function
-- takes the element and returns 'Nothing' if it is done producing the
-- FastString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the FastString and @b@ is used as the next element in
-- a recursive call.
--
-- To preven unfoldr having /O(n^2)/ complexity (as prepending a character
-- to a FastString is /O(n)/, this unfoldr requires a maximum final size
-- of the FastString as an argument. 'cons' can then be implemented in
-- /O(1)/ (i.e.  a 'poke'), and the unfoldr itself has linear
-- complexity. The depth of the recursion is limited to this size, but
-- may be less. For lazy, infinite unfoldr, use 'Data.List.unfoldr'
-- (from 'Data.List').
--
-- Examples:
--
-- > unfoldr 10 (\x -> Just (x, chr (ord x + 1))) '0' == "0123456789"
--
-- The following equation connects the depth-limited unfoldr to the List unfoldr:
--
-- > unfoldr n == take n $ List.unfoldr
--
unfoldr :: Int -> (Char -> Maybe (Char, Char)) -> Char -> FastString
unfoldr i f b = unsafePerformIO $ generate i $ \p -> go p b 0
    where
        go q c n | n == i    = return n      -- stop if we reach `i'
                 | otherwise = case f c of
                                   Nothing        -> return n
                                   Just (a,new_c) -> do 
                                        poke q (c2w a)
                                        go (q `plusPtr` 1) new_c (n+1)

-- | Applied to a predicate and a packed string, 'any' determines if
-- any element of the 'FastString' satisfies the predicate.
any :: (Char -> Bool) -> FastString -> Bool
any f (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where 
        go p q | p == q    = return False
               | otherwise = do c <- liftM w2c $ peek p
                                if f c then return True
                                       else go (p `plusPtr` 1) q

-- | Applied to a predicate and a 'FastString', 'all' determines if
-- all elements of the 'FastString' satisfy the predicate.
all :: (Char -> Bool) -> FastString -> Bool
all f (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where 
        go p q | p == q     = return True  -- end of list
               | otherwise  = do c <- liftM w2c $ peek p
                                 if f c
                                    then go (p `plusPtr` 1) q
                                    else return False

-- | 'takeWhile', applied to a predicate @p@ and a packed string @xs@,
-- returns the longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> FastString -> FastString
takeWhile f ps = seq f $ take (findIndexOrEndPS (not . f) ps) ps
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> FastString -> FastString
dropWhile f ps = seq f $ drop (findIndexOrEndPS (not . f) ps) ps
{-# INLINE dropWhile #-}

-- | /O(1)/ 'take' @n@, applied to a packed string @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> FastString -> FastString
take n ps@(PS x s l)
    | n < 0     = empty
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> FastString -> FastString
drop n ps@(PS x s l)
    | n <= 0    = ps
    | n >  l    = empty
    | otherwise = PS x (s+n) (l-n)
{-# INLINE drop #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> FastString -> (FastString, FastString)
splitAt  n ps  = (take n ps, drop n ps)
{-# INLINE splitAt #-}

-- | 'span' @p xs@ breaks the packed string into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> FastString -> (FastString, FastString)
span  p ps = break (not . p) ps

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> FastString -> (FastString, FastString)
break p ps = case findIndexOrEndPS p ps of n -> (take n ps, drop n ps)

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: FastString -> FastString
reverse (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> 
        c_reverse p (f `plusPtr` s) l -- 99% less space, very much faster

-- | 'elem' is the 'FastString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> FastString -> Bool
elem c ps = case elemIndex c ps of
    Nothing -> False
    Just _  -> True

-- | Map a function over a 'FastString' and concatenate the results
concatMap :: (Char -> FastString) -> FastString -> FastString
concatMap f = foldr (append . f) empty

-- | Concatenate a list of packed strings.
concat :: [FastString] -> FastString
concat []     = empty
concat [ps]   = ps
concat xs     = unsafePerformIO $ do 
    let start_size = 1024
    p <- mallocArray start_size
    f p 0 1024 xs

    where f ptr len _ [] = do 
                ptr' <- reallocArray ptr (len+1)
                poke (ptr' `plusPtr` len) (0::Word8)    -- XXX so CStrings work
                fp   <- newForeignPtr c_free ptr'
                return $ PS fp 0 len

          f ptr len to_go pss@(PS p s l:pss')
           | l <= to_go = do withForeignPtr p $ \pf ->
                                 c_memcpy (ptr `advancePtr` len)
                                          (pf `advancePtr` s) l
                             f ptr (len + l) (to_go - l) pss'

           | otherwise = do let new_total = ((len + to_go) * 2) `max` (len + l)
                            ptr' <- reallocArray ptr new_total
                            f ptr' len (new_total - len) pss

-- | 'FastString' index (subscript) operator, starting from 0.
index :: FastString -> Int -> Char
index ps n 
    | n < 0          = error $ "FastPackedString.index: negative index: " ++ show n
    | n >= length ps = error $ "FastPackedString.index: index too large: " ++ show n 
                                ++ ", length = " ++ show (length ps)
    | otherwise      = w2c $ ps ! n
{-# INLINE index #-}

indexWord8 :: FastString -> Int -> Word8
indexWord8 ps n 
    | n < 0          = error $ "FastPackedString.indexWord8: negative index: " ++ show n
    | n >= length ps = error $ "FastPackedString.indexWord8: index too large: " ++ show n 
                                ++ ", length = " ++ show (length ps)
    | otherwise      = ps ! n
{-# INLINE indexWord8 #-}

-- | 'maximum' returns the maximum value from a 'FastString'
maximum :: FastString -> Char
maximum xs@(PS x s l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = unsafePerformIO $ withForeignPtr x $ \p -> 
                    return $ w2c $ c_maximum (p `plusPtr` s) l

-- | 'maximum' returns the maximum value from a 'FastString'
minimum :: FastString -> Char
minimum xs@(PS x s l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = unsafePerformIO $ withForeignPtr x $ \p -> 
                    return $ w2c $ c_minimum (p `plusPtr` s) l

-- | /o(n)/ breaks a packed string to a list of packed strings, one byte each.
elems :: FastString -> [FastString]
elems (PS _ _ 0) = []
elems (PS x s l) = (PS x s 1:elems (PS x (s+1) (l-1)))
{-# INLINE elems #-}

-- | 'lines' breaks a packed string up into a list of packed strings
-- at newline characters.  The resulting strings do not contain
-- newlines.
lines :: FastString -> [FastString]
lines ps 
    | null ps = []
    | otherwise = case elemIndexWord8 (c2w '\n') ps of
             Nothing -> [ps]
             Just n  -> take n ps : lines (drop (n+1) ps)
{-# INLINE lines #-}

-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [FastString] -> FastString
unlines [] = empty
unlines ss = (concat $ List.intersperse nl ss) `append` nl -- half as much space
    where
      nl = pack "\n"

-- | 'words' breaks a packed string up into a list of words, which
-- were delimited by white space.
words :: FastString -> [FastString]
words ps = Prelude.filter (not.null) (breakAll isSpace ps)


-- | The 'unwords' function is analogous to the 'unwords' function.
unwords :: [FastString] -> FastString
unwords = join $ pack " "

-- | The 'intersperse' function takes a 'Char' and a 'FastString' and
-- \`intersperses\' that 'Char' between the elements of the 'FastString'.
-- It is analogous to the intersperse function on Lists.
intersperse :: Char -> FastString -> FastString
intersperse c ps@(PS x s l)
    | length ps < 2  = ps
    | otherwise      = createPS (2*l-1) $ \p -> withForeignPtr x $ \f ->
                            c_intersperse p (f `plusPtr` s) l (c2w c)

-- | The 'transpose' function transposes the rows and columns of its
-- 'FastString' argument.
transpose :: [FastString] -> [FastString]
transpose ps = Prelude.map pack (List.transpose (Prelude.map unpack ps)) -- better

-- | The 'join' function takes a 'FastString' and a list of
-- 'FastString's and concatenates the list after interspersing the
-- first argument between each element of the list.
join :: FastString -> [FastString] -> FastString
join filler pss = concat (splice pss)
    where
        splice []  = []
        splice [x] = [x]
        splice (x:y:xs) = x:filler:splice (y:xs)

-- | /O(n log(n))/ Sort a FastString using the C function @qsort(3)@.
sort :: FastString -> FastString
sort (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> do
        c_memcpy p (f `plusPtr` s) l
        c_qsort p l -- inplace

-- | /O(n)/ The 'elemIndex' function returns the index of the first element
-- in the given 'FastString' which is equal (by memchr) to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Char -> FastString -> Maybe Int
elemIndex c = elemIndexWord8 (c2w c)
{-# INLINE elemIndex #-}

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> FastString -> [Int]
elemIndices x = findIndices (x==)

-- | The 'findIndex' function takes a predicate and a 'FastString'
-- and returns the index of the first element in the packed string
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> FastString -> Maybe Int
findIndex f = listToMaybe . findIndices f

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> FastString -> [Int]
findIndices p ps = loop 0 ps
	where
       loop _ ps' | null ps'      = []
       loop n ps' | p (unsafeHead ps') = n : loop (n + 1) (unsafeTail ps')
                  | otherwise     = loop (n + 1) (unsafeTail ps')

-- | The 'isPrefixOf' function takes two strings and returns 'True'
-- iff the first string is a prefix of the second.
isPrefixOf :: FastString -> FastString -> Bool
isPrefixOf (PS x1 s1 l1) (PS x2 s2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = unsafePerformIO $ withForeignPtr x1 $ \p1 -> 
        withForeignPtr x2 $ \p2 -> do 
            i <- c_memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) l1
            return (i == 0)

-- | The 'isSuffixOf' function takes two lists and returns 'True'
-- iff the first list is a suffix of the second.
-- Both lists must be finite.
isSuffixOf     :: FastString -> FastString -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

------------------------------------------------------------------------
-- Extensions to the list interface

-- | 'dropSpace' efficiently returns the 'FastString' argument with
-- white space removed from the front. It is more efficient than calling
-- dropWhile for removing whitespace. I.e.
-- 
-- > dropWhile isSpace == dropSpace
--
dropSpace :: FastString -> FastString
dropSpace (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do
    let i = c_firstnonspace (p `plusPtr` s) l
    return $ if i == l then empty else PS x (s+i) (l-i)
{-# INLINE dropSpace #-}

-- | 'dropSpaceEnd' efficiently returns the 'FastString' argument with
-- white space removed from the end. I.e.
-- 
-- > reverse . (dropWhile isSpace) . reverse == dropSpaceEnd
--
dropSpaceEnd :: FastString -> FastString
dropSpaceEnd (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do
    let i = c_lastnonspace (p `plusPtr` s) l
    return $ if i == (-1) then empty else PS x s (i+1)
{-# INLINE dropSpaceEnd #-}

-- | 'breakSpace' returns the pair of 'FastString's when the argument
-- is broken at the first whitespace character. I.e.
-- 
-- > break isSpace == breakSpace
--
breakSpace :: FastString -> (FastString,FastString)
breakSpace (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do 
    let i = c_firstspace (p `plusPtr` s) l
    return $ case () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
    }
{-# INLINE breakSpace #-}

-- | 'spanEnd' behaves like 'span' but from the end of the
-- 'FastString'. I.e.
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . Char.isSpace) ps
-- >    == 
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x) 
--
spanEnd :: (Char -> Bool) -> FastString -> (FastString, FastString)
spanEnd  p ps = splitAt (findFromEndUntilPS (not.p) ps) ps

-- | 'breakOn' breaks its 'FastString' argument at the first occurence
-- of the specified character. It is more efficient than 'break' as it
-- is implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakOn 'c' "abcd"
--
breakOn :: Char -> FastString -> (FastString, FastString)
breakOn c p = case elemIndex c p of
                    Nothing -> (p,empty)
                    Just n -> (take n p, drop n p)
{-# INLINE breakOn #-}

-- | Break a 'FastString' into pieces separated by the 'Char'
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
--
split :: Char -> FastString -> [FastString]
split c = splitWord8 (c2w c)
{-# INLINE split #-}

-- | Like 'breakAll', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> FastString -> [FastString]
tokens p = Prelude.filter (not.null) . breakAll p

-- | Splits a 'FastString' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > breakAll (=='a') "aabbaca" == ["","","bb","c",""]
--
breakAll :: (Char -> Bool) -> FastString -> [FastString]
breakAll p ps = if null rest 
                    then [chunk] 
                    else chunk : breakAll p (unsafeTail rest)
    where 
      (chunk,rest) = break p ps
{-
-- weird, inefficient version. Probably slightly different to the above
breakAll :: (Char -> Bool) -> FastString -> [FastString]
breakAll f ps =
    case [ m | m <- [0..length ps-1], f (w2c (ps ! m)) ] of
        [] -> if null ps then [] else [ps]
        (n:_) -> take n ps : breakAll f (drop (n+1) ps)
-}

-- | /O(n)/ 'breakFirst' breaks the given FastString on the first
-- occurence of @c@. It behaves like 'break', except the delimiter is
-- not returned, and @Nothing@ is returned if the delimiter is not in
-- the FastString. I.e.
--
-- > breakFirst 'b' "aabbcc" == Just ("aa","bcc")
--
-- > breakFirst c xs ==
-- > let (x,y) = break (== c) xs 
-- > in if null y then Nothing else Just (x, drop 1 y))
--
breakFirst :: Char -> FastString -> Maybe (FastString,FastString)
breakFirst c p = case elemIndex c p of
   Nothing -> Nothing
   Just n -> Just (take n p, drop (n+1) p)
{-# INLINE breakFirst #-}

-- | /O(n)/ 'breakLast' behaves like breakFirst, but from the end of the
-- FastString.
--
-- > breakLast ('b') (pack "aabbcc") == Just ("aab","cc")
--
-- and the following are equivalent:
--
-- > breakLast 'c' "abcdef"
-- > let (x,y) = break (=='c') (reverse "abcdef") 
-- > in if null x then Nothing else Just (reverse (drop 1 y), reverse x)
--
breakLast :: Char -> FastString -> Maybe (FastString,FastString)
breakLast c p = case elemIndexLast c p of
    Nothing -> Nothing
    Just n -> Just (take n p, drop (n+1) p)
{-# INLINE breakLast #-}

-- | /O(n)/ The 'elemIndexLast' function returns the last index of the
-- element in the given 'FastString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following holds:
--
-- > elemIndexLast c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
elemIndexLast :: Char -> FastString -> Maybe Int
elemIndexLast c = elemIndexLastWord8 (c2w c)
{-# INLINE elemIndexLast #-}

-- | /O(n)/ Hash a FastString into an 'Int32' value, suitable for use as a key.
hash :: FastString -> Int32
hash (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> 
    go (0 :: Int32) (p `plusPtr` s) l
  where
    go :: Int32 -> Ptr Word8 -> Int -> IO Int32
    go h _ 0 = return h
    go h p n = do w <- peek p
                  let h' = (fromIntegral w) + (rotateL h 8)
                  go h' (p `advancePtr` 1) (n-1)

-- | 'betweenLines' returns the FastString between the two lines
-- given, or Nothing if they do not appear.
-- The returned string is the first and shortest string such 
-- that the line before it is the given first line, and the line 
-- after it is the given second line.
betweenLines :: FastString -- ^ First line to look for
             -> FastString -- ^ Second line to look for
             -> FastString -- ^ 'FastString' to look in
             -> Maybe (FastString)
betweenLines start end ps = 
    case Prelude.break (start ==) (lines ps) of
        (_, _:rest@(PS ps1 s1 _:_)) ->
            case Prelude.break (end ==) rest of
                (_, PS _ s2 _:_) -> Just $ PS ps1 s1 (s2 - s1)
                _ -> Nothing
        _ -> Nothing

-- | 'lines\'' behaves like 'lines', in that it breaks a FastString on
-- newline characters. However, unlike the Prelude functions, 'lines\''
-- and 'unlines\'' correctly reconstruct lines that are missing
-- terminating newlines characters. I.e.
--
-- > unlines  (lines "a\nb\nc")  == "a\nb\nc\n"
-- > unlines' (lines' "a\nb\nc") == "a\nb\nc"
--
-- Note that this means:
--
-- > lines  "a\nb\nc\n" == ["a","b","c"]
-- > lines' "a\nb\nc\n" == ["a","b","c",""]
--
lines' :: FastString -> [FastString]
lines' ps = case elemIndexWord8 (c2w '\n') ps of
             Nothing -> [ps]
             Just n -> take n ps : lines' (drop (n+1) ps)

-- | 'linesCRLF\'' behaves like 'lines\'', but breaks on (\\cr?\\lf)
linesCRLF' :: FastString -> [FastString]
linesCRLF' ps = case elemIndexWord8 (c2w '\n') ps of
                 Nothing -> [ps]
                 Just 0  -> empty : linesCRLF' (drop 1 ps)
                 Just n  -> let k = if ps ! (n-1) == 0xD then n-1 else n
                            in take k ps : linesCRLF' (drop (n+1) ps)

-- | 'unlines\'' behaves like 'unlines', except that it also correctly
-- retores lines that do not have terminating newlines (see the
-- description for 'lines\'').
--
unlines' :: [FastString] -> FastString
unlines' ss = concat $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a:newline: intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = pack "\n"

-- | 'unlines\'' behaves like 'unlines', except that it also correctly
-- retores lines that do not have terminating newlines (see the
-- description for 'lines\''). Uses CRLF instead of LF.
--
unlinesCRLF' :: [FastString] -> FastString
unlinesCRLF' ss = concat $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a:newline: intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = pack "\r\n"

-- | 'words\'' behaves like 'words', with the exception that it produces
-- output on FastStrings with trailing whitespace that can be
-- correctly inverted by 'unwords'. I.e.
--
-- > words  "a b c " == ["a","b","c"]
-- > words' "a b c " == ["a","b","c",""]
--
-- > unwords $ words  "a b c " == "a b c"
-- > unwords $ words' "a b c " == "a b c "
--
words' :: FastString -> [FastString]
words' ps = breakAll isSpace ps

-- | 'unwords\'' behaves like 'unwords'. It is provided for consistency
-- with the other invertable words and lines functions.
unwords' :: [FastString] -> FastString
unwords' = unwords

-- | A variety of 'head' for non-empty FastStrings. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the FastString is non-empty.
unsafeHead :: FastString -> Char
unsafeHead (PS x s _) = w2c $ unsafePerformIO $ 
    withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty FastStrings. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the FastString is non-empty.
unsafeTail :: FastString -> FastString
unsafeTail (PS ps s l) = PS ps (s+1) (l-1)
--  | l == 1    = empty
--  | otherwise = PS ps (s+1) (l-1)
{-# INLINE unsafeTail #-}

------------------------------------------------------------------------
-- Searching for substrings

-- | Check whether one string is a substring of another.
--   @isSubstringOf p s@ is equivalent to @not (null (findSubstrings p s))@.
isSubstringOf :: FastString -- ^ String to search for.
              -> FastString -- ^ String to search in.
              -> Bool
isSubstringOf p s = not $ Prelude.null $ findSubstrings p s

-- | Get the first index of a substring in another string,
--   or 'Nothing' if the string is not found.
--   @findSubstring p s@ is equivalent to @listToMaybe (findSubstrings p s)@.
findSubstring :: FastString -- ^ String to search for.
              -> FastString -- ^ String to seach in.
              -> Maybe Int
findSubstring p s = listToMaybe $ findSubstrings p s

-- Find the indexes of all (possibly overlapping) occurances 
-- of a substring in a string.
-- This function uses the Knuth-Morris-Pratt string matching algorithm.
findSubstrings :: FastString -- ^ String to search for.
               -> FastString -- ^ String to seach in.
               -> [Int]
findSubstrings pat@(PS _ _ m) str@(PS _ _ n) = search 0 0
  where
  patc x = w2c (pat ! x)
  strc x = w2c (str ! x)
  -- maybe we should make kmpNext a UArray before using it in search?
  kmpNext = listArray (0,m) (-1:kmpNextL pat (-1))
  kmpNextL p _ | null p = []
  kmpNextL p j = let j' = next (unsafeHead p) j + 1
                     ps = unsafeTail p
                     x = if not (null ps) && unsafeHead ps == patc j' 
                            then kmpNext Array.! j' else j'
                    in x:kmpNextL ps j'
  search i j = match ++ rest -- i: position in string, j: position in pattern
    where match = if j == m then [(i - j)] else []
          rest = if i == n then [] else search (i+1) (next (strc i) j + 1)
  next c j | j >= 0 && (j == m || c /= patc j) = next c (kmpNext Array.! j)
           | otherwise = j

------------------------------------------------------------------------
-- (Internal) Conversion between 'Word8' and 'Char'

w2c :: Word8 -> Char
#if !defined(__GLASGOW_HASKELL__)
w2c = chr . fromIntegral
#else
w2c = unsafeChr . fromIntegral
#endif
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

------------------------------------------------------------------------

-- | /O(n)/ 'elemIndexWord8' is like 'elemIndex', except
-- that it takes a 'Word8' as the element to search for.
elemIndexWord8 :: Word8 -> FastString -> Maybe Int
elemIndexWord8 c (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do
    let p' = p `plusPtr` s
        q  = memchr p' (fromIntegral c) (fromIntegral l)
    return $ if q == nullPtr then Nothing else Just (q `minusPtr` p')
{-# INLINE elemIndexWord8 #-}

-- | /O(n)/ The 'elemIndexLastWord8' function returns the last index of the
-- element in the given 'FastString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndexLastWord8 :: Word8 -> FastString -> Maybe Int
elemIndexLastWord8 c (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> 
        go (-1) (p `plusPtr` s) 0
    where 
        go h p i | i >= l    = return $ if h < 0 then Nothing else Just h
                 | otherwise = do here <- peekByteOff p i
                                  go (if c == here then i else h) p (i+1)
{-# INLINE elemIndexLastWord8 #-}

-- (Internal) unsafe 'FastString' index (subscript) operator, starting
-- from 0, returning a 'Word8'
(!) :: FastString -> Int -> Word8
(PS x s _l) ! i = unsafePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+i)
{-# INLINE (!) #-}

-- (Internal) 'findIndexOrEndPS' is a variant of findIndex, that returns the
-- length of the string if no element is found, rather than Nothing.
findIndexOrEndPS :: (Char -> Bool) -> FastString -> Int
findIndexOrEndPS f ps
    | null ps      = 0
    | f (unsafeHead ps) = 0
    | otherwise    = seq f $ 1 + findIndexOrEndPS f (unsafeTail ps)

-- (Internal)
findFromEndUntilPS :: (Char -> Bool) -> FastString -> Int
findFromEndUntilPS f ps@(PS x s l) = seq f $
    if null ps then 0
    else if f $ last ps then l
         else findFromEndUntilPS f (PS x s (l-1))

-- (Internal)
splitWord8 :: Word8 -> FastString -> [FastString]
splitWord8 c ps = case elemIndexWord8 c ps of
    Nothing -> if null ps then [] else [ps]
    Just n  -> take n ps : splitWord8 c (drop (n+1) ps)
{-# INLINE splitWord8 #-}

-- -----------------------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = error ("FastPackedString." ++ fun ++ ": empty FastString")

------------------------------------------------------------------------

-- | Convert a 'FastString' in UTF8 form to a 'String'
unpackFromUTF8 :: FastString -> String
unpackFromUTF8 (PS _ _ 0) = []
unpackFromUTF8 (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do 
    outbuf <- mallocArray l
    lout   <- utf8_to_ints outbuf (p `plusPtr` s) l
    when (lout < 0) $ error "Bad UTF8!"
    str    <- (Prelude.map chr) `liftM` peekArray lout outbuf
    free outbuf
    return str

#if defined(__GLASGOW_HASKELL__)
-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @FastString@. A useful way to create an
-- Addr\# is with an unboxed string literal, which is compiled to a
-- static @char []@ by GHC. Establishing the length of the string
-- requires a call to @strlen(3)@. Use 'unsafePackAddress' if you know
-- the length of the string statically. 
--
-- An example:
--
-- > literalFS = packAddress "literal"#
--
packAddress :: Addr# -> FastString
packAddress addr# = unsafePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 (fromIntegral $ c_strlen cstr)
    where
      cstr = Ptr addr# 
{-# INLINE packAddress #-}

-- | /O(1)/ 'unsafePackAddress' provides constant-time construction of
-- 'FastStrings' -- which is ideal for string literals. It packs a
-- null-terminated sequence of bytes into a 'FastString', given a raw
-- 'Addr\#' to the string, and the length of the string. Make sure the
-- length is correct, otherwise use the safer 'packAddress' (where the
-- length will be calculated once at runtime).
unsafePackAddress :: Int -> Addr# -> FastString
unsafePackAddress len addr# = unsafePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 len
    where
      cstr = Ptr addr# 
#endif

-- | Build a FastString from a ForeignPtr
fromForeignPtr :: ForeignPtr Word8 -> Int -> FastString
fromForeignPtr fp l = PS fp 0 l

-- | Deconstruct a ForeignPtr from a FastString
toForeignPtr :: FastString -> (ForeignPtr Word8, Int, Int)
toForeignPtr (PS ps s l) = (ps, s, l)

-- | /O(n)/ Make a copy of the 'FastString' with its own storage. 
--   This is mainly useful to allow the rest of the data pointed
--   to by the 'FastString' to be garbage collected, for example
--   if a large string has been read in, and only a small part of it 
--   is needed in the rest of the program.
copy :: FastString -> FastString
copy (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> 
                    c_memcpy p (f `plusPtr` s) l

-- | Given the maximum size needed and a function to make the contents
-- of a FastString, generate makes the 'FastString'. The
-- generating function is required to return the actual size (<= the
-- maximum size).
generate :: Int -> (Ptr Word8 -> IO Int) -> IO FastString
generate i f = do 
    p <- mallocArray i
    i' <- f p
    p' <- reallocArray p (i'+1)
    poke (p' `plusPtr` i') (0::Word8)    -- XXX so CStrings work
    fp <- newForeignPtr c_free p'
    return $ PS fp 0 i'

#if defined(__GLASGOW_HASKELL__)
-- | Construct a 'FastString' given a C Word8 buffer, a length, and an
-- IO action representing a finalizer.  This function is not available
-- on Hugs.
construct :: (Ptr Word8) -> Int -> IO () -> IO FastString
construct p l f = do 
    fp <- FC.newForeignPtr p f
    return $ PS fp 0 l
#endif

-- | /O(n)/ Build a @FastString@ from a malloced @CString@. This value will
-- have a @free(3)@ finalizer associated to it.
packMallocCString :: CString -> FastString
packMallocCString cstr = unsafePerformIO $ do 
    fp <- newForeignPtr c_free (castPtr cstr)
    return $ PS fp 0 (fromIntegral $ c_strlen cstr)

-- | /O(n)/ Build a @FastString@ from a @CString@. This value will have /no/
-- finalizer associated to it.
packCString :: CString -> FastString
packCString cstr = unsafePerformIO $ do 
    fp <- newForeignPtr_ (castPtr cstr)
    return $ PS fp 0 (fromIntegral $ c_strlen cstr)

-- | /O(1)/ Build a @FastString@ from a @CStringLen@. This value will
-- have /no/ finalizer associated with it.
packCStringLen :: CStringLen -> FastString
packCStringLen (ptr,len) = unsafePerformIO $ do
    fp <- newForeignPtr_ (castPtr ptr)
    return $ PS fp 0 (fromIntegral len)

-- | Use a @FastString@ with a function requiring a null-terminated @CString@.
--   The @CString@ should not be freed afterwards.
useAsCString :: FastString -> (CString -> IO a) -> IO a
useAsCString (PS ps s l) = bracket alloc free_cstring
    where 
      alloc = withForeignPtr ps $ \p -> do 
                buf <- c_malloc (fromIntegral l+1)
                c_memcpy (castPtr buf) (castPtr p `plusPtr` s) (fromIntegral l)
                poke (buf `plusPtr` l) (0::Word8)
                return $ castPtr buf

-- | Use a @FastString@ with a function requiring a @CString@.
--   Warning: modifying the @CString@ will affect the @FastString@.
-- It better be null terminated!
unsafeUseAsCString :: FastString -> (CString -> IO a) -> IO a
unsafeUseAsCString (PS ps s _) ac = withForeignPtr ps $ \p -> ac (castPtr p `plusPtr` s)

-- | Use a @FastString@ with a function requiring a @CStringLen@.
--   Warning: modifying the @CStringLen@ will affect the @FastString@.
unsafeUseAsCStringLen :: FastString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen (PS ps s l) ac = withForeignPtr ps $ \p -> ac (castPtr p `plusPtr` s,l)

-- | A way of creating ForeignPtrs outside the IO monad (although it
-- still isn't entirely "safe", but at least it's convenient.
createPS :: Int -> (Ptr Word8 -> IO ()) -> FastString
createPS l write_ptr = unsafePerformIO $ do 
    fp <- mallocForeignPtr (l+1)
    withForeignPtr fp $ \p -> write_ptr p
    return $ PS fp 0 l

#if defined(__GLASGOW_HASKELL__)
-- | Explicitly run the finaliser associated with a 'FastString'.
-- Further references to this value may generate invalid memory
-- references. This operation is unsafe, as there may be other
-- 'FastStrings' referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'FastString's
-- ever generated from the underlying byte array are no longer live.
unsafeFinalize :: FastString -> IO ()
unsafeFinalize (PS p _ _) = finalizeForeignPtr p
#endif

------------------------------------------------------------------------

-- (internal) GC wrapper of mallocForeignPtrArray
mallocForeignPtr :: Int -> IO (ForeignPtr Word8)
mallocForeignPtr l = do 
    when (l > 1000000) performGC
    fp <- mallocForeignPtrArray (l+1)
    withForeignPtr fp $ \p -> poke (p `plusPtr` l) (0::Word8)
    return fp

-- -----------------------------------------------------------------------------
-- I\/O functions

-- | Outputs a 'FastString' to the specified 'Handle'.
--
-- NOTE: the representation of the 'FastString' in the file is assumed to
-- be in the ISO-8859-1 encoding.  In other words, only the least signficant
-- byte is taken from each character in the 'FastString'.
--
hPut :: Handle -> FastString -> IO ()
hPut _ (PS _ _ 0)  = return ()
hPut h (PS ps 0 l) = withForeignPtr ps $ \p-> hPutBuf h p l
hPut h (PS ps s l) = withForeignPtr ps $ \p-> hPutBuf h (p `plusPtr` s) l

-- | Read a 'FastString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'.
--
-- NOTE: as with 'hPut', the string representation in the file is
-- assumed to be ISO-8859-1.
--
hGet :: Handle -> Int -> IO FastString
hGet _ 0 = return empty
hGet h i = do fp <- mallocForeignPtr i
              l  <- withForeignPtr fp $ \p-> hGetBuf h p i
              return $ PS fp 0 l


-- | hGetNonBlocking is identical to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.
--
hGetNonBlocking :: Handle -> Int -> IO FastString
hGetNonBlocking _ 0 = return empty
hGetNonBlocking h i
    = do fp <- mallocForeignPtr i
         l  <- withForeignPtr fp $ \p -> hGetBufNonBlocking h p i
         return $ PS fp 0 l

-- | Read entire handle contents into a 'FastString'.
--
-- NOTE: as with 'hGet', the string representation in the file is
-- assumed to be ISO-8859-1.
--
hGetContents :: Handle -> IO FastString
hGetContents h = do 
    let start_size = 1024
    p <- mallocArray start_size
    i <- hGetBuf h p start_size
    if i < start_size
        then do p' <- reallocArray p i
                fp <- newForeignPtr c_free p'
                return $ PS fp 0 i
        else f p start_size
    where 
        f p s = do 
        let s' = 2 * s
        p' <- reallocArray p s'
        i  <- hGetBuf h (p' `plusPtr` s) s
        if i < s 
            then do let i' = s + i
                    p'' <- reallocArray p' i'
                    fp  <- newForeignPtr c_free p''
                    return $ PS fp 0 i'
            else f p' s'

-- | Read an entire file directly into a 'FastString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet.
--
-- NOTE: as with 'hGet', the string representation in the file is
-- assumed to be ISO-8859-1.
--
readFile :: FilePath -> IO FastString
readFile f = do 
    h <- openBinaryFile f ReadMode
    l <- hFileSize h
    s <- hGet h $ fromIntegral l
    hClose h
    return s

-- | Write a 'FastString' to a file.
--
writeFile :: FilePath -> FastString -> IO ()
writeFile f ps = do 
    h <- openBinaryFile f WriteMode
    hPut h ps
    hClose h

-- | Like readFile, this reads an entire file directly into a
-- 'FastString', but it is even more efficient.  It involves directly
-- mapping the file to memory.  This has the advantage that the contents
-- of the file never need to be copied.  Also, under memory pressure the
-- page may simply be discarded, while in the case of readFile it would
-- need to be written to swap.  If you read many small files, mmapFile
-- will be less memory-efficient than readFile, since each mmapFile
-- takes up a separate page of memory.  Also, you can run into bus
-- errors if the file is modified.  NOTE: as with 'readFile', the
-- string representation in the file is assumed to be ISO-8859-1.
--
mmapFile :: FilePath -> IO FastString
mmapFile f = 
#if defined(USE_MMAP)
   mmap f >>= \(fp,l) -> return $ PS fp 0 l
#else
   readFile f
#endif

#if defined(USE_MMAP)
mmap :: FilePath -> IO (ForeignPtr Word8, Int)
mmap f = do
    h <- openBinaryFile f ReadMode
    l <- fromIntegral `liftM` hFileSize h
    -- Don't bother mmaping small files because each mmapped file takes up
    -- at least one full VM block.
    if l < mmap_limit
       then do thefp <- mallocForeignPtr l
               withForeignPtr thefp $ \p-> hGetBuf h p l
               hClose h
               return (thefp, l)
       else do
#if defined(__GLASGOW_HASKELL__)
               fd <- fromIntegral `liftM` handleToFd h
               p <- my_mmap l fd
               fp <- if p == nullPtr
                     then
#else
               fp <-
#endif
                          do thefp <- mallocForeignPtr l
                             withForeignPtr thefp $ \p' -> hGetBuf h p' l
                             return thefp
#if defined(__GLASGOW_HASKELL__)
                     else do
                          -- The munmap leads to crashes on OpenBSD.
                          -- maybe there's a use after unmap in there somewhere?
#if !defined(__OpenBSD__)
                             fp <- FC.newForeignPtr p (c_munmap p l >> return ())
#else
                             fp <- FC.newForeignPtr p (return ())
#endif
                             return fp
               c_close fd
#endif
               hClose h
               return (fp, l)
    where mmap_limit = 16*1024

#endif /* USE_MMAP */

-- -----------------------------------------------------------------------------

data LazyFile = LazyString String
              | MMappedFastString FastString
              | LazyFastStrings [FastString]
    deriving Eq

readFileLazily :: FilePath -> IO LazyFile
readFileLazily f = do
#if defined(USE_MMAP)
    liftM MMappedFastString (mmapFile f)
#else
    h <- openBinaryFile f ReadMode
    liftM LazyFastStrings $ readHandleLazily h
  where
    readHandleLazily :: Handle -> IO [FastString]
    readHandleLazily h
     = do let read_rest = do
                  -- We might be making too big a fp here
                  fp <- mallocForeignPtr blocksize
                  lread <- withForeignPtr fp
                         $ \p -> hGetBuf h p blocksize
                  case lread of
                      0 -> return []
                      l | l < blocksize -> do hClose h
                                              return [PS fp 0 l]
                      l -> do rest <- unsafeInterleaveIO read_rest
                              return (PS fp 0 l:rest)
          unsafeInterleaveIO read_rest
        where blocksize = 1024
#endif

-- -----------------------------------------------------------------------------
-- gzReadFile

-- | Read an entire file, which may or may not be gzip compressed, directly
-- into a 'FastString'.

#if defined(USE_ZLIB)

gzReadFile :: FilePath -> IO FastString
gzReadFile f = do
    h <- openBinaryFile f ReadMode
    header <- hGet h 2
    if header /= pack "\31\139"
       then do hClose h
               mmapFile f
       else do hSeek h SeekFromEnd (-4)
               len <- hGetLittleEndInt h
               hClose h
               withCString f $ \fstr-> withCString "rb" $ \rb-> do
                 gzf <- c_gzopen fstr rb
                 when (gzf == nullPtr) $ fail $ "problem opening file "++f
                 fp <- mallocForeignPtr len
                 lread <- withForeignPtr fp $ \p -> c_gzread gzf p len
                 c_gzclose gzf
                 when (lread /= len) $ fail $ "problem gzreading file "++f
                 return $ PS fp 0 len

gzReadFileLazily :: FilePath -> IO LazyFile
gzReadFileLazily f = do
    h <- openBinaryFile f ReadMode
    header <- hGet h 2
    if header == pack "\31\139" then
        do hClose h
           withCString f $ \fstr-> withCString "rb" $ \rb-> do
               gzf <- c_gzopen fstr rb
               when (gzf == nullPtr) $ fail $ "problem opening file "++f
               let read_rest = do
                       -- We might be making too big a fp here
                       fp <- mallocForeignPtr blocksize
                       lread <- withForeignPtr fp
                              $ \p -> c_gzread gzf p blocksize
                       case lread of
                           0 -> do c_gzclose gzf
                                   return []
                           -1 -> fail $ "problem gzreading file "++f
                           l -> do rest <- unsafeInterleaveIO read_rest
                                   return (PS fp 0 l:rest)
               liftM LazyFastStrings read_rest
        else
#if defined(USE_MMAP)
             hClose h >> liftM MMappedFastString (mmapFile f)
#else
             liftM (LazyFastStrings . (header:)) $ readHandleLazily h
#endif
    where blocksize = 1024

hGetLittleEndInt :: Handle -> IO Int
hGetLittleEndInt h = do
    b1 <- ord `liftM` hGetChar h
    b2 <- ord `liftM` hGetChar h
    b3 <- ord `liftM` hGetChar h
    b4 <- ord `liftM` hGetChar h
    return $ b1 + 256*b2 + 65536*b3 + 16777216*b4

gzWriteFile :: FilePath -> FastString -> IO ()
gzWriteFile f ps = gzWriteFilePSs f [ps]

gzWriteFilePSs :: FilePath -> [FastString] -> IO ()
gzWriteFilePSs f pss  =
    withCString f $ \fstr -> withCString "wb" $ \wb -> do
    gzf <- c_gzopen fstr wb
    when (gzf == nullPtr) $ fail $ "problem gzopening file for write: "++f
    mapM_ (gzWriteToGzf gzf) pss `catch`
              \_ -> fail $ "problem gzwriting file: "++f
    c_gzclose gzf

gzWriteToGzf :: Ptr () -> FastString -> IO ()
gzWriteToGzf gzf (PS x s l) = do
    lw <- withForeignPtr x $ \p -> c_gzwrite gzf (p `plusPtr` s) l
    when (lw /= l) $ fail $ "problem in gzWriteToGzf"

#endif /* USE_ZLIB */

------------------------------------------------------------------------
-- TODO reduce the number of foreign imports if possible

foreign import ccall unsafe "static stdlib.h malloc" c_malloc
    :: CInt -> IO (Ptr Word8)

foreign import ccall unsafe "static stdio.h &free" c_free
    :: FunPtr (Ptr Word8 -> IO ())

foreign import ccall unsafe "static stdlib.h free" free_cstring
    :: CString -> IO ()

foreign import ccall unsafe "static string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "static string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "string.h memchr" memchr
    :: Ptr Word8 -> CInt -> CSize -> Ptr Word8

foreign import ccall unsafe "static string.h strlen" c_strlen
    :: CString -> CInt

------------------------------------------------------------------------

foreign import ccall unsafe "static fpstring.h utf8_to_ints" utf8_to_ints
    :: Ptr Int -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "fpstring.h firstnonspace" c_firstnonspace
    :: Ptr Word8 -> Int -> Int

foreign import ccall unsafe "fpstring.h lastnonspace" c_lastnonspace
    :: Ptr Word8 -> Int -> Int

foreign import ccall unsafe "fpstring.h firstspace" c_firstspace
    :: Ptr Word8 -> Int -> Int

foreign import ccall unsafe "static fpstring.h reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "static fpstring.h my_qsort" c_qsort
    :: Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "static fpstring.h intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> Int -> Word8 -> IO ()

foreign import ccall unsafe "static fpstring.h maximum" c_maximum
    :: Ptr Word8 -> Int -> Word8

foreign import ccall unsafe "static fpstring.h minimum" c_minimum
    :: Ptr Word8 -> Int -> Word8

------------------------------------------------------------------------

#if defined(USE_MMAP)
foreign import ccall unsafe "static fpstring.h my_mmap" my_mmap
    :: Int -> Int -> IO (Ptr Word8)

#if !defined(__OpenBSD__)
foreign import ccall unsafe "static sys/mman.h munmap" c_munmap
    :: Ptr Word8 -> Int -> IO Int
#endif

foreign import ccall unsafe "static unistd.h close" c_close
    :: Int -> IO Int
#endif

#if defined(USE_ZLIB)
foreign import ccall unsafe "static zlib.h gzopen" c_gzopen
    :: CString -> CString -> IO (Ptr ())

foreign import ccall unsafe "static zlib.h gzclose" c_gzclose
    :: Ptr () -> IO ()

foreign import ccall unsafe "static zlib.h gzread" c_gzread
    :: Ptr () -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "static zlib.h gzwrite" c_gzwrite
    :: Ptr () -> Ptr Word8 -> Int -> IO Int
#endif
