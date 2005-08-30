--
-- Module      : FastPackedString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Don Stewart 2005
-- License     : GPL (Also happy to license this file as BSD style)
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
-- 
-- This program is free software; you can redistribute it and\/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--

--
-- | An time and space-efficient implementation of strings as packed
-- byte arrays.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- >  import Data.FastPackedString as P
--
-- Original GHC implementation by Bryan O\'Sullivan. Rewritten to use
-- UArray by Simon Marlow. Rewritten to support slices and use
-- ForeignPtr by David Roundy. Cleanups and extensions by Don Stewart.
--

module Data.FastPackedString (

        -- * The @PackedString@ type
        PackedString,           -- abstract, instances: Eq, Ord, Show, Typeable

        -- * Introducing and eliminating @PackedString@s
        empty,                  -- :: PackedString
        pack,                   -- :: String -> PackedString
        unpack,                 -- :: PackedString -> String
        packWords,              -- :: [Word8] -> PackedString
        unpackWords,            -- :: PackedString -> [Word8]

        -- * Basic list-like interface
        cons,         -- :: Char -> PackedString -> PackedString
        snoc,         -- :: PackedString -> Char -> PackedString
        append,       -- :: PackedString -> PackedString -> PackedString
        head,         -- :: PackedString -> Char
        tail,         -- :: PackedString -> PackedString
        last,         -- :: PackedString -> Char
        init,         -- :: PackedString -> PackedString
        null,         -- :: PackedString -> Bool
        length,       -- :: PackedString -> Int
        head1,        -- :: PackedString -> Char
        tail1,        -- :: PackedString -> PackedString

        -- * List transformations
        map,          -- :: (Char -> Char) -> PackedString -> PackedString
        reverse,      -- :: PackedString -> PackedString
        intersperse,  -- :: Char -> PackedString -> PackedString
        transpose,    -- :: [PackedString] -> [PackedString]
        join,         -- :: PackedString -> [PackedString] -> PackedString

        -- * Reducing lists (folds)
        foldl,        -- :: (a -> Char -> a) -> a -> PackedString -> a
        foldr,        -- :: (Char -> a -> a) -> a -> PackedString -> a
        foldl1,       -- :: (Char -> Char -> Char) -> PackedString -> Char
        foldr1,       -- :: (Char -> Char -> Char) -> PackedString -> Char

        -- ** Special folds
        concat,       -- :: [PackedString] -> PackedString
        concatMap,    -- :: (Char -> PackedString) -> PackedString -> PackedString
        any,          -- :: (Char -> Bool) -> PackedString -> Bool
        all,          -- :: (Char -> Bool) -> PackedString -> Bool
        maximum,      -- :: PackedString -> Char
        minimum,      -- :: PackedString -> Char

        -- * Sublists
        take,         -- :: Int -> PackedString -> PackedString
        drop,         -- :: Int -> PackedString -> PackedString
        splitAt,      -- :: Int -> PackedString -> (PackedString, PackedString)

        takeWhile,    -- :: (Char -> Bool) -> PackedString -> PackedString
        dropWhile,    -- :: (Char -> Bool) -> PackedString -> PackedString
        span,         -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
        break,        -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)

        -- * Searching lists

        -- ** Searching by equality
        elem,         -- :: Char -> PackedString -> Bool

        -- ** Searching with a predicate
        filter,       -- :: (Char -> Bool) -> PackedString -> PackedString
        find,         -- :: (Char -> Bool) -> PackedString -> Maybe Char

        -- * Indexing lists
        index,        -- :: PackedString -> Int -> Char
        elemIndex,    -- :: Char -> PackedString -> Maybe Int
        elemIndices,  -- :: Char -> PackedString -> [Int]

        findIndex,    -- :: (Char -> Bool) -> PackedString -> Maybe Int
        findIndices,  -- :: (Char -> Bool) -> PackedString -> [Int]

        -- * Special lists

        -- ** Lines and words
        lines,        -- :: PackedString -> [PackedString]
        words,        -- :: PackedString -> [PackedString]
        unlines,      -- :: [PackedString] -> PackedString
        unwords,      -- :: PackedString -> [PackedString]

        -- ** Ordered lists
        sort,         -- :: PackedString -> PackedString

        -- * Extensions to the list interface
        dropSpace,    -- :: PackedString -> PackedString
        breakSpace,   -- :: PackedString -> Maybe (PackedString,PackedString)
        spanEnd,      -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
        breakOn,      -- :: Char -> PackedString -> (PackedString, PackedString)
        split,        -- :: Char -> PackedString -> [PackedString]
        breakAll,     -- :: (Char -> Bool) -> PackedString -> [PackedString]
        tokens,       -- :: (Char -> Bool) -> PackedString -> [PackedString]

        ------------------------------------------------------------------------

        hashPS,
        breakFirstPS,   -- :: Char -> PackedString -> Maybe (PackedString,PackedString)
        breakLastPS,    -- :: Char -> PackedString -> Maybe (PackedString,PackedString)
        readIntPS,      -- :: PackedString -> Maybe (Int, PackedString)
        fromHex2PS,     -- :: PackedString -> PackedString
        fromPS2Hex,     -- :: PackedString -> PackedString
        betweenLinesPS, --  :: PackedString -> PackedString -> PackedString -> Maybe (PackedString)
        breakAfterNthNewline,   
        breakBeforeNthNewline,

        -- should this even be available?
        unsafeConcatLenPS, -- :: Int -> [PackedString] -> PackedString

        -- * Low-level constructors
        generatePS,             -- :: Int -> (Ptr Word8 -> Int -> IO Int) -> IO PackedString
#if defined(__GLASGOW_HASKELL__)
        constructPS,            -- :: (Ptr Word8) -> Int -> IO () -> IO PackedString
#endif
        mallocedCString2PS,     -- :: CString -> IO PackedString
        withCStringPS,          -- :: PackedString -> (CString -> IO a) -> IO a
        unpackFromUTF8,         -- :: PackedString -> String
        unsafeWithInternals,    -- :: PackedString -> (Ptr Word8 -> Int -> IO a) -> IO a

        -- * I\/O with @PackedString@s
        hGetPS,                 -- :: Handle -> Int -> IO PackedString
        hPutPS,                 -- :: Handle -> PackedString -> IO ()
        hGetContentsPS,         -- :: Handle -> IO PackedString
        readFilePS,             -- :: FilePath -> IO PackedString
        writeFilePS,            -- :: FilePath -> PackedString -> IO ()
        mmapFilePS,             -- :: FilePath -> IO PackedString

        -- * Extensions to the I\/O interface
        LazyFile(..),
        readFileLazily,         -- :: FilePath -> IO LazyFile
#if defined(USE_ZLIB)
        gzReadFilePS,           -- :: FilePath -> IO PackedString
        gzReadFileLazily,       -- :: FilePath -> IO LazyFile
        gzWriteFilePS,          -- :: FilePath -> PackedString -> IO ()
        gzWriteFilePSs,         -- :: FilePath -> [PackedString] -> IO ()
#endif

   ) where

import qualified Prelude
import Prelude hiding (reverse,head,tail,last,init,null,
                       length,map,lines,foldl,foldr,unlines,
                       concat,any,take,drop,splitAt,takeWhile,
                       dropWhile,span,break,elem,filter,unwords,
                       words,maximum,minimum,all,concatMap,
                       foldl1,foldr1)

import qualified Data.List as List (intersperse,transpose)

import Data.Bits                (rotateL)
import Data.Char                (chr, ord, String, isSpace)
import Data.Int                 (Int32)
import Data.Word                (Word8)
import Data.Maybe               (listToMaybe)

import Control.Monad            (when, liftM)
import Control.Exception        (bracket)

import System.IO
import System.IO.Unsafe         (unsafePerformIO, unsafeInterleaveIO)
import System.Mem               (performGC)

import Foreign.Ptr              (Ptr, FunPtr, plusPtr, nullPtr, minusPtr, castPtr)
import Foreign.ForeignPtr       (newForeignPtr, withForeignPtr, mallocForeignPtrArray, ForeignPtr)
import Foreign.Storable         (peekElemOff, peek, poke)
import Foreign.C.String         (CString)
import Foreign.C.Types          (CSize, CLong, CInt)
import Foreign.Marshal.Alloc    (free)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils    (with)

#if defined(__GLASGOW_HASKELL__)
import qualified Foreign.Concurrent as FC (newForeignPtr)

#if defined(USE_MMAP)
import System.Posix             (handleToFd)
#endif

#endif

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a 'String', which supports various
-- efficient operations.  A 'PackedString' contains 8-bit characters only.
--
data PackedString = PS {-# UNPACK #-} !(ForeignPtr Word8) !Int !Int

------------------------------------------------------------------------

instance Eq  PackedString 
    where (==)    = eqPS

instance Ord PackedString 
    where compare = comparePS

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpack ps) r

------------------------------------------------------------------------

-- | /O(n)/ Equality on the 'PackedString' type
eqPS :: PackedString -> PackedString -> Bool
eqPS a b = (comparePS a b) == EQ
{-# INLINE eqPS #-}

-- | /O(n)/ 'comparePS' provides an 'Ordering' for 'PackedStrings' supporting slices.
comparePS :: PackedString -> PackedString -> Ordering
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

-- | /O(1)/ The empty 'PackedString'
empty :: PackedString
empty = unsafePerformIO $ mallocForeignPtr 1 >>= \fp -> return $ PS fp 0 0
{-# NOINLINE empty #-}

-- | /O(n)/ Convert a 'String' into a 'PackedString'
pack :: String -> PackedString
pack str = createPS (Prelude.length str) $ \p -> pokeArray p $ Prelude.map c2w str

-- | /O(n)/ Convert a '[Word8]' into a 'PackedString'
packWords :: [Word8] -> PackedString
packWords s = createPS (Prelude.length s) $ \p -> pokeArray p s

-- | /O(n)/ Convert a 'PackedString' into a 'String'
unpack :: PackedString -> String
unpack (PS ps s l) = Prelude.map w2c $ unsafePerformIO $ withForeignPtr ps $ \p -> 
    peekArray l (p `plusPtr` s)

-- | /O(n)/ Convert a 'PackedString' to a '[Word8]'
unpackWords :: PackedString -> [Word8]
unpackWords ps@(PS x s _)
    | null ps     = []
    | otherwise     =
        (unsafePerformIO $ withForeignPtr x $ \p -> peekElemOff p s) 
            : unpackWords (tail1 ps)

------------------------------------------------------------------------
-- (Internal) Conversion between 'Word8' and 'Char'

w2c :: Word8 -> Char
w2c = chr . fromIntegral
{-# INLINE w2c #-}

c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- -----------------------------------------------------------------------------
-- List-like functions for PackedStrings

-- | /O(n)/ 'cons' is analogous to (:) for lists. Requires a memcpy.
cons :: Char -> PackedString -> PackedString
cons c (PS x s l) = createPS (l+1) $ \p -> withForeignPtr x $ \f -> do
        c_memcpy (p `plusPtr` 1) (f `plusPtr` s) l  -- 99% less space
        poke p (c2w c)

-- | /O(n)/ Append a character to the end of a 'PackedString'
snoc :: PackedString -> Char -> PackedString
snoc (PS x s l) c = createPS (l+1) $ \p -> withForeignPtr x $ \f -> do
        c_memcpy p (f `plusPtr` s) l
        poke (p `plusPtr` l) (c2w c)

-- | /O(1)/ Extract the first element of a packed string, which must be non-empty.
head :: PackedString -> Char
head ps@(PS x s _)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | null ps   = errorEmptyList "head"
  | otherwise = w2c $ unsafePerformIO $ withForeignPtr x $ \p -> peekElemOff p s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a packed string, which must be non-empty.
tail :: PackedString -> PackedString
tail (PS p s l) 
    | l <= 0    = errorEmptyList "tail"
    | l == 1    = empty                                                                    
    | otherwise = PS p (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the last element of a packed string, which must be finite and non-empty.
last :: PackedString -> Char
last ps@(PS x s l)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | null ps   = errorEmptyList "last"
  | otherwise = w2c $ unsafePerformIO $ 
        withForeignPtr x $ \p -> peekElemOff p (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a list except the last one.
init :: PackedString -> PackedString
init (PS p s l) 
    | l <= 0    = errorEmptyList "init"
    | l == 1    = empty                                                                    
    | otherwise = PS p s (l-1)                                                          
{-# INLINE init #-}

-- | /O(1)/ Test whether a packed string is empty.
null :: PackedString -> Bool
null (PS _ _ l) = l == 0
{-# INLINE null #-}

-- | /O(1)/ 'length' returns the length of a packed string as an 'Int'.
length :: PackedString -> Int
length (PS _ _ l) = l
{-# INLINE length #-}

-- | /O(n)/ Append two packed strings
append :: PackedString -> PackedString -> PackedString
append xs ys
    | null xs = ys
    | null ys = xs
    | otherwise  = concat [xs,ys]
{-# INLINE append #-}

-- | /O(n)/ 'map' @f xs@ is the packed string obtained by applying @f@ to each
-- element of @xs@, i.e.,
map :: (Char -> Char) -> PackedString -> PackedString
map k (PS ps s l) = createPS l $ \p -> withForeignPtr ps $ \f -> 
        go (f `plusPtr` s) p l
    where 
        go :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
        go _ _ 0    = return ()
        go f t len  = do w <- peek f
                         ((poke t) . c2w . k . w2c) w
                         go (f `plusPtr` 1) (t `plusPtr` 1) (len - 1)

-- | /O(n)/ 'filter', applied to a predicate and a packed string,
-- returns a packed string containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> PackedString -> PackedString
filter k ps@(PS x s l)
    | null ps   = ps
    | otherwise = unsafePerformIO $ generatePS l $ \p -> withForeignPtr x $ \f -> do
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
find :: (Char -> Bool) -> PackedString -> Maybe Char
find p ps = case filter p ps of
        p' | null p' -> Nothing
           | otherwise -> Just (head1 p')

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a packed string, reduces the
-- packed string using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> PackedString -> a
foldl f v (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        lgo v (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        lgo z p q | p == q    = return z
                  | otherwise = do c <- liftM w2c $ peek p
                                   lgo (f z c) (p `plusPtr` 1) q

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> PackedString -> a
foldr k z (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        go p q | p == q    = return z
               | otherwise = do c  <- liftM w2c $ peek p
                                ws <- go (p `plusPtr` 1) q
                                return $ c `k` ws

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'PackedStrings'.
foldl1 :: (Char -> Char -> Char) -> PackedString -> Char
foldl1 f ps
    | null ps   = errorEmptyList "foldl1"
    | otherwise = foldl f (head1 ps) (tail1 ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'PackedString's
foldr1 :: (Char -> Char -> Char) -> PackedString -> Char
foldr1 f ps
    | null ps        = errorEmptyList "foldr1"
    | length ps == 1 = head1 ps
    | otherwise      = f (head1 ps) (foldr1 f (tail1 ps))

-- | Applied to a predicate and a packed string, 'any' determines if
-- any element of the list satisfies the predicate.
any :: (Char -> Bool) -> PackedString -> Bool
any f (PS x s l) = unsafePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where 
        go p q | p == q    = return False
               | otherwise = do c <- liftM w2c $ peek p
                                if f c then return True
                                       else go (p `plusPtr` 1) q

-- | Applied to a predicate and a 'PackedString', 'all' determines if
-- all elements of the 'PackedString' satisfy the predicate.
all :: (Char -> Bool) -> PackedString -> Bool
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
takeWhile :: (Char -> Bool) -> PackedString -> PackedString
takeWhile f ps = seq f $ take (findIndexOrEndPS (not . f) ps) ps
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> PackedString -> PackedString
dropWhile f ps = seq f $ drop (findIndexOrEndPS (not . f) ps) ps
{-# INLINE dropWhile #-}

-- | 'take' @n@, applied to a packed string @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> PackedString -> PackedString
take n ps@(PS x s l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE take #-}

-- | 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> PackedString -> PackedString
drop n ps@(PS x s l)
    | n <= 0    = ps
    | n >  l    = empty
    | otherwise = PS x (s+n) (l-n)
{-# INLINE drop #-}

-- | 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> PackedString -> (PackedString, PackedString)
splitAt  n ps  = (take n ps, drop n ps)
{-# INLINE splitAt #-}

-- | 'span' @p xs@ breaks the packed string into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
span  p ps = break (not . p) ps

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
break p ps = case findIndexOrEndPS p ps of n -> (take n ps, drop n ps)

-- | 'reverse' @xs@ returns the elements of @xs@ in reverse order.
reverse :: PackedString -> PackedString
reverse (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> 
        c_reverse p (f `plusPtr` s) l -- 99% less space, very much faster

-- | 'elem' is the list membership predicate.
elem :: Char -> PackedString -> Bool
elem c ps = case elemIndex c ps of
    Nothing -> False
    Just _  -> True

-- | Map a function over a 'PackedString' and concatenate the results
concatMap :: (Char -> PackedString) -> PackedString -> PackedString
concatMap f = foldr (append . f) empty

-- | Concatenate a list of packed strings.
concat :: [PackedString] -> PackedString
concat []     = empty
concat [ps]   = ps
concat xs     = unsafePerformIO $ do 
    let start_size = 1024
    p <- mallocArray start_size
    f p 0 1024 xs

    where f ptr len _ [] = do 
                ptr' <- reallocArray ptr len
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

-- | Analogous to @concat@, only you tell it how big the result will
-- be. If you lie then Bad Things will happen.
unsafeConcatLenPS :: Int -> [PackedString] -> PackedString
unsafeConcatLenPS n []   = n `seq` empty
unsafeConcatLenPS _ [ps] = ps
unsafeConcatLenPS total_length pss = createPS total_length $ \p-> cpPSs p pss
    where cpPSs :: Ptr Word8 -> [PackedString] -> IO ()
          cpPSs _ [] = return ()
          cpPSs p (PS x s l:rest) = do 
                withForeignPtr x $ \pf -> c_memcpy p (pf `plusPtr` s) l
                cpPSs (p `plusPtr` l) rest

-- | 'PackedString' index (subscript) operator, starting from 0.
index :: PackedString -> Int -> Char
index ps n 
    | n < 0          = error "FastPackedString.index: negative index"
    | n >= length ps = error "FastPackedString.index: index too large"
    | otherwise      = w2c $ ps ! n
{-# INLINE index #-}

-- | 'maximum' returns the maximum value from a 'PackedString'
maximum :: PackedString -> Char
maximum xs@(PS x s l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = unsafePerformIO $ withForeignPtr x $ \p -> 
                    return $ w2c $ c_maximum (p `plusPtr` s) l

-- | 'maximum' returns the maximum value from a 'PackedString'
minimum :: PackedString -> Char
minimum xs@(PS x s l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = unsafePerformIO $ withForeignPtr x $ \p -> 
                    return $ w2c $ c_minimum (p `plusPtr` s) l

-- | 'lines' breaks a packed string up into a list of packed strings
-- at newline characters.  The resulting strings do not contain
-- newlines.
lines :: PackedString -> [PackedString]
lines ps 
    | null ps = []
    | otherwise = case elemIndexWord8PS (c2w '\n') ps of
             Nothing -> [ps]
             Just n  -> take n ps : lines (drop (n+1) ps)
{-# INLINE lines #-}

-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [PackedString] -> PackedString
unlines [] = empty
unlines ss = (concat $ List.intersperse nl ss) `append` nl -- half as much space
    where
      nl = pack "\n"

-- | 'words' breaks a packed string up into a list of words, which
-- were delimited by white space.
words :: PackedString -> [PackedString]
words ps = Prelude.filter (not.null) (breakAll isSpace ps)

{-
-- darcs version has different behaviour.
-- We may wish to export these under an alternate name.

lines :: PackedString -> [PackedString]
lines ps = case wfind (c2w '\n') ps of
             Nothing -> [ps]
             Just n -> take n ps : lines (drop (n+1) ps)

unlines :: [PackedString] -> PackedString
unlines ss = concat $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a:newline: intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = pack "\n"

words :: PackedString -> [PackedString]
words ps = breakAll isSpace ps
-}

-- | The 'unwords' function is analogous to the 'unwords' function.
unwords :: [PackedString] -> PackedString
unwords = join $ pack " "

-- | The 'intersperse' function takes a 'Char' and a 'PackedString' and
-- \`intersperses\' that 'Char' between the elements of the 'PackedString'.
-- It is analogous to the intersperse function on Lists.
intersperse :: Char -> PackedString -> PackedString
intersperse c ps@(PS x s l)
    | length ps < 2  = ps
    | otherwise      = createPS (2*l-1) $ \p -> withForeignPtr x $ \f ->
                            c_intersperse p (f `plusPtr` s) l (c2w c)

-- | The 'transpose' function transposes the rows and columns of its
-- 'PackedString' argument.
transpose :: [PackedString] -> [PackedString]
transpose ps = Prelude.map pack (List.transpose (Prelude.map unpack ps)) -- better

-- | The 'join' function takes a 'PackedString' and a list of 'PackedString's
-- and concatenates the list after interspersing the first argument between
-- each element of the list.
join :: PackedString -> [PackedString] -> PackedString
join filler pss = concat (splice pss)
    where
        splice []  = []
        splice [x] = [x]
        splice (x:y:xs) = x:filler:splice (y:xs)

-- | /O(n log(n))/ Sort using an /unstable/ sorting algorithm (QSORT(3)).
sort :: PackedString -> PackedString
sort (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> do
        c_memcpy p (f `plusPtr` s) l
        c_qsort p l -- inplace

-- | /O(n)/ The 'elemIndex' function returns the index of the first element
-- in the given 'PackedString' which is equal (by memchr) to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Char -> PackedString -> Maybe Int
elemIndex c ps = elemIndexWord8PS (c2w c) ps
{-# INLINE elemIndex #-}

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> PackedString -> [Int]
elemIndices x = findIndices (x==)

-- | The 'findIndex' function takes a predicate and a 'PackedString'
-- and returns the index of the first element in the packed string
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> PackedString -> Maybe Int
findIndex f = listToMaybe . findIndices f

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> PackedString -> [Int]
findIndices p ps = loop 0 ps
	where
       loop _ ps' | null ps'      = []
       loop n ps' | p (head1 ps') = n : loop (n + 1) (tail1 ps')
                  | otherwise     = loop (n + 1) (tail1 ps')

-- | A variety of 'head' for non-empty 'packedString's.
head1 :: PackedString -> Char
head1 (PS x s _) = w2c $ unsafePerformIO $ 
    withForeignPtr x $ \p -> peekElemOff p s
{-# INLINE head1 #-}

-- | A variety of 'tail' for non-empty 'packedString's
tail1 :: PackedString -> PackedString
tail1 (PS ps s l)
    | l == 1    = empty
    | otherwise = PS ps (s+1) (l-1)
{-# INLINE tail1 #-}

------------------------------------------------------------------------
-- Extensions to the list interface

-- | 'dropWhite' returns the 'PackedString' argument with white space
-- removed from the front. I.e.
-- 
-- > dropWhile isSpace == dropSpace
--
dropSpace :: PackedString -> PackedString
dropSpace (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do
    let i = c_firstnonspace (p `plusPtr` s) l
    return $ if i == l then empty else PS x (s+i) (l-i)
{-# INLINE dropSpace #-}

-- | 'breakSpace' returns the pair of 'PackedString's when the argument
-- is broken at the first whitespace character. I.e.
-- 
-- > break isSpace == breakSpace
--
breakSpace :: PackedString -> (PackedString,PackedString)
breakSpace (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do 
    let i = c_firstspace (p `plusPtr` s) l
    return $ case () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
    }
{-# INLINE breakSpace #-}

-- | 'spanEnd' behaves like 'span' but from the end of the
-- 'PackedString'. I.e.
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . Char.isSpace) ps
-- >    == 
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x) 
--
spanEnd :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanEnd  p ps = splitAt (findFromEndUntilPS (not.p) ps) ps

-- | 'breakOn' breaks its 'PackedString' argument at the first occurence
-- of the specified character, however it is more efficient, as it is
-- implemented using memchr(3). I.e.
-- 
-- > break (=='c') "abcd" == breakOn 'c' "abcd"
--
breakOn :: Char -> PackedString -> (PackedString, PackedString)
breakOn c p = case elemIndex c p of
                    Nothing -> (p,empty)
                    Just n -> (take n p, drop n p)
{-# INLINE breakOn #-}

-- | Break a 'PackedString' into pieces separated by the 'Char'
-- argument, consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X"]
--
split :: Char -> PackedString -> [PackedString]
split c = splitWord8 (c2w c)
{-# INLINE split #-}

-- | Like 'breakAll', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> PackedString -> [PackedString]
tokens p = Prelude.filter (not.null) . breakAll p

-- | Splits a 'PackedString' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > breakAll (=='a') "aabbaca" == ["","","bb","c",""]
--
breakAll :: (Char -> Bool) -> PackedString -> [PackedString]
breakAll p ps = if null rest 
                    then [chunk] 
                    else chunk : breakAll p (tail1 rest)
    where 
      (chunk,rest) = break p ps
{-
-- weird, inefficient version. Probably slightly different to the above
breakAll :: (Char -> Bool) -> PackedString -> [PackedString]
breakAll f ps =
    case [ m | m <- [0..length ps-1], f (w2c (ps ! m)) ] of
        [] -> if null ps then [] else [ps]
        (n:_) -> take n ps : breakAll f (drop (n+1) ps)
-}

------------------------------------------------------------------------

-- | (Internal) /O(n)/ 'elemIndexWord8PS' is like 'elemIndex', except
-- that it takes a 'Word8' as the element to search for.
elemIndexWord8PS :: Word8 -> PackedString -> Maybe Int
elemIndexWord8PS c (PS x s l) = unsafePerformIO $ 
    withForeignPtr x $ \p -> do
        let p' = p `plusPtr` s
            q  = memchr p' (fromIntegral c) (fromIntegral l)
        return $ if q == nullPtr then Nothing else Just (q `minusPtr` p')
{-# INLINE elemIndexWord8PS #-}

-- (Internal) unsafe 'PackedString' index (subscript) operator, starting
-- from 0, returning a 'Word8'
(!) :: PackedString -> Int -> Word8
(PS x s _l) ! i = unsafePerformIO $ withForeignPtr x $ \p -> peekElemOff p (s+i)
{-# INLINE (!) #-}

-- (Internal) 'findIndexOrEndPS' is a variant of findIndex, that returns the
-- length of the string if no element is found, rather than Nothing.
findIndexOrEndPS :: (Char -> Bool) -> PackedString -> Int
findIndexOrEndPS f ps
    | null ps      = 0
    | f (head1 ps) = 0
    | otherwise    = seq f $ 1 + findIndexOrEndPS f (tail1 ps)

-- (Internal)
findFromEndUntilPS :: (Char -> Bool) -> PackedString -> Int
findFromEndUntilPS f ps@(PS x s l) = seq f $
    if null ps then 0
    else if f $ last ps then l
         else findFromEndUntilPS f (PS x s (l-1))

{-# INLINE hashPS #-}
hashPS :: PackedString -> Int32
hashPS (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do 
    hash (p `plusPtr` s) l

hash :: Ptr Word8 -> Int -> IO Int32
hash ptr len = f (0 :: Int32) ptr len
 where f h _ 0 = return h
       f h p n = do x <- peek p
                    let h' =  (fromIntegral x) + (rotateL h 8)
                    f h' (p `advancePtr` 1) (n-1)

{-# INLINE breakFirstPS #-}
breakFirstPS :: Char -> PackedString -> Maybe (PackedString,PackedString)
breakFirstPS c p = case elemIndex c p of
                       Nothing -> Nothing
                       Just n -> Just (take n p, drop (n+1) p)

{-# INLINE breakLastPS #-}
breakLastPS :: Char -> PackedString -> Maybe (PackedString,PackedString)
breakLastPS c p = case findLastPS c p of
                      Nothing -> Nothing
                      Just n -> Just (take n p, drop (n+1) p)

{-# INLINE findLastPS #-}
findLastPS :: Char -> PackedString -> Maybe Int
findLastPS c ps = wfindLastPS (c2w c) ps

{-# INLINE wfindLastPS #-}
wfindLastPS :: Word8 -> PackedString -> Maybe Int
wfindLastPS c (PS x s l) =
    unsafePerformIO $ withForeignPtr x $ \p->
                    findit (-1) (p `plusPtr` s) 0
    where findit h p i = if i >= l
                         then if h < 0
                              then return Nothing
                              else return $ Just h
                         else do here <- peekElemOff p i
                                 if c == here
                                    then findit i p (i+1)
                                    else findit h p (i+1)

splitWord8 :: Word8 -> PackedString -> [PackedString]
splitWord8 c ps = case elemIndexWord8PS c ps of
    Nothing -> if null ps then [] else [ps]
    Just n  -> take n ps : splitWord8 c (drop (n+1) ps)
{-# INLINE splitWord8 #-}

-- | readIntPS skips any whitespace at the beginning of its argument, and
-- reads an Int from the beginning of the PackedString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise it
-- just returns the int read, along with a PackedString containing the
-- remainder of its input.  The actual parsing is done by the standard C
-- library function strtol.
--
readIntPS :: PackedString -> Maybe (Int, PackedString)
readIntPS (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> 
    with p $ \endpp -> do
       val <- c_strtol (p `plusPtr` s) endpp 0
       skipped <- (`minusPtr` (p `plusPtr` s)) `liftM` peek endpp
       if skipped == 0
          then return Nothing
          else return $ Just (fromIntegral val,
                              PS x (s+skipped) (l-skipped))

-- | fromPS2Hex
fromPS2Hex :: PackedString -> PackedString
fromPS2Hex (PS x s l) = createPS (2*l) $ \p -> withForeignPtr x $ \f ->
           conv_to_hex p (f `plusPtr` s) l

-- | fromHex2PS
fromHex2PS :: PackedString -> PackedString
fromHex2PS (PS x s l) = createPS (l `div` 2) $ \p -> withForeignPtr x $ \f ->
           conv_from_hex p (f `plusPtr` s) (l `div` 2)

-- | betweenLinesPS returns the PackedString between the two lines given,
-- or Nothing if they do not appear.
betweenLinesPS :: PackedString -> PackedString -> PackedString -> Maybe (PackedString)
betweenLinesPS start end ps = 
    case Prelude.break (start ==) (lines ps) of
        (_, _:rest@(PS ps1 s1 _:_)) ->
            case Prelude.break (end ==) rest of
                (_, PS _ s2 _:_) -> Just $ PS ps1 s1 (s2 - s1)
                _ -> Nothing
        _ -> Nothing

-- | breakAfterNthNewline
breakAfterNthNewline :: Int -> PackedString -> Maybe (PackedString, PackedString)
breakAfterNthNewline 0 the_ps | null the_ps = Just (empty, empty)
breakAfterNthNewline n the_ps@(PS fp the_s l)
 = unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit 0 s | s == end = return $ Just (the_ps, empty)
          findit _ s | s == end = return Nothing
          findit 0 s = let left_l = s - the_s
                       in return $ Just (PS fp the_s left_l,
                                         PS fp s (l - left_l))
          findit i s = do w <- peekElemOff p s
                          if w == nl then findit (i-1) (s+1)
                                     else findit i (s+1)
          nl = c2w '\n'
          end = the_s + l
      findit n the_s

-- | breakBeforeNthNewline
breakBeforeNthNewline :: Int -> PackedString -> (PackedString, PackedString)
breakBeforeNthNewline 0 the_ps | null the_ps = (empty, empty)
breakBeforeNthNewline n the_ps@(PS fp the_s l)
 = unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit _ s | s == end = return (the_ps, empty)
          findit i s = do w <- peekElemOff p s
                          if w == nl
                            then if i == 0
                                 then let left_l = s - the_s
                                      in return (PS fp the_s left_l,
                                                 PS fp s (l - left_l))
                                 else findit (i-1) (s+1)
                            else findit i (s+1)
          nl = c2w '\n'
          end = the_s + l
      findit n the_s

-- -----------------------------------------------------------------------------

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = error ("FastPackedString." ++ fun ++ ": empty list")

------------------------------------------------------------------------
--
-- The definition of @_substrPS@ is essentially:
-- @take (end - begin + 1) (drop begin str)@.

{-
substrPS :: PackedString -> Int -> Int -> PackedString
substrPS (PS ps s _) begin end = PS ps (s+begin) (1+end-begin)
-}

------------------------------------------------------------------------

-- | Convert a 'PackedString' in UTF8 form to a 'String'
unpackFromUTF8 :: PackedString -> String
unpackFromUTF8 (PS _ _ 0) = []
unpackFromUTF8 (PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do 
    outbuf <- mallocArray l
    lout   <- utf8_to_ints outbuf (p `plusPtr` s) l
    when (lout < 0) $ error "Bad UTF8!"
    str    <- (Prelude.map chr) `liftM` peekArray lout outbuf
    free outbuf
    return str

-- | Given the maximum size needed and a function to make the contents
-- of a PackedString, generatePS makes the 'PackedString'. The
-- generating function is required to return the actual size (<= the
-- maximum size).
generatePS :: Int -> (Ptr Word8 -> IO Int) -> IO PackedString
generatePS i f = do 
    p <- mallocArray i
    i' <- f p
    p' <- reallocArray p i'
    fp <- newForeignPtr c_free p'
    return $ PS fp 0 i'

#if defined(__GLASGOW_HASKELL__)
-- | Construct a 'PackedString' given a C Word8 buffer, a length, and an
-- IO action representing a finalizer.  This function is not available
-- on Hugs.
constructPS :: (Ptr Word8) -> Int -> IO () -> IO PackedString
constructPS p l f = do 
    fp <- FC.newForeignPtr p f
    return $ PS fp 0 l
#endif

-- | Build a @PackedString@ from a malloced @CString@
mallocedCString2PS :: CString -> IO PackedString
mallocedCString2PS cs = do 
    fp <- newForeignPtr c_free (castPtr cs)
    l  <- c_strlen cs
    return $ PS fp 0 (fromIntegral l)

-- | Use a @PackedString@ with a function requiring a @CString@
withCStringPS :: PackedString -> (CString -> IO a) -> IO a
withCStringPS (PS ps s l) = bracket alloc free_cstring
    where 
      alloc = withForeignPtr ps $ \p -> do 
                buf <- c_malloc (fromIntegral l+1)
                c_memcpy (castPtr buf) (castPtr p `plusPtr` s) (fromIntegral l)
                poke (buf `plusPtr` l) (0::Word8)
                return $ castPtr buf

-- | Do something with the internals of a 'PackedString'. Beware of
-- altering the contents!
unsafeWithInternals :: PackedString -> (Ptr Word8 -> Int -> IO a) -> IO a
unsafeWithInternals (PS fp s l) f = withForeignPtr fp $ \p -> f (p `plusPtr` s) l

-- | A way of creating ForeignPtrs outside the IO monad (although it
-- still isn't entirely "safe", but at least it's convenient.
createPS :: Int -> (Ptr Word8 -> IO ()) -> PackedString
createPS l write_ptr = unsafePerformIO $ do 
    fp <- mallocForeignPtr l
    withForeignPtr fp $ \p -> write_ptr p
    return $ PS fp 0 l

------------------------------------------------------------------------

-- (internal) GC wrapper of mallocForeignPtrArray
mallocForeignPtr :: Int -> IO (ForeignPtr Word8)
mallocForeignPtr l = when (l > 1000000) performGC >> mallocForeignPtrArray l

-- -----------------------------------------------------------------------------
-- I\/O functions

-- | Outputs a 'PackedString' to the specified 'Handle'.
--
-- NOTE: the representation of the 'PackedString' in the file is assumed to
-- be in the ISO-8859-1 encoding.  In other words, only the least signficant
-- byte is taken from each character in the 'PackedString'.
--
hPutPS :: Handle -> PackedString -> IO ()
hPutPS _ (PS _ _ 0)  = return ()
hPutPS h (PS ps 0 l) = withForeignPtr ps $ \p-> hPutBuf h p l
hPutPS h (PS ps s l) = withForeignPtr ps $ \p-> hPutBuf h (p `plusPtr` s) l

-- | Read a 'PackedString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'.
--
-- NOTE: as with 'hPutPS', the string representation in the file is
-- assumed to be ISO-8859-1.
--
hGetPS :: Handle -> Int -> IO PackedString
hGetPS _ 0 = return empty
hGetPS h i = do fp <- mallocForeignPtr i
                l  <- withForeignPtr fp $ \p-> hGetBuf h p i
                return $ PS fp 0 l

-- | Read entire handle contents into a 'PackedString'.
--
-- NOTE: as with 'hGetPS', the string representation in the file is
-- assumed to be ISO-8859-1.
--
hGetContentsPS :: Handle -> IO PackedString
hGetContentsPS h = do 
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

-- | Read an entire file directly into a 'PackedString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGetPS.
--
-- NOTE: as with 'hGetPS', the string representation in the file is
-- assumed to be ISO-8859-1.
--
readFilePS :: FilePath -> IO PackedString
readFilePS f = do 
    h <- openBinaryFile f ReadMode
    l <- hFileSize h
    s <- hGetPS h $ fromIntegral l
    hClose h
    return s

-- | Write a 'PackedString' to a file.
--
writeFilePS :: FilePath -> PackedString -> IO ()
writeFilePS f ps = do 
    h <- openBinaryFile f WriteMode
    hPutPS h ps
    hClose h

-- | Like readFilePS, this reads an entire file directly into a
-- 'PackedString', but it is even more efficient.  It involves directly
-- mapping the file to memory.  This has the advantage that the contents
-- of the file never need to be copied.  Also, under memory pressure the
-- page may simply be discarded, wile in the case of readFilePS it would
-- need to be written to swap.  If you read many small files, mmapFilePS
-- will be less memory-efficient than readFilePS, since each mmapFilePS
-- takes up a separate page of memory.  Also, you can run into bus
-- errors if the file is modified.  NOTE: as with 'readFilePS', the
-- string representation in the file is assumed to be ISO-8859-1.
--
mmapFilePS :: FilePath -> IO PackedString
mmapFilePS f = 
#if defined(USE_MMAP)
   mmap f >>= \(fp,l) -> return $ PS fp 0 l
#else
   readFilePS f
#endif

use_mmap :: Bool
#if defined(USE_MMAP)
use_mmap = True
#else
use_mmap = False
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
              | MMappedPackedString PackedString
              | LazyPackedStrings [PackedString]
    deriving Eq

readFileLazily :: FilePath -> IO LazyFile
readFileLazily f =
#if defined(__GLASGOW_HASKELL__)
    if use_mmap
      then liftM MMappedPackedString (mmapFilePS f)
      else
#endif
           do h <- openBinaryFile f ReadMode
              liftM LazyPackedStrings $ readHandleLazily h

readHandleLazily :: Handle -> IO [PackedString]
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

-- -----------------------------------------------------------------------------
-- gzReadFilePS

-- | Read an entire file, which may or may not be gzip compressed, directly
-- into a 'PackedString'.

#if defined(USE_ZLIB)

gzReadFilePS :: FilePath -> IO PackedString
gzReadFilePS f = do
    h <- openBinaryFile f ReadMode
    header <- hGetPS h 2
    if header /= pack "\31\139"
       then do hClose h
               mmapFilePS f
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
    header <- hGetPS h 2
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
               liftM LazyPackedStrings read_rest
#if defined(__GLASGOW_HASKELL__)
        else if use_mmap then
            do hClose h
               liftM MMappedPackedString (mmapFilePS f)
#endif
        else liftM (LazyPackedStrings . (header:)) $ readHandleLazily h
    where blocksize = 1024

hGetLittleEndInt :: Handle -> IO Int
hGetLittleEndInt h = do
    b1 <- ord `liftM` hGetChar h
    b2 <- ord `liftM` hGetChar h
    b3 <- ord `liftM` hGetChar h
    b4 <- ord `liftM` hGetChar h
    return $ b1 + 256*b2 + 65536*b3 + 16777216*b4

gzWriteFilePS :: FilePath -> PackedString -> IO ()
gzWriteFilePS f ps = gzWriteFilePSs f [ps]

gzWriteFilePSs :: FilePath -> [PackedString] -> IO ()
gzWriteFilePSs f pss  =
    withCString f $ \fstr -> withCString "wb" $ \wb -> do
    gzf <- c_gzopen fstr wb
    when (gzf == nullPtr) $ fail $ "problem gzopening file for write: "++f
    mapM_ (gzWriteToGzf gzf) pss `catch`
              \_ -> fail $ "problem gzwriting file: "++f
    c_gzclose gzf

gzWriteToGzf :: Ptr () -> PackedString -> IO ()
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
    :: CString -> IO CInt

foreign import ccall unsafe "static stdlib.h strtol" c_strtol
    :: Ptr Word8 -> Ptr (Ptr Word8) -> Int -> IO CLong

------------------------------------------------------------------------

foreign import ccall unsafe "static fpstring.h utf8_to_ints" utf8_to_ints
    :: Ptr Int -> Ptr Word8 -> Int -> IO Int

foreign import ccall unsafe "fpstring.h firstnonspace" c_firstnonspace
    :: Ptr Word8 -> Int -> Int

foreign import ccall unsafe "fpstring.h firstspace" c_firstspace
    :: Ptr Word8 -> Int -> Int

foreign import ccall unsafe "static fpstring.h conv_to_hex" conv_to_hex
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "static fpstring.h conv_from_hex" conv_from_hex
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

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

foreign import ccall unsafe "static sys/mman.h munmap" c_munmap
    :: Ptr Word8 -> Int -> IO Int

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
