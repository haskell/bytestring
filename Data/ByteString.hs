{-# OPTIONS -cpp -fglasgow-exts -O2 -optc-O2 -funbox-strict-fields #-}
--
-- Module      : ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005
--               (c) Don Stewart 2005-2006
--               (c) Bjorn Bringert 2006
-- License     : BSD-style
--
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi
-- 

--
-- | A time and space-efficient implementation of strings as packed byte
-- arrays, suitable for high performance use, both in terms of large
-- data quantities, or high speed requirements. Strings are encoded as
-- Word8 arrays of bytes, and functions on Chars are provided as a
-- convenience. At all times characters are assumed to be in ISO-8859-1
-- form.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with Prelude functions.  eg.
--
-- > import qualified Data.ByteString as B
--
-- Original GHC implementation by Bryan O\'Sullivan. Rewritten to use
-- UArray by Simon Marlow. Rewritten to support slices and use
-- ForeignPtr by David Roundy. Polished and extended by Don Stewart.
--

module Data.ByteString (

        -- * The @ByteString@ type
        ByteString(..),         -- abstract, instances: Eq, Ord, Show, Read, Data, Typeable

        -- * Introducing and eliminating 'ByteString's
        empty,                  -- :: ByteString
        pack,                   -- :: String -> ByteString
        packChar,               -- :: String -> ByteString
        unpack,                 -- :: ByteString -> String

        -- * Basic interface
        cons,                   -- :: Char -> ByteString -> ByteString
        snoc,                   -- :: ByteString -> Char -> ByteString
        append,                 -- :: ByteString -> ByteString -> ByteString
        head,                   -- :: ByteString -> Char
        tail,                   -- :: ByteString -> ByteString
        last,                   -- :: ByteString -> Char
        init,                   -- :: ByteString -> ByteString
        null,                   -- :: ByteString -> Bool
        length,                 -- :: ByteString -> Int
        inits,                  -- :: ByteString -> [ByteString]
        tails,                  -- :: ByteString -> [ByteString]

        -- * List transformations
        map,                    -- :: (Char -> Char) -> ByteString -> ByteString
        reverse,                -- :: ByteString -> ByteString
        intersperse,            -- :: Char -> ByteString -> ByteString
        transpose,              -- :: [ByteString] -> [ByteString]

        -- * Reducing 'ByteString's
        foldl,                  -- :: (a -> Char -> a) -> a -> ByteString -> a
        foldr,                  -- :: (Char -> a -> a) -> a -> ByteString -> a
        foldl1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char
        foldr1,                 -- :: (Char -> Char -> Char) -> ByteString -> Char

        -- ** Special folds
        concat,                 -- :: [ByteString] -> ByteString
        concatMap,              -- :: (Char -> ByteString) -> ByteString -> ByteString
        any,                    -- :: (Char -> Bool) -> ByteString -> Bool
        all,                    -- :: (Char -> Bool) -> ByteString -> Bool
        maximum,                -- :: ByteString -> Char
        minimum,                -- :: ByteString -> Char

        -- ** Unfolds and string generators
        replicate,              -- :: Int -> Char -> ByteString
        unfoldr,                -- :: (Char -> Maybe (Char, Char)) -> Char -> ByteString

        -- * Substrings
        take,                   -- :: Int -> ByteString -> ByteString
        drop,                   -- :: Int -> ByteString -> ByteString
        splitAt,                -- :: Int -> ByteString -> (ByteString, ByteString)

        takeWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        dropWhile,              -- :: (Char -> Bool) -> ByteString -> ByteString
        span,                   -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        break,                  -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)

        join,                   -- :: ByteString -> [ByteString] -> ByteString
        join2,                  -- :: Char -> ByteString -> ByteString -> ByteString

        -- * Searching 'ByteString's

        -- ** Searching by equality
        elem,                   -- :: Char -> ByteString -> Bool
        notElem,                -- :: Char -> ByteString -> Bool

        -- ** Searching with a predicate
        filter,                 -- :: (Char -> Bool) -> ByteString -> ByteString
        filterChar,             -- :: Char -> ByteString -> ByteString
        filterNotChar,          -- :: Char -> ByteString -> ByteString
        find,                   -- :: (Char -> Bool) -> ByteString -> Maybe Char

        -- ** Searching for substrings
        isPrefixOf,             -- :: ByteString -> ByteString -> Bool
        isSuffixOf,             -- :: ByteString -> ByteString -> Bool
        isSubstringOf,          -- :: ByteString -> ByteString -> Bool
        findSubstring,          -- :: ByteString -> ByteString -> Maybe Int
        findSubstrings,         -- :: ByteString -> ByteString -> [Int]

        -- * Indexing 'ByteString's
        index,                  -- :: ByteString -> Int -> Char
        unsafeIndex,            -- :: ByteString -> Int -> Char
        elemIndex,              -- :: Char -> ByteString -> Maybe Int
        elemIndices,            -- :: Char -> ByteString -> [Int]
        findIndex,              -- :: (Char -> Bool) -> ByteString -> Maybe Int
        findIndices,            -- :: (Char -> Bool) -> ByteString -> [Int]

        -- * Zipping and unzipping ByteString
        zip,                    -- :: ByteString -> ByteString -> [(Char,Char)]
        zipWith,                -- :: (Char -> Char -> c) -> ByteString -> ByteString -> [c]
        unzip,                  -- :: [(Char,Char)] -> (ByteString,ByteString)

        -- * Special 'ByteString's
        elems,                  -- :: ByteString -> [ByteString]

        -- ** Lines and words
        lines,                  -- :: ByteString -> [ByteString]
        words,                  -- :: ByteString -> [ByteString]
        unlines,                -- :: [ByteString] -> ByteString
        unwords,                -- :: ByteString -> [ByteString]

        -- ** Ordered 'ByteString's
        sort,                   -- :: ByteString -> ByteString

        -- * Extensions to the list interface

        -- ** Splitting strings
        split,                  -- :: Char -> ByteString -> [ByteString]
        splitWith,              -- :: (Char -> Bool) -> ByteString -> [ByteString]
        breakOn,                -- :: Char -> ByteString -> (ByteString, ByteString)
        breakSpace,             -- :: ByteString -> Maybe (ByteString,ByteString)
        breakFirst,             -- :: Char -> ByteString -> Maybe (ByteString,ByteString)
        breakLast,              -- :: Char -> ByteString -> Maybe (ByteString,ByteString)
        dropSpace,              -- :: ByteString -> ByteString
        dropSpaceEnd,           -- :: ByteString -> ByteString
        spanEnd,                -- :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
        tokens,                 -- :: (Char -> Bool) -> ByteString -> [ByteString]

        -- ** Indexing
        elemIndexLast,          -- :: Char -> ByteString -> Maybe Int
        lineIndices,            -- :: ByteString -> [Int]

        -- ** Lines and words
        lines',                 -- :: ByteString -> [ByteString]
        unlines',               -- :: [ByteString] -> ByteString
        linesCRLF',             -- :: ByteString -> [ByteString]
        unlinesCRLF',           -- :: [ByteString] -> ByteString
        words',                 -- :: ByteString -> [ByteString]
        unwords',               -- :: ByteString -> [ByteString]
        betweenLines,           -- :: ByteString -> ByteString -> ByteString -> Maybe (ByteString)

        -- ** Unchecked access
        unsafeHead,             -- :: ByteString -> Char
        unsafeTail,             -- :: ByteString -> ByteString

        -- ** Misc
        idx,                    -- :: ByteString -> Int
        hash,                   -- :: ByteString -> Int32
        mapIndexed,             -- :: (Int -> Char -> Char) -> ByteString -> ByteString

        -- * Word8 interface
        packWords,              -- :: [Word8] -> ByteString
        unpackWords,            -- :: ByteString -> [Word8]
        mapWords,               -- :: (Word8 -> Word8) -> ByteString -> ByteString
        mapIndexedWords,        -- :: (Int -> Word8 -> Word8) -> ByteString -> ByteString
        indexWord8,             -- :: ByteString -> Int -> Word8
        unsafeIndexWord8,       -- :: ByteString -> Int -> Word8
        elemIndexWord8,         -- :: Word8 -> ByteString -> Maybe Int
        elemIndexLastWord8,     -- :: Word8 -> ByteString -> Maybe Int
        readInt,                -- :: ByteString -> Maybe Int
        unsafeReadInt,          -- :: ByteString -> Maybe Int

        -- * I\/O with @ByteString@s
        hGetLine,               -- :: Handle -> IO ByteString
        getLine,                -- :: IO ByteString
        hGet,                   -- :: Handle -> Int -> IO ByteString
        hGetNonBlocking,        -- :: Handle -> Int -> IO ByteString
        hPut,                   -- :: Handle -> ByteString -> IO ()
        putStr,                 -- :: ByteString -> IO ()
        putStrLn,               -- :: ByteString -> IO ()
        hGetContents,           -- :: Handle -> IO ByteString
        getContents,            -- :: IO ByteString
        readFile,               -- :: FilePath -> IO ByteString
        writeFile,              -- :: FilePath -> ByteString -> IO ()
        mmapFile,               -- :: FilePath -> IO ByteString
        getArgs,                -- :: IO [ByteString]

        -- * Low-level construction
        generate,               -- :: Int -> (Ptr Word8 -> Int -> IO Int) -> IO ByteString

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
        copy,                   -- :: ByteString -> ByteString
        copyCStringToByteString,-- :: CString -> ByteString

        -- ** Deconstruction
        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)

        -- * Misc
        unpackList, -- eek, otherwise it gets thrown away by the simplifier

   ) where

import qualified Prelude
import Prelude hiding           (reverse,head,tail,last,init,null,
                                 length,map,lines,foldl,foldr,unlines,
                                 concat,any,take,drop,splitAt,takeWhile,
                                 dropWhile,span,break,elem,filter,unwords,
                                 words,maximum,minimum,all,concatMap,
                                 foldl1,foldr1,readFile,writeFile,replicate,
                                 getContents,getLine,putStr,putStrLn,
                                 zip,zipWith,unzip,notElem)

import qualified Data.List as List

import Data.Array               (listArray)
import qualified Data.Array as Array ((!))
import Data.Bits                (rotateL)
import Data.Char                (ord, String, isSpace)
import Data.Int                 (Int32)
import Data.Word                (Word8)
import Data.Maybe               (listToMaybe)

import Control.Monad            (liftM)
import Control.Exception        (bracket)

import System.IO    hiding      (hGetLine,hGetContents,readFile,writeFile,
                                 getContents,getLine,putStr,putStrLn)
import System.IO.Error

import Foreign.Ptr              (Ptr, FunPtr, plusPtr, nullPtr, minusPtr, castPtr)
import Foreign.ForeignPtr       (newForeignPtr, newForeignPtr_, withForeignPtr, 
                                 finalizeForeignPtr, mallocForeignPtrArray, ForeignPtr)
import Foreign.Storable         (peekByteOff, peekElemOff, pokeElemOff, peek, poke)
import Foreign.C.String         (CString, CStringLen)
import Foreign.C.Types          (CSize, CInt, CLong)
import Foreign.Marshal          (alloca)
import Foreign.Marshal.Utils    (with)
import Foreign.Marshal.Array

#if defined(__GLASGOW_HASKELL__)
import qualified Foreign.Concurrent as FC (newForeignPtr)

# if defined(USE_MMAP)
import System.Posix             (handleToFd)
# endif

import Data.Generics            (Data(..), Typeable(..))

import GHC.Base                 (Int(..), Char(..), build, unpackCString#, unsafeChr)
import GHC.Handle
import GHC.IOBase
import GHC.Prim
import GHC.Ptr                  (Ptr(..))
import GHC.ST                   (ST(..))
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
#define STRICT6(f) f a b c d e f | a `seq` b `seq` c `seq` d `seq` e `seq` f `seq` False = undefined

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a 'String', which supports various
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) !Int !Int
#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

instance Eq  ByteString
    where (==)    = eq

instance Ord ByteString
    where compare = comparePS

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read ByteString where
    readsPrec p str = [ (pack x, y) | (x, y) <- readsPrec p str ]

------------------------------------------------------------------------

-- | /O(n)/ Equality on the 'ByteString' type. This implementation
-- uses @memcmp(3)@.
eq :: ByteString -> ByteString -> Bool
eq a b = (comparePS a b) == EQ
{-# INLINE eq #-}

-- | /O(n)/ 'comparePS' provides an 'Ordering' for 'ByteStrings' supporting slices. 
comparePS :: ByteString -> ByteString -> Ordering
comparePS (PS fp1 off1 len1) (PS fp2 off2 len2) = inlinePerformIO $
    withForeignPtr fp1 $ \p1 ->
        withForeignPtr fp2 $ \p2 ->
            cmp (p1 `offPS` off1)
                (p2 `offPS` off2) 0 len1 len2

cmp :: Ptr Word8 -> Ptr Word8 -> Int -> Int -> Int-> IO Ordering
STRICT5(cmp)
cmp p1 p2 n len1 len2
      | n == len1 = if n == len2 then return EQ else return LT
      | n == len2 = return GT
      | otherwise = do
          a <- peekElemOff p1 n
          b <- peekElemOff p2 n
          case a `compare` b of
                EQ -> cmp p1 p2 (n+1) len1 len2
                LT -> return LT
                GT -> return GT
{-# INLINE comparePS #-}

offPS :: Ptr Word8 -> Int -> Ptr Word8
offPS p i = p `plusPtr` i
{-# INLINE offPS #-}

{-
-- Only marginally faster, and only on hugh data sets.

-- | /O(n)/ 'comparePS' provides an 'Ordering' for 'ByteStrings' supporting slices. 
-- This implementation uses @memcmp(3)@ 
comparePS :: ByteString -> ByteString -> Ordering
comparePS (PS _ _ 0) (PS _ _ 0) = EQ    -- short cut for empty strings
comparePS (PS x1 s1 l1) (PS x2 s2 l2) = inlinePerformIO $
    withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- c_memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) (min l1 l2)
            return $ case i `compare` 0 of
                EQ  -> l1 `compare` l2
                x   -> x
{-# INLINE comparePS #-}
-}

-- -----------------------------------------------------------------------------
-- Constructing and destructing packed strings

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = inlinePerformIO $ mallocForeignPtr 1 >>= \fp -> return $ PS fp 0 0
{-# NOINLINE empty #-}

-- | /O(n)/ Convert a 'Char' into a 'ByteString'
packChar :: Char -> ByteString
packChar c = inlinePerformIO $ mallocForeignPtr 2 >>= \fp -> do
    withForeignPtr fp $ \p -> poke p (toEnum (ord c))
    return $ PS fp 0 1
{-# NOINLINE packChar #-}

-- | /O(n)/ Convert a 'String' into a 'ByteString'
pack :: String -> ByteString

#if !defined(__GLASGOW_HASKELL__)

pack str = createPS (Prelude.length str) $ \p -> go p str
    where
        go _ []     = return ()
        go p (x:xs) = poke p (c2w x) >> go (p `plusPtr` 1) xs -- less space than pokeElemOff

#else /* hack away */

pack str = createPS (Prelude.length str) $ \(Ptr p) -> stToIO (go p 0# str)
    where
        go _ _ []        = return ()
        go p i (C# c:cs) = writeByte p i c >> go p (i +# 1#) cs

        writeByte p i c = ST $ \s# ->
            case writeCharOffAddr# p i c s# of s2# -> (# s2#, () #)

{-# RULES
"pack/packAddress" forall s# .
                   pack (unpackCString# s#) = packAddress s#
 #-}

#endif

------------------------------------------------------------------------

-- | /O(n)/ Converts a 'ByteString' to a 'String'.
unpack :: ByteString -> String
unpack ps = build (unpackFoldr ps)
{-# INLINE unpack #-}

unpackList :: ByteString -> [Char]
unpackList (PS fp off len) = withPtr fp $ \p -> do
    let loop _ (-1) acc = return acc
        loop q n acc = do
           a <- peekElemOff q n
           loop q (n-1) (w2c a : acc)
    loop (p `offPS` off) (len-1) []

{-# RULES
"unpack-list"  [1]  forall p  . unpackFoldr p (:) [] = unpackList p
 #-}

unpackFoldr :: ByteString -> (Char -> a -> a) -> a -> a
unpackFoldr (PS fp off len) f c = withPtr fp $ \p -> do
    let loop _ (-1) acc = return acc
        loop q n acc = do
           a <- peekElemOff q n
           loop q (n-1) (w2c a `f` acc)
    loop (p `offPS` off) (len-1) c
{-# INLINE [0] unpackFoldr #-}

{-
--  
-- Abotu the same speed. No nice fusion rules.
--
-- | /O(n)/ Convert a 'ByteString' into a 'String'
unpack :: ByteString -> String
unpack (PS _  _ 0) = []
unpack (PS ps s l) = inlinePerformIO $ withForeignPtr ps $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        go p 0 acc = liftM w2c (peekByteOff p 0) >>= \e -> return (e : acc)
        go p n acc = liftM w2c (peekByteOff p n) >>= \e -> go p (n-1) (e : acc)
{-# INLINE unpack #-}
-}

-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'
packWords :: [Word8] -> ByteString
packWords s = createPS (Prelude.length s) $ \p -> pokeArray p s

-- | /O(n)/ Convert a 'ByteString' to a '[Word8]'
unpackWords :: ByteString -> [Word8]
unpackWords ps@(PS x s _)
    | null ps     = []
    | otherwise     =
        (inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s)
            : unpackWords (unsafeTail ps)

-- -----------------------------------------------------------------------------
-- List-like functions for ByteStrings

-- | /O(n)/ 'cons' is analogous to (:) for lists. Requires a memcpy.
cons :: Char -> ByteString -> ByteString
cons c (PS x s l) = createPS (l+1) $ \p -> withForeignPtr x $ \f -> do
        c_memcpy (p `plusPtr` 1) (f `plusPtr` s) l  -- 99% less space
        poke p (c2w c)
{-# INLINE cons #-}


-- | /O(n)/ Append a character to the end of a 'ByteString'
snoc :: ByteString -> Char -> ByteString
snoc (PS x s l) c = createPS (l+1) $ \p -> withForeignPtr x $ \f -> do
        c_memcpy p (f `plusPtr` s) l
        poke (p `plusPtr` l) (c2w c)
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a packed string, which must be non-empty.
head :: ByteString -> Char
head ps@(PS x s _)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | null ps   = errorEmptyList "head"
  | otherwise = w2c $ inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a packed string, which must be non-empty.
tail :: ByteString -> ByteString
tail (PS p s l)
    | l <= 0    = errorEmptyList "tail"
--  | l == 1    = empty                                                                    
    | otherwise = PS p (s+1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the last element of a packed string, which must be finite and non-empty.
last :: ByteString -> Char
last ps@(PS x s l)        -- ps ! 0 is inlined manually to eliminate a (+0)
  | null ps   = errorEmptyList "last"
  | otherwise = w2c $ inlinePerformIO $
        withForeignPtr x $ \p -> peekByteOff p (s+l-1)
{-# INLINE last #-}

-- | /O(1)/ Return all the elements of a 'ByteString' except the last one.
init :: ByteString -> ByteString
init (PS p s l)
    | l <= 0    = errorEmptyList "init"
    | l == 1    = empty
    | otherwise = PS p s (l-1)
{-# INLINE init #-}

-- | /O(1)/ Test whether a packed string is empty.
null :: ByteString -> Bool
null (PS _ _ l) = l == 0
{-# INLINE null #-}

-- | /O(1)/ 'length' returns the length of a packed string as an 'Int'.
length :: ByteString -> Int
length (PS _ _ l) = l
{-# INLINE length #-}

-- | Return all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
inits (PS x s l) = [PS x s n | n <- [0..l]]

-- | Return all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
tails p | null p    = [empty]
        | otherwise = p : tails (unsafeTail p)

-- less efficent spacewise: tails (PS x s l) = [PS x (s+n) (l-n) | n <- [0..l]]

-- | /O(n)/ Append two packed strings
append :: ByteString -> ByteString -> ByteString
append xs ys
    | null xs = ys
    | null ys = xs
    | otherwise  = concat [xs,ys]
{-# INLINE append #-}

-- ---------------------------------------------------------------------

-- | /O(n)/ 'map' @f xs@ is the packed string obtained by applying @f@ to each
-- element of @xs@, i.e.,
--
map :: (Char -> Char) -> ByteString -> ByteString
map f (PS fp start len) = inlinePerformIO $ withForeignPtr fp $ \p -> do
    new_fp <- mallocForeignPtr len
    withForeignPtr new_fp $ \new_p -> do
        map_ f (len-1) (p `offPS` start) new_p
        return (PS new_fp 0 len)
{-# INLINE map #-}

map_ :: (Char -> Char) -> Int -> Ptr Word8 -> Ptr Word8 -> IO ()
STRICT4(map_)
map_ f' n p1 p2
   | n < 0 = return ()
   | otherwise = do
        x <- peekElemOff p1 n
        pokeElemOff p2 n ((c2w . f' . w2c) x)
        map_ f' (n-1) p1 p2
{-# INLINE map_ #-}

-- ---------------------------------------------------------------------

-- | /O(n)/ 'filter', applied to a predicate and a packed string,
-- returns a packed string containing those characters that satisfy the
-- predicate.
--
filter :: (Char -> Bool) -> ByteString -> ByteString
filter k ps@(PS x s l)
    | null ps   = ps
    | otherwise = inlinePerformIO $ generate l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p l
        return (t `minusPtr` p) -- actual length
    where
        go _ t 0 = return t
        go f t e = do w <- peek f
                      if k (w2c w)
                        then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) (e - 1)
                        else             go (f `plusPtr` 1) t               (e - 1)

-- Almost as good: pack $ foldl (\xs c -> if f c then c : xs else xs) [] ps

--
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single char. It is more efficient to use
-- /filterChar/ in this case.
--
-- > filterChar == filter . (==)
--
-- filterChar is around 3x faster, and uses much less space, than its
-- filter equivalent
--
filterChar :: Char -> ByteString -> ByteString
filterChar c ps@(PS x s l)
    | null ps   = ps
    | otherwise = inlinePerformIO $ generate l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p l
        return (t `minusPtr` p) -- actual length
    where
        cw = c2w c
        go _ t 0 = return t
        go f t e = do w <- peek f
                      if w == cw
                        then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) (e-1)
                        else             go (f `plusPtr` 1) t               (e-1)

--
-- | /O(n)/ A first order equivalent of /filter . (/=)/, for the common
-- case of filtering a single char out of a list. It is more efficient
-- to use /filterNotChar/ in this case.
--
-- > filterNotChar == filter . (/=)
--
-- filterNotChar is around 3x faster, and uses much less space, than its
-- filter equivalent
--
filterNotChar :: Char -> ByteString -> ByteString
filterNotChar c ps@(PS x s l)
    | null ps   = ps
    | otherwise = inlinePerformIO $ generate l $ \p -> withForeignPtr x $ \f -> do
        t <- go (f `plusPtr` s) p l
        return (t `minusPtr` p) -- actual length
    where
        cw = c2w c
        go _ t 0 = return t
        go f t e = do w <- peek f
                      if w /= cw
                        then poke t w >> go (f `plusPtr` 1) (t `plusPtr` 1) (e-1)
                        else             go (f `plusPtr` 1) t               (e-1)

-- | /O(n)/ The 'find' function takes a predicate and a packed string
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> ByteString -> Maybe Char
find p ps = case filter p ps of
        p' | null p' -> Nothing
           | otherwise -> Just (unsafeHead p')

-- ---------------------------------------------------------------------

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a packed string, reduces the
-- packed string using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> ByteString -> a
foldl f v (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        lgo v (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        lgo z p q | p == q    = return z
                  | otherwise = do c <- liftM w2c $ peek p
                                   lgo (f z c) (p `plusPtr` 1) q

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a packed string,
-- reduces the packed string using the binary operator, from right to left.
foldr :: (Char -> a -> a) -> a -> ByteString -> a
foldr k z (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        go p q | p == q    = return z
               | otherwise = do c  <- liftM w2c $ peek p
                                ws <- go (p `plusPtr` 1) q
                                return $ c `k` ws

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteStrings'.
foldl1 :: (Char -> Char -> Char) -> ByteString -> Char
foldl1 f ps
    | null ps   = errorEmptyList "foldl1"
    | otherwise = foldl f (unsafeHead ps) (unsafeTail ps)

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: (Char -> Char -> Char) -> ByteString -> Char
foldr1 f ps
    | null ps        = errorEmptyList "foldr1"
    | length ps == 1 = unsafeHead ps
    | otherwise      = f (unsafeHead ps) (foldr1 f (unsafeTail ps))

-- ---------------------------------------------------------------------

-- | /O(n)/ 'replicate' @n x@ is a packed string of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- This implemenation uses @memset(3)@
--
replicate :: Int -> Char -> ByteString
replicate w c = inlinePerformIO $ generate w $ \ptr -> do
    memset ptr (fromIntegral . c2w $ c) (fromIntegral w)
    return w

{-
-- About 5x slower
replicate w c = inlinePerformIO $ generate w $ \ptr -> go ptr w
    where
        x = fromIntegral . ord $ c
        go _   0 = return w
        go ptr n = poke ptr x >> go (ptr `plusPtr` 1) (n-1)
-}

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function
-- takes the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in
-- a recursive call.
--
-- To preven unfoldr having /O(n^2)/ complexity (as prepending a character
-- to a ByteString is /O(n)/, this unfoldr requires a maximum final size
-- of the ByteString as an argument. 'cons' can then be implemented in
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
unfoldr :: Int -> (Char -> Maybe (Char, Char)) -> Char -> ByteString
unfoldr i f b = inlinePerformIO $ generate i $ \p -> go p b 0
    where
        go q c n | n == i    = return n      -- stop if we reach `i'
                 | otherwise = case f c of
                                   Nothing        -> return n
                                   Just (a,new_c) -> do
                                        poke q (c2w a)
                                        go (q `plusPtr` 1) new_c (n+1)

-- ---------------------------------------------------------------------

-- | Applied to a predicate and a packed string, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Char -> Bool) -> ByteString -> Bool
any f (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
        go (ptr `plusPtr` s) (ptr `plusPtr` (s+l))
    where
        go p q | p == q    = return False
               | otherwise = do c <- liftM w2c $ peek p
                                if f c then return True
                                       else go (p `plusPtr` 1) q

-- | Applied to a predicate and a 'ByteString', 'all' determines if
-- all elements of the 'ByteString' satisfy the predicate.
all :: (Char -> Bool) -> ByteString -> Bool
all f (PS x s l) = inlinePerformIO $ withForeignPtr x $ \ptr ->
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
takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile f ps = seq f $ take (findIndexOrEndPS (not . f) ps) ps
{-# INLINE takeWhile #-}

-- | 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@.
dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile f ps = seq f $ drop (findIndexOrEndPS (not . f) ps) ps
{-# INLINE dropWhile #-}

-- | /O(1)/ 'take' @n@, applied to a packed string @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take n ps@(PS x s l)
    | n < 0     = empty
    | n >= l    = ps
    | otherwise = PS x s n
{-# INLINE take #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or @[]@ if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop n ps@(PS x s l)
    | n <= 0    = ps
    | n >  l    = empty
    | otherwise = PS x (s+n) (l-n)
{-# INLINE drop #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt  n ps  = (take n ps, drop n ps)
{-# INLINE splitAt #-}

-- | 'span' @p xs@ breaks the packed string into two segments. It is
-- equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span  p ps = break (not . p) ps

-- | 'break' @p@ is equivalent to @'span' ('not' . p)@.
break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break p ps = case findIndexOrEndPS p ps of n -> (take n ps, drop n ps)

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
#if defined(USE_CBITS)
reverse (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f ->
        c_reverse p (f `plusPtr` s) l
#else
reverse = pack . Prelude.reverse . unpack
#endif

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate. This
-- implementation uses @memchr(3)@.
elem :: Char -> ByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _       -> True

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Char -> ByteString -> Bool
notElem c ps = case elemIndex c ps of Nothing -> True ; _       -> False

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = foldr (append . f) empty

-- | /O(n)/ Concatenate a list of packed strings.
concat :: [ByteString] -> ByteString
concat []     = empty
concat [ps]   = ps
concat xs     = inlinePerformIO $ do
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
                                 c_memcpy (ptr `plusPtr` len)
                                          (pf `plusPtr` s) l
                             f ptr (len + l) (to_go - l) pss'

           | otherwise = do let new_total = ((len + to_go) * 2) `max` (len + l)
                            ptr' <- reallocArray ptr new_total
                            f ptr' len (new_total - len) pss

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
index :: ByteString -> Int -> Char
index = (w2c .) . indexWord8
{-# INLINE index #-}

-- | /O(1)/ 'ByteString' index, returning a Word8
indexWord8 :: ByteString -> Int -> Word8
indexWord8 ps n
    | n < 0          = error $ "ByteString.indexWord8: negative index: " ++ show n
    | n >= length ps = error $ "ByteString.indexWord8: index too large: " ++ show n
                                ++ ", length = " ++ show (length ps)
    | otherwise      = ps ! n
{-# INLINE indexWord8 #-}

#if defined(USE_CBITS)

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum :: ByteString -> Char
maximum xs@(PS x s l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p ->
                    return $ w2c $ c_maximum (p `plusPtr` s) l

-- | 'minimum' returns the maximum value from a 'ByteString'
minimum :: ByteString -> Char
minimum xs@(PS x s l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p ->
                    return $ w2c $ c_minimum (p `plusPtr` s) l

#else

-- | 'maximum' returns the maximum value from a 'ByteString'
maximum xs@(PS x s l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> do
                        (w :: Word8) <- peek p
                        maximum_ (p `offPS` s) 0 l w
{-# INLINE maximum #-}

maximum_ :: Ptr Word8 -> Int -> Int -> Word8 -> IO Char
STRICT4(maximum_)
maximum_ ptr n m c
    | n >= m    = return (w2c c)
    | otherwise = do w <- peekByteOff ptr n
                     maximum_ ptr (n+1) m (if w > c then w else c)

-- | 'minimum' returns the maximum value from a 'ByteString'
minimum :: ByteString -> Char
minimum xs@(PS x s l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = inlinePerformIO $ withForeignPtr x $ \p -> do
                        (w :: Word8) <- peek p
                        minimum_ (p `offPS` s) 0 l w
{-# INLINE minimum #-}

minimum_ :: Ptr Word8 -> Int -> Int -> Word8 -> IO Char
STRICT4(minimum_)
minimum_ ptr n m c
    | n >= m    = return (w2c c)
    | otherwise = do w <- peekByteOff ptr n
                     minimum_ ptr (n+1) m (if w < c then w else c)
#endif

-- | /O(n)/ breaks a packed string to a list of packed strings, one byte each.
elems :: ByteString -> [ByteString]
elems (PS _ _ 0) = []
elems (PS x s l) = (PS x s 1:elems (PS x (s+1) (l-1)))
{-# INLINE elems #-}

-- | 'lines' breaks a packed string up into a list of packed strings
-- at newline characters.  The resulting strings do not contain
-- newlines.
--
lines :: ByteString -> [ByteString]
lines ps
    | null ps = []
    | otherwise = case search ps of
             Nothing -> [ps]
             Just n  -> take n ps : lines (drop (n+1) ps)
    where search = elemIndexWord8 0x0a
{-# INLINE lines #-}

{-
-- Just as fast, but more complex. Should be much faster, I thought.
lines :: ByteString -> [ByteString]
lines (PS _ _ 0) = []
lines (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
        let ptr = p `plusPtr` s

            STRICT1(loop)
            loop n = do
                let q = memchr (ptr `plusPtr` n) 0x0a (fromIntegral (l-n))
                if q == nullPtr
                    then return [PS x (s+n) (l-n)]
                    else do let i = q `minusPtr` ptr
                            ls <- loop (i+1)
                            return $! PS x (s+n) (i-n) : ls
        loop 0
-}

-- | 'unlines' is an inverse operation to 'lines'.  It joins lines,
-- after appending a terminating newline to each.
unlines :: [ByteString] -> ByteString
unlines [] = empty
unlines ss = (concat $ List.intersperse nl ss) `append` nl -- half as much space
    where nl = pack "\n"

-- | 'words' breaks a packed string up into a list of words, which
-- were delimited by white space.And
--
-- > tokens isSpace = words
--
words :: ByteString -> [ByteString]
words = tokens isSpace

------------------------------------------------------------------------

-- | /O(n)/ Splits a 'ByteString' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (=='a') "aabbaca" == ["","","bb","c",""]
--
splitWith :: (Char -> Bool) -> ByteString -> [ByteString]

#if defined(__GLASGOW_HASKELL__)
splitWith _pred (PS _  _   0) = []
splitWith pred_ (PS fp off len) = splitWith' pred# off len fp
  where pred# c# = pred_ (C# c#)

        splitWith' pred' off' len' fp' = withPtr fp $ \p ->
            splitLoop pred' p 0 off' len' fp'

        splitLoop :: (Char# -> Bool)
                  -> Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]
        splitLoop pred' p idx' off' len' fp'
            | pred' `seq` p `seq` idx' `seq` off' `seq` len' `seq` fp' `seq` False = undefined
            | idx' >= len'  = return [PS fp' off' idx']
            | otherwise = do
                w <- peekElemOff p (off'+idx')
                if pred' (case (w2c w) of C# c# -> c#)
                   then return (PS fp' off' idx' :
                              splitWith' pred' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop pred' p (idx'+1) off' len' fp'
{-# INLINE splitWith #-}

#else
splitWith p ps = if null rest then [chunk] else chunk : splitWith p (unsafeTail rest)
    where (chunk,rest) = break p ps
#endif

------------------------------------------------------------------------

-- | The 'unwords' function is analogous to the 'unlines' function, on words.
unwords :: [ByteString] -> ByteString
unwords = join (pack " ")

-- | /O(n)/ The 'intersperse' function takes a 'Char' and a 'ByteString' and
-- \`intersperses\' that 'Char' between the elements of the 'ByteString'.
-- It is analogous to the intersperse function on Lists.
intersperse :: Char -> ByteString -> ByteString
#if defined(USE_CBITS)
intersperse c ps@(PS x s l)
    | length ps < 2  = ps
    | otherwise      = createPS (2*l-1) $ \p -> withForeignPtr x $ \f ->
                            c_intersperse p (f `plusPtr` s) l (c2w c)
#else
intersperse c = pack . List.intersperse c . unpack
#endif

-- | /O(n^2)/ The 'transpose' function transposes the rows and columns
-- of its 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose ps = Prelude.map pack (List.transpose (Prelude.map unpack ps)) -- better

-- | /O(n)/ The 'join' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
join :: ByteString -> [ByteString] -> ByteString
join filler pss = concat (splice pss)
    where
        splice []  = []
        splice [x] = [x]
        splice (x:y:xs) = x:filler:splice (y:xs)

-- | /O(n log(n))/ Sort a ByteString using the C function @qsort(3)@.
sort :: ByteString -> ByteString
#if defined(USE_CBITS)
sort (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f -> do
        c_memcpy p (f `plusPtr` s) l
        c_qsort p l -- inplace
#else
sort = pack . List.sort . unpack
#endif

-- | /O(n)/ The 'elemIndex' function returns the index of the first element
-- in the given 'ByteString' which is equal (by memchr) to the query
-- element, or 'Nothing' if there is no such element.
elemIndex :: Char -> ByteString -> Maybe Int
elemIndex = elemIndexWord8 . c2w
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
elemIndices :: Char -> ByteString -> [Int]
elemIndices ch (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    let w = fromIntegral . c2w $ ch
        ptr = p `plusPtr` s

        STRICT1(loop)
        loop n = do
                let q = memchr (ptr `plusPtr` n) w (fromIntegral (l - n))
                if q == nullPtr
                    then return []
                    else do let i = q `minusPtr` ptr
                            ls <- loop (i+1)
                            return $! i:ls
    loop 0

{-
-- much slower
elemIndices :: Char -> ByteString -> [Int]
elemIndices c ps = loop 0 ps
   where STRICT2(loop)
         loop _ ps' | null ps'            = []
         loop n ps' | c == unsafeHead ps' = n : loop (n+1) (unsafeTail ps')
                    | otherwise           = loop (n+1) (unsafeTail ps')
-}

-- | The 'findIndex' function takes a predicate and a 'ByteString'
-- and returns the index of the first element in the packed string
-- satisfying the predicate.
findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex = (listToMaybe .) . findIndices

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Char -> Bool) -> ByteString -> [Int]
findIndices p ps = loop 0 ps
   where STRICT2(loop)
         loop _ ps' | null ps'           = []
         loop n ps' | p (unsafeHead ps') = n : loop (n + 1) (unsafeTail ps')
                    | otherwise          = loop (n + 1) (unsafeTail ps')

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs. If one input ByteString is short, excess
-- elements of the longer ByteString are discarded. This is equivalent
-- to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Char,Char)]
zip ps qs
    | null ps || null qs = []
    | otherwise = (unsafeHead ps, unsafeHead qs) : zip (unsafeTail ps) (unsafeTail qs)

-- | 'zipWith' generalises 'zip' by zipping with the function given
-- as the first argument, instead of a tupling function.  For example,
-- @'zipWith' ((. ord) . (+) . ord)@ is applied to two ByteStrings to
-- produce the list of corresponding sums.
zipWith :: (Char -> Char -> a) -> ByteString -> ByteString -> [a]
zipWith f ps qs
    | null ps || null qs = []
    | otherwise = f (unsafeHead ps) (unsafeHead qs) : zipWith f (unsafeTail ps) (unsafeTail qs)

-- | 'unzip' transforms a list of pairs of Chars into a pair of ByteStrings
-- Note that this performs two 'pack' operations.
unzip :: [(Char,Char)] -> (ByteString,ByteString)
unzip ls = (pack (Prelude.map fst ls), pack (Prelude.map snd ls))
{-# INLINE unzip #-}

-- | The 'isPrefixOf' function takes two strings and returns 'True'
-- iff the first string is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (PS x1 s1 l1) (PS x2 s2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = inlinePerformIO $ withForeignPtr x1 $ \p1 ->
        withForeignPtr x2 $ \p2 -> do
            i <- c_memcmp (p1 `plusPtr` s1) (p2 `plusPtr` s2) l1
            return (i == 0)

-- | The 'isSuffixOf' function takes two lists and returns 'True'
-- iff the first list is a suffix of the second.
-- Both lists must be finite.
isSuffixOf     :: ByteString -> ByteString -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

------------------------------------------------------------------------
-- Extensions to the list interface

-- | /O(1)/ 'idx' returns the internal skipped index of the current
-- 'ByteString' from any larger string it was created from, as an 'Int'.
idx :: ByteString -> Int
idx (PS _ s _) = s
{-# INLINE idx #-}

-- | /O(n)/ A map for Word8 operations
mapWords :: (Word8 -> Word8) -> ByteString -> ByteString
mapWords k = mapIndexedWords (const k)

-- | /O(n)/ map, provided with the index at each position
mapIndexed :: (Int -> Char -> Char) -> ByteString -> ByteString
mapIndexed k = mapIndexedWords (\i w -> c2w (k i (w2c w)))

-- | /O(n)/ map Word8 functions, provided with the index at each position
mapIndexedWords :: (Int -> Word8 -> Word8) -> ByteString -> ByteString
mapIndexedWords k (PS ps s l) = createPS l $ \p -> withForeignPtr ps $ \f ->
      go 0 (f `plusPtr` s) p (f `plusPtr` s `plusPtr` l)
    where
        go :: Int -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
        go n f t p | f == p    = return ()
                   | otherwise = do w <- peek f
                                    ((poke t) . k n) w
                                    go (n+1) (f `plusPtr` 1) (t `plusPtr` 1) p

--
-- | /O(n)/ join2. An efficient way to join to two ByteStrings with a
-- char. Around 4 times faster than the generalised join.
--
join2 :: Char -> ByteString -> ByteString -> ByteString
join2 c f g =
    let (ffp, s, l) = toForeignPtr f
        (fgp, t, m) = toForeignPtr g
    in inlinePerformIO $ generate len $ \ptr ->
            withForeignPtr ffp $ \fp ->
                withForeignPtr fgp $ \gp -> do
                    c_memcpy ptr (fp `plusPtr` s) l
                    poke (ptr `plusPtr` l) (sep :: Word8)
                    c_memcpy (ptr `plusPtr` (l + 1)) (gp `plusPtr` t) m
                    return len
    where
      len = length f + length g + 1
      sep = c2w c
{-# INLINE join2 #-}

-- | /O(n)/ Indicies of newlines. Shorthand for 
--
-- > elemIndices '\n'
--
lineIndices :: ByteString -> [Int]
lineIndices = elemIndices '\n'

-- | 'dropSpace' efficiently returns the 'ByteString' argument with
-- white space removed from the front. It is more efficient than calling
-- dropWhile for removing whitespace. I.e.
-- 
-- > dropWhile isSpace == dropSpace
--
dropSpace :: ByteString -> ByteString
dropSpace (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstnonspace (p `plusPtr` s) 0 l
    return $ if i == l then empty else PS x (s+i) (l-i)
{-# INLINE dropSpace #-}

firstnonspace :: Ptr Word8 -> Int -> Int -> IO Int
STRICT3(firstnonspace)
firstnonspace ptr n m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if (isSpace . w2c) w then firstnonspace ptr (n+1) m
                                          else return n

-- | 'dropSpaceEnd' efficiently returns the 'ByteString' argument with
-- white space removed from the end. I.e.
-- 
-- > reverse . (dropWhile isSpace) . reverse == dropSpaceEnd
--
dropSpaceEnd :: ByteString -> ByteString
dropSpaceEnd (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- lastnonspace (p `plusPtr` s) (l-1)
    return $ if i == (-1) then empty else PS x s (i+1)
{-# INLINE dropSpaceEnd #-}

lastnonspace :: Ptr Word8 -> Int -> IO Int
STRICT2(lastnonspace)
lastnonspace ptr n
    | n < 0     = return n
    | otherwise = do w <- peekElemOff ptr n
                     if (isSpace . w2c) w then lastnonspace ptr (n-1)
                                          else return n

-- | 'breakSpace' returns the pair of 'ByteString's when the argument
-- is broken at the first whitespace character. I.e.
-- 
-- > break isSpace == breakSpace
--
breakSpace :: ByteString -> (ByteString,ByteString)
breakSpace (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstspace (p `plusPtr` s) 0 l
    return $ case () of {_
        | i == 0    -> (empty, PS x s l)
        | i == l    -> (PS x s l, empty)
        | otherwise -> (PS x s i, PS x (s+i) (l-i))
    }
{-# INLINE breakSpace #-}

firstspace :: Ptr Word8 -> Int -> Int -> IO Int
STRICT3(firstspace)
firstspace ptr n m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if (not . isSpace . w2c) w then firstspace ptr (n+1) m
                                                else return n

-- | 'spanEnd' behaves like 'span' but from the end of the
-- 'ByteString'. This is more efficient than, say, breakLast, if you
-- need to break near the end of the string. I.e.
--
-- > spanEnd (not.isSpace) "x y z" == ("x y ","z")
--
-- and
--
-- > spanEnd (not . Char.isSpace) ps
-- >    == 
-- > let (x,y) = span (not.isSpace) (reverse ps) in (reverse y, reverse x) 
--
spanEnd :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd  p ps = splitAt (findFromEndUntilPS (not.p) ps) ps

-- | 'breakOn' breaks its 'ByteString' argument at the first occurence
-- of the specified character. It is more efficient than 'break' as it
-- is implemented with @memchr(3)@. I.e.
-- 
-- > break (=='c') "abcd" == breakOn 'c' "abcd"
--
breakOn :: Char -> ByteString -> (ByteString, ByteString)
breakOn c p = case elemIndex c p of
                    Nothing -> (p,empty)
                    Just n -> (take n p, drop n p)
{-# INLINE breakOn #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the 'Char'
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
split _ (PS _ _ 0) = []
split c (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    let ptr = p `plusPtr` s
        w   = fromIntegral (c2w c)

        STRICT1(loop)
        loop n = do
            let q = memchr (ptr `plusPtr` n) w (fromIntegral (l-n))
            if q == nullPtr
                then return [PS x (s+n) (l-n)]
                else do let i = q `minusPtr` ptr
                        ls <- loop (i+1)
                        return $! PS x (s+n) (i-n) : ls
    loop 0
{-# INLINE split #-}

{-
-- slower. but stays inside Haskell.
split _ (PS _  _   0) = []
split x (PS fp off len) = splitWith' off len fp
    where
        (W8# w#) = c2w x

        splitWith' off' len' fp' = withPtr fp $ \p ->
            splitLoop p 0 off' len' fp'

        splitLoop :: Ptr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]

        STRICT5(splitLoop)
        splitLoop p idx' off' len' fp'
            | p `seq` idx' `seq` off' `seq` len' `seq` fp' `seq` False = undefined
            | idx' >= len'  = return [PS fp' off' idx']
            | otherwise = do
                (W8# x#) <- peekElemOff p (off'+idx')
                if word2Int# w# ==# word2Int# x#
                   then return (PS fp' off' idx' :
                              splitWith' (off'+idx'+1) (len'-idx'-1) fp')
                   else splitLoop p (idx'+1) off' len' fp'
-}

-- | Like 'splitWith', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- > tokens (=='a') "aabbaca" == ["bb","c"]
--
tokens :: (Char -> Bool) -> ByteString -> [ByteString]
tokens p = Prelude.filter (not.null) . splitWith p

-- | /O(n)/ 'breakFirst' breaks the given ByteString on the first
-- occurence of @c@. It behaves like 'break', except the delimiter is
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
breakFirst c p = case elemIndex c p of
   Nothing -> Nothing
   Just n -> Just (take n p, drop (n+1) p)
{-# INLINE breakFirst #-}

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
breakLast c p = case elemIndexLast c p of
    Nothing -> Nothing
    Just n -> Just (take n p, drop (n+1) p)
{-# INLINE breakLast #-}

-- | /O(n)/ The 'elemIndexLast' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following holds:
--
-- > elemIndexLast c xs == 
-- > (-) (length xs - 1) `fmap` elemIndex c (reverse xs)
--
-- elemIndexLast needs to 
--
elemIndexLast :: Char -> ByteString -> Maybe Int
elemIndexLast c = elemIndexLastWord8 (c2w c)
{-# INLINE elemIndexLast #-}

-- | /O(n)/ Hash a ByteString into an 'Int32' value, suitable for use as a key.
hash :: ByteString -> Int32
hash (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p ->
    go (0 :: Int32) (p `plusPtr` s) l
  where
    go :: Int32 -> Ptr Word8 -> Int -> IO Int32
    go h _ 0 = return h
    go h p n = do w <- peek p
                  let h' = (fromIntegral w) + (rotateL h 8)
                  go h' (p `plusPtr` 1) (n-1)

-- | 'betweenLines' returns the ByteString between the two lines
-- given, or Nothing if they do not appear.
-- The returned string is the first and shortest string such 
-- that the line before it is the given first line, and the line 
-- after it is the given second line.
betweenLines :: ByteString -- ^ First line to look for
             -> ByteString -- ^ Second line to look for
             -> ByteString -- ^ 'ByteString' to look in
             -> Maybe (ByteString)

betweenLines start end ps =
    case Prelude.break (start ==) (lines ps) of
        (_, _:rest@(PS ps1 s1 _:_)) ->
            case Prelude.break (end ==) rest of
                (_, PS _ s2 _:_) -> Just $ PS ps1 s1 (s2 - s1)
                _ -> Nothing
        _ -> Nothing

-- | 'lines\'' behaves like 'lines', in that it breaks a ByteString on
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
lines' :: ByteString -> [ByteString]
lines' ps = case elemIndexWord8 (c2w '\n') ps of
             Nothing -> [ps]
             Just n -> take n ps : lines' (drop (n+1) ps)

-- | 'linesCRLF\'' behaves like 'lines\'', but breaks on (\\cr?\\lf)
linesCRLF' :: ByteString -> [ByteString]
linesCRLF' ps = case elemIndexWord8 (c2w '\n') ps of
                 Nothing -> [ps]
                 Just 0  -> empty : linesCRLF' (drop 1 ps)
                 Just n  -> let k = if ps ! (n-1) == 0xD then n-1 else n
                            in take k ps : linesCRLF' (drop (n+1) ps)

-- | 'unlines\'' behaves like 'unlines', except that it also correctly
-- retores lines that do not have terminating newlines (see the
-- description for 'lines\'').
--
unlines' :: [ByteString] -> ByteString
unlines' ss = concat $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a:newline: intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = pack "\n"

-- | 'unlines\'' behaves like 'unlines', except that it also correctly
-- retores lines that do not have terminating newlines (see the
-- description for 'lines\''). Uses CRLF instead of LF.
--
unlinesCRLF' :: [ByteString] -> ByteString
unlinesCRLF' ss = concat $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a:newline: intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = pack "\r\n"

-- | 'words\'' behaves like 'words', with the exception that it produces
-- output on ByteStrings with trailing whitespace that can be
-- correctly inverted by 'unwords'. I.e.
--
-- > words  "a b c " == ["a","b","c"]
-- > words' "a b c " == ["a","b","c",""]
--
-- > unwords $ words  "a b c " == "a b c"
-- > unwords $ words' "a b c " == "a b c "
--
words' :: ByteString -> [ByteString]
words' ps = splitWith isSpace ps

-- | 'unwords\'' behaves like 'unwords'. It is provided for consistency
-- with the other invertable words and lines functions.
unwords' :: [ByteString] -> ByteString
unwords' = unwords

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the ByteString is non-empty.
unsafeHead :: ByteString -> Char
unsafeHead (PS x s _) = w2c $ inlinePerformIO $
    withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty ByteStrings. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the ByteString is non-empty.
unsafeTail :: ByteString -> ByteString
unsafeTail (PS ps s l) = PS ps (s+1) (l-1)
{-# INLINE unsafeTail #-}

------------------------------------------------------------------------
-- Searching for substrings

-- | Check whether one string is a substring of another.
--   @isSubstringOf p s@ is equivalent to @not (null (findSubstrings p s))@.
isSubstringOf :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to search in.
              -> Bool

isSubstringOf p s = not $ Prelude.null $ findSubstrings p s

-- | Get the first index of a substring in another string,
--   or 'Nothing' if the string is not found.
--   @findSubstring p s@ is equivalent to @listToMaybe (findSubstrings p s)@.
findSubstring :: ByteString -- ^ String to search for.
              -> ByteString -- ^ String to seach in.
              -> Maybe Int

findSubstring = (listToMaybe .) . findSubstrings

-- | Find the indexes of all (possibly overlapping) occurances of a
-- substring in a string.  This function uses the Knuth-Morris-Pratt
-- string matching algorithm.
findSubstrings :: ByteString -- ^ String to search for.
               -> ByteString -- ^ String to seach in.
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

-- ---------------------------------------------------------------------

-- | /O(n)/ 'elemIndexWord8' is like 'elemIndex', except
-- that it takes a 'Word8' as the element to search for.
elemIndexWord8 :: Word8 -> ByteString -> Maybe Int
elemIndexWord8 c (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p -> do
    let p' = p `plusPtr` s
        q  = memchr p' (fromIntegral c) (fromIntegral l)
    return $ if q == nullPtr then Nothing else Just $! q `minusPtr` p'
{-# INLINE elemIndexWord8 #-}

-- | /O(n)/ The 'elemIndexLastWord8' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
elemIndexLastWord8 :: Word8 -> ByteString -> Maybe Int
elemIndexLastWord8 ch (PS x s l) = inlinePerformIO $ withForeignPtr x $ \p ->
    go (p `plusPtr` s) (l-1)
  where
    STRICT2(go)
    go p i | i < 0     = return Nothing
           | otherwise = do ch' <- peekByteOff p i
                            if ch == ch'
                                then return $ Just i
                                else go p (i-1)
{-# INLINE elemIndexLastWord8 #-}

-- | /O(1)/ Like 'index', but without any bounds checking.
unsafeIndex :: ByteString -> Int -> Char
unsafeIndex = (w2c .) . (!)
{-# INLINE unsafeIndex #-}

-- | /O(1)/ Like 'index', but without any bounds checking.
unsafeIndexWord8 :: ByteString -> Int -> Word8
unsafeIndexWord8 = (!)
{-# INLINE unsafeIndexWord8 #-}

-- (Internal) 'findIndexOrEndPS' is a variant of findIndex, that returns the
-- length of the string if no element is found, rather than Nothing.
findIndexOrEndPS :: (Char -> Bool) -> ByteString -> Int
findIndexOrEndPS f ps
    | null ps      = 0
    | f (unsafeHead ps) = 0
    | otherwise    = seq f $ 1 + findIndexOrEndPS f (unsafeTail ps)

-- (Internal)
findFromEndUntilPS :: (Char -> Bool) -> ByteString -> Int
findFromEndUntilPS f ps@(PS x s l) = seq f $
    if null ps then 0
    else if f $ last ps then l
         else findFromEndUntilPS f (PS x s (l-1))


------------------------------------------------------------------------

#if defined(__GLASGOW_HASKELL__)
-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @ByteString@. A useful way to create an
-- Addr\# is with an unboxed string literal, which is compiled to a
-- static @char []@ by GHC. Establishing the length of the string
-- requires a call to @strlen(3)@. Use 'unsafePackAddress' if you know
-- the length of the string statically.
--
-- An example:
--
-- > literalFS = packAddress "literal"#
--
packAddress :: Addr# -> ByteString
packAddress addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 (fromIntegral $ c_strlen cstr)
    where
      cstr = Ptr addr#
{-# INLINE packAddress #-}

-- | /O(1)/ 'unsafePackAddress' provides constant-time construction of
-- 'ByteStrings' -- which is ideal for string literals. It packs a
-- null-terminated sequence of bytes into a 'ByteString', given a raw
-- 'Addr\#' to the string, and the length of the string. Make sure the
-- length is correct, otherwise use the safer 'packAddress' (where the
-- length will be calculated once at runtime).
unsafePackAddress :: Int -> Addr# -> ByteString
unsafePackAddress len addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 len
    where
      cstr = Ptr addr#
#endif

-- | /O(1)/ Build a ByteString from a ForeignPtr
fromForeignPtr :: ForeignPtr Word8 -> Int -> ByteString
fromForeignPtr fp l = PS fp 0 l

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int)
toForeignPtr (PS ps s l) = (ps, s, l)

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage. 
--   This is mainly useful to allow the rest of the data pointed
--   to by the 'ByteString' to be garbage collected, for example
--   if a large string has been read in, and only a small part of it 
--   is needed in the rest of the program.
copy :: ByteString -> ByteString
copy (PS x s l) = createPS l $ \p -> withForeignPtr x $ \f ->
                    c_memcpy p (f `plusPtr` s) l

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, generate makes the 'ByteString'. The generating
-- function is required to return the actual size (<= the maximum size).
-- The string is padded at the end with a null byte.
--
-- generate is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
--
generate :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
generate i f = do
    p <- mallocArray i
    i' <- f p
    p' <- reallocArray p (i'+1)
    poke (p' `plusPtr` i') (0::Word8)    -- XXX so CStrings work
    fp <- newForeignPtr c_free p'
    return $ PS fp 0 i'

-- | /O(n)/ Build a @ByteString@ from a @CString@. This value will have /no/
-- finalizer associated to it.
packCString :: CString -> ByteString
packCString cstr = inlinePerformIO $ do
    fp <- newForeignPtr_ (castPtr cstr)
    return $ PS fp 0 (fromIntegral $ c_strlen cstr)

-- | /O(1)/ Build a @ByteString@ from a @CStringLen@. This value will
-- have /no/ finalizer associated with it.
packCStringLen :: CStringLen -> ByteString
packCStringLen (ptr,len) = inlinePerformIO $ do
    fp <- newForeignPtr_ (castPtr ptr)
    return $ PS fp 0 (fromIntegral len)

-- | /O(n)/ Build a @ByteString@ from a malloced @CString@. This value will
-- have a @free(3)@ finalizer associated to it.
packMallocCString :: CString -> ByteString
packMallocCString cstr = inlinePerformIO $ do
    fp <- newForeignPtr c_free (castPtr cstr)
    return $ PS fp 0 (fromIntegral $ c_strlen cstr)

#if defined(__GLASGOW_HASKELL__)
-- | /O(1)/ Construct a 'ByteString' given a C Ptr Word8 buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
packCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
packCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $ PS fp 0 l
#endif

------------------------------------------------------------------------

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a null-terminated @CString@.
--   The @CString@ should not be freed afterwards.
useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString (PS ps s l) = bracket alloc free_cstring
    where
      alloc = withForeignPtr ps $ \p -> do
                buf <- c_malloc (fromIntegral l+1)
                c_memcpy (castPtr buf) (castPtr p `plusPtr` s) (fromIntegral l)
                poke (buf `plusPtr` l) (0::Word8)
                return $ castPtr buf

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a @CString@.
-- Warning: modifying the @CString@ will affect the @ByteString@.
-- Why is this function unsafe? It relies on the null byte at the end of
-- the ByteString to be there. This is /not/ the case if your ByteString
-- has been spliced from a larger string (i.e. with take or drop).
-- Unless you can guarantee the null byte, you should use the safe
-- version, which will copy the string first.
--
unsafeUseAsCString :: ByteString -> (CString -> IO a) -> IO a
unsafeUseAsCString (PS ps s _) ac = withForeignPtr ps $ \p -> ac (castPtr p `plusPtr` s)

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a @CStringLen@.
-- Warning: modifying the @CStringLen@ will affect the @ByteString@.
-- This is analogous to unsafeUseAsCString, and comes with the same
-- safety requirements.
--
unsafeUseAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen (PS ps s l) ac = withForeignPtr ps $ \p -> ac (castPtr p `plusPtr` s,l)

-- | A way of creating ForeignPtrs outside the IO monad (although it
-- still isn't entirely "safe", but at least it's convenient.
createPS :: Int -> (Ptr Word8 -> IO ()) -> ByteString
createPS l write_ptr = inlinePerformIO $ do
    fp <- mallocForeignPtr (l+1)
    withForeignPtr fp $ \p -> write_ptr p
    return $ PS fp 0 l
{-# INLINE createPS #-}

#if defined(__GLASGOW_HASKELL__)
-- | Explicitly run the finaliser associated with a 'ByteString'.
-- Further references to this value may generate invalid memory
-- references. This operation is unsafe, as there may be other
-- 'ByteStrings' referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'ByteString's
-- ever generated from the underlying byte array are no longer live.
unsafeFinalize :: ByteString -> IO ()
unsafeFinalize (PS p _ _) = finalizeForeignPtr p
#endif

-- | /O(n)/ Duplicate a CString as a ByteString
copyCStringToByteString :: CString -> ByteString
copyCStringToByteString cstr = inlinePerformIO $ do
    let len = fromIntegral $ c_strlen cstr
    fp <- mallocForeignPtrArray (len+1)
    withForeignPtr fp $ \p -> do
        c_memcpy p (castPtr cstr) len
        poke (p `plusPtr` len) (0 :: Word8)
    return $! PS fp 0 len

-- | readInt skips any whitespace at the beginning of its argument, and
-- reads an Int from the beginning of the ByteString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise
-- it just returns the int read, and the rest of the string.
readInt :: ByteString -> Maybe (Int, ByteString)
readInt p@(PS x s l) = inlinePerformIO $ useAsCString p $ \cstr ->
    with (castPtr cstr) $ \endpp -> do
        val     <- c_strtol (castPtr cstr) endpp 0
        skipped <- (`minusPtr` cstr) `liftM` peek endpp
        return $ if skipped == 0
                 then Nothing
                 else Just (fromIntegral val, PS x (s+skipped) (l-skipped))

-- | unsafeReadInt is like readInt, but requires a null terminated
-- ByteString. It avoids a copy if this is the case. It returns the Int
-- read, if any, and the rest of the string.
unsafeReadInt :: ByteString -> Maybe (Int, ByteString)
unsafeReadInt p@(PS x s l) = inlinePerformIO $ unsafeUseAsCString p $ \cstr ->
    with (castPtr cstr) $ \endpp -> do
        val     <- c_strtol (castPtr cstr) endpp 0
        skipped <- (`minusPtr` cstr) `liftM` peek endpp
        return $ if skipped == 0
                 then Nothing
                 else Just (fromIntegral val, PS x (s+skipped) (l-skipped))

-- TODO poner how this can still be better

-- -----------------------------------------------------------------------------
-- I\/O functions

#if defined(__GLASGOW_HASKELL__)

--
-- | getLine, read a line from stdin.
--
getLine :: IO ByteString
getLine = hGetLine stdin

-- | hGetLine. read a ByteString from a handle
hGetLine :: Handle -> IO ByteString
hGetLine h = wantReadableHandle "FPS.hGetLine" h $ \ handle_ -> do
    case haBufferMode handle_ of
       NoBuffering -> error "no buffering"
       _other      -> hGetLineBuffered handle_

 where
    hGetLineBuffered handle_ = do
        let ref = haBuffer handle_
        buf <- readIORef ref
        hGetLineBufferedLoop handle_ ref buf 0 []

    hGetLineBufferedLoop handle_ ref
            buf@Buffer{ bufRPtr=r, bufWPtr=w, bufBuf=raw } len xss =
        len `seq` do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if (w == off + 1)
                            then writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
                            else writeIORef ref buf{ bufRPtr = off + 1 }
                    mkBigPS new_len (xs:xss)
            else do
                 maybe_buf <- maybeFillReadBuffer (haFD handle_) True (haIsStream handle_)
                                    buf{ bufWPtr=0, bufRPtr=0 }
                 case maybe_buf of
                    -- Nothing indicates we caught an EOF, and we may have a
                    -- partial line to return.
                    Nothing -> do
                         writeIORef ref buf{ bufRPtr=0, bufWPtr=0 }
                         if new_len > 0
                            then mkBigPS new_len (xs:xss)
                            else ioe_EOF
                    Just new_buf ->
                         hGetLineBufferedLoop handle_ ref new_buf new_len (xs:xss)

    -- find the end-of-line character, if there is one
    findEOL r w raw
        | r == w = return w
        | otherwise =  do
            (c,r') <- readCharFromBuffer raw r
            if c == '\n'
                then return r -- NB. not r': don't include the '\n'
                else findEOL r' w raw

    maybeFillReadBuffer fd is_line is_stream buf = catch
        (do buf' <- fillReadBuffer fd is_line is_stream buf
            return (Just buf'))
        (\e -> if isEOFError e then return Nothing else ioError e)

mkPS :: RawBuffer -> Int -> Int -> IO ByteString
mkPS buf start end = do
    let len = end - start
    fp <- mallocForeignPtr (len `quot` 8)
    withForeignPtr fp $ \p -> do
        memcpy_ptr_baoff p buf start (fromIntegral len)
        return (PS fp 0 len)

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps] = return ps
mkBigPS _ pss = return $! concat (Prelude.reverse pss)
#endif

------------------------------------------------------------------------

-- | Outputs a 'ByteString' to the specified 'Handle'.
--
-- The representation of the 'ByteString' in the file is assumed to
-- be in the ISO-8859-1 encoding.  In other words, only the least signficant
-- byte is taken from each character in the 'ByteString'.
--
hPut :: Handle -> ByteString -> IO ()
hPut _ (PS _ _ 0)  = return ()
hPut h (PS ps 0 l) = withForeignPtr ps $ \p-> hPutBuf h p l
hPut h (PS ps s l) = withForeignPtr ps $ \p-> hPutBuf h (p `plusPtr` s) l

--
-- | Write a ByteString to stdout
--
putStr :: ByteString -> IO ()
putStr = hPut stdout

--
-- | Write a ByteString to stdout, appending a newline character
--
putStrLn :: ByteString -> IO ()
putStrLn ps = hPut stdout ps >> hPut stdout nl
    where nl = packChar '\n'

-- | Read a 'ByteString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'.
--
-- As with 'hPut', the string representation in the file is assumed to
-- be ISO-8859-1.
--
hGet :: Handle -> Int -> IO ByteString
hGet _ 0 = return empty
hGet h i = do fp <- mallocForeignPtr i
              l  <- withForeignPtr fp $ \p-> hGetBuf h p i
              return $ PS fp 0 l

-- | hGetNonBlocking is identical to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.
--
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking _ 0 = return empty
hGetNonBlocking h i
    = do fp <- mallocForeignPtr i
         l  <- withForeignPtr fp $ \p -> hGetBufNonBlocking h p i
         return $ PS fp 0 l

--
-- | getContents. Equivalent to hGetContents stdin
--
getContents :: IO ByteString
getContents = hGetContents stdin

-- | Read entire handle contents into a 'ByteString'.
--
-- As with 'hGet', the string representation in the file is assumed to
-- be ISO-8859-1.
--
hGetContents :: Handle -> IO ByteString
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

-- | Read an entire file directly into a 'ByteString'.  This is far more
-- efficient than reading the characters into a 'String' and then using
-- 'pack'.  It also may be more efficient than opening the file and
-- reading it using hGet.
--
-- As with 'hGet', the string representation in the file is assumed to
-- be ISO-8859-1.
--
readFile :: FilePath -> IO ByteString
readFile f = do
    h <- openBinaryFile f ReadMode
    l <- hFileSize h
    s <- hGet h $ fromIntegral l
    hClose h
    return s

-- | Write a 'ByteString' to a file.
--
writeFile :: FilePath -> ByteString -> IO ()
writeFile f ps = do
    h <- openBinaryFile f WriteMode
    hPut h ps
    hClose h

-- | Like readFile, this reads an entire file directly into a
-- 'ByteString', but it is even more efficient.  It involves directly
-- mapping the file to memory.  This has the advantage that the contents
-- of the file never need to be copied.  Also, under memory pressure the
-- page may simply be discarded, while in the case of readFile it would
-- need to be written to swap.  If you read many small files, mmapFile
-- will be less memory-efficient than readFile, since each mmapFile
-- takes up a separate page of memory.  Also, you can run into bus
-- errors if the file is modified.  As with 'readFile', the string
-- representation in the file is assumed to be ISO-8859-1.
--
mmapFile :: FilePath -> IO ByteString
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
               -- unix only :(
               fd <- fromIntegral `liftM` handleToFd h
               p  <- my_mmap l fd
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

------------------------------------------------------------------------
-- Extended IO 

--
-- | A ByteString equivalent for getArgs. More efficient for large argument lists
--
getArgs :: IO [ByteString]
getArgs =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
    getProgArgv p_argc p_argv
    p    <- fromIntegral `liftM` peek p_argc
    argv <- peek p_argv
    Prelude.map packCString `fmap` peekArray (p - 1) (advancePtr argv 1)

-- ---------------------------------------------------------------------
-- Internal

-- Wrapper of mallocForeignPtrArray. Any ByteString allocated this way
-- is padded with a null byte.
mallocForeignPtr :: Int -> IO (ForeignPtr Word8)
mallocForeignPtr l = do
    fp <- mallocForeignPtrArray (l+1)
    withForeignPtr fp $ \p -> poke (p `plusPtr` l) (0::Word8)
    return fp

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = error ("ByteString." ++ fun ++ ": empty ByteString")

-- Unsafe 'ByteString' index (subscript) operator, starting from 0, returning a 'Word8'
(!) :: ByteString -> Int -> Word8
(!) (PS x s _) i = inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+i)
{-# INLINE (!) #-}

-- Conversion between 'Word8' and 'Char'
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

-- Just like inlinePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

-- | Perform an operation with a temporary ByteString
withPtr :: ForeignPtr a -> (Ptr a -> IO b) -> b
withPtr fp io = inlinePerformIO (withForeignPtr fp io)
{-# INLINE withPtr #-}

-- ---------------------------------------------------------------------
-- 
-- Uses standard headers
--

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

foreign import ccall unsafe "string.h memset" memset
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "static string.h strlen" c_strlen
    :: CString -> CInt

foreign import ccall unsafe "static stdlib.h strtol" c_strtol
    :: Ptr Word8 -> Ptr (Ptr Word8) -> Int -> IO CLong

-- ---------------------------------------------------------------------
--
-- Internal magic stuff

foreign import ccall unsafe "RtsAPI.h getProgArgv"
    getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> Int -> CSize -> IO (Ptr ())

-- ---------------------------------------------------------------------
--
-- Uses our C code (sometimes just wrappers)
--

#if defined(USE_CBITS)
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
#endif

-- ---------------------------------------------------------------------
-- 
-- Mmap things

#if defined(USE_MMAP)
foreign import ccall unsafe "static fpstring.h my_mmap" my_mmap
    :: Int -> Int -> IO (Ptr Word8)

foreign import ccall unsafe "static unistd.h close" c_close
    :: Int -> IO Int

#if !defined(__OpenBSD__)
foreign import ccall unsafe "static sys/mman.h munmap" c_munmap
    :: Ptr Word8 -> Int -> IO Int
#endif

#endif
