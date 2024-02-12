{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Data.ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005,
--               (c) Bjorn Bringert 2006,
--               (c) Don Stewart 2005-2008,
--               (c) Duncan Coutts 2006-2013
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time- and space-efficient implementation of byte vectors using
-- packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities and high speed requirements. Byte vectors
-- are encoded as strict 'Word8' arrays of bytes, held in a 'ForeignPtr',
-- and can be passed between C and Haskell with little effort.
--
-- The recomended way to assemble ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'ForeignPtr' by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
--

module Data.ByteString (

        -- * Strict @ByteString@
        ByteString,
        StrictByteString,

        -- ** Heap fragmentation
        -- | With GHC, the 'ByteString' representation uses /pinned memory/,
        -- meaning it cannot be moved by GC. While this is ideal for use with
        -- the foreign function interface and is usually efficient, this
        -- representation may lead to issues with heap fragmentation and wasted
        -- space if the program selectively retains a fraction of many small
        -- 'ByteString's, keeping them live in memory over long durations.
        --
        -- While 'ByteString' is indispensable when working with large blobs of
        -- data and especially when interfacing with native C libraries, be sure
        -- to also check the 'Data.ByteString.Short.ShortByteString' type.
        -- As a type backed by /unpinned/ memory, @ShortByteString@ behaves
        -- similarly to @Text@ (from the @text@ package) on the heap, completely
        -- avoids fragmentation issues, and in many use-cases may better suit
        -- your bytestring-storage needs.

        -- * Introducing and eliminating 'ByteString's
        empty,
        singleton,
        pack,
        unpack,
        fromStrict,
        toStrict,
        fromFilePath,
        toFilePath,

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
        isInfixOf,

        -- ** Encoding validation
        isValidUtf8,

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

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,

        -- ** Packing 'CString's and pointers
        packCString,
        packCStringLen,

        -- ** Using ByteStrings as 'CString's
        useAsCString,
        useAsCStringLen,

        -- * I\/O with 'ByteString's

        -- ** Standard input and output
        getLine,
        getContents,
        putStr,
        interact,

        -- ** Files
        readFile,
        writeFile,
        appendFile,

        -- ** I\/O with Handles
        hGetLine,
        hGetContents,
        hGet,
        hGetSome,
        hGetNonBlocking,
        hPut,
        hPutNonBlocking,
        hPutStr,
  ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,Foldable(..)
                                ,map,lines,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,filter
                                ,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,readFile,writeFile,appendFile,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem
                                )

import Data.Bits                (finiteBitSize, shiftL, (.|.), (.&.))

import Data.ByteString.Internal.Type
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Data.ByteString.Unsafe

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Word                (Word8)

import Control.Exception        (IOException, catch, finally, assert, throwIO)
import Control.Monad            (when)

import Foreign.C.String         (CString, CStringLen)
import Foreign.ForeignPtr       (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Marshal.Array    (allocaArray)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

-- hGetBuf and hPutBuf not available in yhc or nhc
import System.IO                (stdin,stdout,hClose,hFileSize
                                ,hGetBuf,hPutBuf,hGetBufNonBlocking
                                ,hPutBufNonBlocking,withBinaryFile
                                ,IOMode(..),hGetBufSome)
import System.IO.Error          (mkIOError, illegalOperationErrorType)

import Data.IORef
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import GHC.IO.Buffer
import GHC.IO.BufferedIO as Buffered
import GHC.IO.Encoding          (getFileSystemEncoding)
import GHC.Foreign              (newCStringLen, peekCStringLen)
import GHC.Stack.Types          (HasCallStack)
import Data.Char                (ord)

import GHC.Base                 (build)
import GHC.Word hiding (Word8)

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
-- Taking a slice of some static data rather than allocating a new
-- buffer for each call is nice for several reasons. Since it doesn't
-- involve any side effects hidden in a 'GHC.Magic.runRW#' call, it
-- can be simplified to a constructor application. This may enable GHC
-- to perform further optimizations after inlining, and also causes a
-- fresh singleton to take only 4 words of heap space instead of 9.
-- (The buffer object itself would take up 3 words: header, size, and
-- 1 word of content. The ForeignPtrContents object used to keep the
-- buffer alive would need two more.)
singleton c = unsafeTake 1 $ unsafeDrop (fromIntegral c) allBytes
{-# INLINE singleton #-}

-- | A static blob of all possible bytes (0x00 to 0xff) in order
allBytes :: ByteString
allBytes = unsafePackLenLiteral 0x100
  "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4a\x4b\x4c\x4d\x4e\x4f\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5a\x5b\x5c\x5d\x5e\x5f\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6a\x6b\x6c\x6d\x6e\x6f\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7a\x7b\x7c\x7d\x7e\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"#

-- | /O(n)/ Convert a @['Word8']@ into a 'ByteString'.
--
-- For applications with large numbers of string literals, 'pack' can be a
-- bottleneck. In such cases, consider using 'unsafePackAddress' (GHC only).
pack :: [Word8] -> ByteString
pack = packBytes

-- | /O(n)/ Converts a 'ByteString' to a @['Word8']@.
unpack :: ByteString -> [Word8]
unpack bs = build (unpackFoldr bs)
{-# INLINE unpack #-}

--
-- Have unpack fuse with good list consumers
--
unpackFoldr :: ByteString -> (Word8 -> a -> a) -> a -> a
unpackFoldr bs k z = foldr k z bs
{-# INLINE [0] unpackFoldr #-}

{-# RULES
"ByteString unpack-list" [1]  forall bs .
    unpackFoldr bs (:) [] = unpackBytes bs
 #-}

-- | Convert a 'FilePath' to a 'ByteString'.
--
-- The 'FilePath' type is expected to use the file system encoding
-- as reported by 'GHC.IO.Encoding.getFileSystemEncoding'. This
-- encoding allows for round-tripping of arbitrary data on platforms
-- that allow arbitrary bytes in their paths. This conversion
-- function does the same thing that `System.IO.openFile` would
-- do when decoding the 'FilePath'.
--
-- This function is in 'IO' because the file system encoding can be
-- changed. If the encoding can be assumed to be constant in your
-- use case, you may invoke this function via 'unsafePerformIO'.
--
-- @since 0.11.2.0
fromFilePath :: FilePath -> IO ByteString
fromFilePath path = do
    enc <- getFileSystemEncoding
    newCStringLen enc path >>= unsafePackMallocCStringLen

-- | Convert a 'ByteString' to a 'FilePath'.
--
-- This function uses the file system encoding, and resulting 'FilePath's
-- can be safely used with standard IO functions and will reference the
-- correct path in the presence of arbitrary non-UTF-8 encoded paths.
--
-- This function is in 'IO' because the file system encoding can be
-- changed. If the encoding can be assumed to be constant in your
-- use case, you may invoke this function via 'unsafePerformIO'.
--
-- @since 0.11.2.0
toFilePath :: ByteString -> IO FilePath
toFilePath path = do
    enc <- getFileSystemEncoding
    useAsCStringLen path (peekCStringLen enc)

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null (BS _ l) = assert (l >= 0) $ l <= 0
{-# INLINE null #-}

-- ---------------------------------------------------------------------
-- | /O(1)/ 'length' returns the length of a ByteString as an 'Int'.
length :: ByteString -> Int
length (BS _ l) = assert (l >= 0) l
{-# INLINE length #-}

------------------------------------------------------------------------

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Word8 -> ByteString -> ByteString
cons c (BS x len) = unsafeCreateFp (checkedAdd "cons" len 1) $ \p -> do
        pokeFp p c
        memcpyFp (p `plusForeignPtr` 1) x len
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (BS x len) c = unsafeCreateFp (checkedAdd "snoc" len 1) $ \p -> do
        memcpyFp p x len
        pokeFp (p `plusForeignPtr` len) c
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => ByteString -> Word8
head (BS x l)
    | l <= 0    = errorEmptyList "head"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peek p
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => ByteString -> ByteString
tail ps
    | length ps <= 0 = errorEmptyList "tail"
    | otherwise = unsafeDrop 1 ps
{-# INLINE tail #-}

-- | /O(1)/ Extract the 'head' and 'tail' of a ByteString, returning 'Nothing'
-- if it is empty.
uncons :: ByteString -> Maybe (Word8, ByteString)
uncons ps@(BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (accursedUnutterablePerformIO $ unsafeWithForeignPtr x
                                                     $ \p -> peek p,
                        unsafeDrop 1 ps)
{-# INLINE uncons #-}

-- | /O(1)/ Extract the last element of a ByteString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => ByteString -> Word8
last ps@(BS x l)
    | null ps   = errorEmptyList "last"
    | otherwise = accursedUnutterablePerformIO $
                    unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1)
{-# INLINE last #-}

-- | /O(1)/ Returns all the elements of a 'ByteString' except the last one.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => ByteString -> ByteString
init ps
    | null ps   = errorEmptyList "init"
    | otherwise = unsafeDropEnd 1 ps
{-# INLINE init #-}

-- | /O(1)/ Extract the 'init' and 'last' of a ByteString, returning 'Nothing'
-- if it is empty.
unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc ps@(BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (unsafeDropEnd 1 ps,
                        accursedUnutterablePerformIO $
                          unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1))
{-# INLINE unsnoc #-}

-- | /O(n)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append = mappend
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> ByteString -> ByteString
map f (BS srcPtr len) = unsafeCreateFp len $ \dstPtr -> m srcPtr dstPtr
  where
    m !p1 !p2 = map_ 0
      where
      map_ :: Int -> IO ()
      map_ !n
         | n >= len = return ()
         | otherwise = do
              x <- peekFpByteOff p1 n
              pokeFpByteOff p2 n (f x)
              map_ (n+1)
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse (BS x l) = unsafeCreateFp l $ \fp ->
  unsafeWithForeignPtr fp $ \p ->
    unsafeWithForeignPtr x  $ \f ->
      c_reverse p f (fromIntegral l)

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'ByteString' and \`intersperses\' that byte between the elements of
-- the 'ByteString'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse c ps@(BS x l)
    | length ps < 2  = ps
    | otherwise      = unsafeCreateFp (2*l-1) $ \fp ->
      unsafeWithForeignPtr fp $ \p ->
        unsafeWithForeignPtr x $ \f ->
          c_intersperse p f (fromIntegral l) c

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose = P.map pack . List.transpose . P.map unpack

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
--
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z = \(BS fp len) ->
  let
    end = unsafeForeignPtrToPtr fp `plusPtr` (-1)
    -- not tail recursive; traverses array right to left
    go !p | p == end  = z
          | otherwise = let !x = accursedUnutterablePerformIO $ do
                                   x' <- peek p
                                   touchForeignPtr fp
                                   return x'
                        in f (go (p `plusPtr` (-1))) x

  in
    go (end `plusPtr` len)
{-# INLINE foldl #-}

{-
Note [fold inlining]:

GHC will only inline a function marked INLINE
if it is fully saturated (meaning the number of
arguments provided at the call site is at least
equal to the number of lhs arguments).

-}
-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f v = \(BS fp len) ->
          -- see fold inlining
  let
    g ptr = go v ptr
      where
        end  = ptr `plusForeignPtr` len
        -- tail recursive; traverses array left to right
        go !z !p | p == end  = return z
                 | otherwise = do x <- peekFp p
                                  go (f z x) (p `plusForeignPtr` 1)
  in
    accursedUnutterablePerformIO $ g fp
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z = \(BS fp len) ->
          -- see fold inlining
  let
    ptr = unsafeForeignPtrToPtr fp
    end = ptr `plusPtr` len
    -- not tail recursive; traverses array left to right
    go !p | p == end  = z
          | otherwise = let !x = accursedUnutterablePerformIO $ do
                                   x' <- peek p
                                   touchForeignPtr fp
                                   return x'
                         in k x (go (p `plusPtr` 1))
  in
    go ptr
{-# INLINE foldr #-}

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' k v = \(BS fp len) ->
          -- see fold inlining
  let
    g ptr = go v (end `plusForeignPtr` len)
      where
        end = ptr `plusForeignPtr` (-1)
        -- tail recursive; traverses array right to left
        go !z !p | p == end  = return z
                 | otherwise = do x <- peekFp p
                                  go (k x z) (p `plusForeignPtr` (-1))
  in
    accursedUnutterablePerformIO $ g fp

{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteString's.
-- An exception will be thrown in the case of an empty ByteString.
foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 f ps = case uncons ps of
  Nothing     -> errorEmptyList "foldl1"
  Just (h, t) -> foldl f h t
{-# INLINE foldl1 #-}

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty ByteString.
foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' f ps = case uncons ps of
  Nothing     -> errorEmptyList "foldl1'"
  Just (h, t) -> foldl' f h t
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
-- An exception will be thrown in the case of an empty ByteString.
foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 f ps = case unsnoc ps of
  Nothing -> errorEmptyList "foldr1"
  Just (b, c) -> foldr f c b
{-# INLINE foldr1 #-}

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' f ps = case unsnoc ps of
  Nothing -> errorEmptyList "foldr1'"
  Just (b, c) -> foldr' f c b
{-# INLINE foldr1' #-}

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat = mconcat

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f = concat . foldr ((:) . f) []

-- foldr (append . f) empty

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any _ (BS _ 0)   = False
any f (BS x len) = accursedUnutterablePerformIO $ g x
  where
    g ptr = go ptr
      where
        end = ptr `plusForeignPtr` len
        go !p | p == end  = return False
              | otherwise = do c <- peekFp p
                               if f c then return True
                                      else go (p `plusForeignPtr` 1)
{-# INLINE [1] any #-}

{-# RULES
"ByteString specialise any (x ==)" forall x.
    any (x `eqWord8`) = anyByte x
"ByteString specialise any (== x)" forall x.
    any (`eqWord8` x) = anyByte x
  #-}

-- | Is any element of 'ByteString' equal to c?
anyByte :: Word8 -> ByteString -> Bool
anyByte c (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    q <- memchr p c (fromIntegral l)
    return $! q /= nullPtr
{-# INLINE anyByte #-}

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all _ (BS _ 0)   = True
all f (BS x len) = accursedUnutterablePerformIO $ g x
  where
    g ptr = go ptr
      where
        end = ptr `plusForeignPtr` len
        go !p | p == end  = return True  -- end of list
              | otherwise = do c <- peekFp p
                               if f c
                                  then go (p `plusForeignPtr` 1)
                                  else return False
{-# INLINE [1] all #-}

{-# RULES
"ByteString specialise all (x /=)" forall x.
    all (x `neWord8`) = not . anyByte x
"ByteString specialise all (/= x)" forall x.
    all (`neWord8` x) = not . anyByte x
  #-}

------------------------------------------------------------------------

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
-- An exception will be thrown in the case of an empty ByteString.
maximum :: HasCallStack => ByteString -> Word8
maximum xs@(BS x l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      c_maximum p (fromIntegral l)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
-- An exception will be thrown in the case of an empty ByteString.
minimum :: HasCallStack => ByteString -> Word8
minimum xs@(BS x l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      c_minimum p (fromIntegral l)
{-# INLINE minimum #-}

------------------------------------------------------------------------

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f acc = \(BS a len) -> unsafeDupablePerformIO $ do
               -- see fold inlining
    gp   <- mallocByteString len
    let
      go src dst = mapAccumL_ acc 0
        where
          mapAccumL_ !s !n
             | n >= len = return s
             | otherwise = do
                  x <- peekFpByteOff src n
                  let (s', y) = f s x
                  pokeFpByteOff dst n y
                  mapAccumL_ s' (n+1)
    acc' <- go a gp
    return (acc', BS gp len)
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f acc = \(BS a len) -> unsafeDupablePerformIO $ do
               -- see fold inlining
    gp   <- mallocByteString len
    let
      go src dst = mapAccumR_ acc (len-1)
        where
          mapAccumR_ !s (-1) = return s
          mapAccumR_ !s !n   = do
              x  <- peekFpByteOff src n
              let (s', y) = f s x
              pokeFpByteOff dst n y
              mapAccumR_ s' (n-1)
    acc' <- go a gp
    return (acc', BS gp len)
{-# INLINE mapAccumR #-}

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
scanl f v = \(BS a len) -> unsafeCreateFp (checkedAdd "scanl" len 1) $ \q -> do
         -- see fold inlining
        pokeFp q v
        let
          go src dst = scanl_ v 0
            where
              scanl_ !z !n
                  | n >= len  = return ()
                  | otherwise = do
                      x <- peekFpByteOff src n
                      let z' = f z x
                      pokeFpByteOff dst n z'
                      scanl_ z' (n+1)
        go a (q `plusForeignPtr` 1)
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 f ps = case uncons ps of
  Nothing     -> empty
  Just (h, t) -> scanl f h t
{-# INLINE scanl1 #-}

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
scanr
    :: (Word8 -> Word8 -> Word8)
    -- ^ element -> accumulator -> new accumulator
    -> Word8
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanr f v = \(BS a len) -> unsafeCreateFp (checkedAdd "scanr" len 1) $ \b -> do
         -- see fold inlining
        pokeFpByteOff b len v
        let
          go p q = scanr_ v (len-1)
            where
              scanr_ !z !n
                  | n < 0     = return ()
                  | otherwise = do
                      x <- peekFpByteOff p n
                      let z' = f x z
                      pokeFpByteOff q n z'
                      scanr_ z' (n-1)
        go a b
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 f ps = case unsnoc ps of
  Nothing -> empty
  Just (b, c) -> scanr f c b
{-# INLINE scanr1 #-}

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = fst (unfoldrN w (\u -> Just (u,u)) c)
replicate :: Int -> Word8 -> ByteString
replicate w c
    | w <= 0    = empty
    | otherwise = unsafeCreateFp w $ \fptr ->
        unsafeWithForeignPtr fptr $ \ptr ->
                      fillBytes ptr c w
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = concat . unfoldChunk 32 64
  where unfoldChunk n n' x =
          case unfoldrN n f x of
            (s, Nothing) -> [s]
            (s, Just x') -> s : unfoldChunk n' (n+n') x'
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
unfoldrN i f x0
    | i < 0     = (empty, Just x0)
    | otherwise = unsafeDupablePerformIO $ createFpAndTrim' i $ \p -> go p x0 0
  where
    go !p !x !n = go' x n
      where
        go' !x' !n'
          | n' == i    = return (0, n', Just x')
          | otherwise = case f x' of
                          Nothing      -> return (0, n', Nothing)
                          Just (w,x'') -> do pokeFpByteOff p n' w
                                             go' x'' (n'+1)
{-# INLINE unfoldrN #-}

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take n ps@(BS _ l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = unsafeTake n ps
{-# INLINE take #-}

-- | /O(1)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
-- >>> takeEnd 0 "abcdefg"
-- ""
-- >>> takeEnd 4 "abc"
-- "abc"
--
-- @since 0.11.1.0
takeEnd :: Int -> ByteString -> ByteString
takeEnd n ps@(BS _ len)
  | n >= len  = ps
  | n <= 0    = empty
  | otherwise = unsafeTakeEnd n ps
{-# INLINE takeEnd #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or 'empty' if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop n ps@(BS _ l)
    | n <= 0    = ps
    | n >= l    = empty
    | otherwise = unsafeDrop n ps
{-# INLINE drop #-}

-- | /O(1)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
--
-- @since 0.11.1.0
dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps@(BS _ len)
    | n <= 0    = ps
    | n >= len  = empty
    | otherwise = unsafeDropEnd n ps
{-# INLINE dropEnd #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n ps@(BS _ l)
    | n <= 0    = (empty, ps)
    | n >= l    = (ps, empty)
    | otherwise = (unsafeTake n ps, unsafeDrop n ps)
{-# INLINE splitAt #-}

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f ps = unsafeTake (findIndexOrLength (not . f) ps) ps
{-# INLINE [1] takeWhile #-}

{-# RULES
"ByteString specialise takeWhile (x /=)" forall x.
    takeWhile (x `neWord8`) = fst . breakByte x
"ByteString specialise takeWhile (/= x)" forall x.
    takeWhile (`neWord8` x) = fst . breakByte x
"ByteString specialise takeWhile (x ==)" forall x.
    takeWhile (x `eqWord8`) = fst . spanByte x
"ByteString specialise takeWhile (== x)" forall x.
    takeWhile (`eqWord8` x) = fst . spanByte x
  #-}

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd f ps = unsafeDrop (findFromEndUntil (not . f) ps) ps
{-# INLINE takeWhileEnd #-}

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f ps = unsafeDrop (findIndexOrLength (not . f) ps) ps
{-# INLINE [1] dropWhile #-}

{-# RULES
"ByteString specialise dropWhile (x /=)" forall x.
    dropWhile (x `neWord8`) = snd . breakByte x
"ByteString specialise dropWhile (/= x)" forall x.
    dropWhile (`neWord8` x) = snd . breakByte x
"ByteString specialise dropWhile (x ==)" forall x.
    dropWhile (x `eqWord8`) = snd . spanByte x
"ByteString specialise dropWhile (== x)" forall x.
    dropWhile (`eqWord8` x) = snd . spanByte x
  #-}

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd f ps = unsafeTake (findFromEndUntil (not . f) ps) ps
{-# INLINE dropWhileEnd #-}

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
-- Under GHC, a rewrite rule will transform break (==) into a
-- call to the specialised breakByte:
--
-- > break ((==) x) = breakByte x
-- > break (==x) = breakByte x
--
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p ps = case findIndexOrLength p ps of n -> (unsafeTake n ps, unsafeDrop n ps)
{-# INLINE [1] break #-}

-- See bytestring #70
{-# RULES
"ByteString specialise break (x ==)" forall x.
    break (x `eqWord8`) = breakByte x
"ByteString specialise break (== x)" forall x.
    break (`eqWord8` x) = breakByte x
  #-}

-- INTERNAL:

-- | 'breakByte' breaks its ByteString argument at the first occurrence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (==99) "abcd" == breakByte 99 "abcd" -- fromEnum 'c' == 99
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (unsafeTake n p, unsafeDrop n p)
{-# INLINE breakByte #-}

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
--
breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd  p ps = splitAt (findFromEndUntil p ps) ps

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)
{-# INLINE [1] span #-}

-- | 'spanByte' breaks its ByteString argument at the first
-- occurrence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (==99) "abcd" == spanByte 99 "abcd" -- fromEnum 'c' == 99
--
spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c ps@(BS x l) =
    accursedUnutterablePerformIO $  unsafeWithForeignPtr x g
  where
    g p = go 0
      where
        go !i | i >= l    = return (ps, empty)
              | otherwise = do c' <- peekByteOff p i
                               if c /= c'
                                   then return (unsafeTake i ps, unsafeDrop i ps)
                                   else go (i+1)
{-# INLINE spanByte #-}

-- See bytestring #70
{-# RULES
"ByteString specialise span (x ==)" forall x.
    span (x `eqWord8`) = spanByte x
"ByteString specialise span (== x)" forall x.
    span (`eqWord8` x) = spanByte x
  #-}

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
spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd  p ps = splitAt (findFromEndUntil (not.p) ps) ps

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ (BS _  0) = []
splitWith predicate (BS fp len) = splitWith0 0 len fp
  where splitWith0 !off' !len' !fp' =
          accursedUnutterablePerformIO $
              splitLoop 0 off' len' fp'

        splitLoop :: Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]
        splitLoop idx2 off' len' fp' = go idx2
          where
            go idx'
                | idx' >= len'  = return [BS (plusForeignPtr fp' off') idx']
                | otherwise = do
                    w <- peekFpByteOff fp (off'+idx')
                    if predicate w
                       then return (BS (plusForeignPtr fp' off') idx' :
                                  splitWith0 (off'+idx'+1) (len'-idx'-1) fp')
                       else go (idx'+1)
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
split _ (BS _ 0) = []
split w ps@(BS x l) = loop 0
    where
        loop !n =
            let q = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      memchr (p `plusPtr` n)
                             w (fromIntegral (l-n))
            in if q == nullPtr
                then [unsafeDrop n ps]
                else let i = q `minusPtr` unsafeForeignPtrToPtr x
                      in unsafeSlice n i ps : loop (i+1)

{-# INLINE split #-}

unsafeSlice  :: Int -> Int -> ByteString -> ByteString
unsafeSlice a b (BS x _) = BS (plusForeignPtr x a) (b - a)
{-# INLINE unsafeSlice #-}

-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each string in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test. It is about 40% faster than
-- /groupBy (==)/
group :: ByteString -> [ByteString]
group xs = case uncons xs of
  Nothing     -> []
  Just (h, _) -> ys : group zs
    where
        (ys, zs) = spanByte h xs

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k xs = case uncons xs of
  Nothing     -> []
  Just (h, t) -> unsafeTake n xs : groupBy k (unsafeDrop n xs)
    where
        n = 1 + findIndexOrLength (not . k h) t

-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: ByteString -> [ByteString] -> ByteString
intercalate _ [] = mempty
intercalate _ [x] = x -- This branch exists for laziness, not speed
intercalate (BS sepPtr sepLen) (BS hPtr hLen : t) =
  unsafeCreateFp totalLen $ \dstPtr0 -> do
      memcpyFp dstPtr0 hPtr hLen
      let go _ [] = pure ()
          go dstPtr (BS chunkPtr chunkLen : chunks) = do
            memcpyFp dstPtr sepPtr sepLen
            let destPtr' = dstPtr `plusForeignPtr` sepLen
            memcpyFp destPtr' chunkPtr chunkLen
            go (destPtr' `plusForeignPtr` chunkLen) chunks
      go (dstPtr0 `plusForeignPtr` hLen) t
  where
  totalLen = List.foldl' (\acc chunk -> acc +! sepLen +! length chunk) hLen t
  (+!) = checkedAdd "intercalate"
{-# INLINABLE intercalate #-}

-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
--
-- This is a partial function, consider using 'indexMaybe' instead.
index :: HasCallStack => ByteString -> Int -> Word8
index ps n
    | n < 0          = moduleError "index" ("negative index: " ++ show n)
    | n >= length ps = moduleError "index" ("index too large: " ++ show n
                                         ++ ", length = " ++ show (length ps))
    | otherwise      = ps `unsafeIndex` n
{-# INLINE index #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe ps n
    | n < 0          = Nothing
    | n >= length ps = Nothing
    | otherwise      = Just $! ps `unsafeIndex` n
{-# INLINE indexMaybe #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ByteString -> Int -> Maybe Word8
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex c (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    q <- memchr p c (fromIntegral l)
    return $! if q == nullPtr then Nothing else Just $! q `minusPtr` p
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
elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd = findIndexEnd . (==)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int]
elemIndices w (BS x l) = loop 0
    where
        loop !n = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
            q <- memchr (p `plusPtr` n) w (fromIntegral (l - n))
            if q == nullPtr
                then return []
                else let !i = q `minusPtr` p
                      in return $ i : loop (i + 1)
{-# INLINE elemIndices #-}

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int
count w (BS x m) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
    fromIntegral <$> c_count p (fromIntegral m) w
{-# INLINE count #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex k (BS x l) = accursedUnutterablePerformIO $ g x
  where
    g !ptr = go 0
      where
        go !n | n >= l    = return Nothing
              | otherwise = do w <- peekFp $ ptr `plusForeignPtr` n
                               if k w
                                 then return (Just n)
                                 else go (n+1)
{-# INLINE [1] findIndex #-}

-- | /O(n)/ The 'findIndexEnd' function takes a predicate and a 'ByteString' and
-- returns the index of the last element in the ByteString
-- satisfying the predicate.
--
-- @since 0.10.12.0
findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndexEnd k (BS x l) = accursedUnutterablePerformIO $ g x
  where
    g !ptr = go (l-1)
      where
        go !n | n < 0     = return Nothing
              | otherwise = do w <- peekFpByteOff ptr n
                               if k w
                                 then return (Just n)
                                 else go (n-1)
{-# INLINE findIndexEnd #-}

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices p = loop 0
   where
     loop !n !qs = case findIndex p qs of
                     Just !i ->
                        let !j = n+i
                         in j : loop (j+1) (unsafeDrop (i+1) qs)
                     Nothing -> []
{-# INLINE [1] findIndices #-}


{-# RULES
"ByteString specialise findIndex (x ==)" forall x. findIndex (x`eqWord8`) = elemIndex x
"ByteString specialise findIndex (== x)" forall x. findIndex (`eqWord8`x) = elemIndex x
"ByteString specialise findIndices (x ==)" forall x. findIndices (x`eqWord8`) = elemIndices x
"ByteString specialise findIndices (== x)" forall x. findIndices (`eqWord8`x) = elemIndices x
  #-}

-- ---------------------------------------------------------------------
-- Searching ByteStrings

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem c ps = not (c `elem` ps)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter k = \ps@(BS pIn l) ->
        -- see fold inlining.
  if null ps
    then ps
    else
      unsafeDupablePerformIO $ createFpAndTrim l $ \pOut -> do
        let
          go' pf pt = go pf pt
            where
              end = pf `plusForeignPtr` l
              go !f !t | f == end  = return t
                       | otherwise = do
                           w <- peekFp f
                           if k w
                             then pokeFp t w
                               >> go (f `plusForeignPtr` 1) (t `plusForeignPtr` 1)
                             else go (f `plusForeignPtr` 1) t
        t <- go' pIn pOut
        return $! t `minusForeignPtr` pOut -- actual length
{-# INLINE filter #-}

{-
--
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single byte. It is more efficient to use
-- /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
--
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

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f p = case findIndex f p of
                    Just n -> Just (p `unsafeIndex` n)
                    _      -> Nothing
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition f s = unsafeDupablePerformIO $
    do        p <- mallocByteString len
              let end = p `plusForeignPtr` (len - 1)
              mid <- sep 0 p end
              rev mid end
              let i = mid `minusForeignPtr` p
              return (BS p i,
                      BS (p `plusForeignPtr` i) (len - i))
  where
    len  = length s
    incr = (`plusForeignPtr` 1)
    decr = (`plusForeignPtr` (-1))

    sep !i !p1 !p2
       | i == len  = return p1
       | f w       = do pokeFp p1 w
                        sep (i + 1) (incr p1) p2
       | otherwise = do pokeFp p2 w
                        sep (i + 1) p1 (decr p2)
      where
        w = s `unsafeIndex` i

    rev !p1 !p2 -- fixme: surely there are faster ways to do this
      | p1 >= p2  = return ()
      | otherwise = do a <- peekFp p1
                       b <- peekFp p2
                       pokeFp p1 b
                       pokeFp p2 a
                       rev (incr p1) (decr p2)

-- --------------------------------------------------------------------
-- Sarching for substrings

-- |/O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- if the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 p2 (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'stripPrefix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 0.10.8.0
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix bs1@(BS _ l1) bs2
   | bs1 `isPrefixOf` bs2 = Just (unsafeDrop l1 bs2)
   | otherwise = Nothing

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implementation uses memcmp to compare the end of the
-- string only, with no reverse required..
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 (p2 `plusPtr` (l2 - l1)) (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'stripSuffix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix bs1@(BS _ l1) bs2@(BS _ l2)
   | bs1 `isSuffixOf` bs2 = Just (unsafeTake (l2 - l1) bs2)
   | otherwise = Nothing

-- | Check whether one string is a substring of another.
isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf p s = null p || not (null $ snd $ breakSubstring p s)

-- | /O(n)/ Check whether a 'ByteString' represents valid UTF-8.
--
-- @since 0.11.2.0
isValidUtf8 :: ByteString -> Bool
isValidUtf8 (BS ptr len) = accursedUnutterablePerformIO $ unsafeWithForeignPtr ptr $ \p -> do
  -- Use a safe FFI call for large inputs to avoid GC synchronization pauses
  -- in multithreaded contexts.
  -- This specific limit was chosen based on results of a simple benchmark, see:
  -- https://github.com/haskell/bytestring/issues/451#issuecomment-991879338
  -- When changing this function, also consider changing the related function:
  -- Data.ByteString.Short.Internal.isValidUtf8
  i <- if len < 1000000
     then cIsValidUtf8 p (fromIntegral len)
     else cIsValidUtf8Safe p (fromIntegral len)
  pure $ i /= 0

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurrence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
-- Note that calling `breakSubstring x` does some preprocessing work, so
-- you should avoid unnecessarily duplicating breakSubstring calls with the same
-- pattern.
--
breakSubstring :: ByteString -- ^ String to search for
               -> ByteString -- ^ String to search in
               -> (ByteString,ByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat =
  case lp of
    0 -> (empty,)
    1 -> breakByte (unsafeHead pat)
    _ -> if lp * 8 <= finiteBitSize (0 :: Word)
             then shift
             else karpRabin
  where
    unsafeSplitAt i s = (unsafeTake i s, unsafeDrop i s)
    lp                = length pat
    karpRabin :: ByteString -> (ByteString, ByteString)
    karpRabin src
        | length src < lp = (src,empty)
        | otherwise = search (rollingHash $ unsafeTake lp src) lp
      where
        k           = 2891336453 :: Word32
        rollingHash = foldl' (\h b -> h * k + fromIntegral b) 0
        hp          = rollingHash pat
        m           = k ^ lp
        get = fromIntegral . unsafeIndex src
        search !hs !i
            | hp == hs && pat == unsafeTake lp b = u
            | length src <= i                    = (src,empty) -- not found
            | otherwise                          = search hs' (i + 1)
          where
            u@(_, b) = unsafeSplitAt (i - lp) src
            hs' = hs * k +
                  get i -
                  m * get (i - lp)
    {-# INLINE karpRabin #-}

    shift :: ByteString -> (ByteString, ByteString)
    shift !src
        | length src < lp = (src,empty)
        | otherwise       = search (intoWord $ unsafeTake lp src) lp
      where
        intoWord :: ByteString -> Word
        intoWord = foldl' (\w b -> (w `shiftL` 8) .|. fromIntegral b) 0
        wp   = intoWord pat
        mask = (1 `shiftL` (8 * lp)) - 1
        search !w !i
            | w == wp         = unsafeSplitAt (i - lp) src
            | length src <= i = (src, empty)
            | otherwise       = search w' (i + 1)
          where
            b  = fromIntegral (unsafeIndex src i)
            w' = mask .&. ((w `shiftL` 8) .|. b)
    {-# INLINE shift #-}

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of bytes. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> (psH, qsH) : zip psT qsT

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- corresponding sums.
zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith f ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> f psH qsH : zipWith f psT qsT
{-# NOINLINE [1] zipWith #-}

-- | A specialised version of `zipWith` for the common case of a
-- simultaneous map over two ByteStrings, to build a 3rd.
--
-- @since 0.11.1.0
packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith f (BS a l) (BS b m) = unsafeDupablePerformIO $
    createFp len $ go a b
  where
    go p1 p2 = zipWith_ 0
      where
        zipWith_ :: Int -> ForeignPtr Word8 -> IO ()
        zipWith_ !n !r
           | n >= len = return ()
           | otherwise = do
                x <- peekFpByteOff p1 n
                y <- peekFpByteOff p2 n
                pokeFpByteOff r n (f x y)
                zipWith_ (n+1) r

    len = min l m
{-# INLINE packZipWith #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Special lists

-- | /O(n)/ Returns all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators]
inits bs = NE.toList $! initsNE bs

-- | /O(n)/ Returns all initial segments of the given 'ByteString', shortest first.
--
-- @since 0.11.4.0
initsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators]
initsNE ps = empty :| [unsafeTake n ps | n <- [1..length ps]]

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators]
tails bs = NE.toList $! tailsNE bs

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
--
-- @since 0.11.4.0
tailsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators]
tailsNE p | null p    = empty :| []
          | otherwise = p :| tails (unsafeTail p)

-- less efficent spacewise: tails (BS x l) = [BS (plusForeignPtr x n) (l-n) | n <- [0..l]]

{-
Note [Avoid NonEmpty combinators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As of base-4.18, most of the NonEmpty API is surprisingly lazy.
Using it without forcing the arguments yourself is just begging GHC
to make your code waste time allocating useless selector thunks.
This may change in the future. See also this CLC issue:
  https://github.com/haskell/core-libraries-committee/issues/107
But until then, "refactor" with care!

(Even for uses of NonEmpty near lazy ByteStrings, we don't want
the extra laziness of the NonEmpty API.)
-}



-- ---------------------------------------------------------------------
-- ** Ordered 'ByteString's

-- | /O(n)/ Sort a ByteString efficiently, using counting sort.
sort :: ByteString -> ByteString
sort (BS input l)
  -- qsort outperforms counting sort for small arrays
  | l <= 20 = unsafeCreateFp l $ \destFP -> do
    memcpyFp destFP input l
    unsafeWithForeignPtr destFP $ \dest -> c_sort dest (fromIntegral l)
  | otherwise = unsafeCreateFp l $ \p -> allocaArray 256 $ \arr -> do

    fillBytes (castPtr arr) 0 (256 * sizeOf (undefined :: Int))
    unsafeWithForeignPtr input (\x -> countOccurrences arr x l)

    let go 256 !_   = return ()
        go i   !ptr = do n <- peekElemOff arr i
                         when (n /= 0) $
                           fillBytes ptr (fromIntegral @Int @Word8 i) n
                         go (i + 1) (ptr `plusPtr` fromIntegral n)
    unsafeWithForeignPtr p (go 0)
  where
    -- Count the number of occurrences of each byte.
    countOccurrences :: Ptr Int -> Ptr Word8 -> Int -> IO ()
    countOccurrences !counts !str !len = go 0
     where
        go !i | i == len    = return ()
              | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                               x <- peekElemOff counts k
                               pokeElemOff counts k (x + 1)
                               go (i + 1)


-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString (BS fp l) action =
  allocaBytes (l+1) $ \buf -> do
    unsafeWithForeignPtr fp $ \p -> copyBytes buf p l
    pokeByteOff buf l (0::Word8)
    action (castPtr buf)

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a 'CStringLen'.
-- As for 'useAsCString' this function makes a copy of the original @ByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- Beware that this function is not required to add a terminating @\NUL@ byte at the end of the 'CStringLen' it provides.
-- If you need to construct a pointer to a null-terminated sequence, use 'useAsCString'
-- (and measure length independently if desired).
useAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen p@(BS _ l) f = useAsCString p $ \cstr -> f (cstr,l)

------------------------------------------------------------------------

-- | /O(n)./ Construct a new @ByteString@ from a @CString@. The
-- resulting @ByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
packCString :: CString -> IO ByteString
packCString cstr = do
    len <- c_strlen cstr
    packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ByteString@ from a @CStringLen@. The
-- resulting @ByteString@ is an immutable copy of the original @CStringLen@.
-- The @ByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
packCStringLen :: CStringLen -> IO ByteString
packCStringLen (cstr, len) | len >= 0 = createFp len $ \fp ->
    unsafeWithForeignPtr fp $ \p -> copyBytes p (castPtr cstr) len
packCStringLen (_, len) =
    moduleErrorIO "packCStringLen" ("negative length: " ++ show len)

------------------------------------------------------------------------

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
-- This is mainly useful to allow the rest of the data pointed
-- to by the 'ByteString' to be garbage collected, for example
-- if a large string has been read in, and only a small part of it
-- is needed in the rest of the program.
--
copy :: ByteString -> ByteString
copy (BS x l) = unsafeCreateFp l $ \p -> memcpyFp p x l

-- ---------------------------------------------------------------------
-- Line IO

-- | Read a line from stdin.
getLine :: IO ByteString
getLine = hGetLine stdin

{-# DEPRECATED getLine
     "Deprecated since @bytestring-0.12@. Use 'Data.ByteString.Char8.getLine' instead. (Functions that rely on ASCII encodings belong in \"Data.ByteString.Char8\")"
  #-}

-- | Read a line from a handle
hGetLine :: Handle -> IO ByteString
hGetLine h =
  wantReadableHandle_ "Data.ByteString.hGetLine" h $
    \ h_@Handle__{haByteBuffer} -> do
      flushCharReadBuffer h_
      buf <- readIORef haByteBuffer
      if isEmptyBuffer buf
         then fill h_ buf 0 []
         else haveBuf h_ buf 0 []
 where

  fill h_@Handle__{haByteBuffer,haDevice} buf !len xss = do
    (r,buf') <- Buffered.fillReadBuffer haDevice buf
    if r == 0
       then do writeIORef haByteBuffer buf{ bufR=0, bufL=0 }
               if len > 0
                  then mkBigPS len xss
                  else ioe_EOF
       else haveBuf h_ buf' len xss

  haveBuf h_@Handle__{haByteBuffer}
          buf@Buffer{ bufRaw=raw, bufR=w, bufL=r }
          len xss =
    do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if w == off + 1
                            then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                            else writeIORef haByteBuffer buf{ bufL = off + 1 }
                    mkBigPS new_len (xs:xss)
            else fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  -- find the end-of-line character, if there is one
  findEOL r w raw
        | r == w = return w
        | otherwise =  do
            c <- readWord8Buf raw r
            if c == fromIntegral (ord '\n')
                then return r -- NB. not r+1: don't include the '\n'
                else findEOL (r+1) w raw

{-# DEPRECATED hGetLine
     "Deprecated since @bytestring-0.12@. Use 'Data.ByteString.Char8.hGetLine' instead. (Functions that rely on ASCII encodings belong in \"Data.ByteString.Char8\")"
  #-}

mkPS :: RawBuffer Word8 -> Int -> Int -> IO ByteString
mkPS buf start end =
 createFp len $ \fp -> memcpyFp fp (buf `plusForeignPtr` start) len
 where
   len = end - start

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps] = return ps
mkBigPS _ pss = return $! concat (P.reverse pss)

-- ---------------------------------------------------------------------
-- Block IO

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> ByteString -> IO ()
hPut _ (BS _  0) = return ()
hPut h (BS ps l) = unsafeWithForeignPtr ps $ \p-> hPutBuf h p l

-- | Similar to 'hPut' except that it will never block. Instead it returns
-- any tail that did not get written. This tail may be 'empty' in the case that
-- the whole string was written, or the whole original string if nothing was
-- written. Partial writes are also possible.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hPut'.
--
hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking h bs@(BS ps l) = do
  bytesWritten <- unsafeWithForeignPtr ps $ \p-> hPutBufNonBlocking h p l
  return $! drop bytesWritten bs

-- | A synonym for 'hPut', for compatibility
hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

-- | Write a ByteString to 'stdout'.
putStr :: ByteString -> IO ()
putStr = hPut stdout

------------------------------------------------------------------------
-- Low level IO

-- | Read a 'ByteString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'. First argument is the Handle to read from,
-- and the second is the number of bytes to read. It returns the bytes
-- read, up to n, or 'empty' if EOF has been reached.
--
-- 'hGet' is implemented in terms of 'hGetBuf'.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGet' will behave as if EOF was reached.
--
hGet :: Handle -> Int -> IO ByteString
hGet h i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBuf h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGet" i

-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.  If there is no data available to be read, 'hGetNonBlocking'
-- returns 'empty'.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hGet'.
--
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking h i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBufNonBlocking h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGetNonBlocking" i

-- | Like 'hGet', except that a shorter 'ByteString' may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
--
hGetSome :: Handle -> Int -> IO ByteString
hGetSome hh i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBufSome hh p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize hh "hGetSome" i

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []


-- | Read a handle's entire contents strictly into a 'ByteString'.
--
-- This function reads chunks at a time, increasing the chunk size on each
-- read. The final string is then reallocated to the appropriate size. For
-- files > half of available memory, this may lead to memory exhaustion.
-- Consider using 'readFile' in this case.
--
-- The Handle is closed once the contents have been read,
-- or if an exception is thrown.
--
hGetContents :: Handle -> IO ByteString
hGetContents hnd = do
    bs <- hGetContentsSizeHint hnd 1024 2048
            `finally` hClose hnd
    -- don't waste too much space for small files:
    if length bs < 900
      then return $! copy bs
      else return bs

hGetContentsSizeHint :: Handle
                     -> Int -- ^ first read size
                     -> Int -- ^ initial buffer size increment
                     -> IO ByteString
hGetContentsSizeHint hnd =
    readChunks []
  where
    readChunks chunks sz sz' = do
      fp        <- mallocByteString sz
      readcount <- unsafeWithForeignPtr fp $ \buf -> hGetBuf hnd buf sz
      let chunk = BS fp readcount
      -- We rely on the hGetBuf behaviour (not hGetBufSome) where it reads up
      -- to the size we ask for, or EOF. So short reads indicate EOF.
      if readcount < sz && sz > 0
        then return $! concat (P.reverse (chunk : chunks))
        else readChunks (chunk : chunks) sz' ((sz+sz') `min` 32752)
             -- we grow the buffer sizes, but not too huge
             -- we concatenate in the end anyway

-- | getContents. Read stdin strictly. Equivalent to hGetContents stdin
-- The 'Handle' is closed after the contents have been read.
--
getContents :: IO ByteString
getContents = hGetContents stdin

-- | The interact function takes a function of type @ByteString -> ByteString@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device.
--
interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- | Read an entire file strictly into a 'ByteString'.
--
readFile :: FilePath -> IO ByteString
readFile f =
    withBinaryFile f ReadMode $ \h -> do
      -- hFileSize fails if file is not regular file (like
      -- /dev/null). Catch exception and try reading anyway.
      filesz <- catch (hFileSize h) useZeroIfNotRegularFile
      let readsz = (fromIntegral filesz `max` 0) + 1
      hGetContentsSizeHint h readsz (readsz `max` 255)
      -- Our initial size is one bigger than the file size so that in the
      -- typical case we will read the whole file in one go and not have
      -- to allocate any more chunks. We'll still do the right thing if the
      -- file size is 0 or is changed before we do the read.
  where
    useZeroIfNotRegularFile :: IOException -> IO Integer
    useZeroIfNotRegularFile _ = return 0

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

-- | Append a 'ByteString' to a file.
appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun = moduleError fun "empty ByteString"
{-# NOINLINE errorEmptyList #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorIO :: HasCallStack => String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString." ++ fun ++ ':':' ':msg

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil f ps@(BS _ l) = case unsnoc ps of
  Nothing     -> 0
  Just (b, c) ->
    if f c
      then l
      else findFromEndUntil f b
