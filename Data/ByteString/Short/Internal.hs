{-# LANGUAGE DeriveDataTypeable, CPP, BangPatterns, RankNTypes,
             ForeignFunctionInterface, MagicHash, UnboxedTuples,
             UnliftedFFITypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fexpose-all-unfoldings #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK not-home #-}

-- Not all architectures are forgiving of unaligned accesses; whitelist ones
-- which are known not to trap (either to the kernel for emulation, or crash).
#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH) \
    || ((defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)) \
        && defined(__ARM_FEATURE_UNALIGNED)) \
    || defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(powerpc64le_HOST_ARCH)
#define SAFE_UNALIGNED 1
#endif

-- |
-- Module      : Data.ByteString.Short.Internal
-- Copyright   : (c) Duncan Coutts 2012-2013, Julian Ospald 2022
-- License     : BSD-style
--
-- Maintainer  : hasufell@posteo.de
-- Stability   : stable
-- Portability : ghc only
--
-- Internal representation of ShortByteString
--
module Data.ByteString.Short.Internal (

    -- * The @ShortByteString@ type and representation
    ShortByteString(..),

    -- * Introducing and eliminating 'ShortByteString's
    empty,
    singleton,
    pack,
    unpack,
    fromShort,
    toShort,

    -- * Basic interface
    snoc,
    cons,
    append,
    last,
    tail,
    uncons,
    head,
    init,
    unsnoc,
    null,
    length,

    -- * Transforming ShortByteStrings
    map,
    reverse,
    intercalate,

    -- * Reducing 'ShortByteString's (folds)
    foldl,
    foldl',
    foldl1,
    foldl1',

    foldr,
    foldr',
    foldr1,
    foldr1',

    -- ** Special folds
    all,
    any,
    concat,

    -- ** Generating and unfolding ByteStrings
    replicate,
    unfoldr,
    unfoldrN,

    -- * Substrings

    -- ** Breaking strings
    take,
    takeEnd,
    takeWhileEnd,
    takeWhile,
    drop,
    dropEnd,
    dropWhile,
    dropWhileEnd,
    breakEnd,
    break,
    span,
    spanEnd,
    splitAt,
    split,
    splitWith,
    stripSuffix,
    stripPrefix,

    -- * Predicates
    isInfixOf,
    isPrefixOf,
    isSuffixOf,

    -- ** Search for arbitrary substrings
    breakSubstring,

    -- * Searching ShortByteStrings

    -- ** Searching by equality
    elem,

    -- ** Searching with a predicate
    find,
    filter,
    partition,

    -- * Indexing ShortByteStrings
    index,
    indexMaybe,
    (!?),
    elemIndex,
    elemIndices,
    count,
    findIndex,
    findIndices,

    -- * Low level operations
    createFromPtr,
    copyToPtr,

    -- ** Encoding validation
    isValidUtf8,

    -- * Low level conversions
    -- ** Packing 'Foreign.C.String.CString's and pointers
    packCString,
    packCStringLen,

    -- ** Using ShortByteStrings as 'Foreign.C.String.CString's
    useAsCString,
    useAsCStringLen,
  ) where

import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO, checkedAdd)
import qualified Data.ByteString.Internal as BS

import Data.Bifunctor   ( bimap )
import Data.Typeable    (Typeable)
import Data.Data        (Data(..), mkNoRepType)
import Data.Bits        ( FiniteBits (finiteBitSize)
                        , shiftL
#if MIN_VERSION_base(4,12,0) && defined(SAFE_UNALIGNED)
                        , shiftR
#endif
                        , (.&.)
                        , (.|.)
                        )
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import Data.Semigroup   (Semigroup((<>)))
import Data.Monoid      (Monoid(..))
import Data.String      (IsString(..))
import Control.Applicative (pure)
import Control.Exception        (assert)
import Control.Monad    ((>>))
import Control.DeepSeq  (NFData(..))
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types  (CSize(..), CInt(..), CPtrdiff(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Storable (pokeByteOff)

import qualified GHC.Exts
import GHC.Exts ( Int(I#), Int#, Ptr(Ptr), Addr#, Char(C#)
                , State#, RealWorld
                , ByteArray#, MutableByteArray#
                , newByteArray#
                , newPinnedByteArray#
                , byteArrayContents#
                , unsafeCoerce#
                , copyMutableByteArray#
#if MIN_VERSION_base(4,10,0)
                , isByteArrayPinned#
                , isTrue#
#endif
#if MIN_VERSION_base(4,11,0)
                , compareByteArrays#
#endif
                , sizeofByteArray#
                , indexWord8Array#, indexCharArray#
                , writeWord8Array#
                , unsafeFreezeByteArray#
#if MIN_VERSION_base(4,12,0) && defined(SAFE_UNALIGNED)
                ,writeWord64Array#
                ,indexWord8ArrayAsWord64#
#endif
                , setByteArray# )
import GHC.IO
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(PlainPtr))
import GHC.ST         (ST(ST), runST)
import GHC.Stack.Types (HasCallStack)
import GHC.Word


import Prelude ( Eq(..), Ord(..), Ordering(..), Read(..), Show(..)
               , ($), ($!), error, (++), (.), (||)
               , String, userError
               , Bool(..), (&&), otherwise
               , (+), (-), fromIntegral
               , (*)
               , (^)
               , (<$>)
               , return
               , Maybe(..)
               , not
               , snd
               )

import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH

-- | A compact representation of a 'Word8' vector.
--
-- It has a lower memory overhead than a 'ByteString' and does not
-- contribute to heap fragmentation. It can be converted to or from a
-- 'ByteString' (at the cost of copying the string data). It supports very few
-- other operations.
--
-- It is suitable for use as an internal representation for code that needs
-- to keep many short strings in memory, but it /should not/ be used as an
-- interchange type. That is, it should not generally be used in public APIs.
-- The 'ByteString' type is usually more suitable for use in interfaces; it is
-- more flexible and it supports a wide range of operations.
--
data ShortByteString = SBS ByteArray#
    deriving Typeable

-- | @since 0.11.2.0
instance TH.Lift ShortByteString where
#if MIN_VERSION_template_haskell(2,16,0)
  lift sbs = [| unsafePackLenLiteral |]
    `TH.appE` TH.litE (TH.integerL (fromIntegral len))
    `TH.appE` TH.litE (TH.BytesPrimL $ TH.Bytes ptr 0 (fromIntegral len))
    where
      BS ptr len = fromShort sbs
#else
  lift sbs = [| unsafePackLenLiteral |]
    `TH.appE` TH.litE (TH.integerL (fromIntegral len))
    `TH.appE` TH.litE (TH.StringPrimL $ BS.unpackBytes bs)
    where
      bs@(BS _ len) = fromShort sbs
#endif

#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- The ByteArray# representation is always word sized and aligned but with a
-- known byte length. Our representation choice for ShortByteString is to leave
-- the 0--3 trailing bytes undefined. This means we can use word-sized writes,
-- but we have to be careful with reads, see equateBytes and compareBytes below.


instance Eq ShortByteString where
    (==)    = equateBytes

instance Ord ShortByteString where
    compare = compareBytes

instance Semigroup ShortByteString where
    (<>)    = append

instance Monoid ShortByteString where
    mempty  = empty
    mappend = (<>)
    mconcat = concat

instance NFData ShortByteString where
    rnf SBS{} = ()

instance Show ShortByteString where
    showsPrec p ps r = showsPrec p (unpackChars ps) r

instance Read ShortByteString where
    readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

-- | @since 0.10.12.0
instance GHC.Exts.IsList ShortByteString where
  type Item ShortByteString = Word8
  fromList = packBytes
  toList   = unpackBytes

-- | Beware: 'fromString' truncates multi-byte characters to octets.
-- e.g. "枯朶に烏のとまりけり秋の暮" becomes �6k�nh~�Q��n�
instance IsString ShortByteString where
    fromString = packChars

instance Data ShortByteString where
  gfoldl f z txt = z packBytes `f` unpackBytes txt
  toConstr _     = error "Data.ByteString.Short.ShortByteString.toConstr"
  gunfold _ _    = error "Data.ByteString.Short.ShortByteString.gunfold"
  dataTypeOf _   = mkNoRepType "Data.ByteString.Short.ShortByteString"

------------------------------------------------------------------------
-- Simple operations

-- | /O(1)/. The empty 'ShortByteString'.
empty :: ShortByteString
empty = create 0 (\_ -> return ())

-- | /O(1)/ The length of a 'ShortByteString'.
length :: ShortByteString -> Int
length (SBS barr#) = I# (sizeofByteArray# barr#)

-- | /O(1)/ Test whether a 'ShortByteString' is empty.
null :: ShortByteString -> Bool
null sbs = length sbs == 0

-- | /O(1)/ 'ShortByteString' index (subscript) operator, starting from 0.
index :: HasCallStack => ShortByteString -> Int -> Word8
index sbs i
  | i >= 0 && i < length sbs = unsafeIndex sbs i
  | otherwise                = indexError sbs i

-- | /O(1)/ 'ShortByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ShortByteString -> Int -> Maybe Word8
indexMaybe sbs i
  | i >= 0 && i < length sbs = Just $! unsafeIndex sbs i
  | otherwise                = Nothing
{-# INLINE indexMaybe #-}

-- | /O(1)/ 'ShortByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ShortByteString -> Int -> Maybe Word8
(!?) = indexMaybe
{-# INLINE (!?) #-}

unsafeIndex :: ShortByteString -> Int -> Word8
unsafeIndex sbs = indexWord8Array (asBA sbs)

indexError :: HasCallStack => ShortByteString -> Int -> a
indexError sbs i =
  moduleError "index" $ "error in array index: " ++ show i
                        ++ " not in range [0.." ++ show (length sbs) ++ "]"

-- | @since 0.11.2.0
unsafePackLenLiteral :: Int -> Addr# -> ShortByteString
unsafePackLenLiteral len addr# =
    accursedUnutterablePerformIO $ createFromPtr (Ptr addr#) len

------------------------------------------------------------------------
-- Internal utils

asBA :: ShortByteString -> BA
asBA (SBS ba#) = BA# ba#

create :: Int -> (forall s. MBA s -> ST s ()) -> ShortByteString
create len fill =
    runST $ do
      mba <- newByteArray len
      fill mba
      BA# ba# <- unsafeFreezeByteArray mba
      return (SBS ba#)
{-# INLINE create #-}

-- | Given the maximum size needed and a function to make the contents
-- of a ShortByteString, createAndTrim makes the 'ShortByteString'.
-- The generating function is required to return the actual final size
-- (<= the maximum size) and the result value. The resulting byte array
-- is realloced to this size.
createAndTrim :: Int -> (forall s. MBA s -> ST s (Int, a)) -> (ShortByteString, a)
createAndTrim l fill =
    runST $ do
      mba <- newByteArray l
      (l', res) <- fill mba
      if assert (l' <= l) $ l' >= l
          then do
            BA# ba# <- unsafeFreezeByteArray mba
            return (SBS ba#, res)
          else do
            mba2 <- newByteArray l'
            copyMutableByteArray mba 0 mba2 0 l'
            BA# ba# <- unsafeFreezeByteArray mba2
            return (SBS ba#, res)
{-# INLINE createAndTrim #-}

------------------------------------------------------------------------
-- Conversion to and from ByteString

-- | /O(n)/. Convert a 'ByteString' into a 'ShortByteString'.
--
-- This makes a copy, so does not retain the input string.
--
toShort :: ByteString -> ShortByteString
toShort !bs = unsafeDupablePerformIO (toShortIO bs)

toShortIO :: ByteString -> IO ShortByteString
toShortIO (BS fptr len) = do
    mba <- stToIO (newByteArray len)
    let ptr = unsafeForeignPtrToPtr fptr
    stToIO (copyAddrToByteArray ptr mba 0 len)
    touchForeignPtr fptr
    BA# ba# <- stToIO (unsafeFreezeByteArray mba)
    return (SBS ba#)


-- | /O(n)/. Convert a 'ShortByteString' into a 'ByteString'.
--
fromShort :: ShortByteString -> ByteString
#if MIN_VERSION_base(4,10,0)
fromShort (SBS ba#)
  | isTrue# (isByteArrayPinned# ba#) = BS fp len
  where
    addr# = byteArrayContents# ba#
    fp = ForeignPtr addr# (PlainPtr (unsafeCoerce# ba#))
    len = I# (sizeofByteArray# ba#)
#endif
fromShort !sbs = unsafeDupablePerformIO (fromShortIO sbs)

fromShortIO :: ShortByteString -> IO ByteString
fromShortIO sbs = do
    let len = length sbs
    mba@(MBA# mba#) <- stToIO (newPinnedByteArray len)
    stToIO (copyByteArray (asBA sbs) 0 mba 0 len)
    let fp = ForeignPtr (byteArrayContents# (unsafeCoerce# mba#))
                        (PlainPtr mba#)
    return (BS fp len)

-- | /O(1)/ Convert a 'Word8' into a 'ShortByteString'
--
-- @since 0.11.3.0
singleton :: Word8 -> ShortByteString
singleton = \w -> create 1 (\mba -> writeWord8Array mba 0 w)

------------------------------------------------------------------------
-- Packing and unpacking from lists

-- | /O(n)/. Convert a list into a 'ShortByteString'
pack :: [Word8] -> ShortByteString
pack = packBytes

-- | /O(n)/. Convert a 'ShortByteString' into a list.
unpack :: ShortByteString -> [Word8]
unpack = unpackBytes

packChars :: [Char] -> ShortByteString
packChars = \cs -> packLenBytes (List.length cs) (List.map BS.c2w cs)

packBytes :: [Word8] -> ShortByteString
packBytes = \ws -> packLenBytes (List.length ws) ws

packLenBytes :: Int -> [Word8] -> ShortByteString
packLenBytes len ws0 =
    create len (\mba -> go mba 0 ws0)
  where
    go :: MBA s -> Int -> [Word8] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord8Array mba i w
      go mba (i+1) ws

-- Unpacking bytestrings into lists effeciently is a tradeoff: on the one hand
-- we would like to write a tight loop that just blats the list into memory, on
-- the other hand we want it to be unpacked lazily so we don't end up with a
-- massive list data structure in memory.
--
-- Our strategy is to combine both: we will unpack lazily in reasonable sized
-- chunks, where each chunk is unpacked strictly.
--
-- unpackChars does the lazy loop, while unpackAppendBytes and
-- unpackAppendChars do the chunks strictly.

unpackChars :: ShortByteString -> [Char]
unpackChars sbs = unpackAppendCharsLazy sbs []

unpackBytes :: ShortByteString -> [Word8]
unpackBytes sbs = unpackAppendBytesLazy sbs []

-- Why 100 bytes you ask? Because on a 64bit machine the list we allocate
-- takes just shy of 4k which seems like a reasonable amount.
-- (5 words per list element, 8 bytes per word, 100 elements = 4000 bytes)

unpackAppendCharsLazy :: ShortByteString -> [Char] -> [Char]
unpackAppendCharsLazy sbs = go 0 (length sbs)
  where
    sz = 100

    go off len cs
      | len <= sz = unpackAppendCharsStrict sbs off len cs
      | otherwise = unpackAppendCharsStrict sbs off sz  remainder
                      where remainder = go (off+sz) (len-sz) cs

unpackAppendBytesLazy :: ShortByteString -> [Word8] -> [Word8]
unpackAppendBytesLazy sbs = go 0 (length sbs)
  where
    sz = 100

    go off len ws
      | len <= sz = unpackAppendBytesStrict sbs off len ws
      | otherwise = unpackAppendBytesStrict sbs off sz  remainder
                      where remainder = go (off+sz) (len-sz) ws

-- For these unpack functions, since we're unpacking the whole list strictly we
-- build up the result list in an accumulator. This means we have to build up
-- the list starting at the end. So our traversal starts at the end of the
-- buffer and loops down until we hit the sentinal:

unpackAppendCharsStrict :: ShortByteString -> Int -> Int -> [Char] -> [Char]
unpackAppendCharsStrict !sbs off len = go (off-1) (off-1 + len)
  where
    go !sentinal !i !acc
      | i == sentinal = acc
      | otherwise     = let !c = indexCharArray (asBA sbs) i
                        in go sentinal (i-1) (c:acc)

unpackAppendBytesStrict :: ShortByteString -> Int -> Int -> [Word8] -> [Word8]
unpackAppendBytesStrict !sbs off len = go (off-1) (off-1 + len)
  where
    go !sentinal !i !acc
      | i == sentinal = acc
      | otherwise     = let !w = indexWord8Array (asBA sbs) i
                         in go sentinal (i-1) (w:acc)


------------------------------------------------------------------------
-- Eq and Ord implementations

equateBytes :: ShortByteString -> ShortByteString -> Bool
equateBytes sbs1 sbs2 =
    let !len1 = length sbs1
        !len2 = length sbs2
     in len1 == len2
     && 0 == compareByteArrays (asBA sbs1) (asBA sbs2) len1

compareBytes :: ShortByteString -> ShortByteString -> Ordering
compareBytes sbs1 sbs2 =
    let !len1 = length sbs1
        !len2 = length sbs2
        !len  = min len1 len2
     in case compareByteArrays (asBA sbs1) (asBA sbs2) len of
          i | i    < 0    -> LT
            | i    > 0    -> GT
            | len2 > len1 -> LT
            | len2 < len1 -> GT
            | otherwise   -> EQ


------------------------------------------------------------------------
-- Appending and concatenation

append :: ShortByteString -> ShortByteString -> ShortByteString
append src1 src2 =
  let !len1 = length src1
      !len2 = length src2
   in create (len1 + len2) $ \dst -> do
        copyByteArray (asBA src1) 0 dst 0    len1
        copyByteArray (asBA src2) 0 dst len1 len2

concat :: [ShortByteString] -> ShortByteString
concat = \sbss ->
    create (totalLen 0 sbss) (\dst -> copy dst 0 sbss)
  where
    totalLen !acc []          = acc
    totalLen !acc (sbs: sbss) = totalLen (acc + length sbs) sbss

    copy :: MBA s -> Int -> [ShortByteString] -> ST s ()
    copy !_   !_   []                           = return ()
    copy !dst !off (src : sbss) = do
      let !len = length src
      copyByteArray (asBA src) 0 dst off len
      copy dst (off + len) sbss

-- ---------------------------------------------------------------------
-- Basic interface

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ Append a byte to the end of a 'ShortByteString'
-- 
-- Note: copies the entire byte array
--
-- @since 0.11.3.0
snoc :: ShortByteString -> Word8 -> ShortByteString
snoc = \sbs c -> let l = length sbs
                     nl = l + 1
  in create nl $ \mba -> do
      copyByteArray (asBA sbs) 0 mba 0 l
      writeWord8Array mba l c

-- | /O(n)/ 'cons' is analogous to (:) for lists.
--
-- Note: copies the entire byte array
--
-- @since 0.11.3.0
cons :: Word8 -> ShortByteString -> ShortByteString
cons c = \sbs -> let l = length sbs
                     nl = l + 1
  in create nl $ \mba -> do
      writeWord8Array mba 0 c
      copyByteArray (asBA sbs) 0 mba 1 l

-- | /O(1)/ Extract the last element of a ShortByteString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
-- @since 0.11.3.0
last :: HasCallStack => ShortByteString -> Word8
last = \sbs -> case null sbs of
  True -> errorEmptySBS "last"
  False -> indexWord8Array (asBA sbs) (length sbs - 1)

-- | /O(n)/ Extract the elements after the head of a ShortByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- This is a partial function, consider using 'uncons' instead.
--
-- Note: copies the entire byte array
--
-- @since 0.11.3.0
tail :: HasCallStack => ShortByteString -> ShortByteString
tail = \sbs -> 
  let l = length sbs
      nl = l - 1
  in case null sbs of
      True -> errorEmptySBS "tail"
      False -> create nl $ \mba -> copyByteArray (asBA sbs) 1 mba 0 nl

-- | /O(n)/ Extract the head and tail of a ByteString, returning Nothing
-- if it is empty.
--
-- @since 0.11.3.0
uncons :: ShortByteString -> Maybe (Word8, ShortByteString)
uncons = \sbs ->
  let l = length sbs
      nl = l - 1
  in if | l <= 0 -> Nothing
        | otherwise -> let h = indexWord8Array (asBA sbs) 0
                           t = create nl $ \mba -> copyByteArray (asBA sbs) 1 mba 0 nl
                       in Just (h, t)

-- | /O(1)/ Extract the first element of a ShortByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- This is a partial function, consider using 'uncons' instead.
--
-- @since 0.11.3.0
head :: HasCallStack => ShortByteString -> Word8
head = \sbs -> case null sbs of
  True -> errorEmptySBS "head"
  False -> indexWord8Array (asBA sbs) 0

-- | /O(n)/ Return all the elements of a 'ShortByteString' except the last one.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- This is a partial function, consider using 'unsnoc' instead.
--
-- Note: copies the entire byte array
--
-- @since 0.11.3.0
init :: HasCallStack => ShortByteString -> ShortByteString
init = \sbs ->
  let l = length sbs
      nl = l - 1
  in case null sbs of
      True -> errorEmptySBS "init"
      False -> create nl $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl

-- | /O(n)/ Extract the 'init' and 'last' of a ByteString, returning Nothing
-- if it is empty.
--
-- @since 0.11.3.0
unsnoc :: ShortByteString -> Maybe (ShortByteString, Word8)
unsnoc = \sbs ->
  let l = length sbs
      nl = l - 1
  in if | l <= 0 -> Nothing
        | otherwise -> let l' = indexWord8Array (asBA sbs) (l - 1)
                           i = create nl $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl
                       in Just (i, l')


-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ShortByteString obtained by applying @f@ to each
-- element of @xs@.
--
-- @since 0.11.3.0
map :: (Word8 -> Word8) -> ShortByteString -> ShortByteString
map f = \sbs ->
    let l = length sbs
        ba = asBA sbs
    in create l (\mba -> go ba mba 0 l)
  where
    go :: BA -> MBA s -> Int -> Int -> ST s ()
    go !ba !mba !i !l
      | i >= l = return ()
      | otherwise = do
          let w = indexWord8Array ba i
          writeWord8Array mba i (f w)
          go ba mba (i+1) l


-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
--
-- @since 0.11.3.0
reverse :: ShortByteString -> ShortByteString
reverse = \sbs ->
    let l = length sbs
        ba = asBA sbs
-- https://gitlab.haskell.org/ghc/ghc/-/issues/21015
#if MIN_VERSION_base(4,12,0) && defined(SAFE_UNALIGNED)
    in create l (\mba -> go ba mba l)
  where
    go :: forall s. BA -> MBA s -> Int -> ST s ()
    go !ba !mba !l = do
      -- this is equivalent to: (q, r) = l `quotRem` 8 
      let q = l `shiftR` 3
          r = l .&. 7
      i' <- goWord8Chunk 0 r
      goWord64Chunk i' 0 q
     where

      goWord64Chunk :: Int -> Int -> Int -> ST s ()
      goWord64Chunk !off !i' !cl = loop i'
       where
        loop :: Int -> ST s ()
        loop !i
          | i >= cl = return ()
          | otherwise = do
              let w = indexWord8ArrayAsWord64 ba (off + (i * 8))
              writeWord64Array mba (cl - 1 - i) (byteSwap64 w)
              loop (i+1)

      goWord8Chunk :: Int -> Int -> ST s Int
      goWord8Chunk !i' !cl = loop i'
       where
        loop :: Int -> ST s Int
        loop !i
          | i >= cl = return i
          | otherwise = do
              let w = indexWord8Array ba i
              writeWord8Array mba (l - 1 - i) w
              loop (i+1)
#else
    in create l (\mba -> go ba mba 0 l)
   where
    go :: BA -> MBA s -> Int -> Int -> ST s ()
    go !ba !mba !i !l
      | i >= l = return ()
      | otherwise = do
          let w = indexWord8Array ba i
          writeWord8Array mba (l - 1 - i) w
          go ba mba (i+1) l
#endif


-- | /O(n)/ The 'intercalate' function takes a 'ShortByteString' and a list of
-- 'ShortByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- @since 0.11.3.0
intercalate :: ShortByteString -> [ShortByteString] -> ShortByteString
intercalate sep = \case
                    [] -> empty
                    [x] -> x -- This branch exists for laziness, not speed
                    (sbs:t) -> let !totalLen = List.foldl' (\acc chunk -> acc +! length sep +! length chunk) (length sbs) t
                               in create totalLen (\mba ->
                                      let !l = length sbs
                                      in copyByteArray (asBA sbs) 0 mba 0 l >> go mba l t)
 where
  ba = asBA sep
  lba = length sep

  go :: MBA s -> Int -> [ShortByteString] -> ST s ()
  go _ _ [] = pure ()
  go mba !off (chunk:chunks) = do
    let lc = length chunk
    copyByteArray ba 0 mba off lba
    copyByteArray (asBA chunk) 0 mba (off + lba) lc
    go mba (off + lc + lba) chunks
  (+!) = checkedAdd "Short.intercalate"


-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ShortByteString, reduces the
-- ShortByteString using the binary operator, from left to right.
--
-- @since 0.11.3.0
foldl :: (a -> Word8 -> a) -> a -> ShortByteString -> a
foldl f v = List.foldl f v . unpack

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
-- @since 0.11.3.0
foldl' :: (a -> Word8 -> a) -> a -> ShortByteString -> a
foldl' f v = List.foldl' f v . unpack

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ShortByteString,
-- reduces the ShortByteString using the binary operator, from right to left.
--
-- @since 0.11.3.0
foldr :: (Word8 -> a -> a) -> a -> ShortByteString -> a
foldr f v = List.foldr f v . unpack

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
--
-- @since 0.11.3.0
foldr' :: (Word8 -> a -> a) -> a -> ShortByteString -> a
foldr' k v = Foldable.foldr' k v . unpack

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ShortByteString's.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- @since 0.11.3.0
foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldl1 k = List.foldl1 k . unpack

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- @since 0.11.3.0
foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldl1' k = List.foldl1' k . unpack

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ShortByteString's
-- An exception will be thrown in the case of an empty ShortByteString.
--
-- @since 0.11.3.0
foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldr1 k = List.foldr1 k . unpack

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
--
-- @since 0.11.3.0
foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ShortByteString -> Word8
foldr1' k = \sbs -> if null sbs then errorEmptySBS "foldr1'" else foldr' k (last sbs) (init sbs)



-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Applied to a predicate and a 'ShortByteString', 'all' determines
-- if all elements of the 'ShortByteString' satisfy the predicate.
--
-- @since 0.11.3.0
all :: (Word8 -> Bool) -> ShortByteString -> Bool
all k = \sbs ->
  let l = length sbs
      ba = asBA sbs
      w = indexWord8Array ba
      go !n | n >= l    = True
            | otherwise = k (w n) && go (n + 1)
  in go 0


-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
--
-- @since 0.11.3.0
any :: (Word8 -> Bool) -> ShortByteString -> Bool
any k = \sbs ->
  let l = length sbs
      ba = asBA sbs
      w = indexWord8Array ba
      go !n | n >= l    = False
            | otherwise = k (w n) || go (n + 1)
  in go 0



-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n)/ 'take' @n@, applied to a ShortByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
--
-- Note: copies the entire byte array
--
-- @since 0.11.3.0
take :: Int -> ShortByteString -> ShortByteString
take = \n -> \sbs -> let sl = length sbs
                     in if | n >= sl -> sbs
                           | n <= 0  -> empty
                           | otherwise ->
                               let len = min (length sbs) (max 0 n)
                               in create len $ \mba -> copyByteArray (asBA sbs) 0 mba 0 len

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
--
-- @since 0.11.3.0
takeWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
takeWhile f = \sbs -> take (findIndexOrLength (not . f) sbs) sbs

-- | /O(n)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
-- >>> takeEnd 0 "abcdefg"
-- ""
-- >>> takeEnd 4 "abc"
-- "abc"
--
-- @since 0.11.3.0
takeEnd :: Int -> ShortByteString -> ShortByteString
takeEnd n = \sbs -> let sl = length sbs
                    in if | n >= sl   -> sbs
                          | n <= 0    -> empty
                          | otherwise -> create n $ \mba -> copyByteArray (asBA sbs) (max 0 (sl - n)) mba 0 n


-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- @since 0.11.3.0
takeWhileEnd :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
takeWhileEnd f = \sbs -> drop (findFromEndUntil (not . f) sbs) sbs

-- | /O(n)/ 'drop' @n@ @xs@ returns the suffix of @xs@ after the first n elements, or @[]@ if @n > 'length' xs@.
--
-- Note: copies the entire byte array
--
-- @since 0.11.3.0
drop :: Int -> ShortByteString -> ShortByteString
drop = \n -> \sbs ->
  let len = length sbs
      newLen = max 0 (len - max 0 n)
  in if | n <= 0    -> sbs
        | n >= len  -> empty
        | otherwise -> create newLen $ \mba -> copyByteArray (asBA sbs) n mba 0 (len - n)

-- | /O(n)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
--
-- @since 0.11.3.0
dropEnd :: Int -> ShortByteString -> ShortByteString
dropEnd n = \sbs -> let sl = length sbs
                        nl = sl - n
                    in if | n >= sl   -> empty
                          | n <= 0    -> sbs
                          | otherwise -> create nl $ \mba -> copyByteArray (asBA sbs) 0 mba 0 nl

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
--
-- Note: copies the entire byte array
--
-- @since 0.11.3.0
dropWhile :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhile f = \sbs -> drop (findIndexOrLength (not . f) sbs) sbs

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.11.3.0
dropWhileEnd :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
dropWhileEnd f = \sbs -> take (findFromEndUntil (not . f) sbs) sbs

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('takeWhileEnd' (not . p) &&& 'dropWhileEnd' (not . p))@.
--
-- @since 0.11.3.0
breakEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
breakEnd p = \sbs -> splitAt (findFromEndUntil p sbs) sbs

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
-- @since 0.11.3.0
break :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
break = \p -> \sbs -> case findIndexOrLength p sbs of n -> (take n sbs, drop n sbs)

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
-- @since 0.11.3.0
span :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
span p = break (not . p)

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
-- > spanEnd (not . isSpace) sbs
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse sbs) in (reverse y, reverse x)
--
-- @since 0.11.3.0
spanEnd :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
spanEnd p = \sbs -> splitAt (findFromEndUntil (not . p) sbs) sbs

-- | /O(n)/ 'splitAt' @n sbs@ is equivalent to @('take' n sbs, 'drop' n sbs)@.
--
-- Note: copies the substrings
--
-- @since 0.11.3.0
splitAt :: Int -> ShortByteString -> (ShortByteString, ShortByteString)
splitAt n = \sbs -> if
  | n <= 0 -> (empty, sbs)
  | otherwise ->
      let slen = length sbs
      in if | n >= length sbs -> (sbs, empty)
            | otherwise -> 
                let llen = min slen (max 0 n)
                    rlen = max 0 (slen - max 0 n)
                    lsbs = create llen $ \mba -> copyByteArray (asBA sbs) 0 mba 0 llen
                    rsbs = create rlen $ \mba -> copyByteArray (asBA sbs) n mba 0 rlen
                in (lsbs, rsbs)

-- | /O(n)/ Break a 'ShortByteString' into pieces separated by the byte
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
-- Note: copies the substrings
--
-- @since 0.11.3.0
split :: Word8 -> ShortByteString -> [ShortByteString]
split w = splitWith (== w)


-- | /O(n)/ Splits a 'ShortByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
-- @since 0.11.3.0
splitWith :: (Word8 -> Bool) -> ShortByteString -> [ShortByteString]
splitWith p = \sbs -> if
  | null sbs -> []
  | otherwise -> go sbs
  where
    go sbs'
      | null sbs' = [empty]
      | otherwise =
          case break p sbs' of
            (a, b)
              | null b -> [a]
              | otherwise -> a : go (tail b)


-- | /O(n)/ The 'stripSuffix' function takes two ShortByteStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
--
-- @since 0.11.3.0
stripSuffix :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripSuffix sbs1 = \sbs2 -> do
  let l1 = length sbs1
      l2 = length sbs2
  if | isSuffixOf sbs1 sbs2 -> Just $! create (l2 - l1) $ \dst -> do
                                 copyByteArray (asBA sbs2) 0 dst 0 (l2 - l1)
     | otherwise -> Nothing

-- | /O(n)/ The 'stripPrefix' function takes two ShortByteStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 0.11.3.0
stripPrefix :: ShortByteString -> ShortByteString -> Maybe ShortByteString
stripPrefix sbs1 = \sbs2 -> do
  let l1 = length sbs1
      l2 = length sbs2
  if | isPrefixOf sbs1 sbs2 -> Just $! create (l2 - l1) $ \dst -> do
                                 copyByteArray (asBA sbs2) l1 dst 0 (l2 - l1)
     | otherwise -> Nothing


-- ---------------------------------------------------------------------
-- Unfolds and replicates


-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = unfoldr w (\u -> Just (u,u)) c
--
-- @since 0.11.3.0
replicate :: Int -> Word8 -> ShortByteString
replicate w c
    | w <= 0    = empty
    | otherwise = create w (\mba -> setByteArray mba 0 w (fromIntegral c))


-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ShortByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ShortByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- This function is not efficient/safe. It will build a list of @[Word8]@
-- and run the generator until it returns `Nothing`, otherwise recurse infinitely,
-- then finally create a 'ShortByteString'.
--
-- If you know the maximum length, consider using 'unfoldrN'.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
-- @since 0.11.3.0
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ShortByteString
unfoldr f = \x0 -> packBytesRev $ go x0 []
 where
   go x words' = case f x of
                    Nothing -> words'
                    Just (w, x') -> go x' (w:words')

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ShortByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
-- @since 0.11.3.0
unfoldrN :: forall a. Int -> (a -> Maybe (Word8, a)) -> a -> (ShortByteString, Maybe a)
unfoldrN i f = \x0 ->
  if | i < 0     -> (empty, Just x0)
     | otherwise -> createAndTrim i $ \mba -> go mba x0 0

  where
    go :: forall s. MBA s -> a -> Int -> ST s (Int, Maybe a)
    go !mba !x !n = go' x n
      where
        go' :: a -> Int -> ST s (Int, Maybe a)
        go' !x' !n'
          | n' == i    = return (n', Just x')
          | otherwise = case f x' of
                          Nothing       -> return (n', Nothing)
                          Just (w, x'') -> do
                                             writeWord8Array mba n' w
                                             go' x'' (n'+1)



-- --------------------------------------------------------------------
-- Predicates

-- | Check whether one string is a substring of another.
--
-- @since 0.11.3.0
isInfixOf :: ShortByteString -> ShortByteString -> Bool
isInfixOf sbs = \s -> null sbs || not (null $ snd $ (GHC.Exts.inline breakSubstring) sbs s)

-- |/O(n)/ The 'isPrefixOf' function takes two ShortByteStrings and returns 'True'
--
-- @since 0.11.3.0
isPrefixOf :: ShortByteString -> ShortByteString -> Bool
isPrefixOf sbs1 = \sbs2 -> do
  let l1 = length sbs1
      l2 = length sbs2
  if | l1 == 0   -> True
     | l2 < l1   -> False
     | otherwise ->
         let i = compareByteArraysOff (asBA sbs1) 0 (asBA sbs2) 0 l1
         in i == 0

-- | /O(n)/ The 'isSuffixOf' function takes two ShortByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- @since 0.11.3.0
isSuffixOf :: ShortByteString -> ShortByteString -> Bool
isSuffixOf sbs1 = \sbs2 -> do
  let l1 = length sbs1
      l2 = length sbs2
  if | l1 == 0   -> True
     | l2 < l1   -> False
     | otherwise ->
         let i = compareByteArraysOff (asBA sbs1) 0 (asBA sbs2) (l2 - l1) l1
         in i == 0

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
-- To skip to the first occurence of a string:
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
-- @since 0.11.3.0
breakSubstring :: ShortByteString -- ^ String to search for
               -> ShortByteString -- ^ String to search in
               -> (ShortByteString, ShortByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat =
  case lp of
    0 -> (empty,)
    1 -> breakByte (head pat)
    _ -> if lp * 8 <= finiteBitSize (0 :: Word)
             then shift
             else karpRabin
  where
    lp = length pat
    karpRabin :: ShortByteString -> (ShortByteString, ShortByteString)
    karpRabin src
        | length src < lp = (src,empty)
        | otherwise = search (rollingHash $ take lp src) lp
      where
        k           = 2891336453 :: Word32
        rollingHash = foldl' (\h b -> h * k + fromIntegral b) 0
        hp          = rollingHash pat
        m           = k ^ lp
        get = fromIntegral . unsafeIndex src
        search !hs !i
            | hp == hs && pat == take lp b = u
            | length src <= i           = (src, empty) -- not found
            | otherwise                    = search hs' (i + 1)
          where
            u@(_, b) = splitAt (i - lp) src
            hs' = hs * k +
                  get i -
                  m * get (i - lp)
    {-# INLINE karpRabin #-}

    shift :: ShortByteString -> (ShortByteString, ShortByteString)
    shift !src
        | length src < lp = (src, empty)
        | otherwise       = search (intoWord $ take lp src) lp
      where
        intoWord :: ShortByteString -> Word
        intoWord = foldl' (\w b -> (w `shiftL` 8) .|. fromIntegral b) 0
        wp   = intoWord pat
        mask' = (1 `shiftL` (8 * lp)) - 1
        search !w !i
            | w == wp            = splitAt (i - lp) src
            | length src <= i = (src, empty)
            | otherwise       = search w' (i + 1)
          where
            b  = fromIntegral (unsafeIndex src i)
            w' = mask' .&. ((w `shiftL` 8) .|. b)
    {-# INLINE shift #-}
{-# INLINABLE breakSubstring #-}


-- --------------------------------------------------------------------
-- Searching ShortByteString

-- | /O(n)/ 'elem' is the 'ShortByteString' membership predicate.
--
-- @since 0.11.3.0
elem :: Word8 -> ShortByteString -> Bool
elem c = \sbs -> case elemIndex c sbs of Nothing -> False ; _ -> True

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
--
-- @since 0.11.3.0
filter :: (Word8 -> Bool) -> ShortByteString -> ShortByteString
filter k = \sbs -> if
    | null sbs  -> sbs
    | otherwise -> pack . List.filter k . unpack $ sbs

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
-- @since 0.11.3.0
find :: (Word8 -> Bool) -> ShortByteString -> Maybe Word8
find f = \sbs -> case findIndex f sbs of
                    Just n -> Just (sbs `index` n)
                    _      -> Nothing

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p sbs, filter (not . p) sbs)
--
-- @since 0.11.3.0
partition :: (Word8 -> Bool) -> ShortByteString -> (ShortByteString, ShortByteString)
partition f = \sbs -> if
    | null sbs  -> (sbs, sbs)
    | otherwise -> bimap pack pack . List.partition f . unpack $ sbs


-- --------------------------------------------------------------------
-- Indexing ShortByteString

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ShortByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
--
-- @since 0.11.3.0
elemIndex :: Word8 -> ShortByteString -> Maybe Int
elemIndex c = \sbs@(SBS ba#) -> fromIntegral <$> do
    let l = length sbs
    accursedUnutterablePerformIO $ do
      !s <- c_elem_index ba# c (fromIntegral l)
      return $! if s < 0 then Nothing else Just s


-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
--
-- @since 0.11.3.0
elemIndices :: Word8 -> ShortByteString -> [Int]
elemIndices k = findIndices (==k)

-- | count returns the number of times its argument appears in the ShortByteString
--
-- @since 0.11.3.0
count :: Word8 -> ShortByteString -> Int
count w = \sbs@(SBS ba#) -> accursedUnutterablePerformIO $
    fromIntegral <$> c_count ba# (fromIntegral $ length sbs) w

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'ShortByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
--
-- @since 0.11.3.0
findIndex :: (Word8 -> Bool) -> ShortByteString -> Maybe Int
findIndex k = \sbs ->
  let l = length sbs
      ba = asBA sbs
      w = indexWord8Array ba
      go !n | n >= l    = Nothing
            | k (w n)   = Just n
            | otherwise = go (n + 1)
  in go 0


-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
--
-- @since 0.11.3.0
findIndices :: (Word8 -> Bool) -> ShortByteString -> [Int]
findIndices k = \sbs ->
  let l = length sbs
      ba = asBA sbs
      w = indexWord8Array ba
      go !n | n >= l    = []
            | k (w n)   = n : go (n + 1)
            | otherwise = go (n + 1)
  in go 0



------------------------------------------------------------------------
-- Exported low level operations

copyToPtr :: ShortByteString  -- ^ source data
          -> Int              -- ^ offset into source
          -> Ptr a            -- ^ destination
          -> Int              -- ^ number of bytes to copy
          -> IO ()
copyToPtr src off dst len =
    stToIO $
      copyByteArrayToAddr (asBA src) off dst len

createFromPtr :: Ptr a   -- ^ source data
              -> Int     -- ^ number of bytes to copy
              -> IO ShortByteString
createFromPtr !ptr len =
    stToIO $ do
      mba <- newByteArray len
      copyAddrToByteArray ptr mba 0 len
      BA# ba# <- unsafeFreezeByteArray mba
      return (SBS ba#)


------------------------------------------------------------------------
-- Primop wrappers

data BA    = BA# ByteArray#
data MBA s = MBA# (MutableByteArray# s)

indexCharArray :: BA -> Int -> Char
indexCharArray (BA# ba#) (I# i#) = C# (indexCharArray# ba# i#)

indexWord8Array :: BA -> Int -> Word8
indexWord8Array (BA# ba#) (I# i#) = W8# (indexWord8Array# ba# i#)

#if MIN_VERSION_base(4,12,0) && defined(SAFE_UNALIGNED)
indexWord8ArrayAsWord64 :: BA -> Int -> Word64
indexWord8ArrayAsWord64 (BA# ba#) (I# i#) = W64# (indexWord8ArrayAsWord64# ba# i#)
#endif

newByteArray :: Int -> ST s (MBA s)
newByteArray (I# len#) =
    ST $ \s -> case newByteArray# len# s of
                 (# s, mba# #) -> (# s, MBA# mba# #)

newPinnedByteArray :: Int -> ST s (MBA s)
newPinnedByteArray (I# len#) =
    ST $ \s -> case newPinnedByteArray# len# s of
                 (# s, mba# #) -> (# s, MBA# mba# #)

unsafeFreezeByteArray :: MBA s -> ST s BA
unsafeFreezeByteArray (MBA# mba#) =
    ST $ \s -> case unsafeFreezeByteArray# mba# s of
                 (# s, ba# #) -> (# s, BA# ba# #)

writeWord8Array :: MBA s -> Int -> Word8 -> ST s ()
writeWord8Array (MBA# mba#) (I# i#) (W8# w#) =
  ST $ \s -> case writeWord8Array# mba# i# w# s of
               s -> (# s, () #)

#if MIN_VERSION_base(4,12,0) && defined(SAFE_UNALIGNED)
writeWord64Array :: MBA s -> Int -> Word64 -> ST s ()
writeWord64Array (MBA# mba#) (I# i#) (W64# w#) =
  ST $ \s -> case writeWord64Array# mba# i# w# s of
               s -> (# s, () #)
#endif

copyAddrToByteArray :: Ptr a -> MBA RealWorld -> Int -> Int -> ST RealWorld ()
copyAddrToByteArray (Ptr src#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyAddrToByteArray# src# dst# dst_off# len# s of
                 s -> (# s, () #)

copyByteArrayToAddr :: BA -> Int -> Ptr a -> Int -> ST RealWorld ()
copyByteArrayToAddr (BA# src#) (I# src_off#) (Ptr dst#) (I# len#) =
    ST $ \s -> case copyByteArrayToAddr# src# src_off# dst# len# s of
                 s -> (# s, () #)

copyByteArray :: BA -> Int -> MBA s -> Int -> Int -> ST s ()
copyByteArray (BA# src#) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyByteArray# src# src_off# dst# dst_off# len# s of
                 s -> (# s, () #)

setByteArray :: MBA s -> Int -> Int -> Int -> ST s ()
setByteArray (MBA# dst#) (I# off#) (I# len#) (I# c#) =
    ST $ \s -> case setByteArray# dst# off# len# c# s of
                 s -> (# s, () #)

copyMutableByteArray :: MBA s -> Int -> MBA s -> Int -> Int -> ST s ()
copyMutableByteArray (MBA# src#) (I# src_off#) (MBA# dst#) (I# dst_off#) (I# len#) =
    ST $ \s -> case copyMutableByteArray# src# src_off# dst# dst_off# len# s of
                 s -> (# s, () #)


------------------------------------------------------------------------
-- FFI imports
--
compareByteArrays :: BA -> BA -> Int -> Int
compareByteArrays ba1 ba2 = compareByteArraysOff ba1 0 ba2 0

compareByteArraysOff :: BA  -- ^ array 1
                     -> Int -- ^ offset for array 1
                     -> BA  -- ^ array 2
                     -> Int -- ^ offset for array 2
                     -> Int -- ^ length to compare
                     -> Int -- ^ like memcmp
#if MIN_VERSION_base(4,11,0)
compareByteArraysOff (BA# ba1#) (I# ba1off#) (BA# ba2#) (I# ba2off#) (I# len#) =
  I# (compareByteArrays#  ba1# ba1off# ba2# ba2off# len#)
#else
compareByteArraysOff (BA# ba1#) ba1off (BA# ba2#) ba2off len =
  assert (ba1off + len <= (I# (sizeofByteArray# ba1#)))
  $ assert (ba2off + len <= (I# (sizeofByteArray# ba2#)))
  $ fromIntegral $ accursedUnutterablePerformIO $
    c_memcmp_ByteArray ba1#
                       ba1off
                       ba2#
                       ba2off
                       (fromIntegral len)
  

foreign import ccall unsafe "static sbs_memcmp_off"
  c_memcmp_ByteArray :: ByteArray# -> Int -> ByteArray# -> Int -> CSize -> IO CInt
#endif

foreign import ccall unsafe "static sbs_elem_index"
    c_elem_index :: ByteArray# -> Word8 -> CSize -> IO CPtrdiff

foreign import ccall unsafe "static fpstring.h fps_count" c_count
    :: ByteArray# -> CSize -> Word8 -> IO CSize


------------------------------------------------------------------------
-- Primop replacements

copyAddrToByteArray# :: Addr#
                     -> MutableByteArray# RealWorld -> Int#
                     -> Int#
                     -> State# RealWorld -> State# RealWorld

copyByteArrayToAddr# :: ByteArray# -> Int#
                     -> Addr#
                     -> Int#
                     -> State# RealWorld -> State# RealWorld

copyByteArray#       :: ByteArray# -> Int#
                     -> MutableByteArray# s -> Int#
                     -> Int#
                     -> State# s -> State# s

copyAddrToByteArray# = GHC.Exts.copyAddrToByteArray#
copyByteArrayToAddr# = GHC.Exts.copyByteArrayToAddr#
copyByteArray# = GHC.Exts.copyByteArray#

-- | /O(n)./ Construct a new @ShortByteString@ from a @CString@. The
-- resulting @ShortByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
--
-- @since 0.10.10.0
packCString :: CString -> IO ShortByteString
packCString cstr = do
  len <- BS.c_strlen cstr
  packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ShortByteString@ from a @CStringLen@. The
-- resulting @ShortByteString@ is an immutable copy of the original @CStringLen@.
-- The @ShortByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
--
-- @since 0.10.10.0
packCStringLen :: CStringLen -> IO ShortByteString
packCStringLen (cstr, len) | len >= 0 = createFromPtr cstr len
packCStringLen (_, len) =
  moduleErrorIO "packCStringLen" ("negative length: " ++ show len)

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
--
-- @since 0.10.10.0
useAsCString :: ShortByteString -> (CString -> IO a) -> IO a
useAsCString sbs action =
  allocaBytes (l+1) $ \buf -> do
      copyToPtr sbs 0 buf (fromIntegral l)
      pokeByteOff buf l (0::Word8)
      action buf
  where l = length sbs

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CStringLen@.
-- As for @useAsCString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen sbs action =
  allocaBytes l $ \buf -> do
      copyToPtr sbs 0 buf (fromIntegral l)
      action (buf, l)
  where l = length sbs

-- | /O(n)/ Check whether a 'ShortByteString' represents valid UTF-8.
--
-- @since 0.11.3.0
isValidUtf8 :: ShortByteString -> Bool
isValidUtf8 sbs@(SBS ba#) = accursedUnutterablePerformIO $ do
  i <- cIsValidUtf8 ba# (fromIntegral (length sbs))
  return $ i /= 0

foreign import ccall unsafe "bytestring_is_valid_utf8" cIsValidUtf8
  :: ByteArray# -> CSize -> IO CInt

-- ---------------------------------------------------------------------
-- Internal utilities

moduleErrorIO :: HasCallStack => String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString.Short." ++ fun ++ ':':' ':msg


-- Find from the end of the string using predicate.
--
-- Return '0' if the predicate returns false for the entire ShortByteString.
findFromEndUntil :: (Word8 -> Bool) -> ShortByteString -> Int
findFromEndUntil k sbs = go (length sbs - 1)
  where
    ba = asBA sbs
    go !n | n < 0     = 0
          | k (indexWord8Array ba n)   = n + 1
          | otherwise = go (n - 1)

findIndexOrLength :: (Word8 -> Bool) -> ShortByteString -> Int
findIndexOrLength k sbs = go 0
  where
    l = length sbs
    ba = asBA sbs
    go !n | n >= l    = l
          | k (indexWord8Array ba n)   = n
          | otherwise = go (n + 1)


packBytesRev :: [Word8] -> ShortByteString
packBytesRev cs = packLenBytesRev (List.length cs) cs

packLenBytesRev :: Int -> [Word8] -> ShortByteString
packLenBytesRev len ws0 =
    create len (\mba -> go mba len ws0)
  where
    go :: MBA s -> Int -> [Word8] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (w:ws) = do
      writeWord8Array mba (i - 1) w
      go mba (i - 1) ws


breakByte :: Word8 -> ShortByteString -> (ShortByteString, ShortByteString)
breakByte c sbs = case elemIndex c sbs of
    Nothing -> (sbs, empty)
    Just n  -> (take n sbs, drop n sbs)

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptySBS :: HasCallStack => String -> a
errorEmptySBS fun = moduleError fun "empty ShortByteString"
{-# NOINLINE errorEmptySBS #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

