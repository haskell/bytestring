{-# LANGUAGE DeriveDataTypeable, CPP, BangPatterns, RankNTypes,
             ForeignFunctionInterface, MagicHash, UnboxedTuples,
             UnliftedFFITypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Short.Internal
-- Copyright   : (c) Duncan Coutts 2012-2013
-- License     : BSD-style
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : stable
-- Portability : ghc only
--
-- Internal representation of ShortByteString
--
module Data.ByteString.Short.Internal (

    -- * The @ShortByteString@ type and representation
    ShortByteString(..),

    -- * Conversions
    toShort,
    fromShort,
    pack,
    unpack,

    -- * Other operations
    empty, null, length, index, indexMaybe, (!?), unsafeIndex,

    -- * Low level operations
    createFromPtr, copyToPtr,

    -- ** Encoding validation
    isValidUtf8,

    -- * Low level conversions
    -- ** Packing 'CString's and pointers
    packCString,
    packCStringLen,

    -- ** Using ByteStrings as 'CString's
    useAsCString,
    useAsCStringLen
  ) where

import Data.ByteString.Internal (ByteString(..), accursedUnutterablePerformIO)
import qualified Data.ByteString.Internal as BS

import Data.Typeable    (Typeable)
import Data.Data        (Data(..), mkNoRepType)
import Data.Semigroup   (Semigroup((<>)))
import Data.Monoid      (Monoid(..))
import Data.String      (IsString(..))
import Control.DeepSeq  (NFData(..))
import qualified Data.List as List (length)
import Foreign.C.String (CString, CStringLen)
import Foreign.C.Types  (CSize(..), CInt(..))
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
#if MIN_VERSION_base(4,10,0)
                , isByteArrayPinned#
                , isTrue#
#endif
                , sizeofByteArray#
                , indexWord8Array#, indexCharArray#
                , writeWord8Array#, writeCharArray#
                , unsafeFreezeByteArray# )
import GHC.IO
import GHC.ForeignPtr (ForeignPtr(ForeignPtr), ForeignPtrContents(PlainPtr))
import GHC.ST         (ST(ST), runST)
import GHC.Stack.Types (HasCallStack)
import GHC.Word

import Prelude ( Eq(..), Ord(..), Ordering(..), Read(..), Show(..)
               , ($), ($!), error, (++), (.)
               , String, userError
               , Bool(..), (&&), otherwise
               , (+), (-), fromIntegral
               , return
               , Maybe(..) )

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
  error $ "Data.ByteString.Short.index: error in array index; " ++ show i
       ++ " not in range [0.." ++ show (length sbs) ++ ")"

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
fromShort (SBS b#)
  | isTrue# (isByteArrayPinned# b#) = BS fp len
  where
    addr# = byteArrayContents# b#
    fp = ForeignPtr addr# (PlainPtr (unsafeCoerce# b#))
    len = I# (sizeofByteArray# b#)
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


------------------------------------------------------------------------
-- Packing and unpacking from lists

-- | /O(n)/. Convert a list into a 'ShortByteString'
pack :: [Word8] -> ShortByteString
pack = packBytes

-- | /O(n)/. Convert a 'ShortByteString' into a list.
unpack :: ShortByteString -> [Word8]
unpack = unpackBytes

packChars :: [Char] -> ShortByteString
packChars cs = packLenChars (List.length cs) cs

packBytes :: [Word8] -> ShortByteString
packBytes cs = packLenBytes (List.length cs) cs

packLenChars :: Int -> [Char] -> ShortByteString
packLenChars len cs0 =
    create len (\mba -> go mba 0 cs0)
  where
    go :: MBA s -> Int -> [Char] -> ST s ()
    go !_   !_ []     = return ()
    go !mba !i (c:cs) = do
      writeCharArray mba i c
      go mba (i+1) cs

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
unpackChars bs = unpackAppendCharsLazy bs []

unpackBytes :: ShortByteString -> [Word8]
unpackBytes bs = unpackAppendBytesLazy bs []

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
     && 0 == accursedUnutterablePerformIO
               (memcmp_ByteArray (asBA sbs1) (asBA sbs2) len1)

compareBytes :: ShortByteString -> ShortByteString -> Ordering
compareBytes sbs1 sbs2 =
    let !len1 = length sbs1
        !len2 = length sbs2
        !len  = min len1 len2
     in case accursedUnutterablePerformIO
               (memcmp_ByteArray (asBA sbs1) (asBA sbs2) len) of
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
concat sbss =
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

writeCharArray :: MBA s -> Int -> Char -> ST s ()
writeCharArray (MBA# mba#) (I# i#) (C# c#) =
  ST $ \s -> case writeCharArray# mba# i# c# s of
               s -> (# s, () #)

writeWord8Array :: MBA s -> Int -> Word8 -> ST s ()
writeWord8Array (MBA# mba#) (I# i#) (W8# w#) =
  ST $ \s -> case writeWord8Array# mba# i# w# s of
               s -> (# s, () #)

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


------------------------------------------------------------------------
-- FFI imports

memcmp_ByteArray :: BA -> BA -> Int -> IO CInt
memcmp_ByteArray (BA# ba1#) (BA# ba2#) len =
  c_memcmp_ByteArray ba1# ba2# (fromIntegral len)

foreign import ccall unsafe "string.h memcmp"
  c_memcmp_ByteArray :: ByteArray# -> ByteArray# -> CSize -> IO CInt


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
useAsCString bs action =
  allocaBytes (l+1) $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      pokeByteOff buf l (0::Word8)
      action buf
  where l = length bs

-- | /O(n) construction./ Use a @ShortByteString@ with a function requiring a @CStringLen@.
-- As for @useAsCString@ this function makes a copy of the original @ShortByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- @since 0.10.10.0
useAsCStringLen :: ShortByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen bs action =
  allocaBytes l $ \buf -> do
      copyToPtr bs 0 buf (fromIntegral l)
      action (buf, l)
  where l = length bs

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
