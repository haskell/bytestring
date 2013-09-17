{-# LANGUAGE DeriveDataTypeable, CPP, BangPatterns, RankNTypes,
             ForeignFunctionInterface, MagicHash, UnboxedTuples #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module      : Data.ByteString.Short
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
    empty, null, length, index,
    
    -- * Low level operations
    createFromPtr, copyToPtr
  ) where

import Data.ByteString.Internal (ByteString(..), inlinePerformIO)

import Data.Typeable    (Typeable)
import Data.Data        (Data(..), mkNoRepType)
import Data.Monoid      (Monoid(..))
import Control.DeepSeq  (NFData(..))
import qualified Data.List as List (length)
import Foreign.C.Types  (CSize(..), CInt(..), CLong(..))
import Data.Word

import GHC.Exts
import GHC.IO
import GHC.ForeignPtr
import GHC.ST
import GHC.Word

import Prelude ( Eq(..), Ord(..), Ordering(..), Read(..), Show(..)
               , ($), error, (++)
               , Bool(..), (&&), otherwise
               , (+), (-), fromIntegral
               , return )


-- | A compact representation of a 'Word8' vector.
--
-- It has a lower memory overhead than a 'ByteString' and and does not
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

-- The ByteArray# representation is always word sized and aligned but with a
-- known byte length. Our representation choice for ShortByteString is to leave
-- the 0--3 trailing bytes undefined. This means we can use word-sized writes,
-- but we have to be careful with reads, see equateBytes and compareBytes below.


instance Eq ShortByteString where
    (==)    = equateBytes

instance Ord ShortByteString where
    compare = compareBytes

instance Monoid ShortByteString where
    mempty  = empty
    mappend = append
    mconcat = concat

instance NFData ShortByteString

instance Show ShortByteString where
    showsPrec p ps r = showsPrec p (unpackChars ps) r

instance Read ShortByteString where
    readsPrec p str = [ (packChars x, y) | (x, y) <- readsPrec p str ]

instance IsString ShortByteString where
    fromString = packChars

instance Data ShortByteString where
  gfoldl f z txt = z packBytes `f` (unpackBytes txt)
  toConstr _     = error "Data.ByteString.Short.ShortByteString.toConstr"
  gunfold _ _    = error "Data.ByteString.Short.ShortByteString.gunfold"
#if MIN_VERSION_base(4,2,0)
  dataTypeOf _   = mkNoRepType "Data.ByteString.Short.ShortByteString"
#else
  dataTypeOf _   = mkNorepType "Data.ByteString.Short.ShortByteString"
#endif

------------------------------------------------------------------------
-- Simple operations

-- | /O(1)/. The empty 'ShortByteString'.
empty :: ShortByteString
empty = create 0# (\_ s -> s)

-- | /O(1)/ The length of a 'ShortByteString'.
length :: ShortByteString -> Int
length (SBS barr#) = I# (sizeofByteArray# barr#)

-- | /O(1)/ Test whether a 'ShortByteString' is empty.
null :: ShortByteString -> Bool
null (SBS barr#) = sizeofByteArray# barr# ==# 0#

-- | /O(1)/ 'ShortByteString' index (subscript) operator, starting from 0. 
index :: ShortByteString -> Int -> Word8
index (SBS barr#) (I# i#)
  | i# >=# 0# && i# <# sizeofByteArray# barr# = W8# (indexWord8Array# barr# i#)
  | otherwise                                 = indexError barr# i#

indexError :: ByteArray# -> Int# -> a
indexError barr# i# =
  error $ "Data.ByteString.Short.index: error in array index; " ++ show (I# i#)
       ++ " not in range [0.." ++ show (I# (sizeofByteArray# barr#)) ++ ")"
{-# NOINLINE indexError #-}

------------------------------------------------------------------------
-- Internal utils

create :: Int# -> (forall s. MutableByteArray# s -> State# s -> State# s) -> ShortByteString
create len# fill =
    runST (ST (\s ->
      case newByteArray# len# s             of { (# s, mbarr# #) ->
      case fill mbarr# s                    of {    s            ->
      case unsafeFreezeByteArray# mbarr# s  of { (# s, barr# #)  ->
      (# s, SBS barr# #) }}}))

------------------------------------------------------------------------
-- Conversion to and from ByteString

-- | /O(n)/. Convert a 'ByteString' into a 'ShortByteString'.
--
-- This makes a copy, so does not retain the input string.
--
toShort :: ByteString -> ShortByteString
toShort bs = unsafeDupablePerformIO (toShortIO bs)

toShortIO :: ByteString -> IO ShortByteString
toShortIO (PS (ForeignPtr addr# fpc) (I# off#) (I# len#)) =
  IO $ \s ->
    case newByteArray# len# s                         of { (# s, mbarr# #) ->
#if MIN_VERSION_base(4,7,0)
    case copyAddrToByteArray# (plusAddr# addr# off#)
                              mbarr# 0# len# s        of { s ->
#else
    case copyAddrToByteArray  (plusAddr# addr# off#)
                              mbarr# len# s           of { (# s, _ #) ->
#endif
    case touch# fpc s                                 of { s ->
    case unsafeFreezeByteArray# mbarr# s              of { (# s, barr# #) ->
    (# s, SBS barr# #) }}}}


-- | /O(n)/. Convert a 'ShortByteString' into a 'ByteString'.
--
fromShort :: ShortByteString -> ByteString
fromShort sbs = unsafeDupablePerformIO (fromShortIO sbs)

fromShortIO :: ShortByteString -> IO ByteString
fromShortIO (SBS barr#) =
#if MIN_VERSION_base(4,6,0)
    IO (\s ->
      case sizeofByteArray# barr#                     of { size# ->
      case newPinnedByteArray# size# s                of { (# s, mbarr# #) ->
      case copyByteArray# barr# 0# mbarr# 0# size# s  of { s ->
      (# s, PS (ForeignPtr (byteArrayContents# (unsafeCoerce# mbarr#))
                           (PlainPtr mbarr#))
               (I# 0#) (I# size#) #) }}})
#else
    -- Before base 4.6 ForeignPtrContents is not exported from GHC.ForeignPtr
    -- so we cannot get direct access to the mbarr#
    IO (\s ->
      case sizeofByteArray# barr#                         of { size# ->
      case unIO (mallocPlainForeignPtrBytes (I# size#)) s of { (# s, fp@(ForeignPtr addr# fpc) #) ->
      case copyByteArrayToAddr barr# addr# size# s        of { (# s, _ #) ->
      case touch# fpc s                                   of { s ->
      (# s, PS fp (I# 0#) (I# size#) #) }}}})
#endif


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
packLenChars (I# len#) cs0 =
    create len# (\mbarr# s -> go mbarr# 0# cs0 s)
  where
    go :: MutableByteArray# s -> Int# -> [Char] -> State# s -> State# s
    go _      _  []         s = s
    go mbarr# i# (C# c#:cs) s = 
      case writeCharArray# mbarr# i# c# s of { s ->
      go mbarr# (i# +# 1#) cs s }

packLenBytes :: Int -> [Word8] -> ShortByteString
packLenBytes (I# len#) ws0 =
    create len# (\mbarr# s -> go mbarr# 0# ws0 s)
  where
    go :: MutableByteArray# s -> Int# -> [Word8] -> State# s -> State# s
    go _      _  []          s = s
    go mbarr# i# (W8# w#:ws) s = 
      case writeWord8Array# mbarr# i# w# s of { s ->
      go mbarr# (i# +# 1#) ws s }

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
unpackAppendCharsLazy sbs cs0 =
    go 0 (length sbs) cs0
  where
    sz = 100

    go off len cs
      | len <= sz = unpackAppendCharsStrict sbs off len cs
      | otherwise = unpackAppendCharsStrict sbs off sz  remainder
                      where remainder = go (off+sz) (len-sz) cs

unpackAppendBytesLazy :: ShortByteString -> [Word8] -> [Word8]
unpackAppendBytesLazy sbs ws0 =
    go 0 (length sbs) ws0
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
unpackAppendCharsStrict (SBS barr#) (I# off#) (I# len#) cs =
    go (off# -# 1#) (off# -# 1# +# len#) cs
  where
    go sentinal# i# acc
      | i# ==# sentinal# = acc
      | otherwise        = let !c# = indexCharArray# barr# i#
                            in go sentinal# (i# -# 1#) (C# c#:acc)

unpackAppendBytesStrict :: ShortByteString -> Int -> Int -> [Word8] -> [Word8]
unpackAppendBytesStrict (SBS barr#) (I# off#) (I# len#) ws =
    go (off# -# 1#) (off# -# 1# +# len#) ws
  where
    go sentinal# i# acc
      | i# ==# sentinal# = acc
      | otherwise        = let !w# = indexWord8Array# barr# i#
                            in go sentinal# (i# -# 1#) (W8# w#:acc)


------------------------------------------------------------------------
-- Eq and Ord implementations

equateBytes :: ShortByteString -> ShortByteString -> Bool
equateBytes (SBS ba1#) (SBS ba2#) =
    let !len1 = sizeofByteArray# ba1#
        !len2 = sizeofByteArray# ba2#
     in I# len1 == I# len2
     && inlinePerformIO (memcmp_ByteArray ba1# ba2# (fromIntegral (I# len1)))
        == 0

compareBytes :: ShortByteString -> ShortByteString -> Ordering
compareBytes (SBS ba1#) (SBS ba2#) =
    let !len1 = sizeofByteArray# ba1#
        !len2 = sizeofByteArray# ba2#
        !len  = fromIntegral (min (I# len1) (I# len2))
     in case inlinePerformIO (memcmp_ByteArray ba1# ba2# len) of
          i | i < 0             -> LT
            | i > 0             -> GT
            | I# len2 > I# len1 -> LT
            | I# len2 < I# len1 -> GT
            | otherwise         -> EQ


------------------------------------------------------------------------
-- Appending and concatenation

append :: ShortByteString -> ShortByteString -> ShortByteString
append (SBS src1#) (SBS src2#) =
  case sizeofByteArray# src1# of { len1# ->
  case sizeofByteArray# src2# of { len2# ->
  create (len1# +# len2#) $ \dst# s ->
    case copyByteArray# src1# 0# dst# 0#    len1# s of { s ->
    case copyByteArray# src2# 0# dst# len1# len2# s of { s ->
    s }}}}

concat :: [ShortByteString] -> ShortByteString
concat sbss =
    create (totalLen 0# sbss) (\dst# s -> copy dst# 0# sbss s)
  where
    totalLen acc []               = acc
    totalLen acc (SBS ba# : sbss) = totalLen (acc +# sizeofByteArray# ba#) sbss

    copy :: MutableByteArray# s -> Int# -> [ShortByteString] -> State# s -> State# s
    copy _    _    []                s = s
    copy dst# off# (SBS src# : sbss) s =
      case sizeofByteArray# src#                    of { len# ->
      case copyByteArray# src# 0# dst# off# len# s  of { s ->
      copy dst# (off# +# len#) sbss s }}


------------------------------------------------------------------------
-- Exported low level operations

copyToPtr :: ShortByteString  -- ^ source data
          -> Int              -- ^ offset into source
          -> Ptr a            -- ^ destination
          -> Int              -- ^ number of bytes to copy
          -> IO ()
#if MIN_VERSION_base(4,7,0)
copyToPtr (SBS src#) (I# off#) (Ptr dst#) (I# len#) =
  IO $ \s ->
    case copyByteArrayToAddr# src# off# dst# 0# size# s of { s ->
    (# s, () #) }
#else
copyToPtr (SBS src#) off dst len =
    memcpy_src_off dst src# (fromIntegral off) (fromIntegral len)
#endif

createFromPtr :: Ptr a   -- ^ source data
              -> Int     -- ^ number of bytes to copy
              -> IO ShortByteString
createFromPtr (Ptr addr#) (I# len#) =
  IO $ \s ->
    case newByteArray# len# s                         of { (# s, mbarr# #) ->
#if MIN_VERSION_base(4,7,0)
    case copyAddrToByteArray# addr# mbarr# 0# len# s  of { s ->
#else
    case copyAddrToByteArray  addr# mbarr#    len# s  of { (# s, _ #) ->
#endif
    case unsafeFreezeByteArray# mbarr# s              of { (# s, barr# #) ->
    (# s, SBS barr# #) }}}


------------------------------------------------------------------------
-- FFI imports and primop replacements

foreign import ccall unsafe "string.h memcmp"
  memcmp_ByteArray :: ByteArray# -> ByteArray# -> CSize -> IO CInt

#if !(MIN_VERSION_base(4,7,0))
-- These exist as real primops in ghc-7.8, and for before that we use
-- FFI to C memcpy.

copyAddrToByteArray :: Addr# -> MutableByteArray# s -> Int#
                    -> State# RealWorld -> (# State# RealWorld, () #)
copyAddrToByteArray src dst n s =
  unIO (memcpy_AddrToByteArray dst src (fromIntegral (I# n))) s

foreign import ccall unsafe "string.h memcpy"
  memcpy_AddrToByteArray :: MutableByteArray# s -> Addr# -> CSize -> IO ()

foreign import ccall unsafe "fpstring.h fps_memcpy_src_off"
  memcpy_src_off :: Ptr a -> ByteArray# -> CLong -> CSize -> IO ()
#endif

#if !(MIN_VERSION_base(4,6,0))
copyByteArrayToAddr :: ByteArray# -> Addr# -> Int#
                    -> State# RealWorld -> (# State# RealWorld, () #)
copyByteArrayToAddr src dst n s =
  unIO (memcpy_ByteArrayToAddr dst src (fromIntegral (I# n))) s

foreign import ccall unsafe "string.h memcpy"
  memcpy_ByteArrayToAddr :: Addr# -> ByteArray# -> CSize -> IO ()
#endif

