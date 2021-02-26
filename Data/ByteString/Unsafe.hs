{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Module      : Data.ByteString.Unsafe
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : provisional
-- Portability : non-portable
--
-- A module containing unsafe 'ByteString' operations.
--
-- While these functions have a stable API and you may use these functions in
-- applications, do carefully consider the documented pre-conditions;
-- incorrect use can break referential transparency or worse.
--
module Data.ByteString.Unsafe (

        -- * Unchecked access
        unsafeHead,
        unsafeTail,
        unsafeInit,
        unsafeLast,
        unsafeIndex,
        unsafeTake,
        unsafeDrop,

        -- * Low level interaction with CStrings
        -- ** Using ByteStrings with functions for CStrings
        unsafeUseAsCString,
        unsafeUseAsCStringLen,

        -- ** Converting CStrings to ByteStrings
        unsafePackCString,
        unsafePackCStringLen,
        unsafePackMallocCString,
        unsafePackMallocCStringLen,

        unsafePackAddress,
        unsafePackAddressLen,
        unsafePackCStringFinalizer,
        unsafeFinalize,

  ) where

import Data.ByteString.Internal

import Foreign.ForeignPtr       (newForeignPtr_, newForeignPtr, withForeignPtr)
import Foreign.Ptr              (Ptr, castPtr)

import Foreign.Storable         (Storable(..))
import Foreign.C.String         (CString, CStringLen)

import Control.Exception        (assert)

import Data.Word                (Word8)

import qualified Foreign.ForeignPtr as FC (finalizeForeignPtr)
import qualified Foreign.Concurrent as FC (newForeignPtr)

import GHC.Prim                 (Addr#)
import GHC.Ptr                  (Ptr(..))

-- ---------------------------------------------------------------------
--
-- Extensions to the basic interface
--

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the ByteString is non-empty.
unsafeHead :: ByteString -> Word8
unsafeHead (BS x l) = assert (l > 0) $
    accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peek p
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty ByteStrings. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the ByteString is non-empty.
unsafeTail :: ByteString -> ByteString
unsafeTail (BS ps l) = assert (l > 0) $ BS (plusForeignPtr ps 1) (l-1)
{-# INLINE unsafeTail #-}

-- | A variety of 'init' for non-empty ByteStrings. 'unsafeInit' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the ByteString is non-empty.
unsafeInit :: ByteString -> ByteString
unsafeInit (BS ps l) = assert (l > 0) $ BS ps (l-1)
{-# INLINE unsafeInit #-}

-- | A variety of 'last' for non-empty ByteStrings. 'unsafeLast' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the ByteString is non-empty.
unsafeLast :: ByteString -> Word8
unsafeLast (BS x l) = assert (l > 0) $
    accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peekByteOff p (l-1)
{-# INLINE unsafeLast #-}

-- | Unsafe 'ByteString' index (subscript) operator, starting from 0, returning a 'Word8'
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex (BS x l) i = assert (i >= 0 && i < l) $
    accursedUnutterablePerformIO $ withForeignPtr x $ \p -> peekByteOff p i
{-# INLINE unsafeIndex #-}

-- | A variety of 'take' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeTake :: Int -> ByteString -> ByteString
unsafeTake n (BS x l) = assert (0 <= n && n <= l) $ BS x n
{-# INLINE unsafeTake #-}

-- | A variety of 'drop' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop n (BS x l) = assert (0 <= n && n <= l) $ BS (plusForeignPtr x n) (l-n)
{-# INLINE unsafeDrop #-}


-- | /O(1)/ 'unsafePackAddressLen' provides constant-time construction of
-- 'ByteString's, which is ideal for string literals. It packs a sequence
-- of bytes into a 'ByteString', given a raw 'Addr#' to the string, and
-- the length of the string.
--
-- This function is /unsafe/ in two ways:
--
-- * the length argument is assumed to be correct. If the length
-- argument is incorrect, it is possible to overstep the end of the
-- byte array.
--
-- * if the underlying 'Addr#' is later modified, this change will be
-- reflected in the resulting 'ByteString', breaking referential
-- transparency.
--
-- If in doubt, don't use this function.
--
unsafePackAddressLen :: Int -> Addr# -> IO ByteString
unsafePackAddressLen len addr# = do
    p <- newForeignPtr_ (Ptr addr#)
    return $ BS p len
{-# INLINE unsafePackAddressLen #-}

-- | /O(1)/ Construct a 'ByteString' given a Ptr Word8 to a buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
-- This function is /unsafe/, it is possible to break referential
-- transparency by modifying the underlying buffer pointed to by the
-- first argument. Any changes to the original buffer will be reflected
-- in the resulting 'ByteString'.
--
unsafePackCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
unsafePackCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $ BS fp l

-- | Explicitly run the finaliser associated with a 'ByteString'.
-- References to this value after finalisation may generate invalid memory
-- references.
--
-- This function is /unsafe/, as there may be other
-- 'ByteString's referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'ByteString's
-- ever generated from the underlying byte array are no longer live.
--
unsafeFinalize :: ByteString -> IO ()
unsafeFinalize (BS p _) = FC.finalizeForeignPtr p

------------------------------------------------------------------------
-- Packing CStrings into ByteStrings

-- | /O(n)/ Build a 'ByteString' from a 'CString'. This value will have /no/
-- finalizer associated to it, and will not be garbage collected by
-- Haskell. The ByteString length is calculated using /strlen(3)/,
-- and thus the complexity is a /O(n)/.
--
-- This function is /unsafe/. If the 'CString' is later modified, this
-- change will be reflected in the resulting 'ByteString', breaking
-- referential transparency.
--
unsafePackCString :: CString -> IO ByteString
unsafePackCString cstr = do
    fp <- newForeignPtr_ (castPtr cstr)
    l <- c_strlen cstr
    return $! BS fp (fromIntegral l)

-- | /O(1)/ Build a 'ByteString' from a 'CStringLen'. This value will
-- have /no/ finalizer associated with it, and will not be garbage
-- collected by Haskell. This operation has /O(1)/ complexity as we
-- already know the final size, so no /strlen(3)/ is required.
--
-- This function is /unsafe/. If the original 'CStringLen' is later
-- modified, this change will be reflected in the resulting 'ByteString',
-- breaking referential transparency.
--
unsafePackCStringLen :: CStringLen -> IO ByteString
unsafePackCStringLen (ptr,len) = do
    fp <- newForeignPtr_ (castPtr ptr)
    return $! BS fp (fromIntegral len)

-- | /O(n)/ Build a 'ByteString' from a malloced 'CString'. This value will
-- have a @free(3)@ finalizer associated to it.
--
-- This function is /unsafe/. If the original 'CString' is later
-- modified, this change will be reflected in the resulting 'ByteString',
-- breaking referential transparency.
--
-- This function is also unsafe if you call its finalizer twice,
-- which will result in a /double free/ error, or if you pass it
-- a 'CString' not allocated with 'Foreign.Marshal.Alloc.malloc'.
--
unsafePackMallocCString :: CString -> IO ByteString
unsafePackMallocCString cstr = do
    fp <- newForeignPtr c_free_finalizer (castPtr cstr)
    len <- c_strlen cstr
    return $! BS fp (fromIntegral len)

-- | /O(1)/ Build a 'ByteString' from a malloced 'CStringLen'. This
-- value will have a @free(3)@ finalizer associated to it.
--
-- This function is /unsafe/. If the original 'CString' is later
-- modified, this change will be reflected in the resulting 'ByteString',
-- breaking referential transparency.
--
-- This function is also unsafe if you call its finalizer twice,
-- which will result in a /double free/ error, or if you pass it
-- a 'CString' not allocated with 'Foreign.Marshal.Alloc.malloc'.
--
unsafePackMallocCStringLen :: CStringLen -> IO ByteString
unsafePackMallocCStringLen (cstr, len) = do
    fp <- newForeignPtr c_free_finalizer (castPtr cstr)
    return $! BS fp len

-- ---------------------------------------------------------------------

-- | /O(1) construction/ Use a 'ByteString' with a function requiring a
-- 'CString'.
--
-- This function does zero copying, and merely unwraps a 'ByteString' to
-- appear as a 'CString'. It is /unsafe/ in two ways:
--
-- * After calling this function the 'CString' shares the underlying
-- byte buffer with the original 'ByteString'. Thus modifying the
-- 'CString', either in C, or using poke, will cause the contents of the
-- 'ByteString' to change, breaking referential transparency. Other
-- 'ByteString's created by sharing (such as those produced via 'take'
-- or 'drop') will also reflect these changes. Modifying the 'CString'
-- will break referential transparency. To avoid this, use
-- 'Data.ByteString.useAsCString', which makes a copy of the original 'ByteString'.
--
-- * 'CString's are often passed to functions that require them to be
-- null-terminated. If the original 'ByteString' wasn't null terminated,
-- neither will the 'CString' be. It is the programmers responsibility
-- to guarantee that the 'ByteString' is indeed null terminated. If in
-- doubt, use 'Data.ByteString.useAsCString'.
--
-- * The memory may freed at any point after the subcomputation
-- terminates, so the pointer to the storage must *not* be used
-- after this.
--
unsafeUseAsCString :: ByteString -> (CString -> IO a) -> IO a
unsafeUseAsCString (BS ps _) ac = withForeignPtr ps $ \p -> ac (castPtr p)

-- | /O(1) construction/ Use a 'ByteString' with a function requiring a
-- 'CStringLen'.
--
-- This function does zero copying, and merely unwraps a 'ByteString' to
-- appear as a 'CStringLen'. It is /unsafe/:
--
-- * After calling this function the 'CStringLen' shares the underlying
-- byte buffer with the original 'ByteString'. Thus modifying the
-- 'CStringLen', either in C, or using poke, will cause the contents of the
-- 'ByteString' to change, breaking referential transparency. Other
-- 'ByteString's created by sharing (such as those produced via 'take'
-- or 'drop') will also reflect these changes. Modifying the 'CStringLen'
-- will break referential transparency. To avoid this, use
-- 'Data.ByteString.useAsCStringLen', which makes a copy of the original 'ByteString'.
--
-- If 'Data.ByteString.empty' is given, it will pass @('Foreign.Ptr.nullPtr', 0)@.
unsafeUseAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen (BS ps l) f = withForeignPtr ps $ \p -> f (castPtr p,l)
