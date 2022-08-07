{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Data.ByteString.Internal.Utils (
  peekfp,
  pokefp,
  peekfpByteOff,
  pokefpByteOff,
  plusForeignPtr,
  minusForeignPtr,
  memcpyf,
  unsafeWithForeignPtr,

  createf,
  createfUptoN,
  createfUptoN',
  createfAndTrim,
  createfAndTrim',
  unsafeCreatef,
  unsafeCreatefUptoN,
  unsafeCreatefUptoN',
  mallocByteString,
  memcpy,
) where

import {-# SOURCE #-} Data.ByteString.Internal

import Control.Exception (assert)
import Control.Monad (void)
import Data.Word (Word8)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.Exts (Int(I#), minusAddr#)
import GHC.ForeignPtr
  ( ForeignPtr(ForeignPtr)
#if MIN_VERSION_base(4,15,0)
  , unsafeWithForeignPtr
#endif
  , mallocPlainForeignPtrBytes
  )
import GHC.IO (unsafeDupablePerformIO)

#if MIN_VERSION_base(4,10,0)
import GHC.ForeignPtr (plusForeignPtr)
#else
import GHC.Exts (plusAddr#)
#endif


-- Utilities for working with ForeignPtr

#if !MIN_VERSION_base(4,10,0)
-- |Advances the given address by the given offset in bytes.
--
-- The new 'ForeignPtr' shares the finalizer of the original,
-- equivalent from a finalization standpoint to just creating another
-- reference to the original. That is, the finalizer will not be
-- called before the new 'ForeignPtr' is unreachable, nor will it be
-- called an additional time due to this call, and the finalizer will
-- be called with the same address that it would have had this call
-- not happened, *not* the new address.
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr (ForeignPtr addr guts) (I# offset) = ForeignPtr (plusAddr# addr offset) guts
{-# INLINE [0] plusForeignPtr #-}
{-# RULES
"ByteString plusForeignPtr/0" forall fp .
   plusForeignPtr fp 0 = fp
 #-}
#endif

minusForeignPtr :: ForeignPtr a -> ForeignPtr b -> Int
minusForeignPtr (ForeignPtr addr1 _) (ForeignPtr addr2 _)
  = I# (minusAddr# addr1 addr2)

peekfp :: Storable a => ForeignPtr a -> IO a
peekfp fp = unsafeWithForeignPtr fp peek

pokefp :: Storable a => ForeignPtr a -> a -> IO ()
pokefp fp val = unsafeWithForeignPtr fp $ \p -> poke p val

peekfpByteOff :: Storable a => ForeignPtr a -> Int -> IO a
peekfpByteOff fp off = unsafeWithForeignPtr fp $ \p ->
  peekByteOff p off

pokefpByteOff :: Storable a => ForeignPtr b -> Int -> a -> IO ()
pokefpByteOff fp off val = unsafeWithForeignPtr fp $ \p ->
  pokeByteOff p off val

#if !MIN_VERSION_base(4,15,0)
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif


-- Utilities for ByteString creation

-- | Wrapper of 'Foreign.ForeignPtr.mallocForeignPtrBytes' with faster implementation for GHC
--
mallocByteString :: Int -> IO (ForeignPtr a)
mallocByteString = mallocPlainForeignPtrBytes
{-# INLINE mallocByteString #-}

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString.
unsafeCreatef :: Int -> (ForeignPtr Word8 -> IO ()) -> ByteString
unsafeCreatef l f = unsafeDupablePerformIO (createf l f)
{-# INLINE unsafeCreatef #-}

-- | Like 'unsafeCreatef' but instead of giving the final size of the
-- ByteString, it is just an upper bound. The inner action returns
-- the actual size. Unlike 'createfAndTrim' the ByteString is not
-- reallocated if the final size is less than the estimated size.
unsafeCreatefUptoN :: Int -> (ForeignPtr Word8 -> IO Int) -> ByteString
unsafeCreatefUptoN l f = unsafeDupablePerformIO (createfUptoN l f)
{-# INLINE unsafeCreatefUptoN #-}

unsafeCreatefUptoN'
  :: Int -> (ForeignPtr Word8 -> IO (Int, a)) -> (ByteString, a)
unsafeCreatefUptoN' l f = unsafeDupablePerformIO (createfUptoN' l f)
{-# INLINE unsafeCreatefUptoN' #-}

-- | Create ByteString of size @l@ and use action @f@ to fill its contents.
createf :: Int -> (ForeignPtr Word8 -> IO ()) -> IO ByteString
createf l action = do
    fp <- mallocByteString l
    action fp
    return $! BS fp l
{-# INLINE createf #-}

-- | Given a maximum size @l@ and an action @f@ that fills the 'ByteString'
-- starting at the given 'Ptr' and returns the actual utilized length,
-- @`createfUptoN'` l f@ returns the filled 'ByteString'.
createfUptoN :: Int -> (ForeignPtr Word8 -> IO Int) -> IO ByteString
createfUptoN l action = do
    fp <- mallocByteString l
    l' <- action fp
    assert (l' <= l) $ return $! BS fp l'
{-# INLINE createfUptoN #-}

-- | Like 'createfUptoN', but also returns an additional value created by the
-- action.
createfUptoN' :: Int -> (ForeignPtr Word8 -> IO (Int, a)) -> IO (ByteString, a)
createfUptoN' l action = do
    fp <- mallocByteString l
    (l', res) <- action fp
    assert (l' <= l) $ return (BS fp l', res)
{-# INLINE createfUptoN' #-}

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, createfAndTrim makes the 'ByteString'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is reallocated to this size.
--
-- createfAndTrim is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
--
createfAndTrim :: Int -> (ForeignPtr Word8 -> IO Int) -> IO ByteString
createfAndTrim l action = do
    fp <- mallocByteString l
    l' <- action fp
    if assert (0 <= l' && l' <= l) $ l' >= l
        then return $! BS fp l
        else createf l' $ \fp' -> memcpyf fp' fp l'
{-# INLINE createfAndTrim #-}

createfAndTrim' :: Int -> (ForeignPtr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
createfAndTrim' l action = do
    fp <- mallocByteString l
    (off, l', res) <- action fp
    if assert (0 <= l' && l' <= l) $ l' >= l
        then return (BS fp l, res)
        else do ps <- createf l' $ \fp' ->
                        memcpyf fp' (fp `plusForeignPtr` off) l'
                return (ps, res)
{-# INLINE createfAndTrim' #-}


foreign import ccall unsafe "string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memcpy :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memcpy p q s = void $ c_memcpy p q (fromIntegral s)

memcpyf :: ForeignPtr Word8 -> ForeignPtr Word8 -> Int -> IO ()
memcpyf fp fq s = unsafeWithForeignPtr fp $ \p ->
                    unsafeWithForeignPtr fq $ \q -> memcpy p q s
