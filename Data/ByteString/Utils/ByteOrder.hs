{-# LANGUAGE CPP #-}

#include "MachDeps.h"

-- | Why does this module exist? There is "GHC.ByteOrder" in base.
-- But that module is /broken/ until base-4.14/ghc-8.10, so we
-- can't rely on it until we drop support for older ghcs.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/20338
-- and https://gitlab.haskell.org/ghc/ghc/-/issues/18445

module Data.ByteString.Utils.ByteOrder
  ( ByteOrder(..)
  , hostByteOrder
  , whenLittleEndian
  , whenBigEndian
  ) where

import GHC.ByteOrder (ByteOrder(..))

hostByteOrder :: ByteOrder
hostByteOrder =
#ifdef WORDS_BIGENDIAN
  BigEndian
#else
  LittleEndian
#endif

-- | If the host is little-endian, applies the given function to the given arg.
-- If the host is big-endian, returns the second argument unchanged.
whenLittleEndian :: (a -> a) -> a -> a
whenLittleEndian fun val = case hostByteOrder of
  LittleEndian -> fun val
  BigEndian    -> val

-- | If the host is little-endian, returns the second argument unchanged.
-- If the host is big-endian, applies the given function to the given arg.
whenBigEndian :: (a -> a) -> a -> a
whenBigEndian fun val = case hostByteOrder of
  LittleEndian -> val
  BigEndian    -> fun val
