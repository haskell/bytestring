{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#include "bytestring-cpp-macros.h"

-- |
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Conversion of 'Float's and 'Double's to 'Word32's and 'Word64's.
--
module Data.ByteString.Builder.Prim.Internal.Floating
  ( castFloatToWord32
  , castDoubleToWord64
  , encodeFloatViaWord32F
  , encodeDoubleViaWord64F
  ) where

import Data.ByteString.Builder.Prim.Internal
import Data.Word

#if HS_CAST_FLOAT_WORD_OPS_AVAILABLE
import GHC.Float (castFloatToWord32, castDoubleToWord64)
#else
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr

import Data.ByteString.Internal.Type (unsafeDupablePerformIO)
{-
We work around ticket http://ghc.haskell.org/trac/ghc/ticket/4092 by
storing the Float/Double in a temp buffer and peeking it out again from there.
-}

-- | Interpret a 'Float' as a 'Word32' as if through a bit-for-bit copy.
-- (fallback if not available through GHC.Float)
--
-- e.g
--
-- > showHex (castFloatToWord32 1.0) [] = "3f800000"
{-# NOINLINE castFloatToWord32 #-}
castFloatToWord32 :: Float -> Word32
#if (SIZEOF_HSFLOAT != SIZEOF_WORD32) || (ALIGNMENT_HSFLOAT < ALIGNMENT_WORD32)
  #error "don't know how to cast Float to Word32"
#endif
castFloatToWord32 x = unsafeDupablePerformIO (with x (peek . castPtr))

-- | Interpret a 'Double' as a 'Word64' as if through a bit-for-bit copy.
-- (fallback if not available through GHC.Float)
--
-- e.g
--
-- > showHex (castDoubleToWord64 1.0) [] = "3ff0000000000000"
{-# NOINLINE castDoubleToWord64 #-}
castDoubleToWord64 :: Double -> Word64
#if (SIZEOF_HSDOUBLE != SIZEOF_WORD64) || (ALIGNMENT_HSDOUBLE < ALIGNMENT_WORD64)
  #error "don't know how to cast Double to Word64"
#endif
castDoubleToWord64 x = unsafeDupablePerformIO (with x (peek . castPtr))
#endif


-- | Encode a 'Float' using a 'Word32' encoding.
{-# INLINE encodeFloatViaWord32F #-}
encodeFloatViaWord32F :: FixedPrim Word32 -> FixedPrim Float
#if HS_CAST_FLOAT_WORD_OPS_AVAILABLE
encodeFloatViaWord32F = (castFloatToWord32 >$<)
#else
encodeFloatViaWord32F w32fe = fixedPrim (size w32fe) $ \x op -> do
  x' <- with x (peek . castPtr)
  runF w32fe x' op
#endif

-- | Encode a 'Double' using a 'Word64' encoding.
{-# INLINE encodeDoubleViaWord64F #-}
encodeDoubleViaWord64F :: FixedPrim Word64 -> FixedPrim Double
#if HS_CAST_FLOAT_WORD_OPS_AVAILABLE
encodeDoubleViaWord64F = (castDoubleToWord64 >$<)
#else
encodeDoubleViaWord64F w64fe = fixedPrim (size w64fe) $ \x op -> do
  x' <- with x (peek . castPtr)
  runF w64fe x' op
#endif
