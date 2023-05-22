{-# LANGUAGE CPP #-}

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
  ( encodeFloatViaWord32F
  , encodeDoubleViaWord64F
  ) where

import Data.ByteString.Builder.Prim.Internal
import Data.Word

#if HS_CAST_FLOAT_WORD_OPS_AVAILABLE
import GHC.Float (castFloatToWord32, castDoubleToWord64)
#else
import Foreign.Marshal.Utils
import Foreign.Storable
{-
We work around ticket http://ghc.haskell.org/trac/ghc/ticket/4092 by
storing the Float/Double in a temp buffer and peeking it out again from there.
-}
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
encodeDoubleViaWord64F = fixedPrim (size w64fe) $ \x op -> do
  x' <- with x (peek . castPtr)
  runF w64fe x' op
#endif
