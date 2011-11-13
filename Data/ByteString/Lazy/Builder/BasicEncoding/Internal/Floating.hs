{-# LANGUAGE ScopedTypeVariables, MonoPatBinds #-}
-- |
-- Copyright   : (c) 2010 Simon Meier
--
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (uses unsafeCoerce)
--
-- Conversion of 'Float's and 'Double's to 'Word32's and 'Word64's.
--
module Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Floating
    (
      coerceFloatToWord32
    , coerceDoubleToWord64
    ) where


import Foreign

-- | Coerce a 'Float' to a 'Word32' as-is.
{-# INLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = fromFloat

-- | Coerce a 'Double' to a 'Word64' as-is.
{-# INLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = fromFloat

-- The implementation of the following function is based on
--
-- http://hackage.haskell.org/package/data-binary-ieee754-0.4.2.1
--
-- Module: Data.Binary.IEEE754
-- Copyright: 2010 John Millikin <jmillikin@gmail.com>
-- License: MIT
--
fromFloat :: forall w f. (Storable w, Storable f, RealFloat f) => f -> w
fromFloat x
  | isIEEE x && sizeOf (undefined :: f) == sizeOf (undefined :: w) =
      unsafePerformIO $ alloca $ \buf -> do
        poke (castPtr buf) x
        peek buf
  | otherwise = error
      "Coded.Bounded.Encoding.Floating: missing support for encoding floating point numbers on your platform!"

{- The speed of the above implementation is not great. The plan is to use the
   implementations below for real speed once the following ticket is solved:

   See http://hackage.haskell.org/trac/ghc/ticket/4092

-- | Coerce a 'Float' to a 'Word32'; i.e., interpret the 32-bit 'Float' value
-- as an unsigned 32-bit 'Int.
--
-- FIXME: Check with GHC developers if this is really safe. Does the register
-- allocater handle such a case correctly, if the 'Float' is in an FPU
-- register?
{-# INLINE coerceFloatToWord32 #-}
coerceFloatToWord32 :: Float -> Word32
coerceFloatToWord32 = unsafeCoerce

-- | Coerce a 'Double' to a 'Word64'.
{-# INLINE coerceDoubleToWord64 #-}
coerceDoubleToWord64 :: Double -> Word64
coerceDoubleToWord64 = unsafeCoerce
-}

