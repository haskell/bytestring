{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module QuickCheckUtils
  ( Char8(..)
  , String8(..)
  , CByteString(..)
  , Sqrt(..)
  , int64OK
  , tooStrictErr
  ) where

import Test.Tasty.QuickCheck
import Text.Show.Functions

import Control.Monad        ( liftM2 )
import Data.Char
import Data.Word
import Data.Int
import System.IO
import Foreign.C (CChar)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.Stack (withFrozenCallStack, HasCallStack)

import qualified Data.ByteString.Short as SB
import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString.Char8      as PC
import qualified Data.ByteString.Lazy.Char8 as LC

------------------------------------------------------------------------

sizedByteString n = do m <- choose(0, n)
                       fmap P.pack $ vectorOf m arbitrary

instance Arbitrary P.ByteString where
  arbitrary = do
    bs <- sized sizedByteString
    n  <- choose (0, 2)
    return (P.drop n bs) -- to give us some with non-0 offset
  shrink = map P.pack . shrink . P.unpack

instance CoArbitrary P.ByteString where
  coarbitrary s = coarbitrary (P.unpack s)

instance Arbitrary L.ByteString where
  arbitrary = sized $ \n -> do numChunks <- choose (0, n)
                               if numChunks == 0
                                   then return L.empty
                                   else fmap (L.fromChunks .
                                              filter (not . P.null)) $
                                            vectorOf numChunks
                                                     (sizedByteString
                                                          (n `div` numChunks))

  shrink = map L.fromChunks . shrink . L.toChunks

instance CoArbitrary L.ByteString where
  coarbitrary s = coarbitrary (L.unpack s)

newtype CByteString = CByteString P.ByteString
  deriving Show

instance Arbitrary CByteString where
  arbitrary = fmap (CByteString . P.pack . map fromCChar)
                   arbitrary
    where
      fromCChar :: NonZero CChar -> Word8
      fromCChar = fromIntegral . getNonZero

-- | 'Char', but only representing 8-bit characters.
--
newtype Char8 = Char8 Char
  deriving (Eq, Ord, Show)

instance Arbitrary Char8 where
  arbitrary = fmap (Char8 . toChar) arbitrary
    where
      toChar :: Word8 -> Char
      toChar = toEnum . fromIntegral
  shrink (Char8 c) = fmap Char8 (shrink c)

instance CoArbitrary Char8 where
  coarbitrary (Char8 c) = coarbitrary c

-- | 'Char', but only representing 8-bit characters.
--
newtype String8 = String8 String
  deriving (Eq, Ord, Show)

instance Arbitrary String8 where
  arbitrary = fmap (String8 . map toChar) arbitrary
    where
      toChar :: Word8 -> Char
      toChar = toEnum . fromIntegral
  shrink (String8 xs) = fmap String8 (shrink xs)

-- | If a test takes O(n^2) time or memory, it's useful to wrap its inputs
-- into 'Sqrt' so that increasing number of tests affects run time linearly.
newtype Sqrt a = Sqrt { unSqrt :: a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Sqrt a) where
  arbitrary = Sqrt <$> sized
    (\n -> resize (round @Double $ sqrt $ fromIntegral @Int n) arbitrary)
  shrink = map Sqrt . shrink . unSqrt


sizedShortByteString :: Int -> Gen SB.ShortByteString
sizedShortByteString n = do m <- choose(0, n)
                            fmap SB.pack $ vectorOf m arbitrary

instance Arbitrary SB.ShortByteString where
  arbitrary = sized sizedShortByteString
  shrink = map SB.pack . shrink . SB.unpack

instance CoArbitrary SB.ShortByteString where
  coarbitrary s = coarbitrary (SB.unpack s)

-- | This /poison instance/ exists to make accidental mis-use
-- of the @Arbitrary Int64@ instance a bit less likely.
instance {-# OVERLAPPING #-}
  TypeError (Text "Found a test taking a raw Int64 argument."
    :$$: Text "'instance Arbitrary Int64' by default is likely to"
    :$$: Text "produce very large numbers after the first few tests,"
    :$$: Text "which doesn't make great indices into a LazyByteString."
    :$$: Text "For indices, try 'intToIndexTy' in Properties/ByteString.hs."
    :$$: Text ""
    :$$: Text "If very few small-numbers tests is OK, use"
    :$$: Text "'int64OK' to bypass this poison-instance."
  ) => Testable (Int64 -> prop) where
  property = error "poison instance Testable (Int64 -> prop)"

-- | Use this to bypass the poison instance for @Testable (Int64 -> prop)@
-- defined in "QuickCheckUtils".
int64OK :: (Arbitrary a, Show a, Testable b) => (a -> b) -> Property
int64OK f = propertyForAllShrinkShow arbitrary shrink (\v -> [show v]) f

tooStrictErr :: forall a. HasCallStack => a
tooStrictErr = withFrozenCallStack $
  error "A lazy sub-expression was unexpectedly evaluated"
