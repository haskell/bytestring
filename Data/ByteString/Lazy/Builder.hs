
-- | We decided to rename the Builder modules. Sorry about that.
--
-- The old names will hang about for at least once release cycle and then later remove them.
--
module Data.ByteString.Lazy.Builder
  {-# DEPRECATED "Use Data.ByteString.Builder instead" #-} (
  module Data.ByteString.Builder
) where

import Data.ByteString.Builder
