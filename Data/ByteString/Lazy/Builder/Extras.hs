
-- | We decided to rename the Builder modules. Sorry about that.
--
-- The old names will hang about for at least once release cycle before we
-- deprecate them and then later remove them.
--
module Data.ByteString.Lazy.Builder.Extras
{-# DEPRECATED "use Data.ByteString.Builder.Extra instead" #-}
( module Data.ByteString.Builder.Extra
) where

import Data.ByteString.Builder.Extra
