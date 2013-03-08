
-- | We decided to rename the Builder modules. Sorry about that.
--
-- In additon, the ASCII module has been merged into the main
-- "Data.ByteString.Builder" module.
--
-- The old names will hang about for at least once release cycle before we
-- deprecate them and then later remove them.
--
module Data.ByteString.Lazy.Builder.ASCII
{-# DEPRECATED "use Data.ByteString.Builder instead" #-}
( module Data.ByteString.Builder
, byteStringHexFixed
, lazyByteStringHexFixed
) where

import Data.ByteString.Builder
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L

{-# DEPRECATED byteStringHexFixed
    "use Data.ByteString.Builder.byteStringHex instead" #-}
byteStringHexFixed :: S.ByteString -> Builder
byteStringHexFixed = byteStringHex

{-# DEPRECATED lazyByteStringHexFixed
    "use Data.ByteString.Builder.lazyByteStringHex instead" #-}
lazyByteStringHexFixed :: L.ByteString -> Builder
lazyByteStringHexFixed = lazyByteStringHex
