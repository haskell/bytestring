-- allow Data.ByteString.Internal.Utils to use the BS constructor

module Data.ByteString.Internal (
  ByteString (BS)
) where

import Foreign.ForeignPtr (ForeignPtr)
import Data.Word (Word8)

data ByteString = BS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- length
