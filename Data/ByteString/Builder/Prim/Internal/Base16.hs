{-# LANGUAGE MagicHash #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : GHC
--
-- Hexadecimal encoding of nibbles (4-bit) and octets (8-bit) as ASCII
-- characters.
--
-- The current implementation is based on a table based encoding inspired by
-- the code in the 'base64-bytestring' library by Bryan O'Sullivan. In our
-- benchmarks on a 32-bit machine it turned out to be the fastest
-- implementation option.
--
module Data.ByteString.Builder.Prim.Internal.Base16 (
    EncodingTable
  , lowerTable
  , encode8_as_16h
  ) where

import Foreign
import Foreign.C.Types
import GHC.Exts (Addr#, Ptr(..))

-- Creating the encoding table
------------------------------

-- | An encoding table for Base16 encoding.
data EncodingTable = EncodingTable Addr#

foreign import ccall "&hs_bytestring_lower_hex_table"
  c_lower_hex_table :: Ptr CChar

-- | The encoding table for hexadecimal values with lower-case characters;
-- e.g., deadbeef.
lowerTable :: EncodingTable
lowerTable = case c_lower_hex_table of
  Ptr p# -> EncodingTable p#

-- | Encode an octet as 16bit word comprising both encoded nibbles ordered
-- according to the host endianness. Writing these 16bit to memory will write
-- the nibbles in the correct order (i.e. big-endian).
{-# INLINE encode8_as_16h #-}
encode8_as_16h :: EncodingTable -> Word8 -> IO Word16
encode8_as_16h (EncodingTable table) =
    peekElemOff (Ptr table) . fromIntegral
