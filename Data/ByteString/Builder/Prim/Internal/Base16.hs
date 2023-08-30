{-# LANGUAGE Trustworthy #-}
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

import           Foreign
import           GHC.Exts (Addr#, Ptr(..))

-- Creating the encoding table
------------------------------

-- | An encoding table for Base16 encoding.
data EncodingTable = EncodingTable Addr#

-- | The encoding table for hexadecimal values with lower-case characters;
-- e.g., deadbeef.
{-# NOINLINE lowerTable #-}
lowerTable :: EncodingTable
-- Is this buffer guaranteed to be Word16-aligned?
lowerTable = EncodingTable
    "000102030405060708090a0b0c0d0e0f\
    \101112131415161718191a1b1c1d1e1f\
    \202122232425262728292a2b2c2d2e2f\
    \303132333435363738393a3b3c3d3e3f\
    \404142434445464748494a4b4c4d4e4f\
    \505152535455565758595a5b5c5d5e5f\
    \606162636465666768696a6b6c6d6e6f\
    \707172737475767778797a7b7c7d7e7f\
    \808182838485868788898a8b8c8d8e8f\
    \909192939495969798999a9b9c9d9e9f\
    \a0a1a2a3a4a5a6a7a8a9aaabacadaeaf\
    \b0b1b2b3b4b5b6b7b8b9babbbcbdbebf\
    \c0c1c2c3c4c5c6c7c8c9cacbcccdcecf\
    \d0d1d2d3d4d5d6d7d8d9dadbdcdddedf\
    \e0e1e2e3e4e5e6e7e8e9eaebecedeeef\
    \f0f1f2f3f4f5f6f7f8f9fafbfcfdfeff"#


-- | Encode an octet as 16bit word comprising both encoded nibbles ordered
-- according to the host endianness. Writing these 16bit to memory will write
-- the nibbles in the correct order (i.e. big-endian).
{-# INLINE encode8_as_16h #-}
encode8_as_16h :: EncodingTable -> Word8 -> IO Word16
encode8_as_16h (EncodingTable table) =
    peekElemOff (Ptr table) . fromIntegral
