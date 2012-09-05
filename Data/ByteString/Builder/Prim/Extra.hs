{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_HADDOCK hide #-}
{- | Copyright : (c) 2010-2011 Simon Meier
License        : BSD3-style (see LICENSE)

Maintainer     : Simon Meier <iridcode@gmail.com>
Stability      : experimental
Portability    : GHC

An /encoding/ is a conversion function of Haskell values to sequences of bytes.
A /fixed(-size) encoding/ is an encoding that always results in sequence of bytes
  of a pre-determined, fixed length.
An example for a fixed encoding is the big-endian encoding of a 'Word64',
  which always results in exactly 8 bytes.
A /bounded(-size) encoding/ is an encoding that always results in sequence
  of bytes that is no larger than a pre-determined bound.
An example for a bounded encoding is the UTF-8 encoding of a 'Char',
  which results always in less or equal to 4 bytes.
Note that every fixed encoding is also a bounded encoding.
We explicitly identify fixed encodings because they allow some optimizations
  that are impossible with bounded encodings.
In the following,
  we first motivate the use of bounded encodings
  and then give examples of optimizations
  that are only possible with fixed encodings.

Typicall, encodings are implemented efficiently by allocating a buffer
  (a mutable array of bytes)
  and repeatedly executing the following two steps:
  (1) writing to the buffer until it is full and
  (2) handing over the filled part to the consumer of the encoded value.
Step (1) is where bounded encodings are used.
We must use a bounded encoding,
  as we must check that there is enough free space
  /before/ actually writing to the buffer.

In term of expressivity,
  it would be sufficient to construct all encodings
  from the single fixed encoding that encodes a 'Word8' as-is.
However,
  this is not sufficient in terms of efficiency.
It results in unnecessary buffer-full checks and
  it complicates the program-flow for writing to the buffer,
  as buffer-full checks are interleaved with analyzing the value to be
  encoded (e.g., think about the program-flow for UTF-8 encoding).
This has a significant effect on overall encoding performance,
  as encoding primitive Haskell values such as 'Word8's or 'Char's
  lies at the heart of every encoding implementation.

The 'BoundedPrim's provided by this module remove this performance problem.
Intuitively,
  they consist of a tuple of the bound on the maximal number of bytes written
  and the actual implementation of the encoding as
  a function that modifies a mutable buffer.
Hence when executing a 'BoundedPrim',
 the buffer-full check can be done once before the actual writing to the buffer.
The provided 'BoundedPrim's also take care to implement the
  actual writing to the buffer efficiently.
Moreover, combinators are provided to construct new bounded encodings
  from the provided ones.



The result of an encoding can be consumed efficiently,
  if it is represented as a sequence of large enough
  /chunks/ of consecutive memory (i.e., C @char@ arrays).
The precise meaning of /large enough/ is application dependent.
Typically, an average chunk size between 4kb and 32kb is suitable
  for writing the result to disk or sending it over the network.
We desire large enough chunk sizes because each chunk boundary
  incurs extra work that we must be able to amortize.


The need for fixed-size encodings arises when considering
  the efficient implementation of encodings that require the encoding of a
  value to be prefixed with the size of the resulting sequence of bytes.
An efficient implementation avoids unnecessary buffer
We can implement this efficiently as follows.
We first reserve the space for the encoding of the size.
Then, we encode the value.
Finally, we encode the size of the resulting sequence of bytes into
  the reserved space.
For this to work

This works only if the encoding resulting size fits

by first, reserving the space for the encoding
  of the size, then performing the

For efficiency,
  we want to avoid unnecessary copying.


For example, the HTTP/1.0 requires the size of the body to be given in
  the Content-Length field.

chunked-transfer encoding requires each chunk to
  be prefixed with the hexadecimal encoding of the chunk size.


-}

{-
--
--
-- A /bounded encoding/ is an encoding that never results in a sequence
-- longer than some fixed number of bytes. This number of bytes must be
-- independent of the value being encoded. Typical examples of bounded
-- encodings are the big-endian encoding of a 'Word64', which results always
-- in exactly 8 bytes, or the UTF-8 encoding of a 'Char', which results always
-- in less or equal to 4 bytes.
--
-- Typically, encodings are implemented efficiently by allocating a buffer (an
-- array of bytes) and repeatedly executing the following two steps: (1)
-- writing to the buffer until it is full and (2) handing over the filled part
-- to the consumer of the encoded value. Step (1) is where bounded encodings
-- are used. We must use a bounded encoding, as we must check that there is
-- enough free space /before/ actually writing to the buffer.
--
-- In term of expressivity, it would be sufficient to construct all encodings
-- from the single bounded encoding that encodes a 'Word8' as-is. However,
-- this is not sufficient in terms of efficiency. It results in unnecessary
-- buffer-full checks and it complicates the program-flow for writing to the
-- buffer, as buffer-full checks are interleaved with analyzing the value to be
-- encoded (e.g., think about the program-flow for UTF-8 encoding). This has a
-- significant effect on overall encoding performance, as encoding primitive
-- Haskell values such as 'Word8's or 'Char's lies at the heart of every
-- encoding implementation.
--
-- The bounded 'Encoding's provided by this module remove this performance
-- problem. Intuitively, they consist of a tuple of the bound on the maximal
-- number of bytes written and the actual implementation of the encoding as a
-- function that modifies a mutable buffer. Hence when executing a bounded
-- 'Encoding', the buffer-full check can be done once before the actual writing
-- to the buffer. The provided 'Encoding's also take care to implement the
-- actual writing to the buffer efficiently. Moreover, combinators are
-- provided to construct new bounded encodings from the provided ones.
--
-- A typical example for using the combinators is a bounded 'Encoding' that
-- combines escaping the ' and \\ characters with UTF-8 encoding. More
-- precisely, the escaping to be done is the one implemented by the following
-- @escape@ function.
--
-- > escape :: Char -> [Char]
-- > escape '\'' = "\\'"
-- > escape '\\' = "\\\\"
-- > escape c    = [c]
--
-- The bounded 'Encoding' that combines this escaping with UTF-8 encoding is
-- the following.
--
-- > import Data.ByteString.Builder.Prim.Utf8 (char)
-- >
-- > {-# INLINE escapeChar #-}
-- > escapeUtf8 :: BoundedPrim Char
-- > escapeUtf8 =
-- >     encodeIf ('\'' ==) (char <#> char #. const ('\\','\'')) $
-- >     encodeIf ('\\' ==) (char <#> char #. const ('\\','\\')) $
-- >     char
--
-- The definition of 'escapeUtf8' is more complicated than 'escape', because
-- the combinators ('encodeIf', 'encodePair', '#.', and 'char') used in
-- 'escapeChar' compute both the bound on the maximal number of bytes written
-- (8 for 'escapeUtf8') as well as the low-level buffer manipulation required
-- to implement the encoding. Bounded 'Encoding's should always be inlined.
-- Otherwise, the compiler cannot compute the bound on the maximal number of
-- bytes written at compile-time. Without inlinining, it would also fail to
-- optimize the constant encoding of the escape characters in the above
-- example. Functions that execute bounded 'Encoding's also perform
-- suboptimally, if the definition of the bounded 'Encoding' is not inlined.
-- Therefore we add an 'INLINE' pragma to 'escapeUtf8'.
--
-- Currently, the only library that executes bounded 'Encoding's is the
-- 'bytestring' library (<http://hackage.haskell.org/package/bytestring>). It
-- uses bounded 'Encoding's to implement most of its lazy bytestring builders.
-- Executing a bounded encoding should be done using the corresponding
-- functions in the lazy bytestring builder 'Extras' module.
--
-- TODO: Merge with explanation/example below
--
-- Bounded 'E.Encoding's abstract encodings of Haskell values that can be implemented by
-- writing a bounded-size sequence of bytes directly to memory. They are
-- lifted to conversions from Haskell values to 'Builder's by wrapping them
-- with a bound-check. The compiler can implement this bound-check very
-- efficiently (i.e, a single comparison of the difference of two pointers to a
-- constant), because the bound of a 'E.Encoding' is always independent of the
-- value being encoded and, in most cases, a literal constant.
--
-- 'E.Encoding's are the primary means for defining conversion functions from
-- primitive Haskell values to 'Builder's. Most 'Builder' constructors
-- provided by this library are implemented that way.
-- 'E.Encoding's are also used to construct conversions that exploit the internal
-- representation of data-structures.
--
-- For example, 'encodeByteStringWith' works directly on the underlying byte
-- array and uses some tricks to reduce the number of variables in its inner
-- loop. Its efficiency is exploited for implementing the @filter@ and @map@
-- functions in "Data.ByteString.Lazy" as
--
-- > import qualified Data.ByteString.Builder.Prim as P
-- >
-- > filter :: (Word8 -> Bool) -> ByteString -> ByteString
-- > filter p = toLazyByteString . encodeLazyByteStringWithB write
-- >   where
-- >     write = P.condB p P.word8 P.emptyB
-- >
-- > map :: (Word8 -> Word8) -> ByteString -> ByteString
-- > map f = toLazyByteString . encodeLazyByteStringWithB (E.word8 E.#. f)
--
-- Compared to earlier versions of @filter@ and @map@ on lazy 'L.ByteString's,
-- these versions use a more efficient inner loop and have the additional
-- advantage that they always result in well-chunked 'L.ByteString's; i.e, they
-- also perform automatic defragmentation.
--
-- We can also use 'E.Encoding's to improve the efficiency of the following
-- 'renderString' function from our UTF-8 CSV table encoding example in
-- "Data.ByteString.Builder".
--
-- > renderString :: String -> Builder
-- > renderString cs = charUtf8 '"' <> foldMap escape cs <> charUtf8 '"'
-- >   where
-- >     escape '\\' = charUtf8 '\\' <> charUtf8 '\\'
-- >     escape '\"' = charUtf8 '\\' <> charUtf8 '\"'
-- >     escape c    = charUtf8 c
--
-- The idea is to save on 'mappend's by implementing a 'E.Encoding' that escapes
-- characters and using 'encodeListWith', which implements writing a list of
-- values with a tighter inner loop and no 'mappend'.
--
-- > import Data.ByteString.Builder.Extra     -- assume these
-- > import Data.ByteString.Builder.Prim      -- imports are present
-- >        ( BoundedPrim, encodeIf, (<#>), (#.) )
-- > import Data.ByteString.Builder.Prim.Utf8 (char)
-- >
-- > renderString :: String -> Builder
-- > renderString cs =
-- >     charUtf8 '"' <> encodeListWithB escapedUtf8 cs <> charUtf8 '"'
-- >   where
-- >     escapedUtf8 :: BoundedPrim Char
-- >     escapedUtf8 =
-- >       encodeIf (== '\\') (char <#> char #. const ('\\', '\\')) $
-- >       encodeIf (== '\"') (char <#> char #. const ('\\', '\"')) $
-- >       char
--
-- This 'Builder' considers a buffer with less than 8 free bytes as full. As
-- all functions are inlined, the compiler is able to optimize the constant
-- 'E.Encoding's as two sequential 'poke's. Compared to the first implementation of
-- 'renderString' this implementation is 1.7x faster.
--
-}
{-
Internally, 'Builder's are buffer-fill operations that are
given a continuation buffer-fill operation and a buffer-range to be filled.
A 'Builder' first checks if the buffer-range is large enough. If that's
the case, the 'Builder' writes the sequences of bytes to the buffer and
calls its continuation.  Otherwise, it returns a signal that it requires a
new buffer together with a continuation to be called on this new buffer.
Ignoring the rare case of a full buffer-range, the execution cost of a
'Builder' consists of three parts:

  1. The time taken to read the parameters; i.e., the buffer-fill
     operation to call after the 'Builder' is done and the buffer-range to
     fill.

  2. The time taken to check for the size of the buffer-range.

  3. The time taken for the actual encoding.

We can reduce cost (1) by ensuring that fewer buffer-fill function calls are
required. We can reduce cost (2) by fusing buffer-size checks of sequential
writes. For example, when escaping a 'String' using 'renderString', it would
be sufficient to check before encoding a character that at least 8 bytes are
free. We can reduce cost (3) by implementing better primitive 'Builder's.
For example, 'renderCell' builds an intermediate list containing the decimal
representation of an 'Int'. Implementing a direct decimal encoding of 'Int's
to memory would be more efficient, as it requires fewer buffer-size checks
and less allocation. It is also a planned extension of this library.

The first two cost reductions are supported for user code through functions
in "Data.ByteString.Builder.Extra". There, we continue the above example
and drop the generation time to 0.8ms by implementing 'renderString' more
cleverly. The third reduction requires meddling with the internals of
'Builder's and is not recomended in code outside of this library. However,
patches to this library are very welcome.
-}
module Data.ByteString.Builder.Prim.Extra (

  -- * Base-128, variable-length binary encodings
  {- |
There are many options for implementing a base-128 (i.e, 7-bit),
variable-length encoding. The encoding implemented here is the one used by
Google's protocol buffer library
<http://code.google.com/apis/protocolbuffers/docs/encoding.html#varints>.  This
encoding can be implemented efficiently and provides the desired property that
small positive integers result in short sequences of bytes. It is intended to
be used for the new default binary serialization format of the differently
sized 'Word' types. It works as follows.

The most-significant bit (MSB) of each output byte indicates whether
there is a following byte (MSB set to 1) or it is the last byte (MSB set to 0).
The remaining 7-bits are used to encode the input starting with the least
significant 7-bit group of the input (i.e., a little-endian ordering of the
7-bit groups is used).

For example, the value @1 :: Int@ is encoded as @[0x01]@. The value
@128 :: Int@, whose binary representation is @1000 0000@, is encoded as
@[0x80, 0x01]@; i.e., the first byte has its MSB set and the least significant
7-bit group is @000 0000@, the second byte has its MSB not set (it is the last
byte) and its 7-bit group is @000 0001@.
-}
    word8Var
  , word16Var
  , word32Var
  , word64Var
  , wordVar

{- |
The following encodings work by casting the signed integer to the equally sized
unsigned integer. This works well for positive integers, but for negative
integers it always results in the longest possible sequence of bytes,
as their MSB is (by definition) always set.
-}

  , int8Var
  , int16Var
  , int32Var
  , int64Var
  , intVar

{- |
Positive and negative integers of small magnitude can be encoded compactly
  using the so-called ZigZag encoding
  (<http://code.google.com/apis/protocolbuffers/docs/encoding.html#types>).
The /ZigZag encoding/ uses
  even numbers to encode the postive integers and
  odd numbers to encode the negative integers.
For example,
  @0@ is encoded as @0@, @-1@ as @1@, @1@ as @2@, @-2@ as @3@, @2@ as @4@, and
  so on.
Its efficient implementation uses some bit-level magic.
For example

@
zigZag32 :: 'Int32' -> 'Word32'
zigZag32 n = fromIntegral ((n \`shiftL\` 1) \`xor\` (n \`shiftR\` 31))
@

Note that the 'shiftR' is an arithmetic shift that performs sign extension.
The ZigZag encoding essentially swaps the LSB with the MSB and additionally
inverts all bits if the MSB is set.

The following encodings implement the combintion of ZigZag encoding
  together with the above base-128, variable length encodings.
They are intended to become the the new default binary serialization format of
  the differently sized 'Int' types.
-}
  , int8VarSigned
  , int16VarSigned
  , int32VarSigned
  , int64VarSigned
  , intVarSigned


  -- * Chunked / size-prefixed encodings
{- |
Some encodings like ASN.1 BER <http://en.wikipedia.org/wiki/Basic_Encoding_Rules>
or Google's protocol buffers <http://code.google.com/p/protobuf/> require
encoded data to be prefixed with its length. The simple method to achieve this
is to encode the data first into a separate buffer, compute the length of the
encoded data, write it to the current output buffer, and append the separate
buffers. The drawback of this method is that it requires a ...
-}
  , size
  , sizeBound
  -- , withSizeFB
  -- , withSizeBB
  , encodeWithSize

  , encodeChunked

  , wordVarFixedBound
  , wordHexFixedBound
  , wordDecFixedBound

  , word64VarFixedBound
  , word64HexFixedBound
  , word64DecFixedBound

  ) where

import           Data.ByteString.Builder.Internal
import           Data.ByteString.Builder.Prim.Internal.UncheckedShifts
import           Data.ByteString.Builder.Prim.Internal.Base16 (lowerTable, encode4_as_8)

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import           Data.Monoid
import           Data.List (unfoldr)  -- HADDOCK ONLY
import           Data.Char (chr, ord)
import           Control.Monad ((<=<), unless)

import           Data.ByteString.Builder.Prim.Internal hiding (size, sizeBound)
import qualified Data.ByteString.Builder.Prim.Internal as I (size, sizeBound)
import           Data.ByteString.Builder.Prim.Binary
import           Data.ByteString.Builder.Prim.ASCII
import           Data.ByteString.Builder.Prim

import           Foreign

------------------------------------------------------------------------------
-- Adapting 'size' for the public interface.
------------------------------------------------------------------------------

-- | The size of the sequence of bytes generated by this 'FixedPrim'.
size :: FixedPrim a -> Word
size = fromIntegral . I.size

-- | The bound on the size of the sequence of bytes generated by this
-- 'BoundedPrim'.
sizeBound :: BoundedPrim a -> Word
sizeBound = fromIntegral . I.sizeBound


------------------------------------------------------------------------------
-- Base-128 Variable-Length Encodings
------------------------------------------------------------------------------

{-# INLINE encodeBase128 #-}
encodeBase128
    :: forall a b. (Integral a, Bits a, Storable b, Integral b, Num b)
    => (a -> Int -> a) -> BoundedPrim b
encodeBase128 shiftr =
    -- We add 6 because we require the result of (`div` 7) to be rounded up.
    boundedEncoding ((8 * sizeOf (undefined :: b) + 6) `div` 7) (io . fromIntegral)
  where
    io !x !op
      | x' == 0   = do poke8 (x .&. 0x7f)
                       return $! op `plusPtr` 1
      | otherwise = do poke8 ((x .&. 0x7f) .|. 0x80)
                       io x' (op `plusPtr` 1)
      where
        x'    = x `shiftr` 7
        poke8 = poke op . fromIntegral

-- | Base-128, variable length encoding of a 'Word8'.
{-# INLINE word8Var #-}
word8Var :: BoundedPrim Word8
word8Var = encodeBase128 shiftr_w

-- | Base-128, variable length encoding of a 'Word16'.
{-# INLINE word16Var #-}
word16Var :: BoundedPrim Word16
word16Var = encodeBase128 shiftr_w

-- | Base-128, variable length encoding of a 'Word32'.
{-# INLINE word32Var #-}
word32Var :: BoundedPrim Word32
word32Var = encodeBase128 shiftr_w32

-- | Base-128, variable length encoding of a 'Word64'.
{-# INLINE word64Var #-}
word64Var :: BoundedPrim Word64
word64Var = encodeBase128 shiftr_w64

-- | Base-128, variable length encoding of a 'Word'.
{-# INLINE wordVar #-}
wordVar :: BoundedPrim Word
wordVar = encodeBase128 shiftr_w


-- | Base-128, variable length encoding of an 'Int8'.
-- Use 'int8VarSigned' for encoding negative numbers.
{-# INLINE int8Var #-}
int8Var :: BoundedPrim Int8
int8Var = fromIntegral >$< word8Var

-- | Base-128, variable length encoding of an 'Int16'.
-- Use 'int16VarSigned' for encoding negative numbers.
{-# INLINE int16Var #-}
int16Var :: BoundedPrim Int16
int16Var = fromIntegral >$< word16Var

-- | Base-128, variable length encoding of an 'Int32'.
-- Use 'int32VarSigned' for encoding negative numbers.
{-# INLINE int32Var #-}
int32Var :: BoundedPrim Int32
int32Var = fromIntegral >$< word32Var

-- | Base-128, variable length encoding of an 'Int64'.
-- Use 'int64VarSigned' for encoding negative numbers.
{-# INLINE int64Var #-}
int64Var :: BoundedPrim Int64
int64Var = fromIntegral >$< word64Var

-- | Base-128, variable length encoding of an 'Int'.
-- Use 'intVarSigned' for encoding negative numbers.
{-# INLINE intVar #-}
intVar :: BoundedPrim Int
intVar = fromIntegral >$< wordVar

{-# INLINE zigZag #-}
zigZag :: (Storable a, Bits a) => a -> a
zigZag x = (x `shiftL` 1) `xor` (x `shiftR` (8 * sizeOf x - 1))

-- | Base-128, variable length, ZigZag encoding of an 'Int'.
{-# INLINE int8VarSigned #-}
int8VarSigned :: BoundedPrim Int8
int8VarSigned = zigZag >$< int8Var

-- | Base-128, variable length, ZigZag encoding of an 'Int16'.
{-# INLINE int16VarSigned #-}
int16VarSigned :: BoundedPrim Int16
int16VarSigned = zigZag >$< int16Var

-- | Base-128, variable length, ZigZag encoding of an 'Int32'.
{-# INLINE int32VarSigned #-}
int32VarSigned :: BoundedPrim Int32
int32VarSigned = zigZag >$< int32Var

-- | Base-128, variable length, ZigZag encoding of an 'Int64'.
{-# INLINE int64VarSigned #-}
int64VarSigned :: BoundedPrim Int64
int64VarSigned = zigZag >$< int64Var

-- | Base-128, variable length, ZigZag encoding of an 'Int'.
{-# INLINE intVarSigned #-}
intVarSigned :: BoundedPrim Int
intVarSigned = zigZag >$< intVar



------------------------------------------------------------------------------
-- Chunked Encoding Transformer
------------------------------------------------------------------------------

-- | /Heavy inlining./
{-# INLINE encodeChunked #-}
encodeChunked
    :: Word                           -- ^ Minimal free-size
    -> (Word64 -> FixedPrim Word64)
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedPrim Word64)
    -- ^ An encoding for terminating a chunk of the given size.
    -> Builder
    -- ^ Inner Builder to transform
    -> Builder
    -- ^ 'Put' with chunked encoding.
encodeChunked minFree mkBeforeFE afterBE =
    fromPut . putChunked minFree mkBeforeFE afterBE . putBuilder

-- | /Heavy inlining./
{-# INLINE putChunked #-}
putChunked
    :: Word                         -- ^ Minimal free-size
    -> (Word64 -> FixedPrim Word64)
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedPrim Word64)
    -- ^ Encoding a directly inserted chunk.
    -> Put a
    -- ^ Inner Put to transform
    -> Put a
    -- ^ 'Put' with chunked encoding.
putChunked minFree0 mkBeforeFE afterBE p =
    put encodingStep
  where
    minFree, reservedAfter, maxReserved, minBufferSize :: Int
    minFree       = fromIntegral $ max 1 minFree0   -- sanitize and convert to Int

    -- reserved space must be computed for maximum buffer size to cover for all
    -- sizes of the actually returned buffer.
    reservedAfter = I.sizeBound afterBE
    maxReserved   = I.size (mkBeforeFE maxBound) + reservedAfter
    minBufferSize = minFree + maxReserved

    encodingStep k =
        fill (runPut p)
      where
        fill innerStep !(BufferRange op ope)
          | outRemaining < minBufferSize =
              return $! bufferFull minBufferSize op (fill innerStep)
          | otherwise = do
              fillWithBuildStep innerStep doneH fullH insertChunksH brInner
          where
            outRemaining   = ope `minusPtr` op
            beforeFE       = mkBeforeFE $ fromIntegral outRemaining
            reservedBefore = I.size beforeFE

            opInner        = op  `plusPtr` reservedBefore
            opeInner       = ope `plusPtr` (-reservedAfter)
            brInner        = BufferRange opInner opeInner

            wrapChunk :: Ptr Word8 -> IO (Ptr Word8)
            wrapChunk !opInner'
              | innerSize == 0 = return op -- no data written => no chunk to wrap
              | otherwise      = do
                  runF beforeFE innerSize op
                  runB afterBE innerSize opInner'
              where
                innerSize = fromIntegral $ opInner' `minusPtr` opInner

            doneH opInner' x = do
                op' <- wrapChunk opInner'
                let !br' = BufferRange op' ope
                k x br'

            fullH opInner' minSize nextInnerStep = do
                op' <- wrapChunk opInner'
                return $! bufferFull
                  (max minBufferSize (minSize + maxReserved))
                  op'
                  (fill nextInnerStep)

            insertChunksH opInner' n lbsC nextInnerStep
              | n == 0 = do                      -- flush
                  op' <- wrapChunk opInner'
                  return $! insertChunks op' 0 id (fill nextInnerStep)

              | otherwise = do                   -- insert non-empty bytestring
                  op' <- wrapChunk opInner'
                  let !br' = BufferRange op' ope
                  runBuilderWith chunkB (fill nextInnerStep) br'
              where
                nU     = fromIntegral n
                chunkB =
                  encodeWithF (mkBeforeFE nU) nU `mappend`
                  lazyByteStringC n lbsC         `mappend`
                  encodeWithB afterBE nU


-- | /Heavy inlining./ Prefix a 'Builder' with the size of the
-- sequence of bytes that it denotes.
--
-- This function is optimized for streaming use. It tries to prefix the size
-- without copying the output. This is achieved by reserving space for the
-- maximum size to be encoded. This succeeds if the output is smaller than
-- the current free buffer size, which is guaranteed to be at least @8kb@.
--
-- If the output does not fit into the current free buffer size,
-- the method falls back to encoding the data to a separate lazy bytestring,
-- computing the size, and encoding the size before inserting the chunks of
-- the separate lazy bytestring.
{-# INLINE encodeWithSize #-}
encodeWithSize
    ::
       Word
    -- ^ Inner buffer-size.
    -> (Word64 -> FixedPrim Word64)
    -- ^ Given a bound on the maximal size to encode, this function must return
    -- a fixed-size encoding for all smaller sizes.
    -> Builder
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Builder
encodeWithSize innerBufSize mkSizeFE =
    fromPut . putWithSize innerBufSize mkSizeFE . putBuilder

-- | Prefix a 'Put' with the size of its written data.
{-# INLINE putWithSize #-}
putWithSize
    :: forall a.
       Word
    -- ^ Buffer-size for inner driver.
    -> (Word64 -> FixedPrim Word64)
    -- ^ Encoding the size for the fallback case.
    -> Put a
    -- ^ 'Put' to prefix with the length of its sequence of bytes.
    -> Put a
putWithSize innerBufSize mkSizeFE innerP =
    put $ encodingStep
  where
    -- | The minimal free size is such that we can encode any size.
    minFree = I.size $ mkSizeFE maxBound

    encodingStep :: (forall r. (a -> BuildStep r) -> BuildStep r)
    encodingStep k =
        fill (runPut innerP)
      where
        fill :: BuildStep a -> BufferRange -> IO (BuildSignal r)
        fill innerStep !(BufferRange op ope)
          | outRemaining < minFree =
              return $! bufferFull minFree op (fill innerStep)
          | otherwise = do
              fillWithBuildStep innerStep doneH fullH insertChunksH brInner
          where
            outRemaining   = ope `minusPtr` op
            sizeFE         = mkSizeFE $ fromIntegral outRemaining
            reservedBefore = I.size sizeFE
            reservedAfter  = minFree - reservedBefore

            -- leave enough free space such that all sizes can be encodded.
            startInner    = op  `plusPtr` reservedBefore
            opeInner      = ope `plusPtr` (negate reservedAfter)
            brInner       = BufferRange startInner opeInner

            fastPrefixSize :: Ptr Word8 -> IO (Ptr Word8)
            fastPrefixSize !opInner'
              | innerSize == 0 = do runB (toB $ mkSizeFE 0) 0         op
              | otherwise      = do runF (sizeFE)           innerSize op
                                    return opInner'
              where
                innerSize = fromIntegral $ opInner' `minusPtr` startInner

            slowPrefixSize :: Ptr Word8 -> Builder -> BuildStep a -> IO (BuildSignal r)
            slowPrefixSize opInner' bInner nextStep = do
                (x, chunks, payLenChunks) <- toLBS $ runBuilderWith bInner nextStep

                let -- length of payload data in current buffer
                    payLenCur   = opInner' `minusPtr` startInner
                    -- length of whole payload
                    payLen      = fromIntegral payLenCur + fromIntegral payLenChunks
                    -- encoder for payload length
                    sizeFE'     = mkSizeFE payLen
                    -- start of payload in current buffer with the payload
                    -- length encoded before
                    startInner' = op `plusPtr` I.size sizeFE'

                -- move data in current buffer out of the way, if required
                unless (startInner == startInner') $
                    moveBytes startInner' startInner payLenCur
                -- encode payload length at start of the buffer
                runF sizeFE' payLen op
                -- TODO: If we were to change the CIOS definition such that it also
                -- returns the last buffer for writing, we could also fill the
                -- last buffer with 'k' and return the signal, once it is
                -- filled, therefore avoiding unfilled space.
                return $ insertChunks (startInner' `plusPtr` payLenCur)
                                      payLenChunks
                                      chunks
                                      (k x)
              where
                toLBS = runCIOSWithLength <=<
                    buildStepToCIOSUntrimmedWith (fromIntegral innerBufSize)

            doneH :: Ptr Word8 -> a -> IO (BuildSignal r)
            doneH opInner' x = do
                op' <- fastPrefixSize opInner'
                let !br' = BufferRange op' ope
                k x br'

            fullH :: Ptr Word8 -> Int -> BuildStep a -> IO (BuildSignal r)
            fullH opInner' minSize nextInnerStep =
                slowPrefixSize opInner' (ensureFree minSize) nextInnerStep

            insertChunksH :: Ptr Word8 -> Int64 -> LazyByteStringC
                          -> BuildStep a -> IO (BuildSignal r)
            insertChunksH opInner' n lbsC nextInnerStep =
                slowPrefixSize opInner' (lazyByteStringC n lbsC) nextInnerStep


-- | Run a 'ChunkIOStream' and gather its results and their length.
runCIOSWithLength :: ChunkIOStream a -> IO (a, LazyByteStringC, Int64)
runCIOSWithLength =
    go 0 id
  where
    go !l lbsC (Finished x)        = return (x, lbsC, l)
    go !l lbsC (YieldC n lbsC' io) = io >>= go (l + n) (lbsC . lbsC')
    go !l lbsC (Yield1 bs io)      =
        io >>= go (l + fromIntegral (S.length bs)) (lbsC . L.Chunk bs)

-- | Run a 'BuildStep' using the untrimmed strategy.
buildStepToCIOSUntrimmedWith :: Int -> BuildStep a -> IO (ChunkIOStream a)
buildStepToCIOSUntrimmedWith bufSize =
    buildStepToCIOS (untrimmedStrategy bufSize bufSize)
                    (return . Finished)


----------------------------------------------------------------------
-- Padded versions of encodings for streamed prefixing of output sizes
----------------------------------------------------------------------

{-# INLINE appsUntilZero #-}
appsUntilZero :: (Eq a, Num a) => (a -> a) -> a -> Int
appsUntilZero f x0 =
    count 0 x0
  where
    count !n 0 = n
    count !n x = count (succ n) (f x)


{-# INLINE genericVarFixedBound #-}
genericVarFixedBound :: (Eq b, Show b, Bits b, Num a, Integral b)
                => (b -> a -> b) -> b -> FixedPrim b
genericVarFixedBound shiftRight bound =
    fixedEncoding n0 io
  where
    n0 = max 1 $ appsUntilZero (`shiftRight` 7) bound

    io !x0 !op
      | x0 > bound = error err
      | otherwise  = loop 0 x0
      where
        err = "genericVarFixedBound: value " ++ show x0 ++ " > bound " ++ show bound
        loop !n !x
          | n0 <= n + 1 = do poke8 (x .&. 0x7f)
          | otherwise   = do poke8 ((x .&. 0x7f) .|. 0x80)
                             loop (n + 1) (x `shiftRight` 7)
          where
            poke8 = pokeElemOff op n . fromIntegral

{-# INLINE wordVarFixedBound #-}
wordVarFixedBound :: Word -> FixedPrim Word
wordVarFixedBound = genericVarFixedBound shiftr_w

{-# INLINE word64VarFixedBound #-}
word64VarFixedBound :: Word64 -> FixedPrim Word64
word64VarFixedBound = genericVarFixedBound shiftr_w64


-- Somehow this function doesn't really make sense, as the bound must be
-- greater when interpreted as an unsigned integer. These conversions and
-- decisions should be left to the user.
--
--{-# INLINE intVarFixed #-}
--intVarFixed :: Size -> FixedPrim Size
--intVarFixed bound = fromIntegral >$< wordVarFixed (fromIntegral bound)

{-# INLINE genHexFixedBound #-}
genHexFixedBound :: (Num a, Bits a, Integral a)
                 => (a -> Int -> a) -> Char -> a -> FixedPrim a
genHexFixedBound shiftr padding0 bound =
    fixedEncoding n0 io
  where
    n0 = max 1 $ appsUntilZero (`shiftr` 4) bound

    padding = fromIntegral (ord padding0) :: Word8

    io !x0 !op0 =
        loop (op0 `plusPtr` n0) x0
      where
        loop !op !x = do
           let !op' = op `plusPtr` (-1)
           poke op' =<< encode4_as_8 lowerTable (fromIntegral $ x .&. 0xf)
           let !x' = x `shiftr` 4
           unless (op' <= op0) $
             if x' == 0
               then pad (op' `plusPtr` (-1))
               else loop op' x'

        pad !op
          | op < op0  = return ()
          | otherwise = poke op padding >> pad (op `plusPtr` (-1))


{-# INLINE wordHexFixedBound #-}
wordHexFixedBound :: Char -> Word -> FixedPrim Word
wordHexFixedBound = genHexFixedBound shiftr_w

{-# INLINE word64HexFixedBound #-}
word64HexFixedBound :: Char -> Word64 -> FixedPrim Word64
word64HexFixedBound = genHexFixedBound shiftr_w64

-- | Note: Works only for positive numbers.
{-# INLINE genDecFixedBound #-}
genDecFixedBound :: (Num a, Bits a, Integral a)
                 => Char -> a -> FixedPrim a
genDecFixedBound padding0 bound =
    fixedEncoding n0 io
  where
    n0 = max 1 $ appsUntilZero (`div` 10) bound

    padding = fromIntegral (ord padding0) :: Word8

    io !x0 !op0 =
        loop (op0 `plusPtr` n0) x0
      where
        loop !op !x = do
           let !op' = op `plusPtr` (-1)
               !x'  = x `div` 10
           poke op' ((fromIntegral $ (x - x' * 10) + 48) :: Word8)
           unless (op' <= op0) $
             if x' == 0
               then pad (op' `plusPtr` (-1))
               else loop op' x'

        pad !op
          | op < op0  = return ()
          | otherwise = poke op padding >> pad (op `plusPtr` (-1))

{-# INLINE wordDecFixedBound #-}
wordDecFixedBound :: Char -> Word -> FixedPrim Word
wordDecFixedBound = genDecFixedBound

{-# INLINE word64DecFixedBound #-}
word64DecFixedBound :: Char -> Word64 -> FixedPrim Word64
word64DecFixedBound = genDecFixedBound

