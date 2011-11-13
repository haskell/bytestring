{-# LANGUAGE CPP, BangPatterns, MonoPatBinds, ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- |
Module      : Data.ByteString.Lazy.Builder.BasicEncoding
Copyright   : (c) 2010-2011 Simon Meier
            . (c) 2010      Jasper van der Jeugt
License     : BSD3-style (see LICENSE)

Maintainer  : Simon Meier <iridcode@gmail.com>
Stability   : experimental
Portability : tested on GHC only

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

The 'BoundedEncoding's provided by this module remove this performance problem.
Intuitively,
  they consist of a tuple of the bound on the maximal number of bytes written
  and the actual implementation of the encoding as
  a function that modifies a mutable buffer.
Hence when executing a 'BoundedEncoding',
 the buffer-full check can be done once before the actual writing to the buffer.
The provided 'BoundedEncoding's also take care to implement the
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
-- > import Data.ByteString.Lazy.Builder.BasicEncoding.Utf8 (char)
-- >
-- > {-# INLINE escapeChar #-}
-- > escapeUtf8 :: BoundedEncoding Char
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
-- > import qualified Codec.Bounded.Encoding as E
-- >
-- > filter :: (Word8 -> Bool) -> ByteString -> ByteString
-- > filter p = toLazyByteString . encodeLazyByteStringWithB write
-- >   where
-- >     write = E.encodeIf p E.word8 E.emptyEncoding
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
-- "Data.ByteString.Lazy.Builder".
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
-- > import Data.ByteString.Lazy.Builder.Extras     -- assume these three
-- > import Codec.Bounded.Encoding                  -- imports are present
-- >        ( BoundedEncoding, encodeIf, (<#>), (#.) )
-- > import Data.ByteString.Lazy.Builder.BasicEncoding.Utf8 (char)
-- >
-- > renderString :: String -> Builder
-- > renderString cs =
-- >     charUtf8 '"' <> encodeListWithB escapedUtf8 cs <> charUtf8 '"'
-- >   where
-- >     escapedUtf8 :: BoundedEncoding Char
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
in "Data.ByteString.Lazy.Builder.Extras". There, we continue the above example
and drop the generation time to 0.8ms by implementing 'renderString' more
cleverly. The third reduction requires meddling with the internals of
'Builder's and is not recomended in code outside of this library. However,
patches to this library are very welcome.
-}
module Data.ByteString.Lazy.Builder.BasicEncoding (

  -- * Fixed-size encodings
    FixedEncoding
  , size

  -- ** Combinators
  -- | The combinators for 'FixedEncoding's are implemented such that the 'size'
  -- of the resulting 'FixedEncoding' is computed at compile time.
  , emptyF
  , pairF
  , contramapF

  -- ** Builder construction
  -- | In terms of expressivity, the function 'encodeWithF' would be sufficient
  -- for constructing 'Builder's from 'FixedEncoding's. The fused variants of
  -- this function are provided because they allow for more efficient
  -- implementations. Our compilers are just not smart enough yet; and for some
  -- of the employed optimizations (see the code of 'encodeByteStringWithF')
  -- they will very likely never be.
  --
  -- Note that functions marked with \"/Heavy inlining./\" are forced to be
  -- inlined because they must be specialized for concrete encodings,
  -- but are rather heavy in terms of code size. We recommend to define a
  -- top-level function for every concrete instantiation of such a function in
  -- order to share its code. A typical example is the function
  -- 'byteStringHexFixed' from "Data.ByteString.Lazy.Builder.ASCII", which is
  -- implemented as follows.
  --
  -- @
  -- byteStringHexFixed :: S.ByteString -> Builder
  -- byteStringHexFixed = 'encodeByteStringWithF' 'word8HexFixed'
  -- @
  --
  , encodeWithF
  , encodeListWithF
  , encodeUnfoldrWithF

  , encodeByteStringWithF
  , encodeLazyByteStringWithF

  -- * Bounded-size encodings

  , BoundedEncoding
  , sizeBound

  -- ** Combinators
  -- | The combinators for 'BoundedEncoding's are implemented such that the
  -- 'sizeBound' of the resulting 'BoundedEncoding' is computed at compile time.
  , fromF
  , emptyB
  , pairB
  , eitherB
  , ifB
  , contramapB

  -- | We provide overloaded operators for some of the above combinators to
  -- allow for a more convenient syntax. We do not export their corresponding,
  -- as we they are used for overloading only and should not be extended by
  -- the user of this library. We plan to use the @contravariant@ library
  -- <http://hackage.haskell.org/package/contravariant> once it is part of the
  -- Haskell platform.
  , (>*<)
  , (>$<)

  -- ** Builder construction
  , encodeWithB
  , encodeListWithB
  , encodeUnfoldrWithB

  , encodeByteStringWithB
  , encodeLazyByteStringWithB

  -- * Standard encodings of Haskell values

  , module Data.ByteString.Lazy.Builder.BasicEncoding.Binary

  -- ** Character encodings
  , module Data.ByteString.Lazy.Builder.BasicEncoding.ASCII

  -- *** ISO/IEC 8859-1 (Char8)
  -- | The ISO/IEC 8859-1 encoding is an 8-bit encoding often known as Latin-1.
  -- The /Char8/ encoding implemented here works by truncating the Unicode
  -- codepoint to 8-bits and encoding them as a single byte. For the codepoints
  -- 0-255 this corresponds to the ISO/IEC 8859-1 encoding. Note that the
  -- Char8 encoding is equivalent to the ASCII encoding on the Unicode
  -- codepoints 0-127. Hence, functions such as 'intDec' can also be used for
  -- encoding 'Int's as a decimal number with Char8 encoded characters.
  , char8

  -- *** UTF-8
  -- | The UTF-8 encoding can encode all Unicode codepoints.
  -- It is equivalent to the ASCII encoding on the Unicode codepoints 0-127.
  -- Hence, functions such as 'intDec' can also be used for encoding 'Int's as
  -- a decimal number with UTF-8 encoded characters.
  , charUtf8

  -- * Chunked / size-prefixed encodings
{- |
Some encodings like ASN.1 BER <http://en.wikipedia.org/wiki/Basic_Encoding_Rules>
or Google's protocol buffers <http://code.google.com/p/protobuf/> require
encoded data to be prefixed with its length. The simple method to achieve this
is to encode the data first into a separate buffer, compute the length of the
encoded data, write it to the current output buffer, and append the separate
buffers. The drawback of this method is that it requires a ...
-}
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


  -- * Testing support
  -- | The following four functions are intended for testing use
  -- only. They are /not/ efficient. Basic encodings are efficently executed by
  -- creating 'Builder's from them using the @encodeXXX@ functions explained at
  -- the top of this module.

  , evalF
  , evalB

  , showF
  , showB

  ) where

import           Data.ByteString.Lazy.Builder.Internal
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal.UncheckedShifts
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal.Base16 (lowerTable, encode4_as_8)

import qualified Data.ByteString               as S
import qualified Data.ByteString.Internal      as S
import qualified Data.ByteString.Lazy.Internal as L

import           Data.Monoid
import           Data.List (unfoldr)  -- HADDOCK ONLY
import           Data.Char (chr, ord)
import           Control.Monad ((<=<), unless)

-- import Codec.Bounded.Encoding.Internal.Test
-- import Codec.Bounded.Encoding.Bench
import           Data.ByteString.Lazy.Builder.BasicEncoding.Internal hiding (size, sizeBound)
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as I (size, sizeBound)
import           Data.ByteString.Lazy.Builder.BasicEncoding.Binary
import           Data.ByteString.Lazy.Builder.BasicEncoding.ASCII

import           Foreign


------------------------------------------------------------------------------
-- Adapting 'size' for the public interface.
------------------------------------------------------------------------------

-- | The size of the sequence of bytes generated by this 'FixedEncoding'.
size :: FixedEncoding a -> Word
size = fromIntegral . I.size

-- | The bound on the size of the sequence of bytes generated by this
-- 'BoundedEncoding'.
sizeBound :: BoundedEncoding a -> Word
sizeBound = fromIntegral . I.sizeBound


------------------------------------------------------------------------------
-- Creating Builders from bounded encodings
------------------------------------------------------------------------------

-- | Encode a value with a 'FixedEncoding'.
{-# INLINE encodeWithF #-}
encodeWithF :: FixedEncoding a -> (a -> Builder)
encodeWithF = encodeWithB . toB

-- | Encode a list of values from left-to-right with a 'FixedEncoding'.
{-# INLINE encodeListWithF #-}
encodeListWithF :: FixedEncoding a -> ([a] -> Builder)
encodeListWithF = encodeListWithB . toB

-- | Encode a list of values represented as an 'unfoldr' with a 'FixedEncoding'.
{-# INLINE encodeUnfoldrWithF #-}
encodeUnfoldrWithF :: FixedEncoding b -> (a -> Maybe (b, a)) -> a -> Builder
encodeUnfoldrWithF = encodeUnfoldrWithB . toB

-- | /Heavy inlining./ Encode all bytes of a strict 'S.ByteString' from
-- left-to-right with a 'FixedEncoding'. This function is quite versatile. For
-- example, we can use it to construct a 'Builder' that maps every byte before
-- copying it to the buffer to be filled.
--
-- > mapToBuilder :: (Word8 -> Word8) -> S.ByteString -> Builder
-- > mapToBuilder f = encodeByteStringWithF (contramapF f word8)
--
-- We can also use it to hex-encode a strict 'S.ByteString' as shown by the
-- 'byteStringHexFixed' example above.
{-# INLINE encodeByteStringWithF #-}
encodeByteStringWithF :: FixedEncoding Word8 -> (S.ByteString -> Builder)
encodeByteStringWithF = encodeByteStringWithB . toB

-- | /Heavy inlining./ Encode all bytes of a lazy 'L.ByteString' from
-- left-to-right with a 'FixedEncoding'.
{-# INLINE encodeLazyByteStringWithF #-}
encodeLazyByteStringWithF :: FixedEncoding Word8 -> (L.ByteString -> Builder)
encodeLazyByteStringWithF = encodeLazyByteStringWithB . toB

-- IMPLEMENTATION NOTE: Sadly, 'encodeListWith' cannot be used for foldr/build
-- fusion. Its performance relies on hoisting several variables out of the
-- inner loop.  That's not possible when writing 'encodeListWith' as a 'foldr'.
-- If we had stream fusion for lists, then we could fuse 'encodeListWith', as
-- 'encodeWithStream' can keep control over the execution.


-- | Create a 'Builder' that encodes values with the given 'Encoding'.
--
-- We rewrite consecutive uses of 'encodeWith' such that the bound-checks are
-- fused. For example,
--
-- > encodeWithB (word32 c1) `mappend` encodeWithB (word32 c2)
--
-- is rewritten such that the resulting 'Builder' checks only once, if ther are
-- at 8 free bytes, instead of checking twice, if there are 4 free bytes. This
-- optimization is not observationally equivalent in a strict sense, as it
-- influences the boundaries of the generated chunks. However, for a user of
-- this library it is observationally equivalent, as chunk boundaries of a lazy
-- 'L.ByteString' can only be observed through the internal interface.
-- Morevoer, we expect that all 'Encoding's write much fewer than 4kb (the
-- default short buffer size). Hence, it is safe to ignore the additional
-- memory spilled due to the more agressive buffer wrapping introduced by this
-- optimization.
--
{-# INLINE[1] encodeWithB #-}
encodeWithB :: BoundedEncoding a -> (a -> Builder)
encodeWithB w =
    mkBuilder
  where
    bound = I.sizeBound w
    mkBuilder x = builder step
      where
        step k (BufferRange op ope)
          | op `plusPtr` bound <= ope = do
              op' <- runB w x op
              let !br' = BufferRange op' ope
              k br'
          | otherwise = return $ bufferFull bound op (step k)

{-# RULES

"append/encodeWithB" forall w1 w2 x1 x2.
       append (encodeWithB w1 x1) (encodeWithB w2 x2)
     = encodeWithB (pairB w1 w2) (x1, x2)

"append/encodeWithB/assoc_r" forall w1 w2 x1 x2 b.
       append (encodeWithB w1 x1) (append (encodeWithB w2 x2) b)
     = append (encodeWithB (pairB w1 w2) (x1, x2)) b

"append/encodeWithB/assoc_l" forall w1 w2 x1 x2 b.
       append (append b (encodeWithB w1 x1)) (encodeWithB w2 x2)
     = append b (encodeWithB (pairB w1 w2) (x1, x2))
  #-}

-- TODO: The same rules for 'putBuilder (..) >> putBuilder (..)'

-- | Create a 'Builder' that encodes a list of values consecutively using an
-- 'Encoding'. This function is more efficient than the canonical
--
-- > filter p =
-- >  B.toLazyByteString .
-- >  E.encodeLazyByteStringWithF (E.ifF p E.word8) E.emptyF)
-- >
--
-- > mconcat . map (encodeWithB w)
--
-- or
--
-- > foldMap (encodeWithB w)
--
-- because it moves several variables out of the inner loop.
{-# INLINE encodeListWithB #-}
encodeListWithB :: BoundedEncoding a -> [a] -> Builder
encodeListWithB w =
    makeBuilder
  where
    bound = I.sizeBound w
    makeBuilder xs0 = builder $ step xs0
      where
        step xs1 k !(BufferRange op0 ope0) = go xs1 op0
          where
            go [] !op = do
               let !br' = BufferRange op ope0
               k br'

            go xs@(x':xs') !op
              | op `plusPtr` bound <= ope0 = do
                  !op' <- runB w x' op
                  go xs' op'
             | otherwise = return $ bufferFull bound op (step xs k)

-- TODO: Add 'foldMap/encodeWith' its variants
-- TODO: Ensure rewriting 'encodeWithB w . f = encodeWithB (w #. f)'

-- | Create a 'Builder' that encodes a sequence generated from a seed value
-- using an 'Encoding'.
{-# INLINE encodeUnfoldrWithB #-}
encodeUnfoldrWithB :: BoundedEncoding b -> (a -> Maybe (b, a)) -> a -> Builder
encodeUnfoldrWithB w =
    makeBuilder
  where
    bound = I.sizeBound w
    makeBuilder f x0 = builder $ step x0
      where
        step x1 !k = fill x1
          where
            fill x !(BufferRange pf0 pe0) = go (f x) pf0
              where
                go !Nothing        !pf = do
                    let !br' = BufferRange pf pe0
                    k br'
                go !(Just (y, x')) !pf
                  | pf `plusPtr` bound <= pe0 = do
                      !pf' <- runB w y pf
                      go (f x') pf'
                  | otherwise = return $ bufferFull bound pf $
                      \(BufferRange pfNew peNew) -> do
                          !pfNew' <- runB w y pfNew
                          fill x' (BufferRange pfNew' peNew)

-- | Create a 'Builder' that encodes each 'Word8' of a strict 'S.ByteString'
-- using an 'Encoding'. For example, we can write a 'Builder' that filters
-- a strict 'S.ByteString' as follows.
--
-- > import Codec.Bounded.Encoding as E (encodeIf, word8, encodeNothing)
--
-- > filterBS p = E.encodeIf p E.word8 E.encodeNothing
--
{-# INLINE encodeByteStringWithB #-}
encodeByteStringWithB :: BoundedEncoding Word8 -> S.ByteString -> Builder
encodeByteStringWithB w =
    \bs -> builder $ step bs
  where
    bound = I.sizeBound w
    step (S.PS ifp ioff isize) !k =
        goBS (unsafeForeignPtrToPtr ifp `plusPtr` ioff)
      where
        !ipe = unsafeForeignPtrToPtr ifp `plusPtr` (ioff + isize)
        goBS !ip0 !br@(BufferRange op0 ope)
          | ip0 >= ipe = do
              touchForeignPtr ifp -- input buffer consumed
              k br

          | op0 `plusPtr` bound < ope =
              goPartial (ip0 `plusPtr` min outRemaining inpRemaining)

          | otherwise  = return $ bufferFull bound op0 (goBS ip0)
          where
            outRemaining = (ope `minusPtr` op0) `div` bound
            inpRemaining = ipe `minusPtr` ip0

            goPartial !ipeTmp = go ip0 op0
              where
                go !ip !op
                  | ip < ipeTmp = do
                      x   <- peek ip
                      op' <- runB w x op
                      go (ip `plusPtr` 1) op'
                  | otherwise =
                      goBS ip (BufferRange op ope)

-- | Chunk-wise application of 'encodeByteStringWith'.
{-# INLINE encodeLazyByteStringWithB #-}
encodeLazyByteStringWithB :: BoundedEncoding Word8 -> L.ByteString -> Builder
encodeLazyByteStringWithB w =
    L.foldrChunks (\x b -> encodeByteStringWithB w x `mappend` b) mempty

------------------------------------------------------------------------------
-- Chunked Encoding Transformer
------------------------------------------------------------------------------

-- | /Heavy inlining./
{-# INLINE encodeChunked #-}
encodeChunked
    :: Word                           -- ^ Minimal free-size
    -> (Word64 -> FixedEncoding Word64)
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedEncoding Word64)
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
    -> (Word64 -> FixedEncoding Word64)
    -- ^ Given a sizeBound on the maximal encodable size this function must return
    -- a fixed-size encoding for encoding all smaller size.
    -> (BoundedEncoding Word64)
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
                nU = fromIntegral n
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
    -> (Word64 -> FixedEncoding Word64)
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
    -> (Word64 -> FixedEncoding Word64)
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
                (x, lbsC, lenAfter) <- toLBS $ runBuilderWith bInner nextStep

                let curBufLen   = opInner' `minusPtr` startInner
                    lenAll      = fromIntegral lenAfter + fromIntegral curBufLen
                    sizeFE'     = mkSizeFE lenAll
                    startInner' = op `plusPtr` I.size sizeFE'
                -- move data in current buffer out of the way, if required
                unless (startInner == startInner') $
                    moveBytes startInner' startInner curBufLen
                runF sizeFE' lenAll op
                -- TODO: If we were to change the CIOS definition such that it also
                -- returns the last buffer for writing, we could also fill the
                -- last buffer with 'k' and return the signal, once it is
                -- filled, therefore avoiding unfilled space.
                return $ insertChunks (startInner' `plusPtr` curBufLen)
                                      (fromIntegral lenAll)
                                      lbsC
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
appsUntilZero :: Num a => (a -> a) -> a -> Int
appsUntilZero f x0 =
    count 0 x0
  where
    count !n 0 = n
    count !n x = count (succ n) (f x)


{-# INLINE genericVarFixedBound #-}
genericVarFixedBound :: (Bits b, Num a, Integral b)
                => (b -> a -> b) -> b -> FixedEncoding b
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
wordVarFixedBound :: Word -> FixedEncoding Word
wordVarFixedBound = genericVarFixedBound shiftr_w

{-# INLINE word64VarFixedBound #-}
word64VarFixedBound :: Word64 -> FixedEncoding Word64
word64VarFixedBound = genericVarFixedBound shiftr_w64


-- Somehow this function doesn't really make sense, as the bound must be
-- greater when interpreted as an unsigned integer. These conversions and
-- decisions should be left to the user.
--
--{-# INLINE intVarFixed #-}
--intVarFixed :: Size -> FixedEncoding Size
--intVarFixed bound = fromIntegral >$< wordVarFixed (fromIntegral bound)

{-# INLINE genHexFixedBound #-}
genHexFixedBound :: (Num a, Bits a, Integral a)
                 => (a -> Int -> a) -> Char -> a -> FixedEncoding a
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
wordHexFixedBound :: Char -> Word -> FixedEncoding Word
wordHexFixedBound = genHexFixedBound shiftr_w

{-# INLINE word64HexFixedBound #-}
word64HexFixedBound :: Char -> Word64 -> FixedEncoding Word64
word64HexFixedBound = genHexFixedBound shiftr_w64

-- | Note: Works only for positive numbers.
{-# INLINE genDecFixedBound #-}
genDecFixedBound :: (Num a, Bits a, Integral a)
                 => Char -> a -> FixedEncoding a
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
wordDecFixedBound :: Char -> Word -> FixedEncoding Word
wordDecFixedBound = genDecFixedBound

{-# INLINE word64DecFixedBound #-}
word64DecFixedBound :: Char -> Word64 -> FixedEncoding Word64
word64DecFixedBound = genDecFixedBound

------------------------------------------------------------------------------
-- Char8 encoding
------------------------------------------------------------------------------

-- | Char8 encode a 'Char'.
{-# INLINE char8 #-}
char8 :: FixedEncoding Char
char8 = (fromIntegral . ord) >$< word8


------------------------------------------------------------------------------
-- UTF-8 encoding
------------------------------------------------------------------------------

-- | UTF-8 encode a 'Char'.
{-# INLINE charUtf8 #-}
charUtf8 :: BoundedEncoding Char
charUtf8 = boundedEncoding 4 (encodeCharUtf8 f1 f2 f3 f4)
  where
    pokeN n io op  = io op >> return (op `plusPtr` n)

    f1 x1          = pokeN 1 $ \op -> do pokeByteOff op 0 x1

    f2 x1 x2       = pokeN 2 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2

    f3 x1 x2 x3    = pokeN 3 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3

    f4 x1 x2 x3 x4 = pokeN 4 $ \op -> do pokeByteOff op 0 x1
                                         pokeByteOff op 1 x2
                                         pokeByteOff op 2 x3
                                         pokeByteOff op 3 x4

-- | Encode a Unicode character to another datatype, using UTF-8. This function
-- acts as an abstract way of encoding characters, as it is unaware of what
-- needs to happen with the resulting bytes: you have to specify functions to
-- deal with those.
--
{-# INLINE encodeCharUtf8 #-}
encodeCharUtf8 :: (Word8 -> a)                             -- ^ 1-byte UTF-8
               -> (Word8 -> Word8 -> a)                    -- ^ 2-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> a)           -- ^ 3-byte UTF-8
               -> (Word8 -> Word8 -> Word8 -> Word8 -> a)  -- ^ 4-byte UTF-8
               -> Char                                     -- ^ Input 'Char'
               -> a                                        -- ^ Result
encodeCharUtf8 f1 f2 f3 f4 c = case ord c of
    x | x <= 0x7F -> f1 $ fromIntegral x
      | x <= 0x07FF ->
           let x1 = fromIntegral $ (x `shiftR` 6) + 0xC0
               x2 = fromIntegral $ (x .&. 0x3F)   + 0x80
           in f2 x1 x2
      | x <= 0xFFFF ->
           let x1 = fromIntegral $ (x `shiftR` 12) + 0xE0
               x2 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x3 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f3 x1 x2 x3
      | otherwise ->
           let x1 = fromIntegral $ (x `shiftR` 18) + 0xF0
               x2 = fromIntegral $ ((x `shiftR` 12) .&. 0x3F) + 0x80
               x3 = fromIntegral $ ((x `shiftR` 6) .&. 0x3F) + 0x80
               x4 = fromIntegral $ (x .&. 0x3F) + 0x80
           in f4 x1 x2 x3 x4


------------------------------------------------------------------------------
-- Testing encodings
------------------------------------------------------------------------------

-- | /For testing use only./ Evaluate a 'FixedEncoding' on a given value.
evalF :: FixedEncoding a -> a -> [Word8]
evalF fe = S.unpack . S.unsafeCreate (I.size fe) . runF fe

-- | /For testing use only./ Evaluate a 'BoundedEncoding' on a given value.
evalB :: BoundedEncoding a -> a -> [Word8]
evalB be x = S.unpack $ unsafePerformIO $
    S.createAndTrim (I.sizeBound be) $ \op -> do
        op' <- runB be x op
        return (op' `minusPtr` op)

-- | /For testing use only./ Show the result of a 'FixedEncoding' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showF :: FixedEncoding a -> a -> String
showF fe = map (chr . fromIntegral) . evalF fe

-- | /For testing use only./ Show the result of a 'BoundedEncoding' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showB :: BoundedEncoding a -> a -> String
showB be = map (chr . fromIntegral) . evalB be


