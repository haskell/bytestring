{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{- | Copyright : (c) 2010-2011 Simon Meier
                   (c) 2010      Jasper van der Jeugt
License        : BSD3-style (see LICENSE)
Maintainer     : Simon Meier <iridcode@gmail.com>
Portability    : GHC

This module provides the types of fixed-size and bounded-size encodings,
  which are the basic building blocks for constructing 'Builder's.
These types are used to achieve
  application-specific performance improvements of 'Builder's.

/Fixed(-size) encodings/ are encodings that always result in a sequence of bytes
  of a predetermined, fixed length.
An example for a fixed encoding is the big-endian encoding of a 'Word64',
  which always results in exactly 8 bytes.
/Bounded(-size) encodings/ are encodings that always result in a sequence
  of bytes that is no larger than a predetermined bound.
An example for a bounded encoding is the UTF-8 encoding of a 'Char',
  which results always in less or equal to 4 bytes.
Note that every fixed encoding is also a bounded encoding.
In the following, we therefore only refer to fixed encodings,
  where it matters that the resulting sequence of bytes is of a
  of a predetermined, fixed length.
Otherwise, we just refer to bounded encodings.

As said,
  the goal of bounded encodings is to improve the performance of 'Builder's.
These improvements stem from making the two
  most common steps performed by a 'Builder' more efficient.
We explain these two steps in turn.

The first most common step is the concatentation of two 'Builder's.
Internally,
  concatentation corresponds to function composition.
(Note that 'Builder's can be seen as difference-lists
  of buffer-filling functions;
  cf.  <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/dlist>.
)
Function composition is a fast /O(1)/ operation.
However,
  we can use bounded encodings to
  remove some of these function compositions altoghether,
  which is obviously more efficient.

The second most common step performed by a 'Builder' is to fill a buffer
  using a bounded encoding,
  which works as follows.
The 'Builder' checks whether there is enough space left to
  execute the bounded encoding.
If there is, then the 'Builder' executes the bounded encoding
  and calls the next 'Builder' with the updated buffer.
Otherwise,
  the 'Builder' signals its driver that it requires a new buffer.
This buffer must be at least as large as the bound of the encoding.
We can use bounded encodings to reduce the number of buffer-free
  checks by fusing the buffer-free checks of consecutive
  'Builder's.
We can also use bounded encodings to simplify the control flow
  for signalling that a buffer is full by
  ensuring that we check first that there is enough space left
  and only then decide on how to encode a given value.

Let us illustrate these improvements on the
  CSV-table rendering example from "Data.ByteString.Builder".
Its \"hot code\" is the rendering of a table's cells,
  which we implement as follows using only the functions from the
  'Builder' API.

@
import "Data.ByteString.Builder" as B

renderCell :: Cell -> Builder
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = B.intDec i

renderString :: String -> Builder
renderString cs = B.charUtf8 \'\"\' \<\> foldMap escape cs \<\> B.charUtf8 \'\"\'
  where
    escape \'\\\\\' = B.charUtf8 \'\\\\\' \<\> B.charUtf8 \'\\\\\'
    escape \'\\\"\' = B.charUtf8 \'\\\\\' \<\> B.charUtf8 \'\\\"\'
    escape c    = B.charUtf8 c
@

Efficient encoding of 'Int's as decimal numbers is performed by @intDec@.
Optimization potential exists for the escaping of 'String's.
The above implementation has two optimization opportunities.
First,
  the buffer-free checks of the 'Builder's for escaping doublequotes
  and backslashes can be fused.
Second,
  the concatenations performed by 'foldMap' can be eliminated.
The following implementation exploits these optimizations.

@
import qualified Data.ByteString.Builder.Prim  as E
import           Data.ByteString.Builder.Prim
                 ( 'ifB', 'fromF', ('>*<'), ('>$<') )

renderString :: String -\> Builder
renderString cs =
    B.charUtf8 \'\"\' \<\> E.'encodeListWithB' escape cs \<\> B.charUtf8 \'\"\'
  where
    escape :: E.'BoundedPrim' Char
    escape =
      'ifB' (== \'\\\\\') (fixed2 (\'\\\\\', \'\\\\\')) $
      'ifB' (== \'\\\"\') (fixed2 (\'\\\\\', \'\\\"\')) $
      E.'charUtf8'
    &#160;
    {&#45;\# INLINE fixed2 \#&#45;}
    fixed2 x = 'fromF' $ const x '>$<' E.'char7' '>*<' E.'char7'
@

The code should be mostly self-explanatory.
The slightly awkward syntax is because the combinators
  are written such that the size-bound of the resulting 'BoundedPrim'
  can be computed at compile time.
We also explicitly inline the 'fixed2' encoding,
  which encodes a fixed tuple of characters,
  to ensure that the bound compuation happens at compile time.
When encoding the following list of 'String's,
  the optimized implementation of 'renderString' is two times faster.

@
maxiStrings :: [String]
maxiStrings = take 1000 $ cycle [\"hello\", \"\\\"1\\\"\", \"&#955;-w&#246;rld\"]
@

Most of the performance gain stems from using 'encodeListWithB',
  which encodes a list of values from left-to-right with a
  'BoundedPrim'.
It exploits the 'Builder' internals to avoid unnecessary function
  compositions (i.e., concatentations).
In the future,
  we would expect the compiler to perform the optimizations
  implemented in 'encodeListWithB'.
However,
  it seems that the code is currently to complicated for the
  compiler to see through.
Therefore,
  we provide the 'BoundedPrim' escape hatch,
  which allows data structures to provide very efficient encoding traversals,
  like 'encodeListWithB' for lists.

Note that 'BoundedPrim's are a bit verbose, but quite versatile.
Here is an example of a 'BoundedPrim' for combined HTML escapng and
UTF-8 encoding.
It exploits that the escaped character with the maximal Unicode
  codepoint is \'>\'.

@
{&#45;\# INLINE charUtf8HtmlEscaped \#&#45;}
charUtf8HtmlEscaped :: E.BoundedPrim Char
charUtf8HtmlEscaped =
    'ifB' (>  \'\>\' ) E.'charUtf8' $
    'ifB' (== \'\<\' ) (fixed4 (\'&\',(\'l\',(\'t\',\';\')))) $        -- &lt;
    'ifB' (== \'\>\' ) (fixed4 (\'&\',(\'g\',(\'t\',\';\')))) $        -- &gt;
    'ifB' (== \'&\' ) (fixed5 (\'&\',(\'a\',(\'m\',(\'p\',\';\'))))) $  -- &amp;
    'ifB' (== \'\"\' ) (fixed5 (\'&\',(\'\#\',(\'3\',(\'4\',\';\'))))) $  -- &\#34;
    'ifB' (== \'\\\'\') (fixed5 (\'&\',(\'\#\',(\'3\',(\'9\',\';\'))))) $  -- &\#39;
    ('fromF' E.'char7')         -- fallback for 'Char's smaller than \'\>\'
  where
    {&#45;\# INLINE fixed4 \#&#45;}
    fixed4 x = 'fromF' $ const x '>$<'
      E.char7 '>*<' E.char7 '>*<' E.char7 '>*<' E.char7
    &#160;
    {&#45;\# INLINE fixed5 \#&#45;}
    fixed5 x = 'fromF' $ const x '>$<'
      E.char7 '>*<' E.char7 '>*<' E.char7 '>*<' E.char7 '>*<' E.char7
@

This module currently does not expose functions that require the special
  properties of fixed-size encodings.
They are useful for prefixing 'Builder's with their size or for
  implementing chunked encodings.
We will expose the corresponding functions in future releases of this
  library.
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
module Data.ByteString.Builder.Prim (

  -- * Bounded-size primitives

    BoundedPrim

  -- ** Combinators
  -- | The combinators for 'BoundedPrim's are implemented such that the
  -- 'sizeBound' of the resulting 'BoundedPrim' is computed at compile time.
  , emptyB
  , (>*<)
  , (>$<)
  , eitherB
  , condB

  -- ** Builder construction
  , primBounded
  , primMapListBounded
  , primUnfoldrBounded

  , primMapByteStringBounded
  , primMapLazyByteStringBounded

  -- * Fixed-size primitives
  , FixedPrim

  -- ** Combinators
  -- | The combinators for 'FixedPrim's are implemented such that the 'size'
  -- of the resulting 'FixedPrim' is computed at compile time.
  --
  -- The '(>*<)' and '(>$<)' pairing and mapping operators can be used
  -- with 'FixedPrim'.
  , emptyF
  , liftFixedToBounded

  -- ** Builder construction
  -- | In terms of expressivity, the function 'fixedPrim' would be sufficient
  -- for constructing 'Builder's from 'FixedPrim's. The fused variants of
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
  -- 'byteStringHex' from "Data.ByteString.Builder.ASCII", which is
  -- implemented as follows.
  --
  -- @
  -- byteStringHex :: S.ByteString -> Builder
  -- byteStringHex = 'encodeByteStringWithF' 'word8HexFixed'
  -- @
  --
  , primFixed
  , primMapListFixed
  , primUnfoldrFixed

  , primMapByteStringFixed
  , primMapLazyByteStringFixed

  -- * Standard encodings of Haskell values

  , module Data.ByteString.Builder.Prim.Binary

  -- ** Character encodings
  , module Data.ByteString.Builder.Prim.ASCII

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

{-
  -- * Testing support
  -- | The following four functions are intended for testing use
  -- only. They are /not/ efficient. Basic encodings are efficently executed by
  -- creating 'Builder's from them using the @encodeXXX@ functions explained at
  -- the top of this module.

  , evalF
  , evalB

  , showF
  , showB
-}
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

#if MIN_VERSION_base(4,4,0)
import           Foreign hiding (unsafePerformIO, unsafeForeignPtrToPtr)
import           Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import           System.IO.Unsafe (unsafePerformIO)
#else
import           Foreign
#endif

------------------------------------------------------------------------------
-- Creating Builders from bounded encodings
------------------------------------------------------------------------------

-- | Encode a value with a 'FixedPrim'.
{-# INLINE primFixed #-}
primFixed :: FixedPrim a -> (a -> Builder)
primFixed = primBounded . toB

-- | Encode a list of values from left-to-right with a 'FixedPrim'.
{-# INLINE primMapListFixed #-}
primMapListFixed :: FixedPrim a -> ([a] -> Builder)
primMapListFixed = primMapListBounded . toB

-- | Encode a list of values represented as an 'unfoldr' with a 'FixedPrim'.
{-# INLINE primUnfoldrFixed #-}
primUnfoldrFixed :: FixedPrim b -> (a -> Maybe (b, a)) -> a -> Builder
primUnfoldrFixed = primUnfoldrBounded . toB

-- | /Heavy inlining./ Encode all bytes of a strict 'S.ByteString' from
-- left-to-right with a 'FixedPrim'. This function is quite versatile. For
-- example, we can use it to construct a 'Builder' that maps every byte before
-- copying it to the buffer to be filled.
--
-- > mapToBuilder :: (Word8 -> Word8) -> S.ByteString -> Builder
-- > mapToBuilder f = encodeByteStringWithF (contramapF f word8)
--
-- We can also use it to hex-encode a strict 'S.ByteString' as shown by the
-- 'byteStringHex' example above.
{-# INLINE primMapByteStringFixed #-}
primMapByteStringFixed :: FixedPrim Word8 -> (S.ByteString -> Builder)
primMapByteStringFixed = primMapByteStringBounded . toB

-- | /Heavy inlining./ Encode all bytes of a lazy 'L.ByteString' from
-- left-to-right with a 'FixedPrim'.
{-# INLINE primMapLazyByteStringFixed #-}
primMapLazyByteStringFixed :: FixedPrim Word8 -> (L.ByteString -> Builder)
primMapLazyByteStringFixed = primMapLazyByteStringBounded . toB

-- IMPLEMENTATION NOTE: Sadly, 'encodeListWith' cannot be used for foldr/build
-- fusion. Its performance relies on hoisting several variables out of the
-- inner loop.  That's not possible when writing 'encodeListWith' as a 'foldr'.
-- If we had stream fusion for lists, then we could fuse 'encodeListWith', as
-- 'encodeWithStream' can keep control over the execution.


-- | Create a 'Builder' that encodes values with the given 'Encoding'.
--
-- We rewrite consecutive uses of 'primBounded' such that the bound-checks are
-- fused. For example,
--
-- > primBounded (word32 c1) `mappend` primBounded (word32 c2)
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
{-# INLINE[1] primBounded #-}
primBounded :: BoundedPrim a -> (a -> Builder)
primBounded w =
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

"append/primBounded" forall w1 w2 x1 x2.
       append (primBounded w1 x1) (primBounded w2 x2)
     = primBounded (pairB w1 w2) (x1, x2)

"append/primBounded/assoc_r" forall w1 w2 x1 x2 b.
       append (primBounded w1 x1) (append (primBounded w2 x2) b)
     = append (primBounded (pairB w1 w2) (x1, x2)) b

"append/primBounded/assoc_l" forall w1 w2 x1 x2 b.
       append (append b (primBounded w1 x1)) (primBounded w2 x2)
     = append b (primBounded (pairB w1 w2) (x1, x2))
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
-- > mconcat . map (primBounded w)
--
-- or
--
-- > foldMap (primBounded w)
--
-- because it moves several variables out of the inner loop.
{-# INLINE primMapListBounded #-}
primMapListBounded :: BoundedPrim a -> [a] -> Builder
primMapListBounded w =
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
-- TODO: Ensure rewriting 'primBounded w . f = primBounded (w #. f)'

-- | Create a 'Builder' that encodes a sequence generated from a seed value
-- using an 'Encoding'.
{-# INLINE primUnfoldrBounded #-}
primUnfoldrBounded :: BoundedPrim b -> (a -> Maybe (b, a)) -> a -> Builder
primUnfoldrBounded w =
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
{-# INLINE primMapByteStringBounded #-}
primMapByteStringBounded :: BoundedPrim Word8 -> S.ByteString -> Builder
primMapByteStringBounded w =
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

-- | Chunk-wise application of 'primMapByteStringBounded'.
{-# INLINE primMapLazyByteStringBounded #-}
primMapLazyByteStringBounded :: BoundedPrim Word8 -> L.ByteString -> Builder
primMapLazyByteStringBounded w =
    L.foldrChunks (\x b -> primMapByteStringBounded w x `mappend` b) mempty


------------------------------------------------------------------------------
-- Char8 encoding
------------------------------------------------------------------------------

-- | Char8 encode a 'Char'.
{-# INLINE char8 #-}
char8 :: FixedPrim Char
char8 = (fromIntegral . ord) >$< word8


------------------------------------------------------------------------------
-- UTF-8 encoding
------------------------------------------------------------------------------

-- | UTF-8 encode a 'Char'.
{-# INLINE charUtf8 #-}
charUtf8 :: BoundedPrim Char
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
{-
-- | /For testing use only./ Evaluate a 'FixedPrim' on a given value.
evalF :: FixedPrim a -> a -> [Word8]
evalF fe = S.unpack . S.unsafeCreate (I.size fe) . runF fe

-- | /For testing use only./ Evaluate a 'BoundedPrim' on a given value.
evalB :: BoundedPrim a -> a -> [Word8]
evalB be x = S.unpack $ unsafePerformIO $
    S.createAndTrim (I.sizeBound be) $ \op -> do
        op' <- runB be x op
        return (op' `minusPtr` op)

-- | /For testing use only./ Show the result of a 'FixedPrim' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showF :: FixedPrim a -> a -> String
showF fe = map (chr . fromIntegral) . evalF fe

-- | /For testing use only./ Show the result of a 'BoundedPrim' of a given
-- value as a 'String' by interpreting the resulting bytes as Unicode
-- codepoints.
showB :: BoundedPrim a -> a -> String
showB be = map (chr . fromIntegral) . evalB be
-}

