{-# LANGUAGE CPP #-}

-- |
-- Copyright   : (c) 2010-2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Running example for documentation of Data.ByteString.Builder
--

module BenchCSV (benchCSV) where

--  **************************************************************************
-- CamHac 2011: An introduction to Data.ByteString.Builder
--  **************************************************************************


{- The Encoding Problem
 ----------------------

 Encoding: Conversion from a Haskell value to a sequence of bytes.


 Efficient encoding implementation:

   1. represent sequence of bytes as a list of byte arrays (chunks)
   2. generate chunks that are large on average
   3. avoid intermediate copies/datastructures

 Compositionality:

   4. support fast append


 Problem: Provide a library for defining compositional, efficient encodings.

-}



{- Data.ByteString.Builder
 ------------------------------

 A solution to the "Encoding Problem"  (based on the code of blaze-builder).

 Builder creation:

   word8   :: Word8 -> Builder
   int64LE :: Int64 -> Builder
   floatBE :: Float -> Builder
   ....


 Builder composition via its Monoid instance:

   word8 10 `mappend` floatBE 1.4


 Builder execution by converting it to a lazy bytestring:

   toLazyByteString :: Builder -> L.ByteString

-}


{- Typical users of Builders
 ---------------------------

 binary, text, aeson, blaze-html, blaze-textual, warp, snap-server, ...

 => they want support for maximal performance!
 => use of Builders is rather local: in rendering/encoding functions.

-}



{- Notable properties
 --------------------

 * Built-in UTF-8 support: very hard to get efficient otherwise.

     stringUtf8 :: String -> Builder
     intDec :: Int -> Builder
     intHex :: Int -> Builder

 * Fine-grained control over when to copy/reference existing bytestrings

 * EDSL for defining low-level Encodings of bounded values (e.g., Int, Char)
   to improve speed of escaping and similar operations.

 * If used together with iteratee-style IO: no 'unsafePerformIO' required

-}


{- An example problem:
 ---------------------

 Rendering a table in comma-separated-value (CSV) format using UTF-8 encoded
 Unicode characters.

 * We are willing to fuse table-rendering with UTF8-encoding to achieve better
   performance.

-}

import Control.DeepSeq
import Data.Char (ord)
import Data.Foldable (foldMap)
import Data.Monoid

import Test.Tasty.Bench

import qualified Data.ByteString         as S
import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder as B
import           Data.ByteString.Builder.Prim.Internal ( (>*<), (>$<) )
import qualified Data.ByteString.Builder.Prim         as E

-- bytestring benchmarks cannot depend on text because of a circular dependency.
-- Anyways these comparisons are of historical interest only, so disabled for now.
-- A curious soul can re-enable them by moving benchmarks to a separate package
-- and adding text to build-depends.
#ifdef MIN_VERSION_text
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB
#endif

-- Same as above: comparison against DList is of historical interest now,
-- so lets shave off another dependency.
#ifdef MIN_VERSION_dlist
import qualified Data.DList                 as D
#endif

------------------------------------------------------------------------------
-- Simplife CSV Tables
------------------------------------------------------------------------------

data Cell = StringC String
          | IntC Int
          deriving( Eq, Ord, Show )

type Row   = [Cell]
type Table = [Row]

-- Example data
strings :: [String]
strings =  ["hello", "\"1\"", "λ-wörld"]

table :: Table
table = [map StringC strings, map IntC [-3..3]]


-- | The rendered 'table':
--
-- > "hello","\"1\"","λ-wörld"
-- > -3,-2,-1,0,1,2,3
--


-- | A bigger table for benchmarking our encoding functions.
maxiTable :: Table
maxiTable = take 1000 $ cycle table


------------------------------------------------------------------------------
-- String based rendering
------------------------------------------------------------------------------

renderString :: String -> String
renderString cs = "\"" ++ concatMap escape cs ++ "\""
  where
    escape '\\' = "\\"
    escape '\"' = "\\\""
    escape c    = return c

renderCell :: Cell -> String
renderCell (StringC cs) = renderString cs
renderCell (IntC i)     = show i

renderRow :: Row -> String
renderRow []     = ""
renderRow (c:cs) = renderCell c ++ concat [',' : renderCell c' | c' <- cs]

renderTable :: Table -> String
renderTable rs = concat [renderRow r ++ "\n" | r <- rs]

-- 1.36 ms
benchString :: Benchmark
benchString = bench "renderTable maxiTable" $ nf renderTable maxiTable

-- 1.36 ms
benchStringUtf8 :: Benchmark
benchStringUtf8 = bench "utf8 + renderTable maxiTable" $
  nf (L.length . B.toLazyByteString . B.stringUtf8 . renderTable) maxiTable


-- using difference lists:  0.91 ms
--
--  (++) is a performance-grinch!


------------------------------------------------------------------------------
-- Builder based rendering
------------------------------------------------------------------------------

-- As a reminder:
--
-- import  Data.ByteString.Builder       as B

renderStringB :: String -> Builder
renderStringB cs = B.charUtf8 '"' <> foldMap escape cs <> B.charUtf8 '"'
  where
    escape '\\' = B.charUtf8 '\\' <> B.charUtf8 '\\'
    escape '\"' = B.charUtf8 '\\' <> B.charUtf8 '"'
    escape c    = B.charUtf8 c

renderCellB :: Cell -> Builder
renderCellB (StringC cs) = renderStringB cs
renderCellB (IntC i)     = B.intDec i

renderRowB :: Row -> Builder
renderRowB []     = mempty
renderRowB (c:cs) =
    renderCellB c <> mconcat [ B.charUtf8 ',' <> renderCellB c' | c' <- cs ]

renderTableB :: Table -> Builder
renderTableB rs = mconcat [renderRowB r <> B.charUtf8 '\n' | r <- rs]

-- 0.81ms
benchBuilderUtf8 :: Benchmark
benchBuilderUtf8 = bench "utf8 + renderTableB maxiTable" $
  nf (L.length . B.toLazyByteString . renderTableB) maxiTable

-- 1.11x  faster than DList

-- However: touching the whole table 'nf maxiTable' takes  0.27ms

-- 1.16x  faster than DList on the code path other than touching all data
--        (0.91 - 0.27) / (0.82 - 0.27)


------------------------------------------------------------------------------
-- Baseline: Touching all data
------------------------------------------------------------------------------

instance NFData Cell where
  rnf (StringC cs) = rnf cs
  rnf (IntC i)     = rnf i

-- 0.27 ms
benchNF :: Benchmark
benchNF = bench "nf maxiTable" $ nf id maxiTable


------------------------------------------------------------------------------
-- Exploiting bounded encodings
------------------------------------------------------------------------------

{- Why 'Bounded Encodings'?
 --------------------------

 Hot code of encoding implementations:

 * Appending Builders: Optimized already.

 * Encoding primitive Haskell values: room for optimization:

     - reduce buffer-free checks
     - remove jumps/function calls
     - hoist constant values out of inner-loops
       (e.g., the loop for encoding the elements of a list)

 * Bounded encoding:
     an encoding that never takes more than a fixed number of bytes.

     - intuitively: (Int,     Ptr Word8 -> IO (Ptr Word8))
                     ^bound   ^ low-level encoding function

     - compositional: coalesce buffer-checks, ...

       E.encodeIfB :: (a -> Bool)
                   -> BoundedPrim a -> BoundedPrim a -> BoundedPrim a
       E.charUtf8  :: BoundedPrim Char
       (>*<)       :: BoundedPrim a -> BoundedPrim b -> BoundedPrim (a, b)

       (>$<)       :: (b -> a) -> BoundedPrim a -> BoundedPrim b

       ^ BoundedPrims are contrafunctors; like most data-sinks


     - Implementation relies heavily on inlining to compute bounds and
       low-level encoding code during compilation.
-}

renderStringBE :: String -> Builder
renderStringBE cs =
    B.charUtf8 '"' <> E.primMapListBounded escape cs <> B.charUtf8 '"'
  where
    escape :: E.BoundedPrim Char
    escape =
      E.condB (== '\\') (const ('\\', '\\') >$< E.charUtf8 >*< E.charUtf8) $
      E.condB (== '\"') (const ('\\', '\"') >$< E.charUtf8 >*< E.charUtf8) $
      E.charUtf8

renderCellBE :: Cell -> Builder
renderCellBE (StringC cs) = renderStringBE cs
renderCellBE (IntC i)     = B.intDec i

renderRowBE :: Row -> Builder
renderRowBE []     = mempty
renderRowBE (c:cs) =
    renderCellBE c <> mconcat [ B.charUtf8 ',' <> renderCellBE c' | c' <- cs ]

renderTableBE :: Table -> Builder
renderTableBE rs = mconcat [renderRowBE r <> B.charUtf8 '\n' | r <- rs]

-- 0.65 ms
benchBuilderEncodingUtf8 :: Benchmark
benchBuilderEncodingUtf8 = bench "utf8 + renderTableBE maxiTable" $
  nf (L.length . B.toLazyByteString . renderTableBE) maxiTable


-- 1.4x faster than DList based

-- 1.7x faster than DList based on code other than touching all data


------------------------------------------------------------------------------
-- Difference-list based rendering
------------------------------------------------------------------------------

#ifdef MIN_VERSION_dlist

type DString = D.DList Char

renderStringD :: String -> DString
renderStringD cs = return '"' <> foldMap escape cs <> return '"'
  where
    escape '\\' = D.fromList "\\\\"
    escape '\"' = D.fromList "\\\""
    escape c    = return c

renderCellD :: Cell -> DString
renderCellD (StringC cs) = renderStringD cs
renderCellD (IntC i)     = D.fromList $ show i

renderRowD :: Row -> DString
renderRowD []     = mempty
renderRowD (c:cs) =
    renderCellD c <> mconcat [ return ',' <> renderCellD c' | c' <- cs ]

renderTableD :: Table -> DString
renderTableD rs = mconcat [renderRowD r <> return '\n' | r <- rs]

-- 0.91 ms
benchDListUtf8 :: Benchmark
benchDListUtf8 = bench "utf8 + renderTableD maxiTable" $
  nf (L.length . B.toLazyByteString . B.stringUtf8 . D.toList . renderTableD) maxiTable

#endif

------------------------------------------------------------------------------
-- Text Builder
------------------------------------------------------------------------------

#ifdef MIN_VERSION_text

renderStringTB :: String -> TB.Builder
renderStringTB cs = TB.singleton '"' <> foldMap escape cs <> TB.singleton '"'
  where
    escape '\\' = "\\\\"
    escape '\"' = "\\\""
    escape c    = TB.singleton c

renderCellTB :: Cell -> TB.Builder
renderCellTB (StringC cs) = renderStringTB cs
renderCellTB (IntC i)     = TB.decimal i

renderRowTB :: Row -> TB.Builder
renderRowTB []     = mempty
renderRowTB (c:cs) =
    renderCellTB c <> mconcat [ TB.singleton ',' <> renderCellTB c' | c' <- cs ]

renderTableTB :: Table -> TB.Builder
renderTableTB rs = mconcat [renderRowTB r <> TB.singleton '\n' | r <- rs]

-- 0.95 ms
benchTextBuilder :: Benchmark
benchTextBuilder = bench "renderTableTB maxiTable" $
  nf (TL.length . TB.toLazyText . renderTableTB) maxiTable

-- 1.10 ms
benchTextBuilderUtf8 :: Benchmark
benchTextBuilderUtf8 = bench "utf8 + renderTableTB maxiTable" $
  nf (L.length . TL.encodeUtf8 . TB.toLazyText . renderTableTB) maxiTable

#endif

------------------------------------------------------------------------------
-- Benchmarking
------------------------------------------------------------------------------

benchCSV :: Benchmark
benchCSV = bgroup "CSV"
      [ benchNF
      , benchString
      , benchStringUtf8
#ifdef MIN_VERSION_dlist
      , benchDListUtf8
#endif
#ifdef MIN_VERSION_text
      , benchTextBuilder
      , benchTextBuilderUtf8
#endif
      , benchBuilderUtf8
      , benchBuilderEncodingUtf8
      ]
  where
    encodeUtf8CSV = B.toLazyByteString . renderTableBE


{- On a Core 2 Duo 2.2 GHz running a 32-bit Linux:


touching all data:                 0.25 ms
string rendering:                  1.36 ms
string rendering + utf8 encoding:  1.36 ms
DList rendering  + utf8 encoding:  0.91 ms
builder rendering (incl. utf8):    0.82 ms
builder + faster escaping:         0.65 ms

text builder:                      0.95 ms
text builder + utf8 encoding:      1.10 ms
binary builder + char8 (!!):       1.22 ms
DList render + utf8-light:         4.12 ms

How to improve further?
  * Use packed formats for string literals
    - fast memcpy  (that's what blaze-html does for tags)
    - using Text literals should also help


results from criterion:

benchmarking nf maxiTable
mean: 257.2927 us, lb 255.9210 us, ub 259.6692 us, ci 0.950
std dev: 9.026280 us, lb 5.887942 us, ub 12.76582 us, ci 0.950

benchmarking renderTable maxiTable
mean: 1.358458 ms, lb 1.356732 ms, ub 1.362377 ms, ci 0.950
std dev: 12.66932 us, lb 7.110377 us, ub 24.97397 us, ci 0.950

benchmarking utf8 + renderTable maxiTable
mean: 1.364343 ms, lb 1.362391 ms, ub 1.366973 ms, ci 0.950
std dev: 11.65388 us, lb 9.094074 us, ub 17.47765 us, ci 0.950

benchmarking utf8 + renderTableD maxiTable
mean: 909.5255 us, lb 908.0049 us, ub 911.7639 us, ci 0.950
std dev: 9.434182 us, lb 6.906120 us, ub 15.43223 us, ci 0.950

benchmarking utf8-light + renderTable maxiTable
mean: 4.128315 ms, lb 4.121109 ms, ub 4.138436 ms, ci 0.950
std dev: 42.93755 us, lb 32.58115 us, ub 58.61780 us, ci 0.950

benchmarking char8 + renderTableBinB maxiTable
mean: 1.224156 ms, lb 1.222510 ms, ub 1.226101 ms, ci 0.950
std dev: 9.046150 us, lb 7.568433 us, ub 11.74996 us, ci 0.950

benchmarking renderTableTB maxiTable
mean: 954.8066 us, lb 953.6650 us, ub 957.0134 us, ci 0.950
std dev: 7.763098 us, lb 5.072194 us, ub 14.09216 us, ci 0.950

benchmarking utf8 + renderTableTB maxiTable
mean: 1.095913 ms, lb 1.094811 ms, ub 1.098280 ms, ci 0.950
std dev: 7.865781 us, lb 4.189907 us, ub 15.24606 us, ci 0.950

benchmarking utf8 + renderTableB maxiTable
mean: 818.0223 us, lb 816.5118 us, ub 819.9397 us, ci 0.950
std dev: 8.603917 us, lb 6.764347 us, ub 12.29236 us, ci 0.950

benchmarking utf8 + renderTableBE maxiTable
mean: 646.5248 us, lb 645.3735 us, ub 648.2405 us, ci 0.950
std dev: 7.147889 us, lb 5.222494 us, ub 11.82482 us, ci 0.950

-}



{- Conclusion:
 -------------

 * Whenever generating a sequence of bytes: use the 'Builder' type

   => chunks can always be kept large; impossible when exporting only
      a strict/lazy bytestring interface.

   => filtering/mapping lazy bytestrings now automatically defragments
      the output and guarantees a large chunk size.


 * Status of work: API complete, documentation needs more reviewing.


 * Bounded encodings: safely exploiting low-level optimizations

   => a performance advantage on other outputstream-libraries?


                           ---------------
                           - Questions ? -
                           ---------------

-}




{- Implementation outline:
 ------------------------

data BufferRange = BufferRange {-# UNPACK #-} !(Ptr Word8)  -- First byte of range
                               {-# UNPACK #-} !(Ptr Word8)  -- First byte /after/ range

newtype BuildStep a =
    BuildStep { runBuildStep :: BufferRange -> IO (BuildSignal a) }

data BuildSignal a =
    Done             !(Ptr Word8)    -- next free byte in current buffer
                     a               -- return value
  | BufferFull
                     !Int            -- minimal size of next buffer
                     !(Ptr Word8)    -- next free byte in current buffer
                     !(BuildStep a)  -- continuation to call on next buffer
  | InsertByteString
                     !(Ptr Word8)    -- next free byte in current buffer
                     !S.ByteString   -- bytestring to insert directly
                     !(BuildStep a)  -- continuation to call on next buffer


-- | A "difference list" of build-steps.
newtype Builder = Builder (forall r. BuildStep r -> BuildStep r)


-- | The corresponding "Writer" monad.
newtype Put a = Put { unPut :: forall r. (a -> BuildStep r) -> BuildStep r }


-}
