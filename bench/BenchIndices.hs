-- |
-- Copyright   : (c) 2020 Peter Duchovni
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Peter Duchovni <caufeminecraft+github@gmail.com>
--
-- Benchmark elemIndex, findIndex, elemIndices, and findIndices

{-# LANGUAGE BangPatterns        #-}

module BenchIndices (benchIndices) where

import           Data.Foldable                         (foldMap)
import           Data.Maybe                            (listToMaybe)
import           Data.Monoid
import           Data.String
import           Test.Tasty.Bench
import           Prelude                               hiding (words)
import           Data.Word                             (Word8)

import qualified Data.ByteString                       as S
import qualified Data.ByteString.Unsafe                as S


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- ASCII \n to ensure no typos
nl :: Word8
nl = 0xa
{-# INLINE nl #-}

-- non-inlined equality test
nilEq :: Word8 -> Word8 -> Bool
{-# NOINLINE nilEq #-}
nilEq = (==)

-- lines of 200 letters from a to e, followed by repeated letter f
absurdlong :: S.ByteString
absurdlong = S.replicate 200 0x61 <> S.singleton nl
          <> S.replicate 200 0x62 <> S.singleton nl
          <> S.replicate 200 0x63 <> S.singleton nl
          <> S.replicate 200 0x64 <> S.singleton nl
          <> S.replicate 200 0x65 <> S.singleton nl
          <> S.replicate 999999 0x66

benchIndices :: Benchmark
benchIndices = bgroup "Indices"
    [ bgroup "ByteString strict first index" $
        [ bench "FindIndices" $ nf (listToMaybe . S.findIndices (== nl)) absurdlong
        , bench "ElemIndices" $ nf (listToMaybe . S.elemIndices     nl)  absurdlong
        , bench "FindIndex"   $ nf (S.findIndex (== nl)) absurdlong
        , bench "ElemIndex"   $ nf (S.elemIndex     nl)  absurdlong
        ]
    , bgroup "ByteString strict second index" $
        [ bench "FindIndices" $ nf (listToMaybe . tail . S.findIndices (== nl)) absurdlong
        , bench "ElemIndices" $ nf (listToMaybe . tail . S.elemIndices     nl)  absurdlong
        , bench "FindIndex"   $ nf bench_find_index_second absurdlong
        , bench "ElemIndex"   $ nf bench_elem_index_second absurdlong
        ]
    , bgroup "ByteString index equality inlining" $
        [ bench "FindIndices/inlined"     $ nf (S.findIndices    (== nl)) absurdlong
        , bench "FindIndices/non-inlined" $ nf (S.findIndices (nilEq nl)) absurdlong
        , bench "FindIndex/inlined"       $ nf (S.findIndex      (== nl)) absurdlong
        , bench "FindIndex/non-inlined"   $ nf (S.findIndex   (nilEq nl)) absurdlong
        ]
    ]

bench_find_index_second :: S.ByteString -> Maybe Int
bench_find_index_second bs =
  let isNl = (== nl)
   in case S.findIndex isNl bs of
        Just !i -> S.findIndex isNl (S.unsafeDrop (i+1) bs)
        Nothing -> Nothing
{-# INLINE bench_find_index_second #-}

bench_elem_index_second :: S.ByteString -> Maybe Int
bench_elem_index_second bs =
    case S.elemIndex nl bs of
        Just !i -> S.elemIndex nl (S.unsafeDrop (i+1) bs)
        Nothing -> Nothing
{-# INLINE bench_elem_index_second #-}
