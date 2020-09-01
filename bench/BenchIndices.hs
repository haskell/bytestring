{-# LANGUAGE BangPatterns        #-}
-- |
-- Copyright   : (c) 2020 Peter Duchovni
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Peter Duchovni <caufeminecraft+github@gmail.com>
--
-- Benchmark elemIndex, findIndex, elemIndices, and findIndices
module Main (main) where

import           Data.Foldable                         (foldMap)
import           Data.Monoid
import           Data.String
import           Gauge
import           Prelude                               hiding (words)
import           Data.Word                             (Word8)

import qualified Data.ByteString                       as S
import qualified Data.ByteString.Unsafe                as S


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------



-- lines of 200 letters from a to e, followed by repeated letter f
absurdlong :: S.ByteString
absurdlong = S.replicate 200 0x61 <> S.singleton 0xa
          <> S.replicate 200 0x62 <> S.singleton 0xa
          <> S.replicate 200 0x63 <> S.singleton 0xa
          <> S.replicate 200 0x64 <> S.singleton 0xa
          <> S.replicate 200 0x65 <> S.singleton 0xa
          <> S.replicate 999999 0x66

main :: IO ()
main = do
  Gauge.defaultMain
    [ bgroup "ByteString strict first index" $
        [ bench "FindIndices" $ nf bench_find_indices_first absurdlong
        , bench "ElemIndices" $ nf bench_elem_indices_first absurdlong
        , bench "FindIndex"   $ nf bench_find_index_first   absurdlong
        , bench "ElemIndex"   $ nf bench_elem_index_first   absurdlong
        ]
    , bgroup "ByteString strict second index" $
        [ bench "FindIndices" $ nf bench_find_indices_second absurdlong
        , bench "ElemIndices" $ nf bench_elem_indices_second absurdlong
        , bench "FindIndex"   $ nf bench_find_index_second   absurdlong
        , bench "ElemIndex"   $ nf bench_elem_index_second   absurdlong
        ]
    , bgroup "ByteString index equality inlining" $
        [ bench "FindIndices/inlined"     $ nf bench_find_indices_inline   absurdlong
        , bench "FindIndices/non-inlined" $ nf bench_find_indices_noinline absurdlong
        , bench "FindIndex/inlined"       $ nf bench_find_index_inline     absurdlong
        , bench "FindIndex/non-inlined"   $ nf bench_find_index_noinline   absurdlong
        ]
    ]

safeHead :: [Int] -> Maybe Int
safeHead (!x:_) = Just x
safeHead _ = Nothing
{-# INLINE safeHead #-}

bench_find_indices :: S.ByteString -> [Int]
bench_find_indices = S.findIndices (== 0xa)
{-# INLINE bench_find_indices #-}

bench_elem_indices :: S.ByteString -> [Int]
bench_elem_indices = S.elemIndices 0xa
{-# INLINE bench_elem_indices #-}

bench_find_index_first :: S.ByteString -> Maybe Int
bench_find_index_first = S.findIndex (== 0xa)
{-# INLINE bench_find_index_first #-}

bench_elem_index_first :: S.ByteString -> Maybe Int
bench_elem_index_first = S.elemIndex 0xa
{-# INLINE bench_elem_index_first #-}

bench_find_indices_first  :: S.ByteString -> Maybe Int
bench_find_indices_first = safeHead . bench_find_indices
{-# INLINE bench_find_indices_first #-}

bench_elem_indices_first :: S.ByteString -> Maybe Int
bench_elem_indices_first = safeHead . bench_elem_indices
{-# INLINE bench_elem_indices_first #-}



bench_find_index_second :: S.ByteString -> Maybe Int
bench_find_index_second bs =
  let isNl = (== 0xa)
   in case S.findIndex isNl bs of
        Just !i -> S.findIndex isNl (S.unsafeDrop (i+1) bs)
        Nothing -> Nothing
{-# INLINE bench_find_index_second #-}

bench_elem_index_second :: S.ByteString -> Maybe Int
bench_elem_index_second bs =
  let nl = 0xa
   in case S.elemIndex nl bs of
        Just !i -> S.elemIndex nl (S.unsafeDrop (i+1) bs)
        Nothing -> Nothing
{-# INLINE bench_elem_index_second #-}

bench_find_indices_second  :: S.ByteString -> Maybe Int
bench_find_indices_second = safeHead . tail . bench_find_indices
{-# INLINE bench_find_indices_second #-}

bench_elem_indices_second :: S.ByteString -> Maybe Int
bench_elem_indices_second = safeHead . tail . bench_elem_indices
{-# INLINE bench_elem_indices_second #-}

nilEq :: Word8 -> Word8 -> Bool
{-# NOINLINE nilEq #-}
nilEq = (==)

bench_find_indices_inline :: S.ByteString -> [Int]
bench_find_indices_inline = S.findIndices (== 0xa)
{-# INLINE bench_find_indices_inline #-}

bench_find_index_inline :: S.ByteString -> Maybe Int
bench_find_index_inline = S.findIndex (== 0xa)
{-# INLINE bench_find_index_inline #-}

bench_find_indices_noinline :: S.ByteString -> [Int]
bench_find_indices_noinline = S.findIndices (nilEq 0xa)
{-# INLINE bench_find_indices_noinline #-}

bench_find_index_noinline :: S.ByteString -> Maybe Int
bench_find_index_noinline = S.findIndex (nilEq 0xa)
{-# INLINE bench_find_index_noinline #-}
