-- |
-- Copyright   : (c) 2021 Georg Rudoy
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Georg Rudoy <0xd34df00d+github@gmail.com>
--
-- Benchmark count

module BenchCount (benchCount) where

import           Test.Tasty.Bench
import qualified Data.ByteString.Char8 as B

benchCount :: Benchmark
benchCount = bgroup "Count"
  [ bgroup "no matches, same char"       $ mkBenches (1 : commonSizes) (\s -> B.replicate s 'b')
  , bgroup "no matches, different chars" $ mkBenches      commonSizes  (\s -> genCyclic 10 s 'b')
  , bgroup "some matches, alternating"   $ mkBenches      commonSizes  (\s -> genCyclic 2 s 'a')
  , bgroup "some matches, short cycle"   $ mkBenches      commonSizes  (\s -> genCyclic 5 s 'a')
  , bgroup "some matches, long cycle"    $ mkBenches      commonSizes  (\s -> genCyclic 10 s 'a')
  , bgroup "all matches"                 $ mkBenches (1 : commonSizes) (\s -> B.replicate s 'a')
  ]
  where
    commonSizes = [ 10, 100, 1000, 10000, 100000, 1000000 ]
    mkBenches sizes gen = [ bench (show size ++ " chars long") $ nf (B.count 'a') (gen size)
                          | size <- sizes
                          ]
    genCyclic cycleLen size from = B.concat $ replicate (size `div` cycleLen) $ B.pack (take cycleLen [from..])
