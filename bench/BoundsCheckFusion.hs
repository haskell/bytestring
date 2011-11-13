{-# LANGUAGE PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark that the bounds checks fuse.
module Main (main) where

import Prelude hiding (words)
import Criterion.Main
import Data.Monoid
import Data.Foldable (foldMap)

import qualified Data.ByteString                  as S
import qualified Data.ByteString.Lazy             as L

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.Extras
import           Data.ByteString.Lazy.Builder.BasicEncoding
                   ( FixedEncoding, BoundedEncoding, (>$<), (>*<) )
import qualified Data.ByteString.Lazy.Builder.BasicEncoding          as E
import qualified Data.ByteString.Lazy.Builder.Internal               as I
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as I

import Foreign

------------------------------------------------------------------------------
-- Benchmark support
------------------------------------------------------------------------------

countToZero :: Int -> Maybe (Int, Int)
countToZero 0 = Nothing
countToZero n = Just (n, n - 1)


------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- input data (NOINLINE to ensure memoization)
----------------------------------------------

-- | Few-enough repetitions to avoid making GC too expensive.
nRepl :: Int
nRepl = 10000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [1..nRepl]

-- benchmark wrappers
---------------------

{-# INLINE benchB #-}
benchB :: String -> a -> (a -> Builder) -> Benchmark
benchB name x b =
    bench (name ++" (" ++ show nRepl ++ ")") $
        whnf (L.length . toLazyByteString . b) x

{-# INLINE benchBInts #-}
benchBInts :: String -> ([Int] -> Builder) -> Benchmark
benchBInts name = benchB name intData


-- benchmarks
-------------

sanityCheckInfo :: [String]
sanityCheckInfo =
  [ "Sanity checks:"
  , " lengths of input data: " ++ show
      [ length intData ]
  ]

main :: IO ()
main = do
  mapM_ putStrLn sanityCheckInfo
  putStrLn ""
  Criterion.Main.defaultMain
    [ bgroup "Data.ByteString.Lazy.Builder"
        [ -- benchBInts "foldMap intHost" $
            -- foldMap (intHost . fromIntegral)

{-
          benchBInts "mapM_ (\\x -> intHost x `mappend` intHost x)" $
            foldMap ((\x -> intHost x `mappend` intHost x)

        , benchBInts "foldMap (\\x -> intHost x `mappend` intHost x)" $
            foldMap (\x -> intHost x `mappend` intHost x)
-}

          benchBInts "foldMap (left-assoc)" $
            foldMap (\x -> (stringUtf8 "s" `mappend` intHost x) `mappend` intHost x)

        , benchBInts "foldMap (right-assoc)" $
            foldMap (\x -> intHost x `mappend` (intHost x `mappend` stringUtf8 "s"))

        , benchBInts "foldMap [manually fused, left-assoc]" $
            foldMap (\x -> stringUtf8 "s" `mappend` E.encodeWithB (E.fromF $ E.intHost >*< E.intHost) (x, x))

        , benchBInts "foldMap [manually fused, right-assoc]" $
            foldMap (\x -> E.encodeWithB (E.fromF $ E.intHost >*< E.intHost) (x, x) `mappend` stringUtf8 "s")

        -- , benchBInts "encodeListWithF intHost" $
            -- E.encodeListWithF (fromIntegral >$< E.intHost)
        ]
    ]

{-# RULES

"append/encodeWithB" forall w1 w2 x1 x2.
       I.append (E.encodeWithB w1 x1) (E.encodeWithB w2 x2)
     = E.encodeWithB (E.pairB w1 w2) (x1, x2)

"append/encodeWithB/assoc_r" forall w1 w2 x1 x2 b.
       I.append (E.encodeWithB w1 x1) (I.append (E.encodeWithB w2 x2) b)
     = I.append (E.encodeWithB (E.pairB w1 w2) (x1, x2)) b

"append/encodeWithB/assoc_l" forall w1 w2 x1 x2 b.
       I.append (I.append b (E.encodeWithB w1 x1)) (E.encodeWithB w2 x2)
     = I.append b (E.encodeWithB (E.pairB w1 w2) (x1, x2))
  #-}

