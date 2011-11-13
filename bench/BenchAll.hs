{-# LANGUAGE PackageImports, ScopedTypeVariables, BangPatterns #-}
-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Benchmark all 'Builder' functions.
module Main (main) where

import Prelude hiding (words)
import Criterion.Main
import Data.Foldable (foldMap)

import qualified Data.ByteString                  as S
import qualified Data.ByteString.Lazy             as L

import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII
import           Data.ByteString.Lazy.Builder.BasicEncoding
                   ( FixedEncoding, BoundedEncoding, (>$<) )
import qualified Data.ByteString.Lazy.Builder.BasicEncoding          as E
import qualified Data.ByteString.Lazy.Builder.BasicEncoding.Internal as EI

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

-- Half of the integers inside the range of an Int and half of them outside.
{-# NOINLINE integerData #-}
integerData :: [Integer]
integerData = map (\x -> fromIntegral x + fromIntegral (maxBound - nRepl `div` 2)) intData

{-# NOINLINE floatData #-}
floatData :: [Float]
floatData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE doubleData #-}
doubleData :: [Double]
doubleData = map (\x -> (3.14159 * fromIntegral x) ^ (3 :: Int)) intData

{-# NOINLINE byteStringData #-}
byteStringData :: S.ByteString
byteStringData = S.pack $ map fromIntegral intData

{-# NOINLINE lazyByteStringData #-}
lazyByteStringData :: L.ByteString
lazyByteStringData = case S.splitAt (nRepl `div` 2) byteStringData of
    (bs1, bs2) -> L.fromChunks [bs1, bs2]


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

-- | Benchmark a 'FixedEncoding'. Full inlining to enable specialization.
{-# INLINE benchFE #-}
benchFE :: String -> FixedEncoding Int -> Benchmark
benchFE name = benchBE name . E.fromF

-- | Benchmark a 'BoundedEncoding'. Full inlining to enable specialization.
{-# INLINE benchBE #-}
benchBE :: String -> BoundedEncoding Int -> Benchmark
benchBE name e =
  bench (name ++" (" ++ show nRepl ++ ")") $ benchIntEncodingB nRepl e

-- We use this construction of just looping through @n,n-1,..,1@ to ensure that
-- we measure the speed of the encoding and not the speed of generating the
-- values to be encoded.
{-# INLINE benchIntEncodingB #-}
benchIntEncodingB :: Int                  -- ^ Maximal 'Int' to write
                  -> BoundedEncoding Int  -- ^ 'BoundedEncoding' to execute
                  -> IO ()                -- ^ 'IO' action to benchmark
benchIntEncodingB n0 w
  | n0 <= 0   = return ()
  | otherwise = do
      fpbuf <- mallocForeignPtrBytes (n0 * EI.sizeBound w)
      withForeignPtr fpbuf (loop n0) >> return ()
  where
    loop !n !op
      | n <= 0    = return op
      | otherwise = EI.runB w n op >>= loop (n - 1)



-- benchmarks
-------------

sanityCheckInfo :: [String]
sanityCheckInfo =
  [ "Sanity checks:"
  , " lengths of input data: " ++ show
      [ length intData, length floatData, length doubleData, length integerData
      , S.length byteStringData, fromIntegral (L.length lazyByteStringData)
      ]
  ]

main :: IO ()
main = do
  mapM_ putStrLn sanityCheckInfo
  putStrLn ""
  Criterion.Main.defaultMain
    [ bgroup "Data.ByteString.Lazy.Builder"
      [ bgroup "Encoding wrappers"
        [ benchBInts "foldMap word8" $
            foldMap (word8 . fromIntegral)
        , benchBInts "encodeListWithF word8" $
            E.encodeListWithF (fromIntegral >$< E.word8)
        , benchB     "encodeUnfoldrWithF word8" nRepl $
            E.encodeUnfoldrWithF (fromIntegral >$< E.word8) countToZero
        , benchB     "encodeByteStringWithF word8" byteStringData $
            E.encodeByteStringWithF E.word8
        , benchB     "encodeLazyByteStringWithF word8" lazyByteStringData $
            E.encodeLazyByteStringWithF E.word8
        ]

      , bgroup "Non-bounded encodings"
        [ benchB "foldMap floatDec"        floatData          $ foldMap floatDec
        , benchB "foldMap doubleDec"       doubleData         $ foldMap doubleDec
        , benchB "foldMap integerDec"      integerData        $ foldMap integerDec
        , benchB "byteStringHexFixed"      byteStringData     $ byteStringHexFixed
        , benchB "lazyByteStringHexFixed"  lazyByteStringData $ lazyByteStringHexFixed
        ]
      ]

    , bgroup "Data.ByteString.Lazy.Builder.BasicEncoding"
      [ benchFE "char7"      $ toEnum       >$< E.char7
      , benchFE "char8"      $ toEnum       >$< E.char8
      , benchBE "charUtf8"   $ toEnum       >$< E.charUtf8

      -- binary encoding
      , benchFE "int8"       $ fromIntegral >$< E.int8
      , benchFE "word8"      $ fromIntegral >$< E.word8

      -- big-endian
      , benchFE "int16BE"    $ fromIntegral >$< E.int16BE
      , benchFE "int32BE"    $ fromIntegral >$< E.int32BE
      , benchFE "int64BE"    $ fromIntegral >$< E.int64BE

      , benchFE "word16BE"   $ fromIntegral >$< E.word16BE
      , benchFE "word32BE"   $ fromIntegral >$< E.word32BE
      , benchFE "word64BE"   $ fromIntegral >$< E.word64BE

      , benchFE "floatBE"    $ fromIntegral >$< E.floatBE
      , benchFE "doubleBE"   $ fromIntegral >$< E.doubleBE

      -- little-endian
      , benchFE "int16LE"    $ fromIntegral >$< E.int16LE
      , benchFE "int32LE"    $ fromIntegral >$< E.int32LE
      , benchFE "int64LE"    $ fromIntegral >$< E.int64LE

      , benchFE "word16LE"   $ fromIntegral >$< E.word16LE
      , benchFE "word32LE"   $ fromIntegral >$< E.word32LE
      , benchFE "word64LE"   $ fromIntegral >$< E.word64LE

      , benchFE "floatLE"    $ fromIntegral >$< E.floatLE
      , benchFE "doubleLE"   $ fromIntegral >$< E.doubleLE

      -- host-dependent
      , benchFE "int16Host"  $ fromIntegral >$< E.int16Host
      , benchFE "int32Host"  $ fromIntegral >$< E.int32Host
      , benchFE "int64Host"  $ fromIntegral >$< E.int64Host
      , benchFE "intHost"    $ fromIntegral >$< E.intHost

      , benchFE "word16Host" $ fromIntegral >$< E.word16Host
      , benchFE "word32Host" $ fromIntegral >$< E.word32Host
      , benchFE "word64Host" $ fromIntegral >$< E.word64Host
      , benchFE "wordHost"   $ fromIntegral >$< E.wordHost

      , benchFE "floatHost"  $ fromIntegral >$< E.floatHost
      , benchFE "doubleHost" $ fromIntegral >$< E.doubleHost
      ]

    , bgroup "Data.ByteString.Lazy.Builder.BoundedEncoding.ASCII"
      [
      -- decimal number
        benchBE "int8Dec"     $ fromIntegral >$< E.int8Dec
      , benchBE "int16Dec"    $ fromIntegral >$< E.int16Dec
      , benchBE "int32Dec"    $ fromIntegral >$< E.int32Dec
      , benchBE "int64Dec"    $ fromIntegral >$< E.int64Dec
      , benchBE "intDec"      $ fromIntegral >$< E.intDec

      , benchBE "word8Dec"    $ fromIntegral >$< E.word8Dec
      , benchBE "word16Dec"   $ fromIntegral >$< E.word16Dec
      , benchBE "word32Dec"   $ fromIntegral >$< E.word32Dec
      , benchBE "word64Dec"   $ fromIntegral >$< E.word64Dec
      , benchBE "wordDec"     $ fromIntegral >$< E.wordDec

      -- hexadecimal number
      , benchBE "word8Hex"    $ fromIntegral >$< E.word8Hex
      , benchBE "word16Hex"   $ fromIntegral >$< E.word16Hex
      , benchBE "word32Hex"   $ fromIntegral >$< E.word32Hex
      , benchBE "word64Hex"   $ fromIntegral >$< E.word64Hex
      , benchBE "wordHex"     $ fromIntegral >$< E.wordHex

      -- fixed-width hexadecimal numbers
      , benchFE "int8HexFixed"     $ fromIntegral >$< E.int8HexFixed
      , benchFE "int16HexFixed"    $ fromIntegral >$< E.int16HexFixed
      , benchFE "int32HexFixed"    $ fromIntegral >$< E.int32HexFixed
      , benchFE "int64HexFixed"    $ fromIntegral >$< E.int64HexFixed

      , benchFE "word8HexFixed"    $ fromIntegral >$< E.word8HexFixed
      , benchFE "word16HexFixed"   $ fromIntegral >$< E.word16HexFixed
      , benchFE "word32HexFixed"   $ fromIntegral >$< E.word32HexFixed
      , benchFE "word64HexFixed"   $ fromIntegral >$< E.word64HexFixed

      , benchFE "floatHexFixed"    $ fromIntegral >$< E.floatHexFixed
      , benchFE "doubleHexFixed"   $ fromIntegral >$< E.doubleHexFixed
      ]
    ]
