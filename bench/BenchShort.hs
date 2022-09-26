{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedStrings   #-}

module BenchShort (benchShort) where

import           Control.DeepSeq                       (force)
import           Data.Foldable                         (foldMap)
import           Data.Maybe                            (listToMaybe)
import           Data.Monoid
import           Data.String
import           Test.Tasty.Bench
import           Prelude                               hiding (words)

import           Data.ByteString.Short                 (ShortByteString)
import qualified Data.ByteString.Short                 as S

import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra         (byteStringCopy,
                                                        byteStringInsert,
                                                        intHost)
import           Data.ByteString.Builder.Internal      (ensureFree)
import           Data.ByteString.Builder.Prim          (BoundedPrim, FixedPrim,
                                                        (>$<))
import qualified Data.ByteString.Builder.Prim          as P
import qualified Data.ByteString.Builder.Prim.Internal as PI

import           Foreign

import System.Random



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

{-# NOINLINE byteStringData #-}
byteStringData :: S.ShortByteString
byteStringData = S.pack $ map fromIntegral intData

{-# NOINLINE loremIpsum #-}
loremIpsum :: S.ShortByteString
loremIpsum = mconcat
  [ "  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
  , "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis"
  , "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
  , "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu"
  , "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
  , "culpa qui officia deserunt mollit anim id est laborum."
  ]

-- benchmark wrappers
---------------------

{-# INLINE benchB' #-}
benchB' :: String -> a -> (a -> ShortByteString) -> Benchmark
benchB' name x b = bench name $ whnf (S.length . b) x


-- We use this construction of just looping through @n,n-1,..,1@ to ensure that
-- we measure the speed of the encoding and not the speed of generating the
-- values to be encoded.
{-# INLINE benchIntEncodingB #-}
benchIntEncodingB :: Int              -- ^ Maximal 'Int' to write
                  -> BoundedPrim Int  -- ^ 'BoundedPrim' to execute
                  -> IO ()            -- ^ 'IO' action to benchmark
benchIntEncodingB n0 w
  | n0 <= 0   = return ()
  | otherwise = do
      fpbuf <- mallocForeignPtrBytes (n0 * PI.sizeBound w)
      withForeignPtr fpbuf (loop n0) >> return ()
  where
    loop !n !op
      | n <= 0    = return op
      | otherwise = PI.runB w n op >>= loop (n - 1)


-- Helpers
-------------

hashInt :: Int -> Int
hashInt x = iterate step x !! 10
  where
    step a = e
      where b = (a `xor` 61) `xor` (a `shiftR` 16)
            c = b + (b `shiftL` 3)
            d = c `xor` (c `shiftR` 4)
            e = d * 0x27d4eb2d
            f = e `xor` (e `shiftR` 15)

w :: Int -> Word8
w = fromIntegral

hashWord8 :: Word8 -> Word8
hashWord8 = fromIntegral . hashInt . fromIntegral

foldInputs' :: [[Word8]]
foldInputs' = force (S.unpack <$> foldInputs)

foldInputs :: [S.ShortByteString]
foldInputs = map (\k -> S.pack $ if k <= 6 then take (2 ^ k) [32..95] else concat (replicate (2 ^ (k - 6)) [32..95])) [0..16]

largeTraversalInput :: S.ShortByteString
largeTraversalInput = S.concat (replicate 10 byteStringData)

smallTraversalInput :: S.ShortByteString
smallTraversalInput = "The quick brown fox"

zeroes :: S.ShortByteString
zeroes = S.replicate 10000 0

partitionStrict p = nf (S.partition p) . randomStrict $ mkStdGen 98423098
  where randomStrict = fst . S.unfoldrN 10000 (Just . random)

-- ASCII \n to ensure no typos
nl :: Word8
nl = 0xa
{-# INLINE nl #-}

-- non-inlined equality test
nilEq :: Word8 -> Word8 -> Bool
{-# NOINLINE nilEq #-}
nilEq = (==)

-- lines of 200 letters from a to e, followed by repeated letter f
absurdlong :: S.ShortByteString
absurdlong = S.replicate 200 0x61 <> S.singleton nl
          <> S.replicate 200 0x62 <> S.singleton nl
          <> S.replicate 200 0x63 <> S.singleton nl
          <> S.replicate 200 0x64 <> S.singleton nl
          <> S.replicate 200 0x65 <> S.singleton nl
          <> S.replicate 999999 0x66

bench_find_index_second :: ShortByteString -> Maybe Int
bench_find_index_second bs =
  let isNl = (== nl)
   in case S.findIndex isNl bs of
        Just !i -> S.findIndex isNl (S.drop (i+1) bs)
        Nothing -> Nothing
{-# INLINE bench_find_index_second #-}

bench_elem_index_second :: ShortByteString -> Maybe Int
bench_elem_index_second bs =
    case S.elemIndex nl bs of
        Just !i -> S.elemIndex nl (S.drop (i+1) bs)
        Nothing -> Nothing
{-# INLINE bench_elem_index_second #-}



-- benchmarks
-------------

benchShort :: Benchmark
benchShort = bgroup "ShortByteString"
    [ bgroup "Small payload"
      [ benchB' "mempty"        ()  (const mempty)
      , benchB' "UTF-8 String (naive)" "hello world\0" fromString
      , benchB' "String (naive)" "hello world!" fromString
      ]
    , bgroup "intercalate"
      [ bench "intercalate (large)" $ whnf (S.intercalate $ " and also ") (replicate 300 "expression")
      , bench "intercalate (small)" $ whnf (S.intercalate "&") (replicate 30 "foo")
      , bench "intercalate (tiny)" $ whnf (S.intercalate "&") (["foo", "bar", "baz"])
      ]
    , bgroup "partition"
      [
        bgroup "strict"
        [
          bench "mostlyTrueFast"  $ partitionStrict (< (w 225))
        , bench "mostlyFalseFast" $ partitionStrict (< (w 10))
        , bench "balancedFast"    $ partitionStrict (< (w 128))

        , bench "mostlyTrueSlow"  $ partitionStrict (\x -> hashWord8 x < w 225)
        , bench "mostlyFalseSlow" $ partitionStrict (\x -> hashWord8 x < w 10)
        , bench "balancedSlow"    $ partitionStrict (\x -> hashWord8 x < w 128)
        ]
      ]
    , bgroup "folds"
      [ bgroup "strict"
        [ bgroup "foldl" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldl (\acc x -> acc + fromIntegral x) (0 :: Int)) s) foldInputs
        , bgroup "foldl'" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldl' (\acc x -> acc + fromIntegral x) (0 :: Int)) s) foldInputs
        , bgroup "foldr" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldr (\x acc -> fromIntegral x + acc) (0 :: Int)) s) foldInputs
        , bgroup "foldr'" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldr' (\x acc -> fromIntegral x + acc) (0 :: Int)) s) foldInputs
        , bgroup "foldr1'" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldr1' (\x  acc -> fromIntegral x + acc)) s) foldInputs
        , bgroup "unfoldrN" $ map (\s -> bench (show $ S.length s) $
            nf (S.unfoldrN (S.length s) (\a -> Just (a, a + 1))) 0) foldInputs
        , bgroup "filter" $ map (\s -> bench (show $ S.length s) $
            nf (S.filter odd) s) foldInputs
        ]
      ]
    , bgroup "findIndexOrLength"
      [ bench "takeWhile"      $ nf (S.takeWhile even) zeroes
      , bench "dropWhile"      $ nf (S.dropWhile even) zeroes
      , bench "break"          $ nf (S.break odd) zeroes
      ]
    , bgroup "findIndex_"
      [ bench "findIndices"    $ nf (sum . S.findIndices (\x -> x ==  129 || x == 72)) byteStringData
      , bench "find"           $ nf (S.find (>= 198)) byteStringData
      ]
    , bgroup "traversals"
      [ bench "map (+1) large" $ nf (S.map (+ 1)) largeTraversalInput
      , bench "map (+1) small" $ nf (S.map (+ 1)) smallTraversalInput
      ]
    , bgroup "ShortByteString strict first index" $
        [ bench "FindIndices" $ nf (listToMaybe . S.findIndices (== nl)) absurdlong
        , bench "ElemIndices" $ nf (listToMaybe . S.elemIndices     nl)  absurdlong
        , bench "FindIndex"   $ nf (S.findIndex (== nl)) absurdlong
        , bench "ElemIndex"   $ nf (S.elemIndex     nl)  absurdlong
        ]
    , bgroup "ShortByteString strict second index" $
        [ bench "FindIndices" $ nf (listToMaybe . tail . S.findIndices (== nl)) absurdlong
        , bench "ElemIndices" $ nf (listToMaybe . tail . S.elemIndices     nl)  absurdlong
        , bench "FindIndex"   $ nf bench_find_index_second absurdlong
        , bench "ElemIndex"   $ nf bench_elem_index_second absurdlong
        ]
    , bgroup "ShortByteString index equality inlining" $
        [ bench "FindIndices/inlined"     $ nf (S.findIndices    (== nl)) absurdlong
        , bench "FindIndices/non-inlined" $ nf (S.findIndices (nilEq nl)) absurdlong
        , bench "FindIndex/inlined"       $ nf (S.findIndex      (== nl)) absurdlong
        , bench "FindIndex/non-inlined"   $ nf (S.findIndex   (nilEq nl)) absurdlong
        ]
    , bgroup "ShortByteString conversions" $
        [ bgroup "unpack" $ map (\s -> bench (show $ S.length s) $
            nf (\x -> S.unpack x) s) foldInputs
        , bgroup "pack" $ map (\s -> bench (show $ length s) $
            nf S.pack s) foldInputs'
        , bench "unpack and get last element" $ nf (\x -> last . S.unpack $ x) absurdlong
        , bench "unpack and get first 120 elements" $ nf (\x -> take 120 . S.unpack $ x) absurdlong
        ]
    ]
