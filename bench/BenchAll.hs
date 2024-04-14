-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--

{-# LANGUAGE ViewPatterns        #-}

module Main (main) where

import           Control.Exception                     (assert)
import           Data.Foldable                         (foldMap)
import           Data.Monoid
import           Data.Semigroup
import           Data.String
import           Test.Tasty.Bench

import           Prelude                               hiding (words)
import qualified Data.List                             as List
import           Control.DeepSeq
import           Control.Exception
import           Numeric.IEEE
import           GHC.Float                             (powerFloat,
                                                        castWord32ToFloat,
                                                        castWord64ToDouble,
                                                        castFloatToWord32,
                                                        castDoubleToWord64)

import qualified Data.ByteString                       as S
import qualified Data.ByteString.Char8                 as S8
import qualified Data.ByteString.Lazy                  as L
import qualified Data.ByteString.Lazy.Char8            as L8

import           Data.ByteString.Builder
import qualified Data.ByteString.Builder.Extra         as Extra
import qualified Data.ByteString.Builder.Internal      as BI
import           Data.ByteString.Builder.Prim          (BoundedPrim, FixedPrim,
                                                        (>$<))
import qualified Data.ByteString.Builder.Prim          as P
import qualified Data.ByteString.Builder.Prim.Internal as PI

import           Foreign
import           Foreign.ForeignPtr
import qualified GHC.Exts as Exts
import           GHC.Ptr (Ptr(..))

import System.Random

import BenchBoundsCheckFusion
import BenchCount
import BenchCSV
import BenchIndices
import BenchReadInt
import BenchShort

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
nRepl = 100000

{-# NOINLINE intData #-}
intData :: [Int]
intData = [1..nRepl]

{-# NOINLINE smallIntegerData #-}
smallIntegerData :: [Integer]
smallIntegerData = map fromIntegral intData

{-# NOINLINE largeIntegerData #-}
largeIntegerData :: [Integer]
largeIntegerData = map (* (10 ^ (100 :: Integer))) smallIntegerData

{-# NOINLINE floatPosData #-}
floatPosData :: [Float]
floatPosData = map evenlyDistribute intData
  where
  evenlyDistribute :: Int -> Float
  evenlyDistribute x = castWord32ToFloat $ increment * fromIntegral x
  increment = castFloatToWord32 maxFinite `div` fromIntegral nRepl

{-# NOINLINE floatNegData #-}
floatNegData :: [Float]
floatNegData = map negate floatPosData

{-# NOINLINE floatSpecials #-}
floatSpecials :: [Float]
floatSpecials = foldMap (const specials) [1..nRepl `div` length specials]
  where
  specials = [nan, infinity, negate infinity, 0 -0]

{-# NOINLINE doublePosData #-}
doublePosData :: [Double]
doublePosData = map evenlyDistribute intData
  where
  evenlyDistribute :: Int -> Double
  evenlyDistribute x = castWord64ToDouble $ increment * fromIntegral x
  increment = (maximum - minimum) `div` fromIntegral nRepl
  minimum = castDoubleToWord64 $ succIEEE $ 2 ^ 53
  maximum = castDoubleToWord64 maxFinite 

{-# NOINLINE doubleNegData #-}
doubleNegData :: [Double]
doubleNegData = map negate doublePosData

-- f is an integer in the range [1, 2^53).
{-# NOINLINE doublePosSmallData #-}
doublePosSmallData :: [Double]
doublePosSmallData = map evenlyDistribute intData
  where
  evenlyDistribute = assert (increment > 0) $ \x -> castWord64ToDouble $ increment * fromIntegral x + minimum
  increment = (maximum - minimum) `div` fromIntegral nRepl
  minimum = castDoubleToWord64 1.0
  maximum = castDoubleToWord64 $ 2 ^ 53

{-# NOINLINE doubleNegSmallData #-}
doubleNegSmallData :: [Double]
doubleNegSmallData = map negate doublePosSmallData

{-# NOINLINE doubleSpecials #-}
doubleSpecials :: [Double]
doubleSpecials = foldMap (const specials) [1..nRepl `div` length specials]
  where
  specials = [nan, infinity, negate infinity, 0 -0]

{-# NOINLINE byteStringData #-}
byteStringData :: S.ByteString
byteStringData = S.pack $ map fromIntegral intData

{-# NOINLINE lazyByteStringData #-}
lazyByteStringData :: L.ByteString
lazyByteStringData = case S.splitAt (nRepl `div` 2) byteStringData of
    (bs1, bs2) -> L.fromChunks [bs1, bs2]

{-# NOINLINE smallChunksData #-}
smallChunksData :: L.ByteString
smallChunksData = L.fromChunks $ List.unfoldr step (byteStringData, 1)
  where
    step (!s, !i)
      | S.null s = Nothing
      | otherwise = case S.splitAt i s of
          (!s1, !s2) -> Just (s1, (s2, i * 71 `mod` 97))

{-# NOINLINE byteStringChunksData #-}
byteStringChunksData :: [S.ByteString]
byteStringChunksData = map (S.pack . replicate (4 ) . fromIntegral) intData

{-# NOINLINE loremIpsum #-}
loremIpsum :: S.ByteString
loremIpsum = S8.unlines $ map S8.pack
  [ "  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor"
  , "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis"
  , "nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."
  , "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu"
  , "fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in"
  , "culpa qui officia deserunt mollit anim id est laborum."
  ]

-- benchmark wrappers
---------------------

benchB :: String -> a -> (a -> Builder) -> Benchmark
{-# INLINE benchB #-}
benchB name x b = benchB' (name ++" (" ++ show nRepl ++ ")") x b

benchB' :: String -> a -> (a -> Builder) -> Benchmark
{-# INLINE benchB' #-}
benchB' name x mkB =
  env (BI.newBuffer BI.defaultChunkSize) $ \buf ->
    bench name $ whnfAppIO (runBuildStepOn buf . BI.runBuilder . mkB) x

benchB'_ :: String -> Builder -> Benchmark
{-# INLINE benchB'_ #-}
benchB'_ name b =
  env (BI.newBuffer BI.defaultChunkSize) $ \buf ->
    bench name $ whnfIO (runBuildStepOn buf (BI.runBuilder b))

-- | @runBuilderOn@ runs a @BuildStep@'s actions all on the same @Buffer@.
-- It is used to avoid measuring driver allocation overhead.
runBuildStepOn :: BI.Buffer -> BI.BuildStep () -> IO ()
{-# NOINLINE runBuildStepOn #-}
runBuildStepOn (BI.Buffer fp br@(BI.BufferRange op ope)) b = go b
  where
    !len = ope `minusPtr` op

    go :: BI.BuildStep () -> IO ()
    go bs = BI.fillWithBuildStep bs doneH fullH insertChunkH br

    doneH :: Ptr Word8 -> () -> IO ()
    doneH _ _ = touchForeignPtr fp
    -- 'touchForeignPtr' is adequate because the given BuildStep
    -- will always terminate. (We won't measure an infinite loop!)

    fullH :: Ptr Word8 -> Int -> BI.BuildStep () -> IO ()
    fullH _ minLen nextStep
      | len < minLen = throwIO (ErrorCall "runBuilderOn: action expects too long of a BufferRange")
      | otherwise    = go nextStep

    insertChunkH :: Ptr Word8 -> S.ByteString -> BI.BuildStep () -> IO ()
    insertChunkH _ _ nextStep = go nextStep

{-# INLINE benchBInts #-}
benchBInts :: String -> ([Int] -> Builder) -> Benchmark
benchBInts name = benchB name intData

-- | Benchmark a 'FixedPrim'. Full inlining to enable specialization.
{-# INLINE benchFE #-}
benchFE :: String -> FixedPrim Int -> Benchmark
benchFE name = benchBE name . P.liftFixedToBounded

-- | Benchmark a 'BoundedPrim'. Full inlining to enable specialization.
{-# INLINE benchBE #-}
benchBE :: String -> BoundedPrim Int -> Benchmark
benchBE name e =
  bench (name ++" (" ++ show nRepl ++ ")") $ whnfIO (benchIntEncodingB nRepl e)

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

partitionStrict p = nf (S.partition p) . randomStrict $ mkStdGen 98423098
  where randomStrict = fst . S.unfoldrN 10000 (Just . random)

partitionLazy p = nf (L.partition p) . randomLazy $ (0, mkStdGen 98423098)
  where step (k, g)
          | k >= 10000 = Nothing
          | otherwise  = let (x, g') = random g in Just (x, (k + 1, g'))
        randomLazy = L.unfoldr step

easySubstrings, randomSubstrings :: Int -> Int -> (S.ByteString, S.ByteString)
hardSubstrings, pathologicalSubstrings :: Int ->
                                          Int -> (S.ByteString, S.ByteString)

{-# INLINE easySubstrings #-}
easySubstrings n h = (S.replicate n $ w 1,
                      S.replicate h $ w 0)

{-# INLINE randomSubstrings #-}
randomSubstrings n h = (f 48278379 n, f 98403980 h)
  where
    next' g = let (x, g') = next g in (w x, g')
    f g l = fst $ S.unfoldrN l (Just . next') (mkStdGen g)

{-# INLINE hardSubstrings #-}
hardSubstrings n h = (f 48278379 n, f 98403980 h)
  where
    next' g = let (x, g') = next g
              in (w $ x `mod` 4, g')
    f g l = fst $ S.unfoldrN l (Just . next') (mkStdGen g)

{-# INLINE pathologicalSubstrings #-}
pathologicalSubstrings n h =
  (S.replicate n (w 0),
   S.concat . replicate (h `div` n) $ S.replicate (n - 1) (w 0) `S.snoc` w 1)

htmlSubstrings :: S.ByteString -> Int -> Int -> IO (S.ByteString, S.ByteString)
htmlSubstrings s n h =
    do i <- randomRIO (0, l - n)
       return (S.take n . S.drop i $ s', s')
  where
    s' = S.take h s
    l  = S.length s'

-- benchmarks
-------------

sortInputs :: [S.ByteString]
sortInputs = map (`S.take` S.pack [122, 121 .. 32]) [10..25]

foldInputs :: [S.ByteString]
foldInputs = map (\k -> S.pack $ if k <= 6 then take (2 ^ k) [32..95] else concat (replicate (2 ^ (k - 6)) [32..95])) [0..16]

foldInputsLazy :: [L.ByteString]
foldInputsLazy = map (\k -> L.pack $ if k <= 6 then take (2 ^ k) [32..95] else concat (replicate (2 ^ (k - 6)) [32..95])) [0..16]

zeroes :: L.ByteString
zeroes = L.replicate 10000 0

zeroOneRepeating :: L.ByteString
zeroOneRepeating = L.take 10000 (L.cycle (L.pack [0,1]))


largeTraversalInput :: S.ByteString
largeTraversalInput = S.concat (replicate 10 byteStringData)

smallTraversalInput :: S.ByteString
smallTraversalInput = S8.pack "The quick brown fox"

asciiBuf, utf8Buf, halfNullBuf, allNullBuf :: Ptr Word8
asciiBuf = Ptr "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"#
utf8Buf  = Ptr "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\xc0\x80xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"#
halfNullBuf = Ptr "\xc0\x80xx\xc0\x80x\xc0\x80\xc0\x80x\xc0\x80\xc0\x80xx\xc0\x80\xc0\x80xxx\xc0\x80x\xc0\x80x\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80xxx\xc0\x80x\xc0\x80xx\xc0\x80\xc0\x80xxxxxxxxxx\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80x\xc0\x80\xc0\x80x\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80xxx"#
allNullBuf  = Ptr "\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80\xc0\x80"#

asciiLit, utf8Lit :: Ptr Word8 -> Builder
asciiLit (Ptr p#) = P.cstring p#
utf8Lit (Ptr p#) = P.cstringUtf8 p#

asciiStr, utf8Str :: String
asciiStr = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
utf8Str  = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\0xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

main :: IO ()
main = do
  defaultMain
    [ bgroup "Data.ByteString.Builder"
      [ bgroup "Small payload"
        [ benchB'_ "mempty" mempty
        , bench "toLazyByteString mempty" $ nf toLazyByteString mempty
        , benchB'_ "empty (10000 times)" $
            stimes (10000 :: Int) (Exts.lazy BI.empty)
        , benchB'_ "ensureFree 8" (BI.ensureFree 8)
        , benchB'  "intHost 1" 1 Extra.intHost
        , benchB'  "UTF-8 String (12B, naive)" "hello world\0" fromString
        , benchB'_ "UTF-8 String (12B)" $ utf8Lit (Ptr "hello world\xc0\x80"#)
        , benchB'  "UTF-8 String (64B, naive)" utf8Str fromString
        , benchB'_ "UTF-8 String (64B, one null)" $ utf8Lit utf8Buf
        , benchB'
            "UTF-8 String (64B, one null, no shared work)"
            utf8Buf
            utf8Lit
        , benchB'_ "UTF-8 String (64B, half nulls)" $ utf8Lit halfNullBuf
        , benchB'_ "UTF-8 String (64B, all nulls)"  $ utf8Lit allNullBuf
        , benchB'
            "UTF-8 String (64B, all nulls, no shared work)"
            allNullBuf
            utf8Lit
        , benchB'
            "UTF-8 String (1 byte, no shared work)"
            (Ptr "\xc0\x80"#)
            utf8Lit
        , benchB'  "ASCII String (12B, naive)" "hello world!" fromString
        , benchB'_ "ASCII String (12B)" $ asciiLit (Ptr "hello wurld!"#)
        , benchB'  "ASCII String (64B, naive)" asciiStr fromString
        , benchB'_ "ASCII String (64B)" $ asciiLit asciiBuf
        ]

      , bgroup "Encoding wrappers"
        [ benchBInts "foldMap word8" $
            foldMap (word8 . fromIntegral)
        , benchBInts "primMapListFixed word8" $
            P.primMapListFixed (fromIntegral >$< P.word8)
        , benchB     "primUnfoldrFixed word8" nRepl $
            P.primUnfoldrFixed (fromIntegral >$< P.word8) countToZero
        , benchB     "primMapByteStringFixed word8" byteStringData $
            P.primMapByteStringFixed P.word8
        , benchB     "primMapLazyByteStringFixed word8" lazyByteStringData $
            P.primMapLazyByteStringFixed P.word8
        ]
      , bgroup "ByteString insertion" $
            [ benchB "foldMap byteStringInsert" byteStringChunksData
                (foldMap Extra.byteStringInsert)
            , benchB "foldMap byteString" byteStringChunksData
                (foldMap byteString)
            , benchB "foldMap byteStringCopy" byteStringChunksData
                (foldMap Extra.byteStringCopy)
            ]

      , bgroup "Non-bounded encodings"
        [ benchB "byteStringHex"           byteStringData     $ byteStringHex
        , benchB "lazyByteStringHex"       lazyByteStringData $ lazyByteStringHex
          -- Note that the small data corresponds to the intData pre-converted
          -- to Integer.
        , benchB "foldMap integerDec (small)"                     smallIntegerData        $ foldMap integerDec
        , benchB "foldMap integerDec (large)"                     largeIntegerData        $ foldMap integerDec
        , bgroup "RealFloat"
          [ bgroup "FGeneric"
            [ bgroup "Positive"
              [ benchB "Float"       floatPosData       $ foldMap (formatFloat  generic)
              , benchB "Double"      doublePosData      $ foldMap (formatDouble generic)
              , benchB "DoubleSmall" doublePosSmallData $ foldMap (formatDouble generic)
              ]
            , bgroup "Negative"
              [ benchB "Float"       floatNegData  $ foldMap (formatFloat  generic)
              , benchB "Double"      doubleNegData $ foldMap (formatDouble generic)
              , benchB "DoubleSmall" doubleNegData $ foldMap (formatDouble generic)
              ]
            , bgroup "Special"
              [ benchB "Float  Average" floatSpecials  $ foldMap (formatFloat  generic)
              , benchB "Double Average" doubleSpecials $ foldMap (formatDouble generic)
              ]
            ]
          , bgroup "FScientific"
            [ bgroup "Positive"
              [ benchB "Float"       floatPosData       $ foldMap (formatFloat  scientific)
              , benchB "Double"      doublePosData      $ foldMap (formatDouble scientific)
              , benchB "DoubleSmall" doublePosSmallData $ foldMap (formatDouble scientific)
              ]
            , bgroup "Negative"
              [ benchB "Float"       floatNegData       $ foldMap (formatFloat  scientific)
              , benchB "Double"      doubleNegData      $ foldMap (formatDouble scientific)
              , benchB "DoubleSmall" doubleNegSmallData $ foldMap (formatDouble scientific)
              ]
            , bgroup "Special"
              [ benchB "Float  Average" floatSpecials  $ foldMap (formatFloat  scientific)
              , benchB "Double Average" doubleSpecials $ foldMap (formatDouble scientific)
              ]
            ]
          , bgroup "FStandard"
            [ bgroup "Positive"
              [ bgroup "without"
                [ benchB "Float"       floatPosData       $ foldMap (formatFloat  standardDefaultPrecision)
                , benchB "Double"      doublePosData      $ foldMap (formatDouble standardDefaultPrecision)
                , benchB "DoubleSmall" doublePosSmallData $ foldMap (formatDouble standardDefaultPrecision)
                ]
              , bgroup "precision"
                [ benchB "Float-Precision-1"       floatPosData       $ foldMap (formatFloat  (standard 1))
                , benchB "Double-Preciaion-1"      doublePosData      $ foldMap (formatDouble (standard 1))
                , benchB "DoubleSmall-Preciaion-1" doublePosSmallData $ foldMap (formatDouble (standard 1))
                , benchB "Float-Preciaion-6"       floatPosData       $ foldMap (formatFloat  (standard 6))
                , benchB "Double-Preciaion-6"      doublePosData      $ foldMap (formatDouble (standard 6))
                , benchB "DoubleSmall-Preciaion-6" doublePosSmallData $ foldMap (formatDouble (standard 6))
                ]
              ]
            , bgroup "Negative"
              [ bgroup "without"
                [ benchB "Float"       floatNegData       $ foldMap (formatFloat  standardDefaultPrecision)
                , benchB "Double"      doubleNegData      $ foldMap (formatDouble standardDefaultPrecision)
                , benchB "DoubleSmall" doubleNegSmallData $ foldMap (formatDouble standardDefaultPrecision)
                ]
              , bgroup "precision"
                [ benchB "Float-Preciaion-1"       floatNegData       $ foldMap (formatFloat  (standard 1))
                , benchB "Double-Preciaion-1"      doubleNegData      $ foldMap (formatDouble (standard 1))
                , benchB "DoubleSmall-Preciaion-1" doubleNegSmallData $ foldMap (formatDouble (standard 1))
                , benchB "Float-Preciaion-6"       floatNegData       $ foldMap (formatFloat  (standard 6))
                , benchB "Double-Preciaion-6"      doubleNegData      $ foldMap (formatDouble (standard 6))
                , benchB "DoubleSmall-Preciaion-6" doubleNegSmallData $ foldMap (formatDouble (standard 6))
                ]
              ]
            , bgroup "Special"
              [ benchB "Float  Average" floatSpecials  $ foldMap (formatFloat  standardDefaultPrecision)
              , benchB "Double Average" doubleSpecials $ foldMap (formatDouble standardDefaultPrecision)
              ]
            ]
          ]
        ]
      ]

    , bgroup "Data.ByteString.Builder.Prim"
      [ benchFE "char7"      $ toEnum       >$< P.char7
      , benchFE "char8"      $ toEnum       >$< P.char8
      , benchBE "charUtf8"   $ toEnum       >$< P.charUtf8

      -- binary encoding
      , benchFE "int8"       $ fromIntegral >$< P.int8
      , benchFE "word8"      $ fromIntegral >$< P.word8

      -- big-endian
      , benchFE "int16BE"    $ fromIntegral >$< P.int16BE
      , benchFE "int32BE"    $ fromIntegral >$< P.int32BE
      , benchFE "int64BE"    $ fromIntegral >$< P.int64BE

      , benchFE "word16BE"   $ fromIntegral >$< P.word16BE
      , benchFE "word32BE"   $ fromIntegral >$< P.word32BE
      , benchFE "word64BE"   $ fromIntegral >$< P.word64BE

      , benchFE "floatBE"    $ fromIntegral >$< P.floatBE
      , benchFE "doubleBE"   $ fromIntegral >$< P.doubleBE

      -- little-endian
      , benchFE "int16LE"    $ fromIntegral >$< P.int16LE
      , benchFE "int32LE"    $ fromIntegral >$< P.int32LE
      , benchFE "int64LE"    $ fromIntegral >$< P.int64LE

      , benchFE "word16LE"   $ fromIntegral >$< P.word16LE
      , benchFE "word32LE"   $ fromIntegral >$< P.word32LE
      , benchFE "word64LE"   $ fromIntegral >$< P.word64LE

      , benchFE "floatLE"    $ fromIntegral >$< P.floatLE
      , benchFE "doubleLE"   $ fromIntegral >$< P.doubleLE

      -- host-dependent
      , benchFE "int16Host"  $ fromIntegral >$< P.int16Host
      , benchFE "int32Host"  $ fromIntegral >$< P.int32Host
      , benchFE "int64Host"  $ fromIntegral >$< P.int64Host
      , benchFE "intHost"    $ fromIntegral >$< P.intHost

      , benchFE "word16Host" $ fromIntegral >$< P.word16Host
      , benchFE "word32Host" $ fromIntegral >$< P.word32Host
      , benchFE "word64Host" $ fromIntegral >$< P.word64Host
      , benchFE "wordHost"   $ fromIntegral >$< P.wordHost

      , benchFE "floatHost"  $ fromIntegral >$< P.floatHost
      , benchFE "doubleHost" $ fromIntegral >$< P.doubleHost
      ]

    , bgroup "Data.ByteString.Builder.Prim.ASCII"
      [
      -- decimal number
        benchBE "int8Dec"     $ fromIntegral >$< P.int8Dec
      , benchBE "int16Dec"    $ fromIntegral >$< P.int16Dec
      , benchBE "int32Dec"    $ fromIntegral >$< P.int32Dec
      , benchBE "int64Dec"    $ fromIntegral >$< P.int64Dec
      , benchBE "intDec"      $ fromIntegral >$< P.intDec

      , benchBE "word8Dec"    $ fromIntegral >$< P.word8Dec
      , benchBE "word16Dec"   $ fromIntegral >$< P.word16Dec
      , benchBE "word32Dec"   $ fromIntegral >$< P.word32Dec
      , benchBE "word64Dec"   $ fromIntegral >$< P.word64Dec
      , benchBE "wordDec"     $ fromIntegral >$< P.wordDec

      -- hexadecimal number
      , benchBE "word8Hex"    $ fromIntegral >$< P.word8Hex
      , benchBE "word16Hex"   $ fromIntegral >$< P.word16Hex
      , benchBE "word32Hex"   $ fromIntegral >$< P.word32Hex
      , benchBE "word64Hex"   $ fromIntegral >$< P.word64Hex
      , benchBE "wordHex"     $ fromIntegral >$< P.wordHex

      -- fixed-width hexadecimal numbers
      , benchFE "int8HexFixed"     $ fromIntegral >$< P.int8HexFixed
      , benchFE "int16HexFixed"    $ fromIntegral >$< P.int16HexFixed
      , benchFE "int32HexFixed"    $ fromIntegral >$< P.int32HexFixed
      , benchFE "int64HexFixed"    $ fromIntegral >$< P.int64HexFixed

      , benchFE "word8HexFixed"    $ fromIntegral >$< P.word8HexFixed
      , benchFE "word16HexFixed"   $ fromIntegral >$< P.word16HexFixed
      , benchFE "word32HexFixed"   $ fromIntegral >$< P.word32HexFixed
      , benchFE "word64HexFixed"   $ fromIntegral >$< P.word64HexFixed

      , benchFE "floatHexFixed"    $ fromIntegral >$< P.floatHexFixed
      , benchFE "doubleHexFixed"   $ fromIntegral >$< P.doubleHexFixed
      ]
    , bgroup "intersperse"
      [ bench "intersperse" $ whnf (S.intersperse 32) byteStringData
      , bench "intersperse (unaligned)" $ whnf (S.intersperse 32) (S.drop 1 byteStringData)
      ]
    , bgroup "intercalate"
      [ bench "intercalate (large)" $ whnf (S.intercalate $ S8.pack " and also ") (replicate 300 (S8.pack "expression"))
      , bench "intercalate (small)" $ whnf (S.intercalate $ S8.pack "&") (replicate 30 (S8.pack "foo"))
      , bench "intercalate (tiny)" $ whnf (S.intercalate $ S8.pack "&") (S8.pack <$> ["foo", "bar", "baz"])
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
      , bgroup "lazy"
        [
          bench "mostlyTrueFast"  $ partitionLazy (< (w 225))
        , bench "mostlyFalseFast" $ partitionLazy (< (w 10))
        , bench "balancedFast"    $ partitionLazy (< (w 128))

        , bench "mostlyTrueSlow"  $ partitionLazy (\x -> hashWord8 x < w 225)
        , bench "mostlyFalseSlow" $ partitionLazy (\x -> hashWord8 x < w 10)
        , bench "balancedSlow"    $ partitionLazy (\x -> hashWord8 x < w 128)
        ]
      ]
    , bgroup "inits"
      [ bench "strict" $ nf S.inits byteStringData
      , bench "lazy"   $ nf L.inits lazyByteStringData
      , bench "lazy (small chunks)" $ nf L.inits smallChunksData
      ]
    , bgroup "tails"
      [ bench "strict" $ nf S.tails byteStringData
      , bench "lazy"   $ nf L.tails lazyByteStringData
      ]
    , bgroup "splitAtEnd (lazy)" $ let
        testSAE op = \bs -> [op i bs | i <- [0,5..L.length bs]] `deepseq` ()
        {-# INLINE testSAE #-}
      in
      [ bench "takeEnd" $
          nf (testSAE L.takeEnd) lazyByteStringData
      , bench "takeEnd (small chunks)" $
          nf (testSAE L.takeEnd) smallChunksData
      , bench "dropEnd" $
          nf (testSAE L.dropEnd) lazyByteStringData
      , bench "dropEnd (small chunks)" $
          nf (testSAE L.dropEnd) smallChunksData
      ]
    , bgroup "sort" $ map (\s -> bench (S8.unpack s) $ nf S.sort s) sortInputs
    , bgroup "stimes" $ let  st = stimes :: Int -> S.ByteString -> S.ByteString
     in
      [ bench "strict (tiny)" $ whnf (st 4) (S8.pack "test")
      , bench "strict (large)" $ whnf (st 50) byteStringData
      ]
    , bgroup "words"
      [ bench "lorem ipsum" $ nf S8.words loremIpsum
      , bench "one huge word" $ nf S8.words byteStringData
      ]
    , bgroup "folds"
      [ bgroup "strict"
        [ bgroup "foldl'" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldl' (\acc x -> acc + fromIntegral x) (0 :: Int)) s) foldInputs
        , bgroup "foldr'" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldr' (\x acc -> fromIntegral x + acc) (0 :: Int)) s) foldInputs
        , bgroup "foldr1'" $ map (\s -> bench (show $ S.length s) $
            nf (S.foldr1' (\x  acc -> fromIntegral x + acc)) s) foldInputs
        , bgroup "unfoldrN" $ map (\s -> bench (show $ S.length s) $
            nf (S.unfoldrN (S.length s) (\a -> Just (a, a + 1))) 0) foldInputs
        , bgroup "mapAccumL" $ map (\s -> bench (show $ S.length s) $
            nf (S.mapAccumL (\acc x -> (acc + fromIntegral x, succ x)) (0 :: Int)) s) foldInputs
        , bgroup "mapAccumR" $ map (\s -> bench (show $ S.length s) $
            nf (S.mapAccumR (\acc x -> (fromIntegral x + acc, succ x)) (0 :: Int)) s) foldInputs
        , bgroup "scanl" $ map (\s -> bench (show $ S.length s) $
            nf (S.scanl (+) 0) s) foldInputs
        , bgroup "scanr" $ map (\s -> bench (show $ S.length s) $
            nf (S.scanr (+) 0) s) foldInputs
        , bgroup "filter" $ map (\s -> bench (show $ S.length s) $
            nf (S.filter odd) s) foldInputs
        ]
      , bgroup "lazy"
        [ bgroup "foldl'" $ map (\s -> bench (show $ L.length s) $
            nf (L.foldl' (\acc x -> acc + fromIntegral x) (0 :: Int)) s) foldInputsLazy
        , bgroup "foldr'" $ map (\s -> bench (show $ L.length s) $
            nf (L.foldr' (\x acc -> fromIntegral x + acc) (0 :: Int)) s) foldInputsLazy
        , bgroup "foldr1'" $ map (\s -> bench (show $ L.length s) $
            nf (L.foldr1' (\x  acc -> fromIntegral x + acc)) s) foldInputsLazy
        , bgroup "mapAccumL" $ map (\s -> bench (show $ L.length s) $
            nf (L.mapAccumL (\acc x -> (acc + fromIntegral x, succ x)) (0 :: Int)) s) foldInputsLazy
        , bgroup "mapAccumR" $ map (\s -> bench (show $ L.length s) $
            nf (L.mapAccumR (\acc x -> (fromIntegral x + acc, succ x)) (0 :: Int)) s) foldInputsLazy
        , bgroup "scanl" $ map (\s -> bench (show $ L.length s) $
            nf (L.scanl (+) 0) s) foldInputsLazy
        , bgroup "scanr" $ map (\s -> bench (show $ L.length s) $
            nf (L.scanr (+) 0) s) foldInputsLazy
        ]

      ]
    , bgroup "findIndexOrLength"
      [ bench "takeWhile"      $ nf (L.takeWhile even) zeroes
      , bench "dropWhile"      $ nf (L.dropWhile even) zeroes
      , bench "break"          $ nf (L.break odd) zeroes
      , bench "group zeroes"   $ nf L.group zeroes
      , bench "group zero-one" $ nf L.group zeroOneRepeating
      , bench "groupBy (>=)"   $ nf (L.groupBy (>=)) zeroes
      , bench "groupBy (>)"    $ nf (L.groupBy (>)) zeroes
      ]
    , bgroup "findIndex_"
      [ bench "findIndices"    $ nf (sum . S.findIndices (\x -> x ==  129 || x == 72)) byteStringData
      , bench "find"           $ nf (S.find (>= 198)) byteStringData
      ]
    , bgroup "findIndexEnd"
      [ bench "findIndexEnd"   $ nf (S.findIndexEnd (<= 57)) byteStringData
      , bench "elemIndexInd"   $ nf (S.elemIndexEnd 42) byteStringData
      ]
    , bgroup "traversals"
      [ bench "map (+1) large" $ nf (S.map (+ 1)) largeTraversalInput
      , bench "map (+1) small" $ nf (S.map (+ 1)) smallTraversalInput
      ]
    , bgroup "unlines"
      [ bench "lazy"   $ nf L8.unlines (map (L8.pack . show) intData)
      , bench "strict" $ nf S8.unlines (map (S8.pack . show) intData)
      ]
    , benchBoundsCheckFusion
    , benchCount
    , benchCSV
    , benchIndices
    , benchReadInt
    , benchShort
    ]
