-- |
-- Copyright   : (c) 2021 Viktor Dukhovni
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Viktor Dukhovni <ietf-dane@dukhovni.org>
--
-- Benchmark readInt and variants, readWord and variants,
-- readInteger and readNatural

{-# LANGUAGE
    CPP
  , BangPatterns
  , OverloadedStrings
  , TypeApplications
  , ScopedTypeVariables
  #-}

module BenchReadInt (benchReadInt) where

import qualified Data.ByteString.Builder               as B
import qualified Data.ByteString.Char8                 as S
import qualified Data.ByteString.Lazy.Char8            as L
import Test.Tasty.Bench
import Data.Int
import Data.Word
import Numeric.Natural
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Monoid (mconcat)

------------------------------------------------------------------------------
-- Benchmark
------------------------------------------------------------------------------

-- Sum space-separated integers in a ByteString.
loopS :: Integral a
      => (S.ByteString -> Maybe (a, S.ByteString)) -> S.ByteString -> a
loopS rd = go 0
  where
    go !acc !bs = case rd bs of
        Just (i, t) -> case S.uncons t of
            Just (_, t') -> go (acc + i) t'
            Nothing      -> acc + i
        Nothing          -> acc

-- Sum space-separated integers in a ByteString.
loopL :: Integral a
      => (L.ByteString -> Maybe (a, L.ByteString)) -> L.ByteString -> a
loopL rd = go 0
  where
    go !acc !bs = case rd bs of
        Just (i, t) -> case L.uncons t of
            Just (_, t') -> go (acc + i) t'
            Nothing      -> acc + i
        Nothing          -> acc

benchReadInt :: Benchmark
benchReadInt = bgroup "Read Integral"
    [ bgroup "Strict"
        [ bench "ReadInt"     $ nf (loopS S.readInt)     intS
        , bench "ReadInt8"    $ nf (loopS S.readInt8)    int8S
        , bench "ReadInt16"   $ nf (loopS S.readInt16)   int16S
        , bench "ReadInt32"   $ nf (loopS S.readInt32)   int32S
        , bench "ReadInt64"   $ nf (loopS S.readInt64)   int64S
        , bench "ReadWord"    $ nf (loopS S.readWord)    wordS
        , bench "ReadWord8"   $ nf (loopS S.readWord8)   word8S
        , bench "ReadWord16"  $ nf (loopS S.readWord16)  word16S
        , bench "ReadWord32"  $ nf (loopS S.readWord32)  word32S
        , bench "ReadWord64"  $ nf (loopS S.readWord64)  word64S
        , bench "ReadInteger" $ nf (loopS S.readInteger) bignatS
        , bench "ReadNatural" $ nf (loopS S.readNatural) bignatS
        , bench "ReadInteger small" $ nf (loopS S.readInteger) intS
        , bench "ReadNatural small" $ nf (loopS S.readNatural) wordS
        ]

    , bgroup "Lazy"
        [ bench "ReadInt"     $ nf (loopL L.readInt)     intL
        , bench "ReadInt8"    $ nf (loopL L.readInt8)    int8L
        , bench "ReadInt16"   $ nf (loopL L.readInt16)   int16L
        , bench "ReadInt32"   $ nf (loopL L.readInt32)   int32L
        , bench "ReadInt64"   $ nf (loopL L.readInt64)   int64L
        , bench "ReadWord"    $ nf (loopL L.readWord)    wordL
        , bench "ReadWord8"   $ nf (loopL L.readWord8)   word8L
        , bench "ReadWord16"  $ nf (loopL L.readWord16)  word16L
        , bench "ReadWord32"  $ nf (loopL L.readWord32)  word32L
        , bench "ReadWord64"  $ nf (loopL L.readWord64)  word64L
        , bench "ReadInteger" $ nf (loopL L.readInteger) bignatL
        , bench "ReadNatural" $ nf (loopL L.readNatural) bignatL
        , bench "ReadInteger small" $ nf (loopL L.readInteger) intL
        , bench "ReadNatural small" $ nf (loopL L.readNatural) wordL
        ]
    ]
  where
    mkWordL :: forall a. (Integral a, Bounded a)
            => (a -> B.Builder) -> L.ByteString
    mkWordL f = B.toLazyByteString b
      where b = mconcat [f i <> B.char8 ' ' | i <- [n-255..n]]
            n = maxBound @a
    mkWordS f = S.toStrict $ mkWordL f

    mkIntL :: forall a. (Integral a, Bounded a)
           => (a -> B.Builder) -> L.ByteString
    mkIntL f = B.toLazyByteString b
      where b = mconcat [f (i + 128) <> B.char8 ' ' | i <- [n-255..n]]
            n = maxBound @a
    mkIntS f = S.toStrict $ mkIntL f

    wordS, word8S, word16S, word32S, word64S :: S.ByteString
    !wordS = mkWordS B.wordDec
    !word8S = mkWordS B.word8Dec
    !word16S = mkWordS B.word16Dec
    !word32S = mkWordS B.word32Dec
    !word64S = mkWordS B.word64Dec

    intS, int8S, int16S, int32S, int64S :: S.ByteString
    !intS =  mkIntS B.intDec
    !int8S = mkIntS B.int8Dec
    !int16S = mkIntS B.int16Dec
    !int32S = mkIntS B.int32Dec
    !int64S = mkIntS B.int64Dec

    word8L, word16L, word32L, word64L :: L.ByteString
    !wordL = mkWordL B.wordDec
    !word8L = mkWordL B.word8Dec
    !word16L = mkWordL B.word16Dec
    !word32L = mkWordL B.word32Dec
    !word64L = mkWordL B.word64Dec

    intL, int8L, int16L, int32L, int64L :: L.ByteString
    !intL =  mkIntL B.intDec
    !int8L = mkIntL B.int8Dec
    !int16L = mkIntL B.int16Dec
    !int32L = mkIntL B.int32Dec
    !int64L = mkIntL B.int64Dec

    bignatL :: L.ByteString
    !bignatL = B.toLazyByteString b
      where b = mconcat [B.integerDec (powpow i) <> B.char8 ' ' | i <- [0..13]]
            powpow :: Word -> Integer
            powpow n = 2^(2^n :: Word)

    bignatS :: S.ByteString
    !bignatS = S.toStrict bignatL
