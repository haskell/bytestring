-- |
-- Module      : Properties.ByteString
-- Copyright   : (c) Andrew Lelechenko 2021
-- License     : BSD-style

{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- We are happy to sacrifice optimizations in exchange for faster compilation,
-- but need to test rewrite rules. As one can check using -ddump-rule-firings,
-- rewrite rules do not fire in -O0 mode, so we use -O1, but disable almost all
-- optimizations. It roughly halves compilation time.
{-# OPTIONS_GHC -O1 -fenable-rewrite-rules
  -fmax-simplifier-iterations=1 -fsimplifier-phases=0
  -fno-call-arity -fno-case-merge -fno-cmm-elim-common-blocks -fno-cmm-sink
  -fno-cpr-anal -fno-cse -fno-do-eta-reduction -fno-float-in -fno-full-laziness
  -fno-loopification -fno-specialise -fno-strictness #-}

-- BYTESTRING_CHAR8 and BYTESTRING_LAZY are defined in
-- Properties.ByteString{Char8,Lazy,LazyChar8}, which include this file.
#ifndef BYTESTRING_CHAR8

#ifndef BYTESTRING_LAZY
module Properties.ByteString (tests) where
import qualified Data.ByteString as B
import GHC.IO.Encoding
#else
module Properties.ByteStringLazy (tests) where
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Internal as B (invariant)
#endif

import Data.Word

#else

#ifndef BYTESTRING_LAZY
module Properties.ByteStringChar8 (tests) where
import qualified Data.ByteString.Char8 as B
#else
module Properties.ByteStringLazyChar8 (tests) where
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.Internal as B (invariant)
#endif

import Text.Read

#endif

import Control.Arrow
import Data.Char
import Data.Foldable
import qualified Data.List as List
import Data.Semigroup
import Data.String
import Data.Tuple
import Test.Tasty
import Test.Tasty.QuickCheck
import QuickCheckUtils

#ifndef BYTESTRING_CHAR8
toElem :: Word8 -> Word8
toElem = id
#else
toElem :: Char8 -> Char
toElem (Char8 c) = c
#endif

tests :: [TestTree]
tests =
  [ testProperty "pack . unpack" $
    \x -> x === B.pack (B.unpack x)
  , testProperty "unpack . pack" $
    \(map toElem -> xs) -> xs === B.unpack (B.pack xs)
  , testProperty "read . show" $
    \x -> (x :: B.ByteString) === read (show x)
  , testProperty "fromStrict . toStrict" $
    \x -> B.fromStrict (B.toStrict x) === x
  , testProperty "toStrict . fromStrict" $
    \x -> B.toStrict (B.fromStrict x) === x
#ifndef BYTESTRING_LAZY
#ifndef BYTESTRING_CHAR8
  , testProperty "toFilePath >>= fromFilePath" $
    \x -> ioProperty $ do
      r <- B.toFilePath x >>= B.fromFilePath
      pure (r === x)
  , testProperty "fromFilePath >>= toFilePath" $ ioProperty $ do
    let prop x = ioProperty $ do
          r <- B.fromFilePath x >>= B.toFilePath
          pure (r === x)
    -- Normally getFileSystemEncoding returns a Unicode encoding,
    -- but if it is ASCII, we should not generate Unicode filenames.
    enc <- getFileSystemEncoding
    pure $ case textEncodingName enc of
      "ASCII" -> property (prop . getASCIIString)
      _       -> property prop
#endif
#endif

  , testProperty "==" $
    \x y -> (x == y) === (B.unpack x == B.unpack y)
  , testProperty "== refl" $
    \x -> (x :: B.ByteString) == x
  , testProperty "== symm" $
    \x y -> ((x :: B.ByteString) == y) === (y == x)
  , testProperty "== pack unpack" $
    \x -> x == B.pack (B.unpack x)
  , testProperty "== copy" $
    \x -> x == B.copy x

  , testProperty "compare" $
    \x y -> compare x y === compare (B.unpack x) (B.unpack y)
  , testProperty "compare EQ" $
    \x -> compare (x :: B.ByteString) x == EQ
  , testProperty "compare GT" $
    \x (toElem -> c) -> compare (B.snoc x c) x == GT
  , testProperty "compare LT" $
    \x (toElem -> c) -> compare x (B.snoc x c) == LT
  , testProperty "compare GT empty" $
    \x -> not (B.null x) ==> compare x B.empty == GT
  , testProperty "compare LT empty" $
    \x -> not (B.null x) ==> compare B.empty x == LT
  , testProperty "compare GT concat" $
    \x y -> not (B.null y) ==> compare (x <> y) x == GT
  , testProperty "compare char" $
    \(toElem -> c) (toElem -> d) -> compare c d == compare (B.singleton c) (B.singleton d)
#ifndef BYTESTRING_CHAR8
  , testProperty "compare unsigned" $ once $
    compare (B.singleton 255) (B.singleton 127) == GT
#endif

  , testProperty "null" $
    \x -> B.null x === null (B.unpack x)
  , testProperty "empty 0" $ once $
    B.length B.empty === 0
  , testProperty "empty []" $ once $
    B.unpack B.empty === []
  , testProperty "mempty 0" $ once $
    B.length mempty === 0
  , testProperty "mempty []" $ once $
    B.unpack mempty === []

  , testProperty "concat" $
    \xs -> B.unpack (B.concat xs) === concat (map B.unpack xs)
  , testProperty "concat [x,x]" $
    \x -> B.unpack (B.concat [x, x]) === concat [B.unpack x, B.unpack x]
  , testProperty "concat [x,[]]" $
    \x -> B.unpack (B.concat [x, B.empty]) === concat [B.unpack x, []]
  , testProperty "mconcat" $
    \xs -> B.unpack (mconcat xs) === mconcat (map B.unpack xs)
  , testProperty "mconcat [x,x]" $
    \x -> B.unpack (mconcat [x, x]) === mconcat [B.unpack x, B.unpack x]
  , testProperty "mconcat [x,[]]" $
    \x -> B.unpack (mconcat [x, B.empty]) === mconcat [B.unpack x, []]

  , testProperty "null" $
    \x -> B.null x === null (B.unpack x)
  , testProperty "reverse" $
    \x -> B.unpack (B.reverse x) === reverse (B.unpack x)
  , testProperty "transpose" $
    \xs -> map B.unpack (B.transpose xs) === List.transpose (map B.unpack xs)
  , testProperty "group" $
    \x -> map B.unpack (B.group x) === List.group (B.unpack x)
  , testProperty "groupBy" $
    \f x -> map B.unpack (B.groupBy f x) === List.groupBy f (B.unpack x)
  , testProperty "groupBy ==" $
    \x -> map B.unpack (B.groupBy (==) x) === List.groupBy (==) (B.unpack x)
  , testProperty "groupBy /=" $
    \x -> map B.unpack (B.groupBy (/=) x) === List.groupBy (/=) (B.unpack x)
  , testProperty "inits" $
    \x -> map B.unpack (B.inits x) === List.inits (B.unpack x)
  , testProperty "tails" $
    \x -> map B.unpack (B.tails x) === List.tails (B.unpack x)
  , testProperty "all" $
    \f x -> B.all f x === all f (B.unpack x)
  , testProperty "all ==" $
    \(toElem -> c) x -> B.all (== c) x === all (== c) (B.unpack x)
  , testProperty "any" $
    \f x -> B.any f x === any f (B.unpack x)
  , testProperty "any ==" $
    \(toElem -> c) x -> B.any (== c) x === any (== c) (B.unpack x)
  , testProperty "append" $
    \x y -> B.unpack (B.append x y) === B.unpack x ++ B.unpack y
  , testProperty "mappend" $
    \x y -> B.unpack (mappend x y) === B.unpack x `mappend` B.unpack y
  , testProperty "<>" $
    \x y -> B.unpack (x <> y) === B.unpack x <> B.unpack y
  , testProperty "stimes" $
    \(NonNegative n) x -> stimes (n :: Int) (x :: B.ByteString) === mtimesDefault n x

  , testProperty "break" $
    \f x -> (B.unpack *** B.unpack) (B.break f x) === break f (B.unpack x)
  , testProperty "break ==" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.break (== c) x) === break (== c) (B.unpack x)
  , testProperty "break /=" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.break (/= c) x) === break (/= c) (B.unpack x)
  , testProperty "break span" $
    \f x -> B.break f x === B.span (not . f) x
  , testProperty "breakEnd" $
    \f x -> B.breakEnd f x === swap ((B.reverse *** B.reverse) (B.break f (B.reverse x)))
  , testProperty "breakEnd" $
    \f x -> B.breakEnd f x === B.spanEnd (not . f) x

#ifndef BYTESTRING_LAZY
  , testProperty "break breakSubstring" $
    \(toElem -> c) x -> B.break (== c) x === B.breakSubstring (B.singleton c) x
  , testProperty "breakSubstring" $
    \x y -> not (B.null x) ==> B.null (snd (B.breakSubstring x y)) === not (B.isInfixOf x y)
  , testProperty "breakSubstring empty" $
    \x -> B.breakSubstring B.empty x === (B.empty, x)
#endif
#ifdef BYTESTRING_CHAR8
  , testProperty "break isSpace" $
    \x -> (B.unpack *** B.unpack) (B.break isSpace x) === break isSpace (B.unpack x)
#endif

  , testProperty "concatMap" $
    \f x -> B.unpack (B.concatMap f x) === concatMap (B.unpack . f) (B.unpack x)
  , testProperty "concatMap singleton" $
    \x -> B.unpack (B.concatMap B.singleton x) === concatMap (: []) (B.unpack x)

  , testProperty "singleton" $
    \(toElem -> c) -> B.unpack (B.singleton c) === [c]
  , testProperty "cons" $
    \(toElem -> c) x -> B.unpack (B.cons c x) === c : B.unpack x
  , testProperty "cons []" $
    \(toElem -> c) -> B.unpack (B.cons c B.empty) === [c]
  , testProperty "uncons" $
    \x -> fmap (second B.unpack) (B.uncons x) === List.uncons (B.unpack x)
  , testProperty "snoc" $
    \(toElem -> c) x -> B.unpack (B.snoc x c) === B.unpack x ++ [c]
  , testProperty "snoc []" $
    \(toElem -> c) -> B.unpack (B.snoc B.empty c) === [c]
  , testProperty "unsnoc" $
    \x -> fmap (first B.unpack) (B.unsnoc x) === unsnoc (B.unpack x)
#ifdef BYTESTRING_LAZY
  , testProperty "cons'" $
    \(toElem -> c) x -> B.unpack (B.cons' c x) === c : B.unpack x
#endif

  , testProperty "drop" $
    \n x -> B.unpack (B.drop n x) === drop (fromIntegral n) (B.unpack x)
  , testProperty "drop 10" $
    \x -> B.unpack (B.drop 10 x) === drop 10 (B.unpack x)
  , testProperty "dropWhile" $
    \f x -> B.unpack (B.dropWhile f x) === dropWhile f (B.unpack x)
  , testProperty "dropWhile ==" $
    \(toElem -> c) x -> B.unpack (B.dropWhile (== c) x) === dropWhile (== c) (B.unpack x)
  , testProperty "dropWhile /=" $
    \(toElem -> c) x -> B.unpack (B.dropWhile (/= c) x) === dropWhile (/= c) (B.unpack x)
#ifdef BYTESTRING_CHAR8
  , testProperty "dropWhile isSpace" $
    \x -> B.unpack (B.dropWhile isSpace x) === dropWhile isSpace (B.unpack x)
#endif

  , testProperty "take" $
    \n x -> B.unpack (B.take n x) === take (fromIntegral n) (B.unpack x)
  , testProperty "take 10" $
    \x -> B.unpack (B.take 10 x) === take 10 (B.unpack x)
  , testProperty "takeWhile" $
    \f x -> B.unpack (B.takeWhile f x) === takeWhile f (B.unpack x)
  , testProperty "takeWhile ==" $
    \(toElem -> c) x -> B.unpack (B.takeWhile (== c) x) === takeWhile (== c) (B.unpack x)
  , testProperty "takeWhile /=" $
    \(toElem -> c) x -> B.unpack (B.takeWhile (/= c) x) === takeWhile (/= c) (B.unpack x)
#ifdef BYTESTRING_CHAR8
  , testProperty "takeWhile isSpace" $
    \x -> B.unpack (B.takeWhile isSpace x) === takeWhile isSpace (B.unpack x)
#endif

  , testProperty "dropEnd" $
    \n x -> B.dropEnd n x === B.take (B.length x - n) x
  , testProperty "dropWhileEnd" $
    \f x -> B.dropWhileEnd f x === B.reverse (B.dropWhile f (B.reverse x))
  , testProperty "takeEnd" $
    \n x -> B.takeEnd n x === B.drop (B.length x - n) x
  , testProperty "takeWhileEnd" $
    \f x -> B.takeWhileEnd f x === B.reverse (B.takeWhile f (B.reverse x))

#ifdef BYTESTRING_LAZY
  , testProperty "invariant" $
    \x -> B.invariant x
  , testProperty "fromChunks . toChunks" $
    \x -> B.fromChunks (B.toChunks x) === x
  , testProperty "toChunks . fromChunks" $
    \xs -> B.toChunks (B.fromChunks xs) === filter (/= mempty) xs
  , testProperty "append lazy" $
    \(toElem -> c) -> B.head (B.singleton c <> undefined) === c
  , testProperty "compareLength 1" $
    \x -> B.compareLength x (B.length x) === EQ
  , testProperty "compareLength 2" $
    \x (toElem -> c) -> B.compareLength (B.snoc x c) (B.length x) === GT
  , testProperty "compareLength 3" $
    \x -> B.compareLength x (B.length x + 1) === LT
  , testProperty "compareLength 4" $
    \x (toElem -> c) -> B.compareLength (B.snoc x c <> undefined) (B.length x) === GT
  , testProperty "compareLength 5" $
    \x n -> B.compareLength x n === compare (B.length x) n
  , testProperty "dropEnd lazy" $
    \(toElem -> c) -> B.take 1 (B.dropEnd 1 (B.singleton c <> B.singleton c <> B.singleton c <> undefined)) === B.singleton c
  , testProperty "dropWhileEnd lazy" $
    \(toElem -> c) -> B.take 1 (B.dropWhileEnd (const False) (B.singleton c <> undefined)) === B.singleton c
  , testProperty "breakEnd lazy" $
    \(toElem -> c) -> B.take 1 (fst $ B.breakEnd (const True) (B.singleton c <> undefined)) === B.singleton c
  , testProperty "spanEnd lazy" $
    \(toElem -> c) -> B.take 1 (fst $ B.spanEnd (const False) (B.singleton c <> undefined)) === B.singleton c
#endif

  , testProperty "length" $
    \x -> B.length x === fromIntegral (length (B.unpack x))
  , testProperty "count" $
    \(toElem -> c) x -> B.count c x === fromIntegral (length (List.elemIndices c (B.unpack x)))
  -- for long strings, the multiplier is non-round (and not power of 2)
  -- to ensure non-trivial prefix or suffix of the string is handled outside any possible SIMD-based loop,
  -- which typically handles chunks of 16 or 32 or 64 etc bytes.
  , testProperty "count (long strings)" $
    \(toElem -> c) x (Positive n) -> B.count c x * fromIntegral n === B.count c (B.concat $ replicate n x)
  , testProperty "filter" $
    \f x -> B.unpack (B.filter f x) === filter f (B.unpack x)
  , testProperty "filter compose" $
    \f g x -> B.filter f (B.filter g x) === B.filter (\c -> f c && g c) x
  , testProperty "filter ==" $
    \(toElem -> c) x -> B.unpack (B.filter (== c) x) === filter (== c) (B.unpack x)
  , testProperty "filter /=" $
    \(toElem -> c) x -> B.unpack (B.filter (/= c) x) === filter (/= c) (B.unpack x)
  , testProperty "partition" $
    \f x -> (B.unpack *** B.unpack) (B.partition f x) === List.partition f (B.unpack x)

  , testProperty "find" $
    \f x -> B.find f x === find f (B.unpack x)
  , testProperty "findIndex" $
    \f x -> B.findIndex f x === fmap fromIntegral (List.findIndex f (B.unpack x))
  , testProperty "findIndexEnd" $
    \f x -> B.findIndexEnd f x === fmap fromIntegral (findIndexEnd f (B.unpack x))
  , testProperty "findIndices" $
    \f x -> B.findIndices f x === fmap fromIntegral (List.findIndices f (B.unpack x))
  , testProperty "findIndices ==" $
    \(toElem -> c) x -> B.findIndices (== c) x === fmap fromIntegral (List.findIndices (== c) (B.unpack x))

  , testProperty "elem" $
    \(toElem -> c) x -> B.elem c x === elem c (B.unpack x)
  , testProperty "notElem" $
    \(toElem -> c) x -> B.notElem c x === notElem c (B.unpack x)
  , testProperty "elemIndex" $
    \(toElem -> c) x -> B.elemIndex c x === fmap fromIntegral (List.elemIndex c (B.unpack x))
  , testProperty "elemIndexEnd" $
    \(toElem -> c) x -> B.elemIndexEnd c x === fmap fromIntegral (elemIndexEnd c (B.unpack x))
  , testProperty "elemIndices" $
    \(toElem -> c) x -> B.elemIndices c x === fmap fromIntegral (List.elemIndices c (B.unpack x))

  , testProperty "isPrefixOf" $
    \x y -> B.isPrefixOf x y === List.isPrefixOf (B.unpack x) (B.unpack y)
  , testProperty "stripPrefix" $
    \x y -> fmap B.unpack (B.stripPrefix x y) === List.stripPrefix (B.unpack x) (B.unpack y)
  , testProperty "isSuffixOf" $
    \x y -> B.isSuffixOf x y === List.isSuffixOf (B.unpack x) (B.unpack y)
  , testProperty "stripSuffix" $
    \x y -> fmap B.unpack (B.stripSuffix x y) === stripSuffix (B.unpack x) (B.unpack y)
#ifndef BYTESTRING_LAZY
  , testProperty "isInfixOf" $
    \x y -> B.isInfixOf x y === List.isInfixOf (B.unpack x) (B.unpack y)
#endif

  , testProperty "map" $
    \f x -> B.unpack (B.map (toElem . f) x) === map (toElem . f) (B.unpack x)
  , testProperty "map compose" $
    \f g x -> B.map (toElem . f) (B.map (toElem . g) x) === B.map (toElem . f . toElem . g) x
  , testProperty "replicate" $
    \n (toElem -> c) -> B.unpack (B.replicate (fromIntegral n) c) === replicate n c
  , testProperty "replicate 0" $
    \(toElem -> c) -> B.unpack (B.replicate 0 c) === replicate 0 c

  , testProperty "span" $
    \f x -> (B.unpack *** B.unpack) (B.span f x) === span f (B.unpack x)
  , testProperty "span ==" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.span (== c) x) === span (== c) (B.unpack x)
  , testProperty "span /=" $
    \(toElem -> c) x -> (B.unpack *** B.unpack) (B.span (/= c) x) === span (/= c) (B.unpack x)
  , testProperty "spanEnd" $
    \f x -> B.spanEnd f x === swap ((B.reverse *** B.reverse) (B.span f (B.reverse x)))
  , testProperty "split" $
    \(toElem -> c) x -> map B.unpack (B.split c x) === split c (B.unpack x)
  , testProperty "split empty" $
    \(toElem -> c) -> B.split c B.empty === []
  , testProperty "splitWith" $
    \f x -> map B.unpack (B.splitWith f x) === splitWith f (B.unpack x)
  , testProperty "splitWith split" $
    \(toElem -> c) x -> B.splitWith (== c) x === B.split c x
  , testProperty "splitWith empty" $
    \f -> B.splitWith f B.empty === []
  , testProperty "splitWith length" $
    \f x -> let splits = B.splitWith f x; l1 = fromIntegral (length splits); l2 = B.length (B.filter f x) in
      (l1 == l2 || l1 == l2 + 1) && sum (map B.length splits) + l2 == B.length x
  , testProperty "splitAt" $
    \n x -> (B.unpack *** B.unpack) (B.splitAt n x) === splitAt (fromIntegral n) (B.unpack x)

  , testProperty "head" $
    \x -> not (B.null x) ==> B.head x === head (B.unpack x)
  , testProperty "last" $
    \x -> not (B.null x) ==> B.last x === last (B.unpack x)
  , testProperty "tail" $
    \x -> not (B.null x) ==> B.unpack (B.tail x) === tail (B.unpack x)
  , testProperty "tail length" $
    \x -> not (B.null x) ==> B.length x === 1 + B.length (B.tail x)
  , testProperty "init" $
    \x -> not (B.null x) ==> B.unpack (B.init x) === init (B.unpack x)
  , testProperty "init length" $
    \x -> not (B.null x) ==> B.length x === 1 + B.length (B.init x)
  , testProperty "maximum" $
    \x -> not (B.null x) ==> B.maximum x === maximum (B.unpack x)
  , testProperty "minimum" $
    \x -> not (B.null x) ==> B.minimum x === minimum (B.unpack x)

  , testProperty "foldl" $
    \f (toElem -> c) x -> B.foldl ((toElem .) . f) c x === foldl ((toElem .) . f) c (B.unpack x)
  , testProperty "foldl'" $
    \f (toElem -> c) x -> B.foldl' ((toElem .) . f) c x === foldl' ((toElem .) . f) c (B.unpack x)
  , testProperty "foldr" $
    \f (toElem -> c) x -> B.foldr ((toElem .) . f) c x === foldr ((toElem .) . f) c (B.unpack x)
  , testProperty "foldr'" $
    \f (toElem -> c) x -> B.foldr' ((toElem .) . f) c x === foldr' ((toElem .) . f) c (B.unpack x)

  , testProperty "foldl cons" $
    \x -> B.foldl (flip B.cons) B.empty x === B.reverse x
  , testProperty "foldr cons" $
    \x -> B.foldr B.cons B.empty x === x
  , testProperty "foldl special" $
    \x (toElem -> c) -> B.unpack (B.foldl (\acc t -> if t == c then acc else B.cons t acc) B.empty x) ===
      foldl (\acc t -> if t == c then acc else t : acc) [] (B.unpack x)
  , testProperty "foldr special" $
    \x (toElem -> c) -> B.unpack (B.foldr (\t acc -> if t == c then acc else B.cons t acc) B.empty x) ===
      foldr (\t acc -> if t == c then acc else t : acc) [] (B.unpack x)

  , testProperty "foldl1" $
    \f x -> not (B.null x) ==> B.foldl1 ((toElem .) . f) x === foldl1 ((toElem .) . f) (B.unpack x)
  , testProperty "foldl1'" $
    \f x -> not (B.null x) ==> B.foldl1' ((toElem .) . f) x === List.foldl1' ((toElem .) . f) (B.unpack x)
  , testProperty "foldr1" $
    \f x -> not (B.null x) ==> B.foldr1 ((toElem .) . f) x === foldr1 ((toElem .) . f) (B.unpack x)
  , testProperty "foldr1'" $ -- there is not Data.List.foldr1'
    \f x -> not (B.null x) ==> B.foldr1' ((toElem .) . f) x === foldr1 ((toElem .) . f) (B.unpack x)

  , testProperty "foldl1 const" $
    \x -> not (B.null x) ==> B.foldl1 const x === B.head x
  , testProperty "foldl1 flip const" $
    \x -> not (B.null x) ==> B.foldl1 (flip const) x === B.last x
  , testProperty "foldr1 const" $
    \x -> not (B.null x) ==> B.foldr1 const x === B.head x
  , testProperty "foldr1 flip const" $
    \x -> not (B.null x) ==> B.foldr1 (flip const) x === B.last x
  , testProperty "foldl1 max" $
    \x -> not (B.null x) ==> B.foldl1 max x === B.foldl max minBound x
  , testProperty "foldr1 max" $
    \x -> not (B.null x) ==> B.foldr1 max x === B.foldr max minBound x

  , testProperty "scanl" $
    \f (toElem -> c) x -> B.unpack (B.scanl ((toElem .) . f) c x) === scanl ((toElem .) . f) c (B.unpack x)
  , testProperty "scanl foldl" $
    \f (toElem -> c) x -> not (B.null x) ==> B.last (B.scanl ((toElem .) . f) c x) === B.foldl ((toElem .) . f) c x

  , testProperty "scanr" $
    \f (toElem -> c) x -> B.unpack (B.scanr ((toElem .) . f) c x) === scanr ((toElem .) . f) c (B.unpack x)
  , testProperty "scanl1" $
    \f x -> B.unpack (B.scanl1 ((toElem .) . f) x) === scanl1 ((toElem .) . f) (B.unpack x)
  , testProperty "scanl1 empty" $
    \f -> B.scanl1 f B.empty === B.empty
  , testProperty "scanr1" $
    \f x -> B.unpack (B.scanr1 ((toElem .) . f) x) === scanr1 ((toElem .) . f) (B.unpack x)
  , testProperty "scanr1 empty" $
    \f -> B.scanr1 f B.empty === B.empty

#ifndef BYTESTRING_LAZY
  , testProperty "sort" $
    \x -> B.unpack (B.sort x) === List.sort (B.unpack x)
#endif

  , testProperty "intersperse" $
    \(toElem -> c) x -> B.unpack (B.intersperse c x) === List.intersperse c (B.unpack x)
  , testProperty "intercalate" $
    \x ys -> B.unpack (B.intercalate x ys) === List.intercalate (B.unpack x) (map B.unpack ys)
  , testProperty "intercalate 'c' [x,y]" $
    \(toElem -> c) x y -> B.unpack (B.intercalate (B.singleton c) [x, y]) === List.intercalate [c] [B.unpack x, B.unpack y]
  , testProperty "intercalate split" $
    \(toElem -> c) x -> B.intercalate (B.singleton c) (B.split c x) === x

  , testProperty "mapAccumL" $
    \f (toElem -> c) x -> second B.unpack (B.mapAccumL ((second toElem .) . f) c x) ===
      List.mapAccumL ((second toElem .) . f) c (B.unpack x)
  , testProperty "mapAccumR" $
    \f (toElem -> c) x -> second B.unpack (B.mapAccumR ((second toElem .) . f) c x) ===
      List.mapAccumR ((second toElem .) . f) c (B.unpack x)

  , testProperty "zip" $
    \x y -> B.zip x y === zip (B.unpack x) (B.unpack y)
  , testProperty "zipWith" $
    \f x y -> (B.zipWith f x y :: [Int]) === zipWith f (B.unpack x) (B.unpack y)
  , testProperty "packZipWith" $
    \f x y -> B.unpack (B.packZipWith ((toElem .) . f) x y) === zipWith ((toElem .) . f) (B.unpack x) (B.unpack y)
  , testProperty "unzip" $
    \(fmap (toElem *** toElem) -> xs) -> (B.unpack *** B.unpack) (B.unzip xs) === unzip xs

  , testProperty "index" $
    \(NonNegative n) x -> fromIntegral n < B.length x ==> B.index x (fromIntegral n) === B.unpack x !! n
  , testProperty "indexMaybe" $
    \(NonNegative n) x -> fromIntegral n < B.length x ==> B.indexMaybe x (fromIntegral n) === Just (B.unpack x !! n)
  , testProperty "indexMaybe Nothing" $
    \n x -> (n :: Int) < 0 || fromIntegral n >= B.length x ==> B.indexMaybe x (fromIntegral n) === Nothing
  , testProperty "!?" $
    \n x -> B.indexMaybe x (fromIntegral (n :: Int)) === x B.!? (fromIntegral n)

#ifdef BYTESTRING_CHAR8
  , testProperty "isString" $
    \x -> x === fromString (B.unpack x)
  , testProperty "readInt 1" $
    \x -> fmap (second B.unpack) (B.readInt x) === readInt (B.unpack x)
  , testProperty "readInt 2" $
    \n -> B.readInt (B.pack (show n)) === Just (n, B.empty)
  , testProperty "readInteger 1" $
    \x -> fmap (second B.unpack) (B.readInteger x) === readInteger (B.unpack x)
  , testProperty "readInteger 2" $
    \n -> B.readInteger (B.pack (show n)) === Just (n, B.empty)
  , testProperty "lines" $
    \x -> map B.unpack (B.lines x) === lines (B.unpack x)
  , testProperty "lines \\n" $ once $
    let x = B.pack "one\ntwo\n\n\nfive\n\nseven\n" in
    map B.unpack (B.lines x) === lines (B.unpack x)
  , testProperty "unlines" $
    \xs -> B.unpack (B.unlines xs) === unlines (map B.unpack xs)
  , testProperty "words" $
    \x -> map B.unpack (B.words x) === words (B.unpack x)
  , testProperty "unwords" $
    \xs -> B.unpack (B.unwords xs) === unwords (map B.unpack xs)
#endif

#ifndef BYTESTRING_LAZY
  , testProperty "unfoldrN" $
    \n f (toElem -> c) -> B.unpack (fst (B.unfoldrN n (fmap (first toElem) . f) c)) ===
      take (fromIntegral n) (List.unfoldr (fmap (first toElem) . f) c)
  , testProperty "unfoldrN replicate" $
    \n (toElem -> c) -> fst (B.unfoldrN n (\t -> Just (t, t)) c) === B.replicate n c
  , testProperty "unfoldr" $
    \n a (toElem -> c) -> B.unpack (B.unfoldr (\x -> if x <= 100 * n then Just (c, x + 1 :: Int) else Nothing) a) ===
      List.unfoldr (\x -> if x <= 100 * n then Just (c, x + 1) else Nothing) a
#endif

#ifdef BYTESTRING_LAZY
  , testProperty "unfoldr" $
    \n f (toElem -> a) -> B.unpack (B.take (fromIntegral n) (B.unfoldr (fmap (first toElem) . f) a)) ===
      take n (List.unfoldr (fmap (first toElem) . f) a)
  , testProperty "repeat" $
    \n (toElem -> c) -> B.take (fromIntegral (n :: Int)) (B.repeat c) ===
      B.take (fromIntegral n) (B.unfoldr (\a -> Just (a, a)) c)
  , testProperty "cycle" $
    \n x -> not (B.null x) ==> B.take (fromIntegral (n :: Int)) (B.cycle x) ===
      B.take (fromIntegral n) (B.concat (List.unfoldr (\a -> Just (a, a)) x))
  , testProperty "iterate" $
    \n f (toElem -> a) -> B.take (fromIntegral (n :: Int)) (B.iterate (toElem . f) a) ===
      B.take (fromIntegral n) (B.unfoldr (\x -> Just (toElem (f x), toElem (f x))) a)
#endif

#ifndef BYTESTRING_CHAR8
  -- issue #393
  , testProperty "fromString non-char8" $
    \s -> fromString s == B.pack (map (fromIntegral . ord :: Char -> Word8) s)
  , testProperty "fromString literal" $
    fromString "\0\1\2\3\4" == B.pack [0,1,2,3,4]
#endif
  ]

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc xs = Just (init xs, last xs)

findIndexEnd :: (a -> Bool) -> [a] -> Maybe Int
findIndexEnd f xs = fmap (\n -> length xs - 1 - n) (List.findIndex f (reverse xs))

elemIndexEnd :: Eq a => a -> [a] -> Maybe Int
elemIndexEnd c xs = fmap (\n -> length xs - 1 - n) (List.elemIndex c (reverse xs))

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix x y = fmap reverse (List.stripPrefix (reverse x) (reverse y))

split :: Eq a => a -> [a] -> [[a]]
split c = splitWith (== c)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f ys = go [] ys
  where
    go acc [] = [reverse acc]
    go acc (x : xs)
      | f x       = reverse acc : go [] xs
      | otherwise = go (x : acc) xs

#ifdef BYTESTRING_CHAR8
readInt :: String -> Maybe (Int, String)
readInt xs = case readInteger xs of
  Just (y, zs)
    | y' <- fromInteger y, toInteger y' == y -> Just (y', zs)
  otherwise -> Nothing

readInteger :: String -> Maybe (Integer, String)
readInteger ('+' : xs) = readIntegerUnsigned xs
readInteger ('-' : xs) = fmap (first negate) (readIntegerUnsigned xs)
readInteger xs = readIntegerUnsigned xs

readIntegerUnsigned :: String -> Maybe (Integer, String)
readIntegerUnsigned xs = case readMaybe ys of
  Just y -> Just (y, zs)
  otherwise -> Nothing
  where
    (ys, zs) = span isDigit xs
#endif
