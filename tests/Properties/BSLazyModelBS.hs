{-# LANGUAGE CPP #-}
module Properties.BSLazyModelBS
    ( bp_tests
) where

import Data.Char

import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString            as P
import qualified Data.ByteString.Char8      as C

import qualified Data.ByteString.Lazy.Char8 as D

import Test.QuickCheck
import QuickCheckUtils
#if defined(HAVE_TEST_FRAMEWORK)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
#else
import TestFramework
#endif

import Shared

bp_tests = testGroup "ByteString.Lazy <=> ByteString && Some other stuff apparently"
    [ testProperty "all"         prop_allBP
    , testProperty "any"         prop_anyBP
    , testProperty "append"      prop_appendBP
    , testProperty "compare"     prop_compareBP
    , testProperty "concat"      prop_concatBP
    , testProperty "cons"        prop_consBP
    , testProperty "cons'"       prop_consBP'
    , testProperty "uncons"      prop_unconsBP
    , testProperty "unsnoc"      prop_unsnocBP
    , testProperty "eq"          prop_eqBP
    , testProperty "filter"      prop_filterBP
    , testProperty "find"        prop_findBP
    , testProperty "findIndex"   prop_findIndexBP
    , testProperty "findIndexEnd"prop_findIndexEndBP
    , testProperty "findIndices" prop_findIndicesBP
    , testProperty "foldl"       prop_foldlBP
    , testProperty "foldl'"      prop_foldlBP'
    , testProperty "foldl1"      prop_foldl1BP
    , testProperty "foldl1'"     prop_foldl1BP'
    , testProperty "foldr"       prop_foldrBP
    , testProperty "foldr'"      prop_foldrBP'
    , testProperty "foldr1"      prop_foldr1BP
    , testProperty "foldr1'"     prop_foldr1BP'
    , testProperty "mapAccumL"   prop_mapAccumLBP
    , testProperty "unfoldr"     prop_unfoldrBP
    , testProperty "unfoldr 2"   prop_unfoldr2BP
    , testProperty "unfoldr 2"   prop_unfoldr2CP
    , testProperty "head"        prop_headBP
    , testProperty "init"        prop_initBP
    , testProperty "isPrefixOf"  prop_isPrefixOfBP
    , testProperty "isSuffixOf"  prop_isSuffixOfBP
    , testProperty "stripPrefix" prop_stripPrefixBP
    , testProperty "stripSuffix" prop_stripSuffixBP
    , testProperty "last"        prop_lastBP
    , testProperty "length"      prop_lengthBP
    , testProperty "readInt"     prop_readIntBP
    , testProperty "lines"       prop_linesBP
    , testProperty "lines \\n"   prop_linesNLBP
    , testProperty "map"         prop_mapBP
    , testProperty "maximum   "  prop_maximumBP
    , testProperty "minimum"     prop_minimumBP
    , testProperty "null"        prop_nullBP
    , testProperty "reverse"     prop_reverseBP
    , testProperty "snoc"        prop_snocBP
    , testProperty "tail"        prop_tailBP
    , testProperty "scanl"       prop_scanlBP
    , testProperty "transpose"   prop_transposeBP
    , testProperty "replicate"   prop_replicateBP
    , testProperty "take"        prop_takeBP
    , testProperty "drop"        prop_dropBP
    , testProperty "splitAt"     prop_splitAtBP
    , testProperty "takeWhile"   prop_takeWhileBP
    , testProperty "dropWhile"   prop_dropWhileBP
    , testProperty "break"       prop_breakBP
    , testProperty "span"        prop_spanBP
    , testProperty "split"       prop_splitBP
    , testProperty "count"       prop_countBP
    , testProperty "group"       prop_groupBP
    , testProperty "groupBy"     prop_groupByBP
    , testProperty "inits"       prop_initsBP
    , testProperty "tails"       prop_tailsBP
    , testProperty "elem"        prop_elemBP
    , testProperty "notElem"     prop_notElemBP
    , testProperty "elemIndex"   prop_elemIndexBP
    , testProperty "elemIndexEnd"prop_elemIndexEndBP
    , testProperty "elemIndices" prop_elemIndicesBP
    , testProperty "intersperse" prop_intersperseBP
    , testProperty "concatMap"   prop_concatMapBP
    ]

--
-- ByteString.Lazy <=> ByteString
--

prop_concatBP       = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                      L.concat               `eq1`  P.concat
prop_nullBP         = L.null                 `eq1`  P.null
prop_reverseBP      = L.reverse              `eq1`  P.reverse

prop_transposeBP    = L.transpose            `eq1`  P.transpose
prop_groupBP        = L.group                `eq1`  P.group
prop_groupByBP      = L.groupBy              `eq2`  P.groupBy
prop_initsBP        = L.inits                `eq1`  P.inits
prop_tailsBP        = L.tails                `eq1`  P.tails
prop_allBP          = L.all                  `eq2`  P.all
prop_anyBP          = L.any                  `eq2`  P.any
prop_appendBP       = L.append               `eq2`  P.append
prop_breakBP        = L.break                `eq2`  P.break
prop_concatMapBP    = forAll (sized $ \n -> resize (n `div` 4) arbitrary) $
                      L.concatMap            `eq2`  P.concatMap
prop_consBP         = L.cons                 `eq2`  P.cons
prop_consBP'        = L.cons'                `eq2`  P.cons
prop_unconsBP       = L.uncons               `eq1`  P.uncons
prop_unsnocBP       = L.unsnoc               `eq1`  P.unsnoc
prop_countBP        = L.count                `eq2`  ((toInt64 .) . P.count)
prop_dropBP         = (L.drop. toInt64)      `eq2`  P.drop
prop_dropWhileBP    = L.dropWhile            `eq2`  P.dropWhile
prop_filterBP       = L.filter               `eq2`  P.filter
prop_findBP         = L.find                 `eq2`  P.find
prop_findIndexBP    = L.findIndex            `eq2`  ((fmap toInt64 .) . P.findIndex)
prop_findIndexEndBP = L.findIndexEnd         `eq2`  ((fmap toInt64 .) . P.findIndexEnd)
prop_findIndicesBP  = L.findIndices          `eq2`  ((fmap toInt64 .) . P.findIndices)
prop_isPrefixOfBP   = L.isPrefixOf           `eq2`  P.isPrefixOf
prop_stripPrefixBP  = L.stripPrefix          `eq2`  P.stripPrefix
prop_isSuffixOfBP   = L.isSuffixOf           `eq2`  P.isSuffixOf
prop_stripSuffixBP  = L.stripSuffix          `eq2`  P.stripSuffix
prop_mapBP          = L.map                  `eq2`  P.map
prop_replicateBP    = forAll arbitrarySizedIntegral $
                      (L.replicate. toInt64) `eq2`  P.replicate
prop_snocBP         = L.snoc                 `eq2`  P.snoc
prop_spanBP         = L.span                 `eq2`  P.span
prop_splitBP        = L.split                `eq2`  P.split
prop_splitAtBP      = (L.splitAt. toInt64)   `eq2`  P.splitAt
prop_takeBP         = (L.take   . toInt64)   `eq2`  P.take
prop_takeWhileBP    = L.takeWhile            `eq2`  P.takeWhile
prop_elemBP         = L.elem                 `eq2`  P.elem
prop_notElemBP      = L.notElem              `eq2`  P.notElem
prop_elemIndexBP    = L.elemIndex            `eq2`  ((fmap toInt64 .) . P.elemIndex)
prop_elemIndexEndBP = L.elemIndexEnd         `eq2`  ((fmap toInt64 .) . P.elemIndexEnd)
prop_elemIndicesBP  = L.elemIndices          `eq2`  ((fmap toInt64 .) . P.elemIndices)
prop_intersperseBP  = L.intersperse          `eq2`  P.intersperse
prop_lengthBP       = L.length               `eq1`  (toInt64 . P.length)
prop_readIntBP      = D.readInt              `eq1`  C.readInt
prop_linesBP        = D.lines                `eq1`  C.lines

-- double check:
-- Currently there's a bug in the lazy bytestring version of lines, this
-- catches it:
prop_linesNLBP      = eq1 D.lines C.lines x
    where x = D.pack "one\ntwo\n\n\nfive\n\nseven\n"

prop_headBP         = L.head        `eqnotnull1` P.head
prop_initBP         = L.init        `eqnotnull1` P.init
prop_lastBP         = L.last        `eqnotnull1` P.last
prop_maximumBP      = L.maximum     `eqnotnull1` P.maximum
prop_minimumBP      = L.minimum     `eqnotnull1` P.minimum
prop_tailBP         = L.tail        `eqnotnull1` P.tail
prop_foldl1BP       = L.foldl1      `eqnotnull2` P.foldl1
prop_foldl1BP'      = L.foldl1'     `eqnotnull2` P.foldl1'
prop_foldr1BP       = L.foldr1      `eqnotnull2` P.foldr1
prop_foldr1BP'      = L.foldr1      `eqnotnull2` P.foldr1'
prop_scanlBP        = L.scanl       `eqnotnull3` P.scanl


prop_eqBP        = eq2
    ((==) :: B -> B -> Bool)
    ((==) :: P -> P -> Bool)
prop_compareBP   = eq2
    ((compare) :: B -> B -> Ordering)
    ((compare) :: P -> P -> Ordering)
prop_foldlBP     = eq3
    (L.foldl     :: (X -> W -> X) -> X -> B -> X)
    (P.foldl     :: (X -> W -> X) -> X -> P -> X)
prop_foldlBP'    = eq3
    (L.foldl'    :: (X -> W -> X) -> X -> B -> X)
    (P.foldl'    :: (X -> W -> X) -> X -> P -> X)
prop_foldrBP     = eq3
    (L.foldr     :: (W -> X -> X) -> X -> B -> X)
    (P.foldr     :: (W -> X -> X) -> X -> P -> X)
prop_foldrBP'    = eq3
    (L.foldr     :: (W -> X -> X) -> X -> B -> X)
    (P.foldr'    :: (W -> X -> X) -> X -> P -> X)
prop_mapAccumLBP = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B -> (X, B))
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))

prop_unfoldrBP   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                     fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)

prop_unfoldr2BP   =
  forAll arbitrarySizedIntegral $ \n ->
  forAll arbitrarySizedIntegral $ \a ->
  eq2
    ((\n a -> P.take (n*100) $
        P.unfoldr    (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)
    ((\n a ->                     fst $
        P.unfoldrN (n*100) (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)
    n a

prop_unfoldr2CP   =
  forAll arbitrarySizedIntegral $ \n ->
  forAll arbitrarySizedIntegral $ \a ->
  eq2
    ((\n a -> C.take (n*100) $
        C.unfoldr    (\x -> if x <= (n*100) then Just (chr (x `mod` 256), x + 1) else Nothing) a)
                :: Int -> Int -> P)
    ((\n a ->                     fst $
        C.unfoldrN (n*100) (\x -> if x <= (n*100) then Just (chr (x `mod` 256), x + 1) else Nothing) a)
                :: Int -> Int -> P)
    n a
