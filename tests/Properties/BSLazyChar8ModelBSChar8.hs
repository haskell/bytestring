{-# LANGUAGE CPP #-}
module Properties.BSLazyChar8ModelBSChar8
    ( cc_tests
) where

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

------------------------------------------------------------------------
-- ByteString.Lazy.Char8 <=> ByteString.Char8

cc_tests = testGroup "ByteString.Lazy.Char8 <=> ByteString.Char8"
    [ testProperty "prop_concatCC"      prop_concatCC
    , testProperty "prop_nullCC"        prop_nullCC
    , testProperty "prop_reverseCC"     prop_reverseCC
    , testProperty "prop_transposeCC"   prop_transposeCC
    , testProperty "prop_groupCC"       prop_groupCC
    , testProperty "prop_groupByCC"     prop_groupByCC
    , testProperty "prop_initsCC"       prop_initsCC
    , testProperty "prop_tailsCC"       prop_tailsCC
    , testProperty "prop_allCC"         prop_allCC
    , testProperty "prop_anyCC"         prop_anyCC
    , testProperty "prop_appendCC"      prop_appendCC
    , testProperty "prop_breakCC"       prop_breakCC
    , testProperty "prop_concatMapCC"   prop_concatMapCC
    , testProperty "prop_consCC"        prop_consCC
    , testProperty "prop_consCC'"       prop_consCC'
    , testProperty "prop_unconsCC"      prop_unconsCC
    , testProperty "prop_unsnocCC"      prop_unsnocCC
    , testProperty "prop_countCC"       prop_countCC
    , testProperty "prop_dropCC"        prop_dropCC
    , testProperty "prop_dropWhileCC"   prop_dropWhileCC
    , testProperty "prop_filterCC"      prop_filterCC
    , testProperty "prop_findCC"        prop_findCC
    , testProperty "prop_findIndexCC"   prop_findIndexCC
    , testProperty "prop_findIndicesCC" prop_findIndicesCC
    , testProperty "prop_isPrefixCC"    prop_isPrefixOfCC
    , testProperty "prop_isSuffixCC"    prop_isSuffixOfCC
    , testProperty "prop_stripPrefixCC" prop_stripPrefixCC
    , testProperty "prop_stripSuffixCC" prop_stripSuffixCC
    , testProperty "prop_mapCC"         prop_mapCC
    , testProperty "prop_replicateCC"   prop_replicateCC
    , testProperty "prop_snocCC"        prop_snocCC
    , testProperty "prop_spanCC"        prop_spanCC
    , testProperty "prop_splitCC"       prop_splitCC
    , testProperty "prop_splitAtCC"     prop_splitAtCC
    , testProperty "prop_takeCC"        prop_takeCC
    , testProperty "prop_takeWhileCC"   prop_takeWhileCC
    , testProperty "prop_elemCC"        prop_elemCC
    , testProperty "prop_notElemCC"     prop_notElemCC
    , testProperty "prop_elemIndexCC"   prop_elemIndexCC
    , testProperty "prop_elemIndicesCC" prop_elemIndicesCC
    , testProperty "prop_lengthCC"      prop_lengthCC
    , testProperty "prop_headCC"        prop_headCC
    , testProperty "prop_initCC"        prop_initCC
    , testProperty "prop_lastCC"        prop_lastCC
    , testProperty "prop_maximumCC"     prop_maximumCC
    , testProperty "prop_minimumCC"     prop_minimumCC
    , testProperty "prop_tailCC"        prop_tailCC
    , testProperty "prop_foldl1CC"      prop_foldl1CC
    , testProperty "prop_foldl1CC'"     prop_foldl1CC'
    , testProperty "prop_foldr1CC"      prop_foldr1CC
    , testProperty "prop_foldr1CC'"     prop_foldr1CC'
    , testProperty "prop_scanlCC"       prop_scanlCC
    , testProperty "prop_intersperseCC" prop_intersperseCC

    , testProperty "prop_foldlCC"       prop_foldlCC
    , testProperty "prop_foldlCC'"      prop_foldlCC'
    , testProperty "prop_foldrCC"       prop_foldrCC
    , testProperty "prop_foldrCC'"      prop_foldrCC'
    , testProperty "prop_mapAccumLCC"   prop_mapAccumLCC
    ]


--
-- ByteString.Lazy.Char8 <=> ByteString.Char8
--

prop_concatCC       = D.concat                `eq1`  C.concat
prop_nullCC         = D.null                  `eq1`  C.null
prop_reverseCC      = D.reverse               `eq1`  C.reverse
prop_transposeCC    = D.transpose             `eq1`  C.transpose
prop_groupCC        = D.group                 `eq1`  C.group
prop_groupByCC      = D.groupBy               `eq2`  C.groupBy
prop_initsCC        = D.inits                 `eq1`  C.inits
prop_tailsCC        = D.tails                 `eq1`  C.tails
prop_allCC          = D.all                   `eq2`  C.all
prop_anyCC          = D.any                   `eq2`  C.any
prop_appendCC       = D.append                `eq2`  C.append
prop_breakCC        = D.break                 `eq2`  C.break
prop_concatMapCC    = forAll (sized $ \n -> resize (min 50 n) arbitrary) $
                      D.concatMap             `eq2`  C.concatMap
prop_consCC         = D.cons                  `eq2`  C.cons
prop_consCC'        = D.cons'                 `eq2`  C.cons
prop_unconsCC       = D.uncons                `eq1`  C.uncons
prop_unsnocCC       = D.unsnoc                `eq1`  C.unsnoc
prop_countCC        = D.count                 `eq2`  ((toInt64 .) . C.count)
prop_dropCC         = (D.drop . toInt64)      `eq2`  C.drop
prop_dropWhileCC    = D.dropWhile             `eq2`  C.dropWhile
prop_filterCC       = D.filter                `eq2`  C.filter
prop_findCC         = D.find                  `eq2`  C.find
prop_findIndexCC    = D.findIndex             `eq2`  ((fmap toInt64 .) . C.findIndex)
prop_findIndicesCC  = D.findIndices           `eq2`  ((fmap toInt64 .) . C.findIndices)
prop_isPrefixOfCC   = D.isPrefixOf            `eq2`  C.isPrefixOf
prop_stripPrefixCC  = D.stripPrefix           `eq2`  C.stripPrefix
prop_isSuffixOfCC   = D.isSuffixOf            `eq2`  C.isSuffixOf
prop_stripSuffixCC  = D.stripSuffix           `eq2`  C.stripSuffix
prop_mapCC          = D.map                   `eq2`  C.map
prop_replicateCC    = forAll arbitrarySizedIntegral $
                      (D.replicate . toInt64) `eq2`  C.replicate
prop_snocCC         = D.snoc                  `eq2`  C.snoc
prop_spanCC         = D.span                  `eq2`  C.span
prop_splitCC        = D.split                 `eq2`  C.split
prop_splitAtCC      = (D.splitAt . toInt64)   `eq2`  C.splitAt
prop_takeCC         = (D.take    . toInt64)   `eq2`  C.take
prop_takeWhileCC    = D.takeWhile             `eq2`  C.takeWhile
prop_elemCC         = D.elem                  `eq2`  C.elem
prop_notElemCC      = D.notElem               `eq2`  C.notElem
prop_elemIndexCC    = D.elemIndex             `eq2`  ((fmap toInt64 .) . C.elemIndex)
prop_elemIndicesCC  = D.elemIndices           `eq2`  ((fmap toInt64 .) . C.elemIndices)
prop_lengthCC       = D.length                `eq1`  (toInt64 . C.length)

prop_headCC         = D.head        `eqnotnull1` C.head
prop_initCC         = D.init        `eqnotnull1` C.init
prop_lastCC         = D.last        `eqnotnull1` C.last
prop_maximumCC      = D.maximum     `eqnotnull1` C.maximum
prop_minimumCC      = D.minimum     `eqnotnull1` C.minimum
prop_tailCC         = D.tail        `eqnotnull1` C.tail
prop_foldl1CC       = D.foldl1      `eqnotnull2` C.foldl1
prop_foldl1CC'      = D.foldl1'     `eqnotnull2` C.foldl1'
prop_foldr1CC       = D.foldr1      `eqnotnull2` C.foldr1
prop_foldr1CC'      = D.foldr1      `eqnotnull2` C.foldr1'
prop_scanlCC        = D.scanl       `eqnotnull3` C.scanl

prop_intersperseCC = D.intersperse  `eq2` C.intersperse

prop_foldlCC     = eq3
    (D.foldl     :: (X -> Char -> X) -> X -> B -> X)
    (C.foldl     :: (X -> Char -> X) -> X -> P -> X)
prop_foldlCC'    = eq3
    (D.foldl'    :: (X -> Char -> X) -> X -> B -> X)
    (C.foldl'    :: (X -> Char -> X) -> X -> P -> X)
prop_foldrCC     = eq3
    (D.foldr     :: (Char -> X -> X) -> X -> B -> X)
    (C.foldr     :: (Char -> X -> X) -> X -> P -> X)
prop_foldrCC'    = eq3
    (D.foldr     :: (Char -> X -> X) -> X -> B -> X)
    (C.foldr'    :: (Char -> X -> X) -> X -> P -> X)
prop_mapAccumLCC = eq3
    (D.mapAccumL :: (X -> Char -> (X,Char)) -> X -> B -> (X, B))
    (C.mapAccumL :: (X -> Char -> (X,Char)) -> X -> P -> (X, P))