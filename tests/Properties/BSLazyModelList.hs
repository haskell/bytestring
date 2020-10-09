{-# LANGUAGE CPP #-}
module Properties.BSLazyModelList
    ( bl_tests
) where

import Data.Int (Int64)
import Data.List

import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString            as P
import qualified Data.ByteString.Char8      as C

import qualified Data.ByteString.Lazy.Char8 as LC
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
-- ByteString.Lazy <=> List

bl_tests = testGroup "ByteString.Lazy <=> List && Some other stuff apparently"
    [ testProperty "all"         prop_allBL
    , testProperty "any"         prop_anyBL
    , testProperty "append"      prop_appendBL
    , testProperty "compare"     prop_compareBL
    , testProperty "concat"      prop_concatBL
    , testProperty "cons"        prop_consBL
    , testProperty "eq"          prop_eqBL
    , testProperty "filter"      prop_filterBL
    , testProperty "find"        prop_findBL
    , testProperty "findIndex"   prop_findIndexBL
    , testProperty "findIndexEnd"prop_findIndexEndBL
    , testProperty "findIndices" prop_findIndicesBL
    , testProperty "foldl"       prop_foldlBL
    , testProperty "foldl'"      prop_foldlBL'
    , testProperty "foldl1"      prop_foldl1BL
    , testProperty "foldl1'"     prop_foldl1BL'
    , testProperty "foldr"       prop_foldrBL
    , testProperty "foldr1"      prop_foldr1BL
    , testProperty "mapAccumL"   prop_mapAccumLBL
    , testProperty "mapAccumR"   prop_mapAccumRBL
    , testProperty "mapAccumR"   prop_mapAccumRDL
    , testProperty "mapAccumR"   prop_mapAccumRCC
    , testProperty "unfoldr"     prop_unfoldrBL
    , testProperty "unfoldr"     prop_unfoldrLC
    , testProperty "unfoldr"     prop_cycleLC
    , testProperty "iterate"     prop_iterateLC
    , testProperty "iterate"     prop_iterateLC_2
    , testProperty "iterate"     prop_iterateL
    , testProperty "repeat"      prop_repeatLC
    , testProperty "repeat"      prop_repeatL
    , testProperty "head"        prop_headBL
    , testProperty "init"        prop_initBL
    , testProperty "isPrefixOf"  prop_isPrefixOfBL
    , testProperty "isSuffixOf"  prop_isSuffixOfBL
    , testProperty "stripPrefix" prop_stripPrefixBL
    , testProperty "stripSuffix" prop_stripSuffixBL
    , testProperty "last"        prop_lastBL
    , testProperty "length"      prop_lengthBL
    , testProperty "map"         prop_mapBL
    , testProperty "maximum"     prop_maximumBL
    , testProperty "minimum"     prop_minimumBL
    , testProperty "null"        prop_nullBL
    , testProperty "reverse"     prop_reverseBL
    , testProperty "snoc"        prop_snocBL
    , testProperty "tail"        prop_tailBL
    , testProperty "transpose"   prop_transposeBL
    , testProperty "replicate"   prop_replicateBL
    , testProperty "take"        prop_takeBL
    , testProperty "drop"        prop_dropBL
    , testProperty "splitAt"     prop_splitAtBL
    , testProperty "takeWhile"   prop_takeWhileBL
    , testProperty "dropWhile"   prop_dropWhileBL
    , testProperty "break"       prop_breakBL
    , testProperty "span"        prop_spanBL
    , testProperty "group"       prop_groupBL
    , testProperty "groupBy"     prop_groupByBL
    , testProperty "inits"       prop_initsBL
    , testProperty "tails"       prop_tailsBL
    , testProperty "elem"        prop_elemBL
    , testProperty "notElem"     prop_notElemBL
    , testProperty "lines"       prop_linesBL
    , testProperty "elemIndex"   prop_elemIndexBL
    , testProperty "elemIndexEnd"prop_elemIndexEndBL
    , testProperty "elemIndices" prop_elemIndicesBL
    , testProperty "concatMap"   prop_concatMapBL
    ]

--
-- properties comparing ByteString.Lazy `eq1` List
--

prop_concatBL       = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                      L.concat                `eq1` (concat    :: [[W]] -> [W])
prop_lengthBL       = L.length                `eq1` (toInt64 . length    :: [W] -> Int64)
prop_nullBL         = L.null                  `eq1` (null      :: [W] -> Bool)
prop_reverseBL      = L.reverse               `eq1` (reverse   :: [W] -> [W])
prop_transposeBL    = L.transpose             `eq1` (transpose :: [[W]] -> [[W]])
prop_groupBL        = L.group                 `eq1` (group     :: [W] -> [[W]])
prop_groupByBL      = L.groupBy               `eq2` (groupBy   :: (W -> W -> Bool) -> [W] -> [[W]])
prop_initsBL        = L.inits                 `eq1` (inits     :: [W] -> [[W]])
prop_tailsBL        = L.tails                 `eq1` (tails     :: [W] -> [[W]])
prop_allBL          = L.all                   `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyBL          = L.any                   `eq2` (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendBL       = L.append                `eq2` ((++)      :: [W] -> [W] -> [W])
prop_breakBL        = L.break                 `eq2` (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatMapBL    = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                      L.concatMap             `eq2` (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consBL         = L.cons                  `eq2` ((:)       :: W -> [W] -> [W])
prop_dropBL         = (L.drop . toInt64)      `eq2` (drop      :: Int -> [W] -> [W])
prop_dropWhileBL    = L.dropWhile             `eq2` (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterBL       = L.filter                `eq2` (filter    :: (W -> Bool ) -> [W] -> [W])
prop_findBL         = L.find                  `eq2` (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndicesBL  = L.findIndices           `eq2` ((fmap toInt64 .) . findIndices:: (W -> Bool) -> [W] -> [Int64])
prop_findIndexBL    = L.findIndex             `eq2` ((fmap toInt64 .) . findIndex :: (W -> Bool) -> [W] -> Maybe Int64)
prop_findIndexEndBL = L.findIndexEnd          `eq2` ((fmap toInt64 .) . findIndexEnd :: (W -> Bool) -> [W] -> Maybe Int64)
prop_isPrefixOfBL   = L.isPrefixOf            `eq2` (isPrefixOf:: [W] -> [W] -> Bool)
prop_stripPrefixBL  = L.stripPrefix           `eq2` (stripPrefix:: [W] -> [W] -> Maybe [W])
prop_isSuffixOfBL   = L.isSuffixOf            `eq2` (isSuffixOf:: [W] -> [W] -> Bool)
prop_stripSuffixBL  = L.stripSuffix           `eq2` (stripSuffix :: [W] -> [W] -> Maybe [W])
prop_mapBL          = L.map                   `eq2` (map       :: (W -> W) -> [W] -> [W])
prop_replicateBL    = forAll arbitrarySizedIntegral $
                      (L.replicate . toInt64) `eq2` (replicate :: Int -> W -> [W])
prop_snocBL         = L.snoc                  `eq2` ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanBL         = L.span                  `eq2` (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtBL      = (L.splitAt . toInt64)   `eq2` (splitAt :: Int -> [W] -> ([W],[W]))
prop_takeBL         = (L.take    . toInt64)   `eq2` (take    :: Int -> [W] -> [W])
prop_takeWhileBL    = L.takeWhile             `eq2` (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemBL         = L.elem                  `eq2` (elem      :: W -> [W] -> Bool)
prop_notElemBL      = L.notElem               `eq2` (notElem   :: W -> [W] -> Bool)
prop_elemIndexBL    = L.elemIndex             `eq2` ((fmap toInt64 .) . elemIndex   :: W -> [W] -> Maybe Int64)
prop_elemIndexEndBL = L.elemIndexEnd          `eq2` ((fmap toInt64 .) . elemIndexEnd:: W -> [W] -> Maybe Int64)
prop_elemIndicesBL  = L.elemIndices           `eq2` ((fmap toInt64 .) . elemIndices :: W -> [W] -> [Int64])
prop_linesBL        = D.lines                 `eq1` (lines     :: String -> [String])

prop_foldl1BL       = L.foldl1  `eqnotnull2` (foldl1    :: (W -> W -> W) -> [W] -> W)
prop_foldl1BL'      = L.foldl1' `eqnotnull2` (foldl1'   :: (W -> W -> W) -> [W] -> W)
prop_foldr1BL       = L.foldr1  `eqnotnull2` (foldr1    :: (W -> W -> W) -> [W] -> W)
prop_headBL         = L.head    `eqnotnull1` (head      :: [W] -> W)
prop_initBL         = L.init    `eqnotnull1` (init      :: [W] -> [W])
prop_lastBL         = L.last    `eqnotnull1` (last      :: [W] -> W)
prop_maximumBL      = L.maximum `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumBL      = L.minimum `eqnotnull1` (minimum   :: [W] -> W)
prop_tailBL         = L.tail    `eqnotnull1` (tail      :: [W] -> [W])

prop_eqBL         = eq2
    ((==) :: B   -> B   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_compareBL    = eq2
    ((compare) :: B   -> B   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlBL      = eq3
    (L.foldl  :: (X -> W -> X) -> X -> B   -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W] -> X)
prop_foldlBL'     = eq3
    (L.foldl' :: (X -> W -> X) -> X -> B   -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W] -> X)
prop_foldrBL      = eq3
    (L.foldr  :: (W -> X -> X) -> X -> B   -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W] -> X)
prop_mapAccumLBL  = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

prop_mapAccumRBL  = eq3
    (L.mapAccumR :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

prop_mapAccumRDL :: (X -> Char8 -> (X, Char8)) -> X -> B -> Bool
prop_mapAccumRDL f = eq3
    (D.mapAccumR :: (X -> Char -> (X,Char)) -> X -> B   -> (X, B))
    (  mapAccumR :: (X -> Char -> (X,Char)) -> X -> [Char] -> (X, [Char]))
    (castFn f)

prop_mapAccumRCC :: (X -> Char8 -> (X, Char8)) -> X -> P -> Bool
prop_mapAccumRCC f = eq3
    (C.mapAccumR :: (X -> Char -> (X,Char)) -> X -> P   -> (X, P))
    (  mapAccumR :: (X -> Char -> (X,Char)) -> X -> [Char] -> (X, [Char]))
    (castFn f)

prop_unfoldrBL =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                  take n $
          unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

prop_unfoldrLC   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.unfoldr    f a) :: Int -> (X -> Maybe (Char,X)) -> X -> B)
    ((\n f a ->                     fst $
        C.unfoldrN n f a) :: Int -> (X -> Maybe (Char,X)) -> X -> P)

prop_cycleLC  a   =
  not (LC.null a) ==>
  forAll arbitrarySizedIntegral $
  eq1
    ((\n   -> LC.take (fromIntegral n) $
              LC.cycle a
     ) :: Int -> B)

    ((\n   -> LC.take (fromIntegral (n::Int)) . LC.concat $
              unfoldr (\x ->  Just (x,x) ) a
     ) :: Int -> B)


prop_iterateLC :: Int -> (Char8 -> Char8) -> Char8 -> Bool
prop_iterateLC n f (Char8 c) =
  eq3
    (\n f a -> LC.take (fromIntegral n) $ LC.iterate  f a)
    (\n f a -> fst $ C.unfoldrN n (\a -> Just (f a, f a)) a)
    n
    (castFn f :: Char -> Char)
    c

prop_iterateLC_2 :: Int -> (Char8 -> Char8) -> Char8 -> Bool
prop_iterateLC_2 n f (Char8 c) =
  eq3
    (\n f a -> LC.take (fromIntegral (n :: Int)) $ LC.iterate f a)
    (\n f a -> LC.take (fromIntegral (n :: Int)) $ LC.unfoldr (\a -> Just (f a, f a)) a)
    n
    (castFn f :: Char -> Char)
    c

prop_iterateL   =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.iterate  f a) :: Int -> (W -> W) -> W -> B)
    ((\n f a -> fst $
        P.unfoldrN n (\a -> Just (f a, f a)) a) :: Int -> (W -> W) -> W -> P)

prop_repeatLC   =
  forAll arbitrarySizedIntegral $
  eq2
    ((\n a -> LC.take (fromIntegral n) $
        LC.repeat a) :: Int -> Char -> B)
    ((\n a -> fst $
        C.unfoldrN n (\a -> Just (a, a)) a) :: Int -> Char -> P)

prop_repeatL   =
  forAll arbitrarySizedIntegral $
  eq2
    ((\n a -> L.take (fromIntegral n) $
        L.repeat a) :: Int -> W -> B)
    ((\n a -> fst $
        P.unfoldrN n (\a -> Just (a, a)) a) :: Int -> W -> P)