{-# LANGUAGE CPP #-}
module Properties.BSModelList
    ( pl_tests
) where

import Data.List
import Data.String

import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString            as P
import qualified Data.ByteString.Char8      as C

import qualified Data.ByteString.Lazy.Char8 as LC

import Test.QuickCheck
import QuickCheckUtils
#if defined(HAVE_TEST_FRAMEWORK)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
#else
import TestFramework
#endif

import Shared



pl_tests = testGroup "ByteString <=> List && Some other stuff apparently"
    [ testProperty "all"         prop_allPL
    , testProperty "any"         prop_anyPL
    , testProperty "append"      prop_appendPL
    , testProperty "compare"     prop_comparePL
    , testProperty "concat"      prop_concatPL
    , testProperty "cons"        prop_consPL
    , testProperty "eq"          prop_eqPL
    , testProperty "filter"      prop_filterPL
    , testProperty "filter rules"prop_filterPL_rule
    , testProperty "filter rules"prop_filterLC_rule
    , testProperty "partition"   prop_partitionPL
    , testProperty "partition"   prop_partitionLL
    , testProperty "find"        prop_findPL
    , testProperty "findIndex"   prop_findIndexPL
    , testProperty "findIndexEnd"prop_findIndexEndPL
    , testProperty "findIndices" prop_findIndicesPL
    , testProperty "foldl"       prop_foldlPL
    , testProperty "foldl'"      prop_foldlPL'
    , testProperty "foldl1"      prop_foldl1PL
    , testProperty "foldl1'"     prop_foldl1PL'
    , testProperty "foldr1"      prop_foldr1PL
    , testProperty "foldr"       prop_foldrPL
    , testProperty "mapAccumL"   prop_mapAccumLPL
    , testProperty "mapAccumR"   prop_mapAccumRPL
    , testProperty "unfoldr"     prop_unfoldrPL
    , testProperty "scanl"       prop_scanlPL
    , testProperty "scanl1"      prop_scanl1PL
    , testProperty "scanl1"      prop_scanl1CL
    , testProperty "scanr"       prop_scanrCL
    , testProperty "scanr"       prop_scanrPL
    , testProperty "scanr1"      prop_scanr1PL
    , testProperty "scanr1"      prop_scanr1CL
    , testProperty "head"        prop_headPL
    , testProperty "init"        prop_initPL
    , testProperty "last"        prop_lastPL
    , testProperty "maximum"     prop_maximumPL
    , testProperty "minimum"     prop_minimumPL
    , testProperty "tail"        prop_tailPL
    , testProperty "zip"         prop_zipPL
    , testProperty "zip"         prop_zipLL
    , testProperty "zip"         prop_zipCL
    , testProperty "unzip"       prop_unzipPL
    , testProperty "unzip"       prop_unzipLL
    , testProperty "unzip"       prop_unzipCL
    , testProperty "zipWith"          prop_zipWithPL
--  , testProperty "zipWith"          prop_zipWithCL
    , testProperty "zipWith rules"   prop_zipWithPL_rules
--  , testProperty "zipWith/zipWith'" prop_zipWithPL'

    , testProperty "isPrefixOf"  prop_isPrefixOfPL
    , testProperty "isSuffixOf"  prop_isSuffixOfPL
    , testProperty "isInfixOf"   prop_isInfixOfPL
    , testProperty "stripPrefix" prop_stripPrefixPL
    , testProperty "stripSuffix" prop_stripSuffixPL
    , testProperty "length"      prop_lengthPL
    , testProperty "map"         prop_mapPL
    , testProperty "null"        prop_nullPL
    , testProperty "reverse"     prop_reversePL
    , testProperty "snoc"        prop_snocPL
    , testProperty "transpose"   prop_transposePL
    , testProperty "replicate"   prop_replicatePL
    , testProperty "take"        prop_takePL
    , testProperty "drop"        prop_dropPL
    , testProperty "splitAt"     prop_splitAtPL
    , testProperty "takeWhile"   prop_takeWhilePL
    , testProperty "dropWhile"   prop_dropWhilePL
    , testProperty "break"       prop_breakPL
    , testProperty "span"        prop_spanPL
    , testProperty "group"       prop_groupPL
    , testProperty "groupBy"     prop_groupByPL
    , testProperty "inits"       prop_initsPL
    , testProperty "tails"       prop_tailsPL
    , testProperty "elem"        prop_elemPL
    , testProperty "notElem"     prop_notElemPL
    , testProperty "lines"       prop_linesPL
    , testProperty "elemIndex"   prop_elemIndexPL
    , testProperty "elemIndex"   prop_elemIndexCL
    , testProperty "elemIndices" prop_elemIndicesPL
    , testProperty "concatMap"   prop_concatMapPL
    , testProperty "IsString"    prop_isstring
    , testProperty "IsString LC" prop_isstring_lc
    ]


--
-- And finally, check correspondance between Data.ByteString and List
--

prop_lengthPL     = (fromIntegral.P.length :: P -> Int) `eq1` (length :: [W] -> Int)
prop_nullPL       = P.null      `eq1` (null      :: [W] -> Bool)
prop_reversePL    = P.reverse   `eq1` (reverse   :: [W] -> [W])
prop_transposePL  = P.transpose `eq1` (transpose :: [[W]] -> [[W]])
prop_groupPL      = P.group     `eq1` (group     :: [W] -> [[W]])
prop_groupByPL    = P.groupBy   `eq2` (groupBy   :: (W -> W -> Bool) -> [W] -> [[W]])
prop_initsPL      = P.inits     `eq1` (inits     :: [W] -> [[W]])
prop_tailsPL      = P.tails     `eq1` (tails     :: [W] -> [[W]])
prop_concatPL     = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                    P.concat    `eq1` (concat    :: [[W]] -> [W])
prop_allPL        = P.all       `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyPL        = P.any       `eq2`    (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendPL     = P.append    `eq2`    ((++)      :: [W] -> [W] -> [W])
prop_breakPL      = P.break     `eq2`    (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatMapPL  = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $
                    P.concatMap `eq2`    (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consPL       = P.cons      `eq2`    ((:)       :: W -> [W] -> [W])
prop_dropPL       = P.drop      `eq2`    (drop      :: Int -> [W] -> [W])
prop_dropWhilePL  = P.dropWhile `eq2`    (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterPL     = P.filter    `eq2`    (filter    :: (W -> Bool ) -> [W] -> [W])
prop_filterPL_rule= (\x -> P.filter ((==) x))  `eq2` -- test rules
                    ((\x -> filter ((==) x)) :: W -> [W] -> [W])

-- under lambda doesn't fire?
prop_filterLC_rule= (f)  `eq2` -- test rules
                    ((\x -> filter ((==) x)) :: Char -> [Char] -> [Char])
    where
         f x s = LC.filter ((==) x) s

prop_partitionPL  = P.partition `eq2`    (partition :: (W -> Bool ) -> [W] -> ([W],[W]))
prop_partitionLL  = L.partition `eq2`    (partition :: (W -> Bool ) -> [W] -> ([W],[W]))
prop_findPL       = P.find      `eq2`    (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndexPL  = P.findIndex `eq2`    (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_findIndexEndPL = P.findIndexEnd `eq2` (findIndexEnd :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfPL = P.isPrefixOf`eq2`    (isPrefixOf:: [W] -> [W] -> Bool)
prop_isSuffixOfPL = P.isSuffixOf`eq2`    (isSuffixOf:: [W] -> [W] -> Bool)
prop_isInfixOfPL  = P.isInfixOf `eq2`    (isInfixOf:: [W] -> [W] -> Bool)
prop_stripPrefixPL = P.stripPrefix`eq2`  (stripPrefix:: [W] -> [W] -> Maybe [W])
prop_stripSuffixPL = P.stripSuffix`eq2`  (stripSuffix:: [W] -> [W] -> Maybe [W])
prop_mapPL        = P.map       `eq2`    (map       :: (W -> W) -> [W] -> [W])
prop_replicatePL  = forAll arbitrarySizedIntegral $
                    P.replicate `eq2`    (replicate :: Int -> W -> [W])
prop_snocPL       = P.snoc      `eq2`    ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanPL       = P.span      `eq2`    (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtPL    = P.splitAt   `eq2`    (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takePL       = P.take      `eq2`    (take      :: Int -> [W] -> [W])
prop_takeWhilePL  = P.takeWhile `eq2`    (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemPL       = P.elem      `eq2`    (elem      :: W -> [W] -> Bool)
prop_notElemPL    = P.notElem   `eq2`    (notElem   :: W -> [W] -> Bool)
prop_elemIndexPL  = P.elemIndex `eq2`    (elemIndex :: W -> [W] -> Maybe Int)
prop_linesPL      = C.lines     `eq1`    (lines     :: String -> [String])
prop_findIndicesPL= P.findIndices`eq2`   (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_elemIndicesPL= P.elemIndices`eq2`   (elemIndices:: W -> [W] -> [Int])
prop_zipPL        = P.zip        `eq2`   (zip :: [W] -> [W] -> [(W,W)])
prop_zipCL        = C.zip        `eq2`   (zip :: [Char] -> [Char] -> [(Char,Char)])
prop_zipLL        = L.zip        `eq2`   (zip :: [W] -> [W] -> [(W,W)])
prop_unzipPL      = P.unzip      `eq1`   (unzip :: [(W,W)] -> ([W],[W]))
prop_unzipLL      = L.unzip      `eq1`   (unzip :: [(W,W)] -> ([W],[W]))

prop_unzipCL :: [(Char8, Char8)] -> Bool
prop_unzipCL xs   = (C.unzip     `eq1`   (unzip :: [(Char,Char)] -> ([Char],[Char])))
                    [ (a,b) | (Char8 a, Char8 b) <- xs ]

prop_foldl1PL     = P.foldl1    `eqnotnull2` (foldl1   :: (W -> W -> W) -> [W] -> W)
prop_foldl1PL'    = P.foldl1'   `eqnotnull2` (foldl1' :: (W -> W -> W) -> [W] -> W)
prop_foldr1PL     = P.foldr1    `eqnotnull2` (foldr1 :: (W -> W -> W) -> [W] -> W)
prop_scanlPL      = P.scanl     `eqnotnull3` (scanl  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanl1PL     = P.scanl1    `eqnotnull2` (scanl1 :: (W -> W -> W) -> [W] -> [W])
prop_scanrPL      = P.scanr     `eqnotnull3` (scanr  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanr1PL     = P.scanr1    `eqnotnull2` (scanr1 :: (W -> W -> W) -> [W] -> [W])
prop_headPL       = P.head      `eqnotnull1` (head      :: [W] -> W)
prop_initPL       = P.init      `eqnotnull1` (init      :: [W] -> [W])
prop_lastPL       = P.last      `eqnotnull1` (last      :: [W] -> W)
prop_maximumPL    = P.maximum   `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumPL    = P.minimum   `eqnotnull1` (minimum   :: [W] -> W)
prop_tailPL       = P.tail      `eqnotnull1` (tail      :: [W] -> [W])

prop_scanl1CL :: (Char8 -> Char8 -> Char8) -> P -> Property
prop_scanrCL  :: (Char8 -> Char8 -> Char8) -> Char8 -> P -> Property
prop_scanr1CL :: (Char8 -> Char8 -> Char8) -> P -> Property

prop_scanl1CL f = eqnotnull2
    C.scanl1
    (scanl1 :: (Char -> Char -> Char) -> [Char] -> [Char])
    (castFn f)

prop_scanrCL f (Char8 c) = eqnotnull3
    C.scanr
    (scanr  :: (Char -> Char -> Char) -> Char -> [Char] -> [Char])
    (castFn f)
    c

prop_scanr1CL f = eqnotnull2
    C.scanr1
    (scanr1 :: (Char -> Char -> Char) -> [Char] -> [Char])
    (castFn f)

-- prop_zipWithPL'   = P.zipWith'  `eq3` (zipWith :: (W -> W -> W) -> [W] -> [W] -> [W])

prop_zipWithPL    = (P.zipWith  :: (W -> W -> X) -> P   -> P   -> [X]) `eq3`
                      (zipWith  :: (W -> W -> X) -> [W] -> [W] -> [X])

prop_zipWithPL_rules   = (P.zipWith  :: (W -> W -> W) -> P -> P -> [W]) `eq3`
                         (zipWith    :: (W -> W -> W) -> [W] -> [W] -> [W])

prop_eqPL      = eq2
    ((==) :: P   -> P   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_comparePL = eq2
    ((compare) :: P   -> P   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlPL   = eq3
    (P.foldl  :: (X -> W -> X) -> X -> P        -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldlPL'  = eq3
    (P.foldl' :: (X -> W -> X) -> X -> P        -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldrPL   = eq3
    (P.foldr  :: (W -> X -> X) -> X -> P        -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W]      -> X)
prop_mapAccumLPL= eq3
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_mapAccumRPL= eq3
    (P.mapAccumR :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_unfoldrPL =
  forAll arbitrarySizedIntegral $
  eq3
    ((\n f a ->      fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)
    ((\n f a ->   take n $
          unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

prop_elemIndexCL :: String8 -> Char8 -> Bool
prop_elemIndexCL (String8 xs) (Char8 c) =
    (elemIndex c xs) == (C.elemIndex c (C.pack xs))

------------------------------------------------------------------------

-- Test IsString, Show, Read, pack, unpack
prop_isstring    :: String8 -> Bool
prop_isstring_lc :: String8 -> Bool

prop_isstring    (String8 x) = C.unpack  (fromString x :: C.ByteString) == x
prop_isstring_lc (String8 x) = LC.unpack (fromString x :: LC.ByteString) == x
