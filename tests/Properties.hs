#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

import Test.QuickCheck.Batch
import Test.QuickCheck
import Text.Show.Functions

import Data.List
import Data.Char
import Data.Word
import Data.Maybe
import Data.Int (Int64)

import Text.Printf

import System.Environment
import System.IO
import System.Random

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString      as P
import Prelude hiding (abs)

import QuickCheckUtils

instance Arbitrary Char where
    arbitrary     = choose ('a', 'i') -- since we have to test words, unlines too
    coarbitrary c = variant (ord c `rem` 4)

instance Arbitrary Word8 where
    arbitrary = choose (97, 105)
    coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 4))

instance Random Word8 where
  randomR (a,b) g = case randomR (fromIntegral a :: Integer
                                 ,fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x :: Word8, g)
  random g        = randomR (minBound,maxBound) g

instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . L.LPS . filter (not. P.null) -- maintain the invariant.
    coarbitrary s = coarbitrary (L.unpack s)

instance Arbitrary P.ByteString where
  arbitrary = P.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (P.unpack s)

------------------------------------------------------------------------

main = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-20s: " s >> a n) tests

tests =
    [("invariant",          mytest prop_invariant)
    ,("eq 1",               mytest prop_eq1)
    ,("eq 2",               mytest prop_eq2)
    ,("eq 3",               mytest prop_eq3)
    ,("compare 1",          mytest prop_compare1)
    ,("compare 2",          mytest prop_compare2)
    ,("compare 3",          mytest prop_compare3)
    ,("compare 4",          mytest prop_compare4)
    ,("compare 5",          mytest prop_compare5)
    ,("compare 6",          mytest prop_compare6)
    ,("compare 7",          mytest prop_compare7)
    ,("compare 8",          mytest prop_compare8)
    ,("empty 1",            mytest prop_empty1)
    ,("empty 2",            mytest prop_empty2)
    ,("pack/unpack",        mytest prop_packunpack)
    ,("unpack/pack",        mytest prop_unpackpack)
    ,("null",               mytest prop_null)
    ,("length 1",           mytest prop_length1)
    ,("length 2",           mytest prop_length2)
    ,("cons 1"    ,         mytest prop_cons1)
    ,("cons 2"    ,         mytest prop_cons2)
    ,("cons 3"    ,         mytest prop_cons3)
    ,("cons 4"    ,         mytest prop_cons4)
    ,("snoc"    ,           mytest prop_snoc1)
    ,("head/pack",          mytest prop_head)
    ,("head/unpack",        mytest prop_head1)
    ,("tail/pack",          mytest prop_tail)
    ,("tail/unpack",        mytest prop_tail1)
    ,("last",               mytest prop_last)
    ,("init",               mytest prop_init)
    ,("append 1",           mytest prop_append1)
    ,("append 2",           mytest prop_append2)
    ,("append 3",           mytest prop_append3)
    ,("map 1",              mytest prop_map1)
    ,("map 2",              mytest prop_map2)
    ,("map 3",              mytest prop_map3)
    ,("filter 1",           mytest prop_filter1)
    ,("filter 2",           mytest prop_filter2)
    ,("reverse",            mytest prop_reverse)
    ,("reverse1",           mytest prop_reverse1)
    ,("reverse2",           mytest prop_reverse2)
    ,("transpose",          mytest prop_transpose)
    ,("foldl",              mytest prop_foldl)
    ,("foldl/reverse",      mytest prop_foldl_1)
    ,("foldr",              mytest prop_foldr)
    ,("foldr/id",           mytest prop_foldr_1)
    ,("foldl1/foldl",       mytest prop_foldl1_1)
    ,("foldl1/head",        mytest prop_foldl1_2)
    ,("foldl1/tail",        mytest prop_foldl1_3)
    ,("foldr1/foldr",       mytest prop_foldr1_1)
    ,("foldr1/last",        mytest prop_foldr1_2)
    ,("foldr1/head",        mytest prop_foldr1_3)
    ,("concat 1",           mytest prop_concat1)
    ,("concat 2",           mytest prop_concat2)
    ,("concat/pack",        mytest prop_concat3)
    ,("concatMap",          mytest prop_concatMap)
    ,("any",                mytest prop_any)
    ,("all",                mytest prop_all)
    ,("maximum",            mytest prop_maximum)
    ,("minimum",            mytest prop_minimum)
    ,("replicate 1",        mytest prop_replicate1)
    ,("replicate 2",        mytest prop_replicate2)
    ,("take",               mytest prop_take1)
    ,("drop",               mytest prop_drop1)
    ,("splitAt",            mytest prop_drop1)
    ,("takeWhile",          mytest prop_takeWhile)
    ,("dropWhile",          mytest prop_dropWhile)
    ,("break",              mytest prop_break)
    ,("span",               mytest prop_span)
    ,("break/span",         mytest prop_breakspan)
    ,("break/breakByte",    mytest prop_breakByte)
    ,("span/spanByte",      mytest prop_spanByte)
    ,("breakFirst/break",   mytest prop_breakFirst)
    ,("split",              mytest prop_split)
    ,("splitWith",          mytest prop_splitWith)
    ,("join.split/id",      mytest prop_joinsplit)
    ,("join/joinByte",      mytest prop_joinjoinByte)
    ,("group",              mytest prop_group)
    ,("groupBy",            mytest prop_groupBy)
    ,("index",              mytest prop_index)
    ,("elemIndex",          mytest prop_elemIndex)
    ,("elemIndices",        mytest prop_elemIndices)
    ,("count/elemIndices",  mytest prop_count)
    ,("findIndex",          mytest prop_findIndex)
    ,("findIndices",        mytest prop_findIndicies)
    ,("find",               mytest prop_find)
    ,("find/findIndex",     mytest prop_find_findIndex)
    ,("elem",               mytest prop_elem)
    ,("notElem",            mytest prop_notElem)
    ,("elem/notElem",       mytest prop_elem_notelem)
    ,("filterByte 1",       mytest prop_filterByte)
    ,("filterByte 2",       mytest prop_filterByte2)
    ,("filterNotByte 1",    mytest prop_filterNotByte)
    ,("filterNotByte 2",    mytest prop_filterNotByte2)
    ,("isPrefixOf",         mytest prop_isPrefixOf)

------------------------------------------------------------------------
-- ByteString.Lazy <=> List

    ,("all",         mytest prop_allBL)
    ,("any",         mytest prop_anyBL)
    ,("append",      mytest prop_appendBL)
    ,("compare",     mytest prop_compareBL)
    ,("concat",      mytest prop_concatBL)
    ,("concatMap",   mytest prop_concatMapBL)
    ,("cons",        mytest prop_consBL)
    ,("eq",          mytest prop_eqBL)
    ,("filter",      mytest prop_filterBL)
    ,("find",        mytest prop_findBL)
    ,("findIndex",   mytest prop_findIndexBL)
    ,("findIndices", mytest prop_findIndicesBL)
    ,("foldl",       mytest prop_foldlBL)
    ,("foldl",       mytest prop_foldlBL')
    ,("foldl1",      mytest prop_foldl1BL)
    ,("foldr",       mytest prop_foldrBL)
    ,("foldr1",      mytest prop_foldr1BL)
    ,("head",        mytest prop_headBL)
    ,("init",        mytest prop_initBL)
    ,("isPrefixOf",  mytest prop_isPrefixOfBL)
    ,("last",        mytest prop_lastBL)
    ,("length",      mytest prop_lengthBL)
    ,("map",         mytest prop_mapBL)
    ,("maximum",     mytest prop_maximumBL)
    ,("minimum",     mytest prop_minimumBL)
    ,("null",        mytest prop_nullBL)
    ,("reverse",     mytest prop_reverseBL)
    ,("snoc",        mytest prop_snocBL)
    ,("tail",        mytest prop_tailBL)
    ,("transpose",   mytest prop_transposeBL)
    ,("replicate",   mytest prop_replicateBL)
    ,("take",        mytest prop_takeBL)
    ,("drop",        mytest prop_dropBL)
    ,("splitAt",     mytest prop_splitAtBL)
    ,("takeWhile",   mytest prop_takeWhileBL)
    ,("dropWhile",   mytest prop_dropWhileBL)
    ,("break",       mytest prop_breakBL)
    ,("span",        mytest prop_spanBL)
    ,("group",       mytest prop_groupBL)
    ,("elem",        mytest prop_elemBL)
    ,("notElem",     mytest prop_notElemBL)
    ,("elemIndex",   mytest prop_elemIndexBL)
    ,("elemIndices", mytest prop_elemIndicesBL)

------------------------------------------------------------------------
-- ByteString.Lazy <=> ByteString

    ,("all",         mytest prop_allBP)
    ,("any",         mytest prop_anyBP)
    ,("append",      mytest prop_appendBP)
    ,("compare",     mytest prop_compareBP)
    ,("concat",      mytest prop_concatBP)
    ,("concatMap",   mytest prop_concatMapBP)
    ,("cons",        mytest prop_consBP)
    ,("eq",          mytest prop_eqBP)
    ,("filter",      mytest prop_filterBP)
    ,("find",        mytest prop_findBP)
    ,("findIndex",   mytest prop_findIndexBP)
    ,("findIndices", mytest prop_findIndicesBP)
    ,("foldl",       mytest prop_foldlBP)
    ,("foldl",       mytest prop_foldlBP')
    ,("foldl1",      mytest prop_foldl1BP)
    ,("foldr",       mytest prop_foldrBP)
    ,("foldr1",      mytest prop_foldr1BP)
    ,("head",        mytest prop_headBP)
    ,("init",        mytest prop_initBP)
    ,("isPrefixOf",  mytest prop_isPrefixOfBP)
    ,("last",        mytest prop_lastBP)
    ,("length",      mytest prop_lengthBP)
    ,("map",         mytest prop_mapBP)
    ,("maximum   ",  mytest prop_maximumBP)
    ,("minimum"   ,  mytest prop_minimumBP)
    ,("null",        mytest prop_nullBP)
    ,("reverse",     mytest prop_reverseBP)
    ,("snoc",        mytest prop_snocBP)
    ,("tail",        mytest prop_tailBP)
    ,("transpose",   mytest prop_transposeBP)
    ,("replicate",   mytest prop_replicateBP)
    ,("take",        mytest prop_takeBP)
    ,("drop",        mytest prop_dropBP)
    ,("splitAt",     mytest prop_splitAtBP)
    ,("takeWhile",   mytest prop_takeWhileBP)
    ,("dropWhile",   mytest prop_dropWhileBP)
    ,("break",       mytest prop_breakBP)
    ,("span",        mytest prop_spanBP)
    ,("split",       mytest prop_splitBP)
    ,("count",       mytest prop_countBP)
    ,("group",       mytest prop_groupBP)
    ,("elem",        mytest prop_elemBP)
    ,("notElem",     mytest prop_notElemBP)
    ,("elemIndex",   mytest prop_elemIndexBP)
    ,("elemIndices", mytest prop_elemIndicesBP)

------------------------------------------------------------------------
-- ByteString <=> List

    ,("all",         mytest prop_allPL)
    ,("any",         mytest prop_anyPL)
    ,("append",      mytest prop_appendPL)
    ,("compare",     mytest prop_comparePL)
    ,("concat",      mytest prop_concatPL)
    ,("concatMap",   mytest prop_concatMapPL)
    ,("cons",        mytest prop_consPL)
    ,("eq",          mytest prop_eqPL)
    ,("filter",      mytest prop_filterPL)
    ,("find",        mytest prop_findPL)
    ,("findIndex",   mytest prop_findIndexPL)
    ,("findIndices", mytest prop_findIndicesPL)
    ,("foldl",       mytest prop_foldlPL)
    ,("foldl'",      mytest prop_foldlPL')

{-
    ,("foldl1",      mytest prop_foldl1PL)
    ,("foldr",       mytest prop_foldrPL)
    ,("foldr1",      mytest prop_foldr1PL)
    ,("head",        mytest prop_headPL)
    ,("init",        mytest prop_initPL)
    ,("last",        mytest prop_lastPL)
    ,("maximum",     mytest prop_maximumPL)
    ,("minimum",     mytest prop_minimumPL)
    ,("tail",        mytest prop_tailPL)
-}

    ,("isPrefixOf",  mytest prop_isPrefixOfPL)
    ,("length",      mytest prop_lengthPL)
    ,("map",         mytest prop_mapPL)
    ,("null",        mytest prop_nullPL)
    ,("reverse",     mytest prop_reversePL)
    ,("snoc",        mytest prop_snocPL)
    ,("transpose",   mytest prop_transposePL)
    ,("replicate",   mytest prop_replicatePL)
    ,("take",        mytest prop_takePL)
    ,("drop",        mytest prop_dropPL)
    ,("splitAt",     mytest prop_splitAtPL)
    ,("takeWhile",   mytest prop_takeWhilePL)
    ,("dropWhile",   mytest prop_dropWhilePL)
    ,("break",       mytest prop_breakPL)
    ,("span",        mytest prop_spanPL)
    ,("group",       mytest prop_groupPL)
    ,("elem",        mytest prop_elemPL)
    ,("notElem",     mytest prop_notElemPL)
    ,("elemIndex",   mytest prop_elemIndexPL)
    ,("elemIndices", mytest prop_elemIndicesPL)

    ]

------------------------------------------------------------------------
-- check the correspondence between Lists and Lazy bytestrings and
-- normal ones.
--
--  i.e.    Lazy    ==   Byte
--              \\      //
--                 List 
--

class ModeledBy a b where
  abs :: a -> b  -- get the abstract vale from a concrete value

instance ModeledBy ByteString [Word8] where
  abs = L.unpack . checkInvariant

instance ModeledBy a b => ModeledBy [a] [b] where
  abs = map abs

instance ModeledBy a b => ModeledBy (a,a) (b,b) where
  abs = \(a,b) -> (abs a, abs b) -- yeah?

instance ModeledBy a b => ModeledBy (Maybe a) (Maybe b) where
  abs = fmap abs

instance ModeledBy a b => ModeledBy (c -> a) (c -> b) where
  abs = (abs.)

instance ModeledBy Bool  Bool  where abs = id
instance ModeledBy Int   Int   where abs = id
instance ModeledBy Int64 Int64 where abs = id
instance ModeledBy Int   Int64 where abs = fromIntegral
instance ModeledBy Word8 Word8 where abs = id
instance ModeledBy Ordering Ordering  where abs = id

compare1 f f' a     = abs (f a)     == f' (abs a)
compare2 f f' a b   = abs (f a b)   == f' (abs a) (abs b)
compare3 f f' a b c = abs (f a b c) == f' (abs a) (abs b) (abs c)

notNull1 f = \x   -> (not . L.null $ x) ==> f x
notNull2 f = \x y -> (not . L.null $ y) ==> f x y

-- fix polymorphic args so we can QC them.
type X = Int
type W = Word8

------------------------------------------------------------------------
-- ByteString.Lazy <=> List

prop_eqBL         = compare2 ((==) :: ByteString -> ByteString -> Bool)
                            ((==) :: [W]    -> [W]    -> Bool)
prop_compareBL    = compare2 ((compare) :: ByteString -> ByteString -> Ordering)
                            ((compare) :: [W]    -> [W]    -> Ordering)
prop_foldlBL      = compare3 (L.foldl :: (X -> W -> X) -> X -> L.ByteString -> X)
                            (  foldl :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldlBL'     = compare3 (L.foldl' :: (X -> W -> X) -> X -> L.ByteString -> X)
                            (   foldl' :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldrBL      = compare3 (L.foldr :: (W -> X -> X) -> X -> L.ByteString -> X)
                            (  foldr :: (W -> X -> X) -> X -> [W]      -> X)
prop_allBL        = compare2 L.all               (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyBL        = compare2 L.any               (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendBL     = compare2 L.append            ((++)      :: [W] -> [W] -> [W])
prop_breakBL      = compare2 L.break             (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatBL     = compare1 L.concat            (concat    :: [[W]] -> [W])
prop_concatMapBL  = compare2 L.concatMap         (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consBL       = compare2 L.cons              ((:)       :: W -> [W] -> [W])
prop_dropBL       = compare2 L.drop              (drop      :: Int -> [W] -> [W])
prop_dropWhileBL  = compare2 L.dropWhile         (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterBL     = compare2 L.filter            (filter    :: (W -> Bool ) -> [W] -> [W])
prop_findBL       = compare2 L.find              (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndicesBL= compare2 L.findIndices       (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_findIndexBL  = compare2 L.findIndex         (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfBL = compare2 L.isPrefixOf        (isPrefixOf:: [W] -> [W] -> Bool)
prop_lengthBL     = compare1 L.length            (fromIntegral . length :: [W] -> Int64)
prop_mapBL        = compare2 L.map               (map       :: (W -> W) -> [W] -> [W])
prop_nullBL       = compare1 L.null              (null      :: [W] -> Bool)
prop_replicateBL  = compare2 L.replicate         (replicate :: Int -> W -> [W])
prop_reverseBL    = compare1 L.reverse           (reverse   :: [W] -> [W])
prop_snocBL       = compare2 L.snoc              ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanBL       = compare2 L.span              (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtBL    = compare2 L.splitAt           (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takeBL       = compare2 L.take              (take      :: Int -> [W] -> [W])
prop_takeWhileBL  = compare2 L.takeWhile         (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_transposeBL  = compare1 L.transpose         (transpose :: [[W]] -> [[W]])
prop_groupBL      = compare1 L.group             (group     :: [W] -> [[W]])
prop_elemBL       = compare2 L.elem              (elem      :: W -> [W] -> Bool)
prop_notElemBL    = compare2 L.notElem           (notElem   :: W -> [W] -> Bool)
prop_elemIndexBL  = compare2 L.elemIndex         (elemIndex :: W -> [W] -> Maybe Int)
prop_elemIndicesBL= compare2 L.elemIndices       (elemIndices:: W -> [W] -> [Int])
prop_foldl1BL     = notNull2 $ compare2 L.foldl1 (foldl1    :: (W -> W -> W) -> [W] -> W)
prop_foldr1BL     = notNull2 $ compare2 L.foldr1 (foldr1    :: (W -> W -> W) -> [W] -> W)
prop_headBL       = notNull1 $ compare1 L.head   (head      :: [W] -> W)
prop_initBL       = notNull1 $ compare1 L.init   (init      :: [W] -> [W])
prop_lastBL       = notNull1 $ compare1 L.last   (last      :: [W] -> W)
prop_maximumBL    = notNull1 $ compare1 L.maximum(maximum   :: [W] -> W)
prop_minimumBL    = notNull1 $ compare1 L.minimum(minimum   :: [W] -> W)
prop_tailBL       = notNull1 $ compare1 L.tail   (tail      :: [W] -> [W])

------------------------------------------------------------------------
-- ByteString.Lazy <=> ByteString

instance ModeledBy ByteString P.ByteString  where
  abs = abstr . checkInvariant

abstr :: ByteString -> P.ByteString
abstr (LPS []) = P.empty
abstr (LPS xs) = P.concat xs

prop_eqBP           = compare2 ((==) :: ByteString   -> ByteString   -> Bool)
                               ((==) :: P.ByteString -> P.ByteString -> Bool)
prop_compareBP      = compare2 ((compare) :: ByteString -> ByteString -> Ordering)
                               ((compare) :: P.ByteString -> P.ByteString -> Ordering)
prop_foldlBP        = compare3 (L.foldl :: (X -> W -> X) -> X -> L.ByteString -> X)
                               (P.foldl :: (X -> W -> X) -> X -> P.ByteString -> X)
prop_foldlBP'       = compare3 (L.foldl' :: (X -> W -> X) -> X -> L.ByteString -> X)
                               (P.foldl' :: (X -> W -> X) -> X -> P.ByteString -> X)
prop_foldrBP        = compare3 (L.foldr :: (W -> X -> X) -> X -> L.ByteString -> X)
                               (P.foldr :: (W -> X -> X) -> X -> P.ByteString -> X)

prop_allBP          = compare2 L.all        P.all
prop_anyBP          = compare2 L.any        P.any
prop_appendBP       = compare2 L.append     P.append
prop_breakBP        = compare2 L.break      P.break
prop_concatBP       = compare1 L.concat     P.concat
prop_concatMapBP    = compare2 L.concatMap  P.concatMap
prop_consBP         = compare2 L.cons       P.cons
prop_countBP        = compare2 L.count      P.count
prop_dropBP         = compare2 L.drop       P.drop
prop_dropWhileBP    = compare2 L.dropWhile  P.dropWhile
prop_filterBP       = compare2 L.filter     P.filter
prop_findBP         = compare2 L.find       P.find
prop_findIndexBP    = compare2 L.findIndex  P.findIndex
prop_findIndicesBP  = compare2 L.findIndices P.findIndices
prop_isPrefixOfBP   = compare2 L.isPrefixOf P.isPrefixOf
prop_lengthBP       = compare1 L.length     (fromIntegral . P.length :: P.ByteString -> Int64)
prop_mapBP          = compare2 L.map        P.map
prop_nullBP         = compare1 L.null       P.null
prop_replicateBP    = compare2 L.replicate  P.replicate
prop_reverseBP      = compare1 L.reverse    P.reverse
prop_snocBP         = compare2 L.snoc       P.snoc
prop_spanBP         = compare2 L.span       P.span
prop_splitBP        = compare2 L.split      P.split
prop_splitAtBP      = compare2 L.splitAt    P.splitAt
prop_takeBP         = compare2 L.take       P.take
prop_takeWhileBP    = compare2 L.takeWhile  P.takeWhile
prop_transposeBP    = compare1 L.transpose  P.transpose
prop_groupBP        = compare1 L.group      P.group
prop_elemBP         = compare2 L.elem       P.elem
prop_notElemBP      = compare2 L.notElem    P.notElem
prop_elemIndexBP    = compare2 L.elemIndex  P.elemIndex
prop_elemIndicesBP  = compare2 L.elemIndices P.elemIndices

prop_foldl1BP       = notNull2 $ compare2 L.foldl1 P.foldl1
prop_foldr1BP       = notNull2 $ compare2 L.foldr1 P.foldr1
prop_headBP         = notNull1 $ compare1 L.head    P.head
prop_initBP         = notNull1 $ compare1 L.init    P.init
prop_lastBP         = notNull1 $ compare1 L.last    P.last
prop_maximumBP      = notNull1 $ compare1 L.maximum P.maximum
prop_minimumBP      = notNull1 $ compare1 L.minimum P.minimum
prop_tailBP         = notNull1 $ compare1 L.tail    P.tail

------------------------------------------------------------------------
-- and finally, check correspondance between Data.ByteString and List

instance ModeledBy P.ByteString [Word8] where
    abs = P.unpack

type P = P.ByteString
type B = L.ByteString

prop_eqPL         = compare2 ((==) :: P -> P -> Bool)
                             ((==) :: [W]    -> [W]    -> Bool)
prop_comparePL    = compare2 ((compare) :: P -> P -> Ordering)
                             ((compare) :: [W]    -> [W]    -> Ordering)
prop_foldlPL      = compare3 (P.foldl :: (X -> W -> X) -> X -> P -> X)
                             (  foldl :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldlPL'     = compare3 (P.foldl' :: (X -> W -> X) -> X -> P -> X)
                             (  foldl' :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldrPL      = compare3 (P.foldr :: (W -> X -> X) -> X -> P -> X)
                             (  foldr :: (W -> X -> X) -> X -> [W]      -> X)
prop_allPL        = compare2 P.all               (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyPL        = compare2 P.any               (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendPL     = compare2 P.append            ((++)      :: [W] -> [W] -> [W])
prop_breakPL      = compare2 P.break             (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatPL     = compare1 P.concat            (concat    :: [[W]] -> [W])
prop_concatMapPL  = compare2 P.concatMap         (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consPL       = compare2 P.cons              ((:)       :: W -> [W] -> [W])
prop_dropPL       = compare2 P.drop              (drop      :: Int -> [W] -> [W])
prop_dropWhilePL  = compare2 P.dropWhile         (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterPL     = compare2 P.filter            (filter    :: (W -> Bool ) -> [W] -> [W])
prop_findPL       = compare2 P.find              (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndicesPL= compare2 P.findIndices       (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_findIndexPL  = compare2 P.findIndex         (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfPL = compare2 P.isPrefixOf        (isPrefixOf:: [W] -> [W] -> Bool)
prop_lengthPL     = compare1 P.length            (fromIntegral . length :: [W] -> Int64)
prop_mapPL        = compare2 P.map               (map       :: (W -> W) -> [W] -> [W])
prop_nullPL       = compare1 P.null              (null      :: [W] -> Bool)
prop_replicatePL  = compare2 P.replicate         (replicate :: Int -> W -> [W])
prop_reversePL    = compare1 P.reverse           (reverse   :: [W] -> [W])
prop_snocPL       = compare2 P.snoc              ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanPL       = compare2 P.span              (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtPL    = compare2 P.splitAt           (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takePL       = compare2 P.take              (take      :: Int -> [W] -> [W])
prop_takeWhilePL  = compare2 P.takeWhile         (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_transposePL  = compare1 P.transpose         (transpose :: [[W]] -> [[W]])
prop_groupPL      = compare1 P.group             (group     :: [W] -> [[W]])
prop_elemPL       = compare2 P.elem              (elem      :: W -> [W] -> Bool)
prop_notElemPL    = compare2 P.notElem           (notElem   :: W -> [W] -> Bool)
prop_elemIndexPL  = compare2 P.elemIndex         (elemIndex :: W -> [W] -> Maybe Int)
prop_elemIndicesPL= compare2 P.elemIndices       (elemIndices:: W -> [W] -> [Int])
{-
prop_foldl1PL     = notNull2 $ compare2 P.foldl1 (foldl1    :: (W -> W -> W) -> [W] -> W)
prop_foldr1PL     = notNull2 $ compare2 P.foldr1 (foldr1    :: (W -> W -> W) -> [W] -> W)
prop_headPL       = notNull1 $ compare1 P.head   (head      :: [W] -> W)
prop_initPL       = notNull1 $ compare1 P.init   (init      :: [W] -> [W])
prop_lastPL       = notNull1 $ compare1 P.last   (last      :: [W] -> W)
prop_maximumPL    = notNull1 $ compare1 P.maximum(maximum   :: [W] -> W)
prop_minimumPL    = notNull1 $ compare1 P.minimum(minimum   :: [W] -> W)
prop_tailPL       = notNull1 $ compare1 P.tail   (tail      :: [W] -> [W])
-}

-- prop_interspersePL = compare2 L.intersperse (intersperse :: W -> [W] -> [W])

------------------------------------------------------------------------
--
-- miscellaneous tests
--

invariant :: L.ByteString -> Bool
invariant (LPS []) = True
invariant (LPS xs) = all (not . P.null) xs

-- In a form more useful for QC testing (and it's lazy)
checkInvariant :: L.ByteString -> L.ByteString
checkInvariant (LPS lps) = LPS (check lps)
  where check []     = []
        check (x:xs) | P.null x  = error ("invariant violation: " ++ show lps)
                     | otherwise = x : check xs

prop_invariant = invariant

------------------------------------------------------------------------

prop_eq1 xs      = xs == (unpack . pack $ xs)
prop_eq2 xs      = xs == (xs :: ByteString)
prop_eq3 xs ys   = (xs == ys) == (unpack xs == unpack ys)

------------------------------------------------------------------------

prop_compare1 xs  = (pack xs         `compare` pack xs) == EQ
prop_compare2 xs c = (pack (xs++[c]) `compare` pack xs) == GT
prop_compare3 xs c = (pack xs `compare` pack (xs++[c])) == LT

prop_compare4 xs  = (not (null xs)) ==> (pack xs  `compare` L.empty) == GT
prop_compare5 xs  = (not (null xs)) ==> (L.empty `compare` pack xs) == LT
prop_compare6 xs ys= (not (null ys)) ==> (pack (xs++ys)  `compare` pack xs) == GT

prop_compare7 x  y = x `compare` y == (L.packByte x `compare` L.packByte y)
prop_compare8 xs ys = xs `compare` ys == (L.pack xs `compare` L.pack ys)

------------------------------------------------------------------------

prop_empty1 = L.length L.empty == 0
prop_empty2 = L.unpack L.empty == []

------------------------------------------------------------------------

prop_packunpack s = (L.unpack . L.pack) s == id s
prop_unpackpack s = (L.pack . L.unpack) s == id s

------------------------------------------------------------------------

prop_null xs = null (L.unpack xs) == L.null xs

------------------------------------------------------------------------

prop_length1 xs = fromIntegral (length xs) == L.length (L.pack xs)

prop_length2 xs = L.length xs == length1 xs
  where length1 ys
            | L.null ys = 0
            | otherwise = 1 + length1 (L.tail ys)

------------------------------------------------------------------------

prop_cons1 c xs = unpack (L.cons c (pack xs)) == (c:xs)
prop_cons2 c    = L.packByte c == (c `L.cons` L.empty)
prop_cons3 c    = unpack (L.packByte c) == (c:[])
prop_cons4 c    = (c `L.cons` L.empty)  == pack (c:[])

prop_snoc1 xs c = xs ++ [c] == unpack ((pack xs) `L.snoc` c)

------------------------------------------------------------------------

prop_head  xs = (not (null xs)) ==> head xs == (L.head . pack) xs
prop_head1 xs = not (L.null xs) ==> L.head xs == head (L.unpack xs)

prop_tail xs  = not (L.null xs) ==> L.tail xs == pack (tail (unpack xs))
prop_tail1 xs = (not (null xs)) ==> tail xs   == (unpack . L.tail . pack) xs

------------------------------------------------------------------------

prop_last xs     = (not (null xs)) ==> last xs    == (L.last . pack) xs

prop_init xs     =
    (not (null xs)) ==>
    init xs    == (unpack . L.init . pack) xs

------------------------------------------------------------------------

prop_append1 xs    = (xs ++ xs) == (unpack $ pack xs `L.append` pack xs)
prop_append2 xs ys = (xs ++ ys) == (unpack $ pack xs `L.append` pack ys)
prop_append3 xs ys = L.append xs ys == pack (unpack xs ++ unpack ys)

------------------------------------------------------------------------

prop_map1 f xs   = L.map f (pack xs)    == pack (map f xs)
prop_map2 f g xs = L.map f (L.map g xs) == L.map (f . g) xs
prop_map3 f xs   = map f xs == (unpack . L.map f .  pack) xs

prop_filter1 c xs = (filter (/=c) xs) == (unpack $ L.filter (/=c) (pack xs))
prop_filter2 p xs = (filter p xs) == (unpack $ L.filter p (pack xs))

------------------------------------------------------------------------

prop_reverse  xs = reverse xs          == (unpack . L.reverse . pack) xs
prop_reverse1 xs = L.reverse (pack xs) == pack (reverse xs)
prop_reverse2 xs = reverse (unpack xs) == (unpack . L.reverse) xs

prop_transpose xs = (transpose xs) == ((map unpack) . L.transpose . (map pack)) xs

------------------------------------------------------------------------

prop_foldl f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char


prop_foldr f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char

prop_foldl_1 xs = L.foldl (\xs c -> c `L.cons` xs) L.empty xs == L.reverse xs
prop_foldr_1 xs = L.foldr (\c xs -> c `L.cons` xs) L.empty xs == id xs

------------------------------------------------------------------------

prop_foldl1_1 xs =
    (not . L.null) xs ==>
    L.foldl1 (\x c -> if c > x then c else x)   xs ==
    L.foldl  (\x c -> if c > x then c else x) 0 xs

prop_foldl1_2 xs =
    (not . L.null) xs ==>
    L.foldl1 const xs == L.head xs

prop_foldl1_3 xs =
    (not . L.null) xs ==>
    L.foldl1 (flip const) xs == L.last xs

------------------------------------------------------------------------

prop_foldr1_1 xs =
    (not . L.null) xs ==>
    L.foldr1 (\c x -> if c > x then c else x)   xs ==
    L.foldr  (\c x -> if c > x then c else x) 0 xs

prop_foldr1_2 xs =
    (not . L.null) xs ==>
    L.foldr1 (flip const) xs == L.last xs

prop_foldr1_3 xs =
    (not . L.null) xs ==>
    L.foldr1 const xs == L.head xs

prop_concat1 xs = (concat [xs,xs]) == (unpack $ L.concat [pack xs, pack xs])
prop_concat2 xs = (concat [xs,[]]) == (unpack $ L.concat [pack xs, pack []])
prop_concat3 xss = L.concat (map pack xss) == pack (concat xss)

prop_concatMap xs = L.concatMap L.packByte xs == (pack . concatMap (:[]) . unpack) xs

------------------------------------------------------------------------

prop_any xs a = (any (== a) xs) == (L.any (== a) (pack xs))
prop_all xs a = (all (== a) xs) == (L.all (== a) (pack xs))

prop_maximum xs = (not (null xs)) ==> (maximum xs) == (L.maximum ( pack xs ))
prop_minimum xs = (not (null xs)) ==> (minimum xs) == (L.minimum ( pack xs ))

------------------------------------------------------------------------

prop_replicate1 n c =
    (n >= 0) ==> unpack (L.replicate n c) == replicate n c

prop_replicate2 c = unpack (L.replicate 0 c) == replicate 0 c

------------------------------------------------------------------------

prop_take1 i xs = L.take i (pack xs) == pack (take i xs)
prop_drop1 i xs = L.drop i (pack xs) == pack (drop i xs)

prop_splitAt i xs = collect (i >= 0 && i < length xs) $
    L.splitAt i (pack xs) == let (a,b) = splitAt i xs in (pack a, pack b)

prop_takeWhile f xs = L.takeWhile f (pack xs) == pack (takeWhile f xs)
prop_dropWhile f xs = L.dropWhile f (pack xs) == pack (dropWhile f xs)

prop_break f xs = L.break f (pack xs) ==
    let (a,b) = break f xs in (pack a, pack b)

prop_breakspan xs c = L.break (== c) xs == L.span (/= c) xs

prop_span xs a = (span (/=a) xs) == (let (x,y) = L.span (/=a) (pack xs) in (unpack x, unpack y))

prop_breakByte xs c = L.break (== c) xs == L.breakByte c xs

prop_spanByte c xs = (L.span (==c) xs) == L.spanByte c xs

prop_breakFirst c xs = (let (x,y) = break (==c) xs
                        in if null y then Nothing
                                     else Just (pack x, pack $ drop 1 y)) ==
                       (L.breakFirst c (pack xs))

{-
prop_breakLast c xs = (let (x,y) = break (==c) (reverse xs)
                       in if null y then Nothing
                                    else Just (pack (reverse $ drop 1 y), pack (reverse x))) ==
                       (L.breakLast c (pack xs))
-}

------------------------------------------------------------------------

prop_split c xs = (map L.unpack . map checkInvariant . L.split c $ xs)
               == (map P.unpack . P.split c . P.pack . L.unpack $ xs)

prop_splitWith f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map L.length splits) == L.length xs - l2
  where splits = L.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = L.length (L.filter f xs)

prop_joinsplit c xs = L.join (pack [c]) (L.split c xs) == id xs

------------------------------------------------------------------------

prop_group xs       = group xs == (map unpack . L.group . pack) xs
prop_groupBy  f xs  = groupBy f xs == (map unpack . L.groupBy f . pack) xs

------------------------------------------------------------------------

prop_joinjoinByte xs ys c = L.joinWithByte c xs ys == L.join (L.packByte c) [xs,ys]

------------------------------------------------------------------------

prop_index xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == L.pack xs `L.index` i
  where indices = choose (0, length xs -1)

prop_elemIndex xs c = (elemIndex c xs) == (L.elemIndex c (pack xs))

prop_elemIndices xs c = elemIndices c xs == L.elemIndices c (pack xs)

prop_count c xs = length (L.elemIndices c xs) == L.count c xs

prop_findIndex xs f = (findIndex f xs) == (L.findIndex f (pack xs))
prop_findIndicies xs f = (findIndices f xs) == (L.findIndices f (pack xs))

------------------------------------------------------------------------

prop_elem    xs c = (c `elem` xs)    == (c `L.elem` (pack xs))
prop_notElem xs c = (c `notElem` xs) == (L.notElem c (pack xs))
prop_elem_notelem xs c = c `L.elem` xs == not (c `L.notElem` xs)

prop_filterByte  xs c = L.filterByte c xs == L.filter (==c) xs
prop_filterByte2 xs c = unpack (L.filterByte c xs) == filter (==c) (unpack xs)

prop_filterNotByte  xs c = L.filterNotByte c xs == L.filter (/=c) xs
prop_filterNotByte2 xs c = unpack (L.filterNotByte c xs) == filter (/=c) (unpack xs)

prop_find p xs = find p xs == L.find p (pack xs)

prop_find_findIndex p xs =
    L.find p xs == case L.findIndex p xs of
                                Just n -> Just (xs `L.index` n)
                                _      -> Nothing

------------------------------------------------------------------------

prop_isPrefixOf xs ys = isPrefixOf xs ys == (pack xs `L.isPrefixOf` pack ys)

------------------------------------------------------------------------


{-
prop_sort1 xs = sort xs == (unpack . L.sort . pack) xs
prop_sort2 xs = (not (null xs)) ==> (L.head . L.sort . pack $ xs) == minimum xs
prop_sort3 xs = (not (null xs)) ==> (L.last . L.sort . pack $ xs) == maximum xs
prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (L.head . L.sort) (L.append (pack xs) (pack ys)) == min (minimum xs) (minimum ys)

prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (L.last . L.sort) (L.append (pack xs) (pack ys)) == max (maximum xs) (maximum ys)

prop_elems xs = L.elems == map pack (elems (unpack xs))
-}
