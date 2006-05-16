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

--  arbitrary     = choose (minBound, maxBound) -- since we have to test words, unlines too
--  coarbitrary c = variant (ord c `rem` 16)

instance Arbitrary Word8 where
    arbitrary = choose (97, 105)
    coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 4))

--  arbitrary = choose (minBound, maxBound)
--  coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 16))

instance Random Word8 where
  randomR (a,b) g = case randomR (fromIntegral a :: Integer
                                 ,fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x :: Word8, g)

  random g        = randomR (minBound,maxBound) g

instance Arbitrary ByteString where
    arbitrary     = arbitrary >>= return . LPS . filter (not. P.null) -- maintain the invariant.
    coarbitrary s = coarbitrary (L.unpack s)

instance Arbitrary P.ByteString where
    arbitrary = P.pack `fmap` arbitrary
    coarbitrary s = coarbitrary (P.unpack s)

-- i.e. [[a,b,c], [c,d]]

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
    ,("breakLast/break",    mytest prop_breakFirst)
    ,("split",              mytest prop_split)
    ,("splitWith",          mytest prop_splitWith)
    ,("join.split/id",      mytest prop_joinsplit)
    ,("join/joinByte",      mytest prop_joinjoinByte)
    ,("group",              mytest prop_group)
--  ,("groupBy",            mytest prop_groupBy)
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

{-
    ,("sort 1",             mytest prop_sort1)
    ,("sort 2",             mytest prop_sort2)
    ,("sort 3",             mytest prop_sort3)
    ,("sort 4",             mytest prop_sort4)
    ,("sort 5",             mytest prop_sort5)
-}

------------------------------------------------------------------------
    ,("cons",        mytest prop_cons')
    ,("snoc",        mytest prop_snoc')
    ,("null",        mytest prop_null')
    ,("length",      mytest prop_length')
    ,("head",        mytest prop_head')
    ,("tail",        mytest prop_tail')
    ,("last",        mytest prop_last')
    ,("init",        mytest prop_init')
    ,("append",      mytest prop_append')
    ,("map",         mytest prop_map')
    ,("reverse",     mytest prop_reverse')
--  ,("intersperse", mytest prop_intersperse')
    ,("transpose",   mytest prop_transpose')
    ,("foldl",       mytest prop_foldl')
    ,("foldr",       mytest prop_foldr')
    ,("foldl1",      mytest prop_foldl1')
    ,("foldr1",      mytest prop_foldr1')
    ,("concat",      mytest prop_concat')
    ,("concatMap",   mytest prop_concatMap')
    ,("any",         mytest prop_any')
    ,("all",         mytest prop_all')
    ,("maximum",     mytest prop_maximum')
    ,("minimum",     mytest prop_minimum')
    ,("isPrefixOf",  mytest prop_isPrefixOf')

------------------------------------------------------------------------

    ,("cons",        mytest prop_cons'')
    ,("snoc",        mytest prop_snoc'')
    ,("null",        mytest prop_null'')
    ,("length",      mytest prop_length'')
    ,("head",        mytest prop_head'')
    ,("tail",        mytest prop_tail'')
    ,("last",        mytest prop_last'')
    ,("init",        mytest prop_init'')
    ,("append",      mytest prop_append'')
    ,("isPrefixOf",  mytest prop_isPrefixOf'')
    ,("any",         mytest prop_any'')
    ,("all",         mytest prop_all'')

    ]

------------------------------------------------------------------------

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

prop_breakLast c xs = (let (x,y) = break (==c) (reverse xs)
                       in if null y then Nothing
                                    else Just (pack (reverse $ drop 1 y), pack (reverse x))) ==
                       (L.breakLast c (pack xs))

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
-- prop_groupBy  f xs  = groupBy f xs == (map unpack . L.groupBy f . pack) xs

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

------------------------------------------------------------------------

class ModeledBy a b where
  abs :: a -> b  -- get the abstract vale from a concrete value

instance ModeledBy ByteString [Word8] where
  abs = L.unpack . checkInvariant

instance ModeledBy a b => ModeledBy [a] [b] where
  abs = map abs

instance ModeledBy a b => ModeledBy (c -> a) (c -> b) where
  abs = (abs.)

instance ModeledBy Bool  Bool  where abs = id
instance ModeledBy Int   Int   where abs = id
instance ModeledBy Int64 Int64 where abs = id
instance ModeledBy Word8 Word8 where abs = id

compare1 f f' a     = abs (f a)     == f' (abs a)
compare2 f f' a b   = abs (f a b)   == f' (abs a) (abs b)
compare3 f f' a b c = abs (f a b c) == f' (abs a) (abs b) (abs c)

notNull1 f = \x   -> (not . L.null $ x) ==> f x
notNull2 f = \x y -> (not . L.null $ y) ==> f x y

-- fix polymorphic args so we can QC them.
type Any = Int

------------------------------------------------------------------------


prop_cons'   = compare2 L.cons   ((:) :: Word8 -> [Word8] -> [Word8])
prop_snoc'   = compare2 L.snoc   ((\xs x -> xs ++ [x]) :: [Word8] -> Word8 -> [Word8])
prop_null'   = compare1 L.null   (null :: [Word8] -> Bool)
prop_length' = compare1 L.length (fromIntegral . length :: [Word8] -> Int64)

prop_head'   = notNull1 $ compare1 L.head   (head :: [Word8] -> Word8)
prop_tail'   = notNull1 $ compare1 L.tail  (tail :: [Word8] -> [Word8])
prop_last'   = notNull1 $ compare1 L.last   (last :: [Word8] -> Word8)
prop_init'   = notNull1 $ compare1 L.init   (init :: [Word8] -> [Word8])

prop_append' = compare2 L.append ((++) :: [Word8] -> [Word8] -> [Word8])

prop_map'         = compare2 L.map         (map         :: (Word8 -> Word8) -> [Word8] -> [Word8])
prop_reverse'     = compare1 L.reverse     (reverse     :: [Word8] -> [Word8])
-- prop_intersperse' = compare2 L.intersperse (intersperse :: Word8 -> [Word8] -> [Word8])
prop_transpose'   = compare1 L.transpose   (transpose   :: [[Word8]] -> [[Word8]])

prop_foldl'  = compare3 (L.foldl :: (Any -> Word8 -> Any) -> Any -> L.ByteString -> Any)
                        (  foldl :: (Any -> Word8 -> Any) -> Any -> [Word8]      -> Any)
prop_foldr'  = compare3 (L.foldr :: (Word8 -> Any -> Any) -> Any -> L.ByteString -> Any)
                        (  foldr :: (Word8 -> Any -> Any) -> Any -> [Word8]      -> Any)

prop_foldl1' = notNull2 $ compare2
    L.foldl1 (foldl1 :: (Word8 -> Word8 -> Word8) -> [Word8] -> Word8)

prop_foldr1' = notNull2 $ compare2
    L.foldr1 (foldr1 :: (Word8 -> Word8 -> Word8) -> [Word8] -> Word8)

prop_concat'     = compare1 L.concat    (concat    :: [[Word8]] -> [Word8])
prop_concatMap'  = compare2 L.concatMap (concatMap :: (Word8 -> [Word8]) -> [Word8] -> [Word8])
prop_any'        = compare2 L.any       (any       :: (Word8 -> Bool) -> [Word8] -> Bool)
prop_all'        = compare2 L.all       (all       :: (Word8 -> Bool) -> [Word8] -> Bool)
prop_maximum'    = notNull1 $ compare1 L.maximum   (maximum   :: [Word8] -> Word8)
prop_minimum'    = notNull1 $ compare1 L.minimum   (minimum   :: [Word8] -> Word8)
prop_isPrefixOf' = compare2 L.isPrefixOf (isPrefixOf :: [Word8] -> [Word8] -> Bool)

--prop_mapIndexed = compare2 L.mapIndexed mapIndexed

------------------------------------------------------------------------
-- TODO now check correspondance to Data.ByteString, using 'abstr'

instance ModeledBy ByteString P.ByteString  where
  abs = abstr . checkInvariant

abstr :: ByteString -> P.ByteString
abstr (LPS []) = P.empty
abstr (LPS xs) = P.concat xs

prop_cons''         = compare2 L.cons               P.cons
prop_snoc''         = compare2 L.snoc               P.snoc
prop_null''         = compare1 L.null               P.null
prop_length''       = compare1 L.length (fromIntegral . P.length :: P.ByteString -> Int64)
prop_head''         = notNull1 $ compare1 L.head    P.head
prop_tail''         = notNull1 $ compare1 L.tail    P.tail
prop_last''         = notNull1 $ compare1 L.last    P.last
prop_init''         = notNull1 $ compare1 L.init    P.init
prop_append''       = compare2 L.append             P.append
prop_isPrefixOf''   = compare2 L.isPrefixOf         P.isPrefixOf
prop_any''          = compare2 L.any                P.any
prop_all''          = compare2 L.all                P.all
