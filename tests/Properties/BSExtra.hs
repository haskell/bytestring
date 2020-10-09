
{-# LANGUAGE CPP #-}
module Properties.BSExtra
    ( bb_tests
) where


import Data.List
import Data.Maybe
import Data.Char
import Data.Monoid
import Control.Applicative

import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
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


bb_tests = testGroup "extra ByteString properties"
    [ testProperty "bijection"      prop_bijectionBB
    , testProperty "bijection'"     prop_bijectionBB'
    , testProperty "pack/unpack"    prop_packunpackBB
    , testProperty "unpack/pack"    prop_packunpackBB'
    , testProperty "eq 1"           prop_eq1BB
    , testProperty "eq 2"           prop_eq2BB
    , testProperty "eq 3"           prop_eq3BB
    , testProperty "compare 1"      prop_compare1BB
    , testProperty "compare 2"      prop_compare2BB
    , testProperty "compare 3"      prop_compare3BB
    , testProperty "compare 4"      prop_compare4BB
    , testProperty "compare 5"      prop_compare5BB
    , testProperty "compare 6"      prop_compare6BB
    , testProperty "compare 7"      prop_compare7BB
    , testProperty "compare 7"      prop_compare7LL
    , testProperty "compare 8"      prop_compare8BB
    , testProperty "empty 1"        prop_nil1BB
    , testProperty "empty 2"        prop_nil2BB
    , testProperty "empty 1 monoid" prop_nil1LL_monoid
    , testProperty "empty 2 monoid" prop_nil2LL_monoid
    , testProperty "empty 1 monoid" prop_nil1BB_monoid
    , testProperty "empty 2 monoid" prop_nil2BB_monoid

    , testProperty "null"           prop_nullBB
    , testProperty "length 1"       prop_lengthBB
    , testProperty "length 2"       prop_lengthSBB
    , testProperty "cons 1"         prop_consBB
    , testProperty "cons 2"         prop_cons1BB
    , testProperty "cons 3"         prop_cons2BB
    , testProperty "cons 4"         prop_cons3BB
    , testProperty "cons 5"         prop_cons4BB
    , testProperty "snoc"           prop_snoc1BB
    , testProperty "head 1"         prop_head1BB
    , testProperty "head 2"         prop_head2BB
    , testProperty "head 3"         prop_head3BB
    , testProperty "tail"           prop_tailBB
    , testProperty "tail 1"         prop_tail1BB
    , testProperty "last"           prop_lastBB
    , testProperty "last 1"         prop_last1BB
    , testProperty "init"           prop_initBB
    , testProperty "init 1"         prop_init1BB
    , testProperty "append 1"       prop_append1BB
    , testProperty "append 2"       prop_append2BB
    , testProperty "append 3"       prop_append3BB
    , testProperty "mappend 1"      prop_append1BB_monoid
    , testProperty "mappend 2"      prop_append2BB_monoid
    , testProperty "mappend 3"      prop_append3BB_monoid

    , testProperty "map 1"          prop_map1BB
    , testProperty "map 2"          prop_map2BB
    , testProperty "map 3"          prop_map3BB
    , testProperty "filter1"        prop_filter1BB
    , testProperty "filter2"        prop_filter2BB
    , testProperty "map fusion"     prop_mapfusionBB
    , testProperty "filter fusion"  prop_filterfusionBB
    , testProperty "reverse 1"      prop_reverse1BB
    , testProperty "reverse 2"      prop_reverse2BB
    , testProperty "reverse 3"      prop_reverse3BB
    , testProperty "foldl 1"        prop_foldl1BB
    , testProperty "foldl 2"        prop_foldl2BB
    , testProperty "foldr 1"        prop_foldr1BB
    , testProperty "foldr 2"        prop_foldr2BB
    , testProperty "foldl1 1"       prop_foldl1_1BB
    , testProperty "foldl1 2"       prop_foldl1_2BB
    , testProperty "foldl1 3"       prop_foldl1_3BB
    , testProperty "foldr1 1"       prop_foldr1_1BB
    , testProperty "foldr1 2"       prop_foldr1_2BB
    , testProperty "foldr1 3"       prop_foldr1_3BB
    , testProperty "scanl/foldl"    prop_scanlfoldlBB
    , testProperty "all"            prop_allBB
    , testProperty "any"            prop_anyBB
    , testProperty "take"           prop_takeBB
    , testProperty "drop"           prop_dropBB
    , testProperty "takeWhile_ne"   prop_takeWhileBB_ne
    , testProperty "takeWhile_eq"   prop_takeWhileBB_eq
    , testProperty "dropWhile_ne"   prop_dropWhileBB_ne
    , testProperty "dropWhile_eq"   prop_dropWhileBB_eq
    , testProperty "dropWhile_isSpace" prop_dropWhileCC_isSpace
    , testProperty "splitAt"        prop_splitAtBB
    , testProperty "span"           prop_spanBB
    , testProperty "break"          prop_breakBB
    , testProperty "elem"           prop_elemBB
    , testProperty "notElem"        prop_notElemBB

    , testProperty "concat 1"       prop_concat1BB
    , testProperty "concat 2"       prop_concat2BB
    , testProperty "concat 3"       prop_concatBB
    , testProperty "mconcat 1"      prop_concat1BB_monoid
    , testProperty "mconcat 2"      prop_concat2BB_monoid
    , testProperty "mconcat 3"      prop_concatBB_monoid

    , testProperty "mconcat 1"      prop_concat1LL_monoid
    , testProperty "mconcat 2"      prop_concat2LL_monoid
    , testProperty "mconcat 3"      prop_concatLL_monoid

    , testProperty "lines"          prop_linesBB
    , testProperty "unlines"        prop_unlinesBB
    , testProperty "unlines"        prop_unlinesLC
    , testProperty "words"          prop_wordsBB
    , testProperty "words"          prop_wordsLC
    , testProperty "unwords"        prop_unwordsBB
    , testProperty "group"          prop_groupBB
    , testProperty "groupBy 0"      prop_groupByBB
    , testProperty "groupBy 1"      prop_groupBy1CC
    , testProperty "groupBy 2"      prop_groupBy1BB
    , testProperty "groupBy 3"      prop_groupBy2CC
    , testProperty "join"           prop_joinBB
    , testProperty "elemIndex 1"    prop_elemIndex1BB
    , testProperty "elemIndex 2"    prop_elemIndex2BB
    , testProperty "findIndex"      prop_findIndexBB
    , testProperty "findIndexEnd"   prop_findIndexEndBB
    , testProperty "findIndicies"   prop_findIndiciesBB
    , testProperty "elemIndices"    prop_elemIndicesBB
    , testProperty "find"           prop_findBB
    , testProperty "find/findIndex" prop_find_findIndexBB
    , testProperty "sort 1"         prop_sort1BB
    , testProperty "sort 2"         prop_sort2BB
    , testProperty "sort 3"         prop_sort3BB
    , testProperty "sort 4"         prop_sort4BB
    , testProperty "sort 5"         prop_sort5BB
    , testProperty "intersperse"    prop_intersperseBB
    , testProperty "maximum"        prop_maximumBB
    , testProperty "minimum"        prop_minimumBB
    , testProperty "strip"          prop_strip
--  , testProperty "breakChar"      prop_breakCharBB
--  , testProperty "spanChar 1"     prop_spanCharBB
--  , testProperty "spanChar 2"     prop_spanChar_1BB
--  , testProperty "breakSpace"     prop_breakSpaceBB
--  , testProperty "dropSpace"      prop_dropSpaceBB
    , testProperty "spanEnd"        prop_spanEndBB
    , testProperty "breakEnd"       prop_breakEndBB
    , testProperty "breakEnd"       prop_breakEndCC
    , testProperty "elemIndexEnd 1" prop_elemIndexEnd1BB
    , testProperty "elemIndexEnd 1" prop_elemIndexEnd1CC
    , testProperty "elemIndexEnd 2" prop_elemIndexEnd2BB
    , testProperty "elemIndexEnd 1" prop_elemIndexEnd1LL
    , testProperty "elemIndexEnd 2" prop_elemIndexEnd2LL
--  , testProperty "words'"         prop_wordsBB'
--  , testProperty "lines'"         prop_linesBB'
--  , testProperty "dropSpaceEnd"   prop_dropSpaceEndBB
    , testProperty "unfoldr"        prop_unfoldrBB
    , testProperty "prefix"         prop_prefixBB
    , testProperty "prefix"         prop_prefixLL
    , testProperty "suffix"         prop_suffixBB
    , testProperty "suffix"         prop_suffixLL
    , testProperty "stripPrefix"    prop_stripPrefixBB
    , testProperty "stripPrefix"    prop_stripPrefixLL
    , testProperty "stripSuffix"    prop_stripSuffixBB
    , testProperty "stripSuffix"    prop_stripSuffixLL
    , testProperty "copy"           prop_copyBB
    , testProperty "copy"           prop_copyLL
    , testProperty "inits"          prop_initsBB
    , testProperty "tails"          prop_tailsBB
    , testProperty "breakSubstring 1"prop_breakSubstringBB
    , testProperty "breakSubstring 3"prop_breakSubstring_isInfixOf

    , testProperty "replicate1"     prop_replicate1BB
    , testProperty "replicate2"     prop_replicate2BB
    , testProperty "replicate3"     prop_replicate3BB
    , testProperty "readInt"        prop_readintBB
    , testProperty "readInt 2"      prop_readint2BB
    , testProperty "readInteger"    prop_readintegerBB
    , testProperty "readInteger 2"  prop_readinteger2BB
    , testProperty "read"           prop_readLL
    , testProperty "read"           prop_readBB
    , testProperty "Lazy.readInt"   prop_readintLL
    , testProperty "Lazy.readInt"   prop_readintLL
    , testProperty "Lazy.readInteger" prop_readintegerLL
    , testProperty "mconcat 1"      prop_append1LL_monoid
    , testProperty "mconcat 2"      prop_append2LL_monoid
    , testProperty "mconcat 3"      prop_append3LL_monoid
--  , testProperty "filterChar1"    prop_filterChar1BB
--  , testProperty "filterChar2"    prop_filterChar2BB
--  , testProperty "filterChar3"    prop_filterChar3BB
--  , testProperty "filterNotChar1" prop_filterNotChar1BB
--  , testProperty "filterNotChar2" prop_filterNotChar2BB
    , testProperty "tail"           prop_tailSBB
    , testProperty "index"          prop_indexBB
    , testProperty "unsafeIndex"    prop_unsafeIndexBB
--  , testProperty "map'"           prop_mapBB'
    , testProperty "filter"         prop_filterBB
    , testProperty "elem"           prop_elemSBB
    , testProperty "take"           prop_takeSBB
    , testProperty "drop"           prop_dropSBB
    , testProperty "splitAt"        prop_splitAtSBB
    , testProperty "foldl"          prop_foldlBB
    , testProperty "foldr"          prop_foldrBB
    , testProperty "takeWhile "     prop_takeWhileSBB
    , testProperty "dropWhile "     prop_dropWhileSBB
    , testProperty "span "          prop_spanSBB
    , testProperty "break "         prop_breakSBB
    , testProperty "breakspan"      prop_breakspan_1BB
    , testProperty "lines "         prop_linesSBB
    , testProperty "unlines "       prop_unlinesSBB
    , testProperty "words "         prop_wordsSBB
    , testProperty "unwords "       prop_unwordsSBB
    , testProperty "unwords "       prop_unwordsSLC
--     , testProperty "wordstokens"    prop_wordstokensBB
    , testProperty "splitWith_empty" prop_splitWithBB_empty
    , testProperty "splitWith"      prop_splitWithBB
    , testProperty "split_empty"    prop_splitBB_empty
    , testProperty "joinsplit"      prop_joinsplitBB
    , testProperty "intercalate"    prop_intercalatePL
--     , testProperty "lineIndices"    prop_lineIndices1BB
    , testProperty "count"          prop_countBB
--  , testProperty "linessplit"     prop_linessplit2BB
    , testProperty "splitsplitWith" prop_splitsplitWithBB
--  , testProperty "joinjoinpath"   prop_joinjoinpathBB
    , testProperty "zip"            prop_zipBB
    , testProperty "zip"            prop_zipLC
    , testProperty "zip1"           prop_zip1BB
    , testProperty "zipWith"        prop_zipWithBB
    , testProperty "zipWith"        prop_zipWithCC
    , testProperty "zipWith"        prop_zipWithLC
--  , testProperty "zipWith'"       prop_zipWith'BB
    , testProperty "unzip"          prop_unzipBB
    , testProperty "concatMap"      prop_concatMapBB
--  , testProperty "join/joinByte"  prop_join_spec
    ]

------------------------------------------------------------------------
-- Misc ByteString properties

prop_nil1BB = P.length P.empty == 0
prop_nil2BB = P.unpack P.empty == []
prop_nil1BB_monoid = P.length mempty == 0
prop_nil2BB_monoid = P.unpack mempty == []

prop_nil1LL_monoid = L.length mempty == 0
prop_nil2LL_monoid = L.unpack mempty == []

prop_tailSBB xs = not (P.null xs) ==> P.tail xs == P.pack (tail (P.unpack xs))

prop_nullBB xs = null (P.unpack xs) == P.null xs

prop_lengthBB xs = P.length xs == length1 xs
    where
        length1 ys
            | P.null ys = 0
            | otherwise = 1 + length1 (P.tail ys)

prop_lengthSBB xs = length xs == P.length (P.pack xs)

prop_indexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.index` i
  where indices = choose (0, length xs -1)

prop_unsafeIndexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_mapfusionBB f g xs = P.map f (P.map g xs) == P.map (f . g) xs

prop_filterBB f xs = P.filter f (P.pack xs) == P.pack (filter f xs)

prop_filterfusionBB f g xs = P.filter f (P.filter g xs) == P.filter (\c -> f c && g c) xs

prop_elemSBB x xs = P.elem x (P.pack xs) == elem x xs

prop_takeSBB i xs = P.take i (P.pack xs) == P.pack (take i xs)
prop_dropSBB i xs = P.drop i (P.pack xs) == P.pack (drop i xs)

prop_splitAtSBB i xs = -- collect (i >= 0 && i < length xs) $
    P.splitAt i (P.pack xs) ==
    let (a,b) = splitAt i xs in (P.pack a, P.pack b)

prop_foldlBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where _ = c :: Char

prop_scanlfoldlBB f z xs = not (P.null xs) ==> P.last (P.scanl f z xs) == P.foldl f z xs

prop_foldrBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where _ = c :: Char

prop_takeWhileSBB f xs = P.takeWhile f (P.pack xs) == P.pack (takeWhile f xs)
prop_dropWhileSBB f xs = P.dropWhile f (P.pack xs) == P.pack (dropWhile f xs)

prop_spanSBB f xs = P.span f (P.pack xs) ==
    let (a,b) = span f xs in (P.pack a, P.pack b)

prop_breakSBB f xs = P.break f (P.pack xs) ==
    let (a,b) = break f xs in (P.pack a, P.pack b)

prop_breakspan_1BB xs c = P.break (== c) xs == P.span (/= c) xs

prop_linesSBB (String8 xs) = C.lines (C.pack xs) == map C.pack (lines xs)

prop_unlinesSBB xss = C.unlines (map C.pack xss) == C.pack (unlines xss)

prop_wordsSBB (String8 xs) =
    C.words (C.pack xs) == map C.pack (words xs)

prop_wordsLC (String8 xs) =
    LC.words (LC.pack xs) == map LC.pack (words xs)

prop_unwordsSBB xss = C.unwords (map C.pack xss) == C.pack (unwords xss)
prop_unwordsSLC xss = LC.unwords (map LC.pack xss) == LC.pack (unwords xss)

prop_splitWithBB_empty f = P.splitWith f mempty == []

prop_splitWithBB f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map P.length splits) == P.length xs - l2
  where splits = P.splitWith f xs
        l1 = length splits
        l2 = P.length (P.filter f xs)

prop_splitBB_empty c = P.split c mempty == []

prop_joinsplitBB c xs = P.intercalate (P.pack [c]) (P.split c xs) == xs

prop_intercalatePL c x y =

    P.intercalate (P.singleton c) (x : y : []) ==
 --     intercalate (singleton c) (s1 : s2 : [])

    P.pack (intercalate [c] [P.unpack x,P.unpack y])

-- prop_linessplitBB xs =
--     (not . C.null) xs ==>
--     C.lines' xs == C.split '\n' xs

-- false:
{-
prop_linessplit2BB xs =
   (not . C.null) xs ==>
    C.lines xs == C.split '\n' xs ++ (if C.last xs == '\n' then [C.empty] else [])
-}

prop_splitsplitWithBB c xs = P.split c xs == P.splitWith (== c) xs

prop_bijectionBB  (Char8 c) = (P.w2c . P.c2w) c == id c
prop_bijectionBB'        w  = (P.c2w . P.w2c) w == id w

prop_packunpackBB  s = (P.unpack . P.pack) s == id s
prop_packunpackBB' s = (P.pack . P.unpack) s == id s

prop_eq1BB xs      = xs            == (P.unpack . P.pack $ xs)
prop_eq2BB xs      = xs == (xs :: P.ByteString)
prop_eq3BB xs ys   = (xs == ys) == (P.unpack xs == P.unpack ys)

prop_compare1BB xs  = (P.pack xs         `compare` P.pack xs) == EQ
prop_compare2BB xs c = (P.pack (xs++[c]) `compare` P.pack xs) == GT
prop_compare3BB xs c = (P.pack xs `compare` P.pack (xs++[c])) == LT

prop_compare4BB xs  = (not (null xs)) ==> (P.pack xs  `compare` P.empty) == GT
prop_compare5BB xs  = (not (null xs)) ==> (P.empty `compare` P.pack xs) == LT
prop_compare6BB xs ys= (not (null ys)) ==> (P.pack (xs++ys)  `compare` P.pack xs) == GT

prop_compare7BB (Char8 x) (Char8 y) =
                        x  `compare` y  == (C.singleton x `compare` C.singleton y)
prop_compare8BB xs ys = xs `compare` ys == (P.pack xs `compare` P.pack ys)

prop_consBB  c xs = P.unpack (P.cons c (P.pack xs)) == (c:xs)
prop_cons1BB (String8 xs)
                  = 'X' : xs == C.unpack ('X' `C.cons` (C.pack xs))
prop_cons2BB xs c = c : xs == P.unpack (c `P.cons` (P.pack xs))
prop_cons3BB (Char8 c)
                  = C.unpack (C.singleton c) == (c:[])
prop_cons4BB c    = (c `P.cons` P.empty)  == P.pack (c:[])

prop_snoc1BB xs c = xs ++ [c] == P.unpack ((P.pack xs) `P.snoc` c)

prop_head1BB xs     = (not (null xs)) ==> head  xs  == (P.head . P.pack) xs
prop_head2BB xs    = (not (null xs)) ==> head xs   == (P.unsafeHead . P.pack) xs
prop_head3BB xs    = not (P.null xs) ==> P.head xs == head (P.unpack xs)

prop_tailBB xs     = (not (null xs)) ==> tail xs    == (P.unpack . P.tail . P.pack) xs
prop_tail1BB xs    = (not (null xs)) ==> tail xs    == (P.unpack . P.unsafeTail. P.pack) xs

prop_lastBB xs     = (not (null xs)) ==> last xs    == (P.last . P.pack) xs
prop_last1BB xs    = (not (null xs)) ==> last xs    == (P.unsafeLast . P.pack) xs

prop_initBB xs     =
    (not (null xs)) ==>
    init xs    == (P.unpack . P.init . P.pack) xs
prop_init1BB xs     =
    (not (null xs)) ==>
    init xs    == (P.unpack . P.unsafeInit . P.pack) xs

-- prop_null xs = (null xs) ==> null xs == (nullPS (pack xs))

prop_append1BB xs    = (xs ++ xs) == (P.unpack $ P.pack xs `P.append` P.pack xs)
prop_append2BB xs ys = (xs ++ ys) == (P.unpack $ P.pack xs `P.append` P.pack ys)
prop_append3BB xs ys = P.append xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_append1BB_monoid xs    = (xs ++ xs) == (P.unpack $ P.pack xs `mappend` P.pack xs)
prop_append2BB_monoid xs ys = (xs ++ ys) == (P.unpack $ P.pack xs `mappend` P.pack ys)
prop_append3BB_monoid xs ys = mappend xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_append1LL_monoid xs    = (xs ++ xs) == (L.unpack $ L.pack xs `mappend` L.pack xs)
prop_append2LL_monoid xs ys = (xs ++ ys) == (L.unpack $ L.pack xs `mappend` L.pack ys)
prop_append3LL_monoid xs ys = mappend xs ys == L.pack (L.unpack xs ++ L.unpack ys)

prop_map1BB f xs   = P.map f (P.pack xs)    == P.pack (map f xs)
prop_map2BB f g xs = P.map f (P.map g xs) == P.map (f . g) xs
prop_map3BB f xs   = map f xs == (P.unpack . P.map f .  P.pack) xs
-- prop_mapBB' f xs   = P.map' f (P.pack xs) == P.pack (map f xs)

prop_filter1BB (String8 xs) = (filter (=='X') xs) == (C.unpack $ C.filter (=='X') (C.pack xs))
prop_filter2BB p        xs  = (filter p       xs) == (P.unpack $ P.filter p (P.pack xs))

prop_findBB p xs = find p xs == P.find p (P.pack xs)

prop_find_findIndexBB p xs =
    P.find p xs == case P.findIndex p xs of
                                Just n -> Just (xs `P.unsafeIndex` n)
                                _      -> Nothing

prop_foldl1BB xs a = ((foldl (\x c -> if c == a then x else c:x) [] xs)) ==
                   (P.unpack $ P.foldl (\x c -> if c == a then x else c `P.cons` x) P.empty (P.pack xs))
prop_foldl2BB xs = P.foldl (\xs c -> c `P.cons` xs) P.empty (P.pack xs) == P.reverse (P.pack xs)

prop_foldr1BB xs a = ((foldr (\c x -> if c == a then x else c:x) [] xs)) ==
                (P.unpack $ P.foldr (\c x -> if c == a then x else c `P.cons` x)
                    P.empty (P.pack xs))

prop_foldr2BB xs = P.foldr (\c xs -> c `P.cons` xs) P.empty (P.pack xs) == (P.pack xs)

prop_foldl1_1BB xs =
    (not . P.null) xs ==>
    P.foldl1 (\x c -> if c > x then c else x)   xs ==
    P.foldl  (\x c -> if c > x then c else x) 0 xs

prop_foldl1_2BB xs =
    (not . P.null) xs ==>
    P.foldl1 const xs == P.head xs

prop_foldl1_3BB xs =
    (not . P.null) xs ==>
    P.foldl1 (flip const) xs == P.last xs

prop_foldr1_1BB xs =
    (not . P.null) xs ==>
    P.foldr1 (\c x -> if c > x then c else x)   xs ==
    P.foldr  (\c x -> if c > x then c else x) 0 xs

prop_foldr1_2BB xs =
    (not . P.null) xs ==>
    P.foldr1 (flip const) xs == P.last xs

prop_foldr1_3BB xs =
    (not . P.null) xs ==>
    P.foldr1 const xs == P.head xs

prop_takeWhileBB_ne xs a =
  (takeWhile (/= a) xs) == (P.unpack . (P.takeWhile (/= a)) . P.pack) xs
prop_takeWhileBB_eq xs a =
  (takeWhile (== a) xs) == (P.unpack . (P.takeWhile (== a)) . P.pack) xs

prop_dropWhileBB_ne xs a =
  (dropWhile (/= a) xs) == (P.unpack . (P.dropWhile (/= a)) . P.pack) xs
prop_dropWhileBB_eq xs a =
  (dropWhile (== a) xs) == (P.unpack . (P.dropWhile (== a)) . P.pack) xs

prop_dropWhileCC_isSpace (String8 xs) =
        (dropWhile isSpace xs) ==
       (C.unpack .  (C.dropWhile isSpace) . C.pack) xs

prop_takeBB xs = (take 10 xs) == (P.unpack . (P.take 10) . P.pack) xs

prop_dropBB xs = (drop 10 xs) == (P.unpack . (P.drop 10) . P.pack) xs

prop_splitAtBB i xs = -- collect (i >= 0 && i < length xs) $
    splitAt i xs ==
    let (x,y) = P.splitAt i (P.pack xs) in (P.unpack x, P.unpack y)

prop_spanBB xs a = (span (/=a) xs) == (let (x,y) = P.span (/=a) (P.pack xs)
                                     in (P.unpack x, P.unpack y))

prop_breakBB xs a = (break (/=a) xs) == (let (x,y) = P.break (/=a) (P.pack xs)
                                       in (P.unpack x, P.unpack y))

prop_reverse1BB xs = (reverse xs) == (P.unpack . P.reverse . P.pack) xs
prop_reverse2BB xs = P.reverse (P.pack xs) == P.pack (reverse xs)
prop_reverse3BB xs = reverse (P.unpack xs) == (P.unpack . P.reverse) xs

prop_elemBB xs a = (a `elem` xs) == (a `P.elem` (P.pack xs))

prop_notElemBB c xs = P.notElem c (P.pack xs) == notElem c xs

-- should try to stress it
prop_concat1BB xs = (concat [xs,xs]) == (P.unpack $ P.concat [P.pack xs, P.pack xs])
prop_concat2BB xs = (concat [xs,[]]) == (P.unpack $ P.concat [P.pack xs, P.pack []])
prop_concatBB xss = P.concat (map P.pack xss) == P.pack (concat xss)

prop_concat1BB_monoid xs = (concat [xs,xs]) == (P.unpack $ mconcat [P.pack xs, P.pack xs])
prop_concat2BB_monoid xs = (concat [xs,[]]) == (P.unpack $ mconcat [P.pack xs, P.pack []])
prop_concatBB_monoid xss = mconcat (map P.pack xss) == P.pack (concat xss)

prop_concat1LL_monoid xs = (concat [xs,xs]) == (L.unpack $ mconcat [L.pack xs, L.pack xs])
prop_concat2LL_monoid xs = (concat [xs,[]]) == (L.unpack $ mconcat [L.pack xs, L.pack []])
prop_concatLL_monoid xss = mconcat (map L.pack xss) == L.pack (concat xss)

prop_concatMapBB xs = C.concatMap C.singleton xs == (C.pack . concatMap (:[]) . C.unpack) xs

prop_anyBB xs a = (any (== a) xs) == (P.any (== a) (P.pack xs))
prop_allBB xs a = (all (== a) xs) == (P.all (== a) (P.pack xs))

prop_linesBB (String8 xs) =
    (lines xs) == ((map C.unpack) . C.lines . C.pack) xs

prop_unlinesBB (String8 xs) =
    (unlines.lines) xs == (C.unpack. C.unlines . C.lines .C.pack) xs
prop_unlinesLC (String8 xs) =
    (unlines.lines) xs == (LC.unpack. LC.unlines .  LC.lines .LC.pack) xs

prop_wordsBB (String8 xs) =
    (words xs) == ((map C.unpack) . C.words . C.pack) xs
-- prop_wordstokensBB xs = C.words xs == C.tokens isSpace xs

prop_unwordsBB (String8 xs) =
    (C.pack.unwords.words) xs == (C.unwords . C.words .C.pack) xs

prop_groupBB xs   = group xs == (map P.unpack . P.group . P.pack) xs

prop_groupByBB  xs = groupBy (==) xs == (map P.unpack . P.groupBy (==) . P.pack) xs
prop_groupBy1BB xs = groupBy (/=) xs == (map P.unpack . P.groupBy (/=) . P.pack) xs
prop_groupBy1CC (String8 xs) = groupBy (==) xs == (map C.unpack . C.groupBy (==) . C.pack) xs
prop_groupBy2CC (String8 xs) = groupBy (/=) xs == (map C.unpack . C.groupBy (/=) . C.pack) xs

prop_joinBB (String8 xs) (String8 ys) =
    (concat . (intersperse ys) . lines) xs ==
    (C.unpack $ C.intercalate (C.pack ys) (C.lines (C.pack xs)))

prop_elemIndex1BB (String8 xs)           = (elemIndex 'X' xs) == (C.elemIndex 'X' (C.pack xs))
prop_elemIndex2BB (String8 xs) (Char8 c) = (elemIndex  c  xs) == (C.elemIndex  c  (C.pack xs))

-- prop_lineIndices1BB xs = C.elemIndices '\n' xs == C.lineIndices xs

prop_countBB c xs = length (P.elemIndices c xs) == P.count c xs

prop_elemIndexEnd1BB c xs = (P.elemIndexEnd c (P.pack xs)) ==
                           (case P.elemIndex c (P.pack (reverse xs)) of
                                Nothing -> Nothing
                                Just i  -> Just (length xs -1 -i))

prop_elemIndexEnd1CC c xs = (C.elemIndexEnd c (C.pack xs)) ==
                           (case C.elemIndex c (C.pack (reverse xs)) of
                                Nothing -> Nothing
                                Just i  -> Just (length xs -1 -i))

prop_elemIndexEnd2BB c xs = (P.elemIndexEnd c (P.pack xs)) ==
                           ((-) (length xs - 1) `fmap` P.elemIndex c (P.pack $ reverse xs))

prop_elemIndexEnd1LL c xs = (L.elemIndexEnd c (L.pack xs)) ==
                           (case L.elemIndex c (L.pack (reverse xs)) of
                                Nothing -> Nothing
                                Just i  -> Just (fromIntegral (length xs) -1 -i))

prop_elemIndexEnd2LL c xs = (L.elemIndexEnd c (L.pack xs)) ==
                           ((-) (fromIntegral (length xs) - 1) `fmap` L.elemIndex c (L.pack $ reverse xs))

prop_elemIndicesBB xs c = elemIndices c xs == P.elemIndices c (P.pack xs)

prop_findIndexBB xs a = (findIndex (==a) xs) == (P.findIndex (==a) (P.pack xs))

prop_findIndexEndBB xs a = (findIndexEnd (==a) xs) == (P.findIndexEnd (==a) (P.pack xs))

prop_findIndiciesBB xs c = (findIndices (==c) xs) == (P.findIndices (==c) (P.pack xs))

-- example properties from QuickCheck.Batch
prop_sort1BB xs = sort xs == (P.unpack . P.sort . P.pack) xs
prop_sort2BB xs = (not (null xs)) ==> (P.head . P.sort . P.pack $ xs) == minimum xs
prop_sort3BB xs = (not (null xs)) ==> (P.last . P.sort . P.pack $ xs) == maximum xs
prop_sort4BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.head . P.sort) (P.append (P.pack xs) (P.pack ys)) == min (minimum xs) (minimum ys)
prop_sort5BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.last . P.sort) (P.append (P.pack xs) (P.pack ys)) == max (maximum xs) (maximum ys)

prop_intersperseBB c xs = (intersperse c xs) == (P.unpack $ P.intersperse c (P.pack xs))

-- prop_transposeBB xs = (transpose xs) == ((map P.unpack) . P.transpose .  (map P.pack)) xs

prop_maximumBB xs = (not (null xs)) ==> (maximum xs) == (P.maximum ( P.pack xs ))
prop_minimumBB xs = (not (null xs)) ==> (minimum xs) == (P.minimum ( P.pack xs ))

prop_strip = C.strip `eq1` (C.dropSpace . C.reverse . C.dropSpace . C.reverse)

-- prop_dropSpaceBB xs    = dropWhile isSpace xs == C.unpack (C.dropSpace (C.pack xs))
-- prop_dropSpaceEndBB xs = (C.reverse . (C.dropWhile isSpace) . C.reverse) (C.pack xs) ==
--                        (C.dropSpaceEnd (C.pack xs))

-- prop_breakSpaceBB xs =
--     (let (x,y) = C.breakSpace (C.pack xs)
--      in (C.unpack x, C.unpack y)) == (break isSpace xs)

prop_spanEndBB xs =
        (C.spanEnd (not . isSpace) (C.pack xs)) ==
        (let (x,y) = C.span (not.isSpace) (C.reverse (C.pack xs)) in (C.reverse y,C.reverse x))

prop_breakEndBB p xs = P.breakEnd (not.p) xs == P.spanEnd p xs
prop_breakEndCC p xs = C.breakEnd (not.p) xs == C.spanEnd p xs

{-
prop_breakCharBB c xs =
        (break (==c) xs) ==
        (let (x,y) = C.breakChar c (C.pack xs) in (C.unpack x, C.unpack y))

prop_spanCharBB c xs =
        (break (/=c) xs) ==
        (let (x,y) = C.spanChar c (C.pack xs) in (C.unpack x, C.unpack y))

prop_spanChar_1BB c xs =
        (C.span (==c) xs) == C.spanChar c xs

prop_wordsBB' xs =
    (C.unpack . C.unwords  . C.words' . C.pack) xs ==
    (map (\c -> if isSpace c then ' ' else c) xs)

-- prop_linesBB' xs = (C.unpack . C.unlines' . C.lines' . C.pack) xs == (xs)
-}

prop_unfoldrBB c =
    forAll arbitrarySizedIntegral $ \n ->
      (fst $ C.unfoldrN n fn c) == (C.pack $ take n $ unfoldr fn c)
  where
    fn x = Just (x, chr (ord x + 1))

prop_prefixBB xs ys = isPrefixOf xs ys == (P.pack xs `P.isPrefixOf` P.pack ys)
prop_prefixLL xs ys = isPrefixOf xs ys == (L.pack xs `L.isPrefixOf` L.pack ys)
prop_suffixBB xs ys = isSuffixOf xs ys == (P.pack xs `P.isSuffixOf` P.pack ys)
prop_suffixLL xs ys = isSuffixOf xs ys == (L.pack xs `L.isSuffixOf` L.pack ys)

prop_stripPrefixBB xs ys = (P.pack <$> stripPrefix xs ys) == (P.pack xs `P.stripPrefix` P.pack ys)
prop_stripPrefixLL xs ys = (L.pack <$> stripPrefix xs ys) == (L.pack xs `L.stripPrefix` L.pack ys)
prop_stripSuffixBB xs ys = (P.pack <$> stripSuffix xs ys) == (P.pack xs `P.stripSuffix` P.pack ys)
prop_stripSuffixLL xs ys = (L.pack <$> stripSuffix xs ys) == (L.pack xs `L.stripSuffix` L.pack ys)

prop_copyBB xs = let p = P.pack xs in P.copy p == p
prop_copyLL xs = let p = L.pack xs in L.copy p == p

prop_initsBB xs = inits xs == map P.unpack (P.inits (P.pack xs))

prop_tailsBB xs = tails xs == map P.unpack (P.tails (P.pack xs))

-- correspondance between break and breakSubstring
prop_breakSubstringBB c l
    = P.break (== c) l == P.breakSubstring (P.singleton c) l

prop_breakSubstring_isInfixOf s l
    = P.isInfixOf s l == if P.null s then True
                                     else case P.breakSubstring s l of
                                            (x,y) | P.null y  -> False
                                                  | otherwise -> True

prop_replicate1BB c = forAll arbitrarySizedIntegral $ \n ->
                      P.unpack (P.replicate n c) == replicate n c
prop_replicate2BB c = forAll arbitrarySizedIntegral $ \n ->
                      P.replicate n c == fst (P.unfoldrN n (\u -> Just (u,u)) c)

prop_replicate3BB c = P.unpack (P.replicate 0 c) == replicate 0 c

prop_readintBB n = (fst . fromJust . C.readInt . C.pack . show) n == (n :: Int)
prop_readintLL n = (fst . fromJust . D.readInt . D.pack . show) n == (n :: Int)

prop_readBB x = (read . show) x == (x :: P.ByteString)
prop_readLL x = (read . show) x == (x :: L.ByteString)

prop_readint2BB (String8 s) =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in C.readInt (C.pack s') == Nothing

prop_readintegerBB n = (fst . fromJust . C.readInteger . C.pack . show) n == (n :: Integer)
prop_readintegerLL n = (fst . fromJust . D.readInteger . D.pack . show) n == (n :: Integer)

prop_readinteger2BB (String8 s) =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in C.readInteger (C.pack s') == Nothing

-- prop_filterChar1BB c xs = (filter (==c) xs) == ((C.unpack . C.filterChar c . C.pack) xs)
-- prop_filterChar2BB c xs = (C.filter (==c) (C.pack xs)) == (C.filterChar c (C.pack xs))
-- prop_filterChar3BB c xs = C.filterChar c xs == C.replicate (C.count c xs) c

-- prop_filterNotChar1BB c xs = (filter (/=c) xs) == ((C.unpack . C.filterNotChar c . C.pack) xs)
-- prop_filterNotChar2BB c xs = (C.filter (/=c) (C.pack xs)) == (C.filterNotChar c (C.pack xs))

-- prop_joinjoinpathBB xs ys c = C.joinWithChar c xs ys == C.join (C.singleton c) [xs,ys]

prop_zipBB  xs ys = zip xs ys == P.zip (P.pack xs) (P.pack ys)
prop_zipLC (String8 xs) (String8 ys)
                  = zip xs ys == LC.zip (LC.pack xs) (LC.pack ys)
prop_zip1BB xs ys = P.zip xs ys == zip (P.unpack xs) (P.unpack ys)

prop_zipWithBB xs ys = P.zipWith (,) xs ys == P.zip xs ys
prop_zipWithCC xs ys = C.zipWith (,) xs ys == C.zip xs ys
prop_zipWithLC xs ys = LC.zipWith (,) xs ys == LC.zip xs ys
-- prop_zipWith'BB xs ys = P.pack (P.zipWith (+) xs ys) == P.zipWith' (+) xs ys

prop_unzipBB x = let (xs,ys) = unzip x in (P.pack xs, P.pack ys) == P.unzip x


-- prop_zipwith_spec f p q =
--   P.pack (P.zipWith f p q) == P.zipWith' f p q
--   where _ = f :: Word8 -> Word8 -> Word8

-- prop_join_spec c s1 s2 =
--  P.join (P.singleton c) (s1 : s2 : []) == P.joinWithByte c s1 s2


prop_compare7LL (Char8 x) (Char8 y) =
                      x  `compare` y  == (LC.singleton x `compare` LC.singleton y)