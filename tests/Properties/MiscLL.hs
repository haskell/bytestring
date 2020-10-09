
{-# LANGUAGE CPP #-}
module Properties.MiscLL
    ( ll_tests
) where

import Data.List
import Data.Word
import Data.Char
import Data.Maybe
import Data.Monoid
import Control.Applicative

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Short      as Short

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
-- Extra lazy properties

ll_tests = testGroup "Extra lazy properties && lots of other stuff apparently"
    [ testProperty "eq 1"               prop_eq1
    , testProperty "eq 2"               prop_eq2
    , testProperty "eq 3"               prop_eq3
    , testProperty "eq refl"            prop_eq_refl
    , testProperty "eq symm"            prop_eq_symm
    , testProperty "compare 1"          prop_compare1
    , testProperty "compare 2"          prop_compare2
    , testProperty "compare 3"          prop_compare3
    , testProperty "compare 4"          prop_compare4
    , testProperty "compare 5"          prop_compare5
    , testProperty "compare 6"          prop_compare6
    , testProperty "compare 7"          prop_compare7
    , testProperty "compare 8"          prop_compare8
    , testProperty "empty 1"            prop_empty1
    , testProperty "empty 2"            prop_empty2
    , testProperty "pack/unpack"        prop_packunpack
    , testProperty "unpack/pack"        prop_unpackpack
    , testProperty "null"               prop_null
    , testProperty "length 1"           prop_length1
    , testProperty "length 2"           prop_length2
    , testProperty "cons 1"             prop_cons1
    , testProperty "cons 2"             prop_cons2
    , testProperty "cons 3"             prop_cons3
    , testProperty "cons 4"             prop_cons4
    , testProperty "snoc"               prop_snoc1
    , testProperty "head/pack"          prop_head
    , testProperty "head/unpack"        prop_head1
    , testProperty "tail/pack"          prop_tail
    , testProperty "tail/unpack"        prop_tail1
    , testProperty "last"               prop_last
    , testProperty "init"               prop_init
    , testProperty "append 1"           prop_append1
    , testProperty "append 2"           prop_append2
    , testProperty "append 3"           prop_append3
    , testProperty "map 1"              prop_map1
    , testProperty "map 2"              prop_map2
    , testProperty "map 3"              prop_map3
    , testProperty "filter 1"           prop_filter1
    , testProperty "filter 2"           prop_filter2
    , testProperty "reverse"            prop_reverse
    , testProperty "reverse1"           prop_reverse1
    , testProperty "reverse2"           prop_reverse2
    , testProperty "transpose"          prop_transpose
    , testProperty "foldl"              prop_foldl
    , testProperty "foldl/reverse"      prop_foldl_1
    , testProperty "foldr"              prop_foldr
    , testProperty "foldr/id"           prop_foldr_1
    , testProperty "foldl1/foldl"       prop_foldl1_1
    , testProperty "foldl1/head"        prop_foldl1_2
    , testProperty "foldl1/tail"        prop_foldl1_3
    , testProperty "foldr1/foldr"       prop_foldr1_1
    , testProperty "foldr1/last"        prop_foldr1_2
    , testProperty "foldr1/head"        prop_foldr1_3
    , testProperty "concat 1"           prop_concat1
    , testProperty "concat 2"           prop_concat2
    , testProperty "concat/pack"        prop_concat3
    , testProperty "any"                prop_any
    , testProperty "all"                prop_all
    , testProperty "maximum"            prop_maximum
    , testProperty "minimum"            prop_minimum
    , testProperty "replicate 1"        prop_replicate1
    , testProperty "replicate 2"        prop_replicate2
    , testProperty "take"               prop_take1
    , testProperty "takeEnd"            prop_takeEnd
    , testProperty "drop"               prop_drop1
    , testProperty "dropEnd"            prop_dropEnd
    , testProperty "splitAt"            prop_drop1
    , testProperty "takeWhile"          prop_takeWhile
    , testProperty "dropWhile"          prop_dropWhile
    , testProperty "takeWhileEnd"       prop_takeWhileEnd
    , testProperty "dropWhileEnd"       prop_dropWhileEnd
    , testProperty "break"              prop_break
    , testProperty "span"               prop_span
    , testProperty "splitAt"            prop_splitAt
    , testProperty "break/span"         prop_breakspan
    , testProperty "split"              prop_split
    , testProperty "splitWith_empty"    prop_splitWith_empty
    , testProperty "splitWith"          prop_splitWith
    , testProperty "splitWith_empty"    prop_splitWith_D_empty
    , testProperty "splitWith"          prop_splitWith_D
    , testProperty "splitWith_empty"    prop_splitWith_C_empty
    , testProperty "splitWith"          prop_splitWith_C
    , testProperty "split_empty"        prop_split_empty
    , testProperty "join.split/id"      prop_joinsplit
--  , testProperty "join/joinByte"      prop_joinjoinByte
    , testProperty "group"              prop_group
    , testProperty "groupBy"            prop_groupBy
    , testProperty "groupBy"            prop_groupBy_LC
    , testProperty "index"              prop_index
    , testProperty "index"              prop_index_D
    , testProperty "index"              prop_index_C
    , testProperty "indexMaybe"         prop_indexMaybe_Just_P
    , testProperty "indexMaybe"         prop_indexMaybe_Just_L
    , testProperty "indexMaybe"         prop_indexMaybe_Nothing_P
    , testProperty "indexMaybe"         prop_indexMaybe_Nothing_L
    , testProperty "elemIndex"          prop_elemIndex
    , testProperty "elemIndices"        prop_elemIndices
    , testProperty "count/elemIndices"  prop_count
    , testProperty "findIndex"          prop_findIndex
    , testProperty "findIndexEnd"       prop_findIndexEnd
    , testProperty "findIndices"        prop_findIndicies
    , testProperty "find"               prop_find
    , testProperty "find/findIndex"     prop_find_findIndex
    , testProperty "elem"               prop_elem
    , testProperty "notElem"            prop_notElem
    , testProperty "elem/notElem"       prop_elem_notelem
--  , testProperty "filterByte 1"       prop_filterByte
--  , testProperty "filterByte 2"       prop_filterByte2
--  , testProperty "filterNotByte 1"    prop_filterNotByte
--  , testProperty "filterNotByte 2"    prop_filterNotByte2
    , testProperty "isPrefixOf"         prop_isPrefixOf
    , testProperty "isSuffixOf"         prop_isSuffixOf
    , testProperty "stripPrefix"        prop_stripPrefix
    , testProperty "stripSuffix"        prop_stripSuffix
    , testProperty "concatMap"          prop_concatMap
    , testProperty "isSpace"            prop_isSpaceWord8
    ]

------------------------------------------------------------------------
--
-- These are miscellaneous tests left over. Or else they test some
-- property internal to a type (i.e. head . sort == minimum), without
-- reference to a model type.
--

prop_eq_refl  x     = x        == (x :: ByteString)
prop_eq_symm  x y   = (x == y) == (y == (x :: ByteString))

prop_eq1 xs      = xs == (unpack . pack $ xs)
prop_eq2 xs      = xs == (xs :: ByteString)
prop_eq3 xs ys   = (xs == ys) == (unpack xs == unpack ys)

prop_compare1 xs   = (pack xs        `compare` pack xs) == EQ
prop_compare2 xs c = (pack (xs++[c]) `compare` pack xs) == GT
prop_compare3 xs c = (pack xs `compare` pack (xs++[c])) == LT

prop_compare4 xs    = (not (null xs)) ==> (pack xs  `compare` L.empty) == GT
prop_compare5 xs    = (not (null xs)) ==> (L.empty `compare` pack xs) == LT
prop_compare6 xs ys = (not (null ys)) ==> (pack (xs++ys)  `compare` pack xs) == GT

prop_compare7 x  y  = x  `compare` y  == (L.singleton x `compare` L.singleton y)
prop_compare8 xs ys = xs `compare` ys == (L.pack xs `compare` L.pack ys)


prop_empty1 = L.length L.empty == 0
prop_empty2 = L.unpack L.empty == []

prop_packunpack s = (L.unpack . L.pack) s == id s
prop_unpackpack s = (L.pack . L.unpack) s == id s

prop_null xs = null (L.unpack xs) == L.null xs

prop_length1 xs = fromIntegral (length xs) == L.length (L.pack xs)

prop_length2 xs = L.length xs == length1 xs
  where length1 ys
            | L.null ys = 0
            | otherwise = 1 + length1 (L.tail ys)

prop_cons1 c xs = unpack (L.cons c (pack xs)) == (c:xs)
prop_cons2 c    = L.singleton c == (c `L.cons` L.empty)
prop_cons3 c    = unpack (L.singleton c) == (c:[])
prop_cons4 c    = (c `L.cons` L.empty)  == pack (c:[])

prop_snoc1 xs c = xs ++ [c] == unpack ((pack xs) `L.snoc` c)

prop_head  xs = (not (null xs)) ==> head xs == (L.head . pack) xs
prop_head1 xs = not (L.null xs) ==> L.head xs == head (L.unpack xs)

prop_tail xs  = not (L.null xs) ==> L.tail xs == pack (tail (unpack xs))
prop_tail1 xs = (not (null xs)) ==> tail xs   == (unpack . L.tail . pack) xs

prop_last xs  = (not (null xs)) ==> last xs    == (L.last . pack) xs

prop_init xs  =
    (not (null xs)) ==>
    init xs   == (unpack . L.init . pack) xs

prop_append1 xs    = (xs ++ xs) == (unpack $ pack xs `L.append` pack xs)
prop_append2 xs ys = (xs ++ ys) == (unpack $ pack xs `L.append` pack ys)
prop_append3 xs ys = L.append xs ys == pack (unpack xs ++ unpack ys)

prop_map1 f xs   = L.map f (pack xs)    == pack (map f xs)
prop_map2 f g xs = L.map f (L.map g xs) == L.map (f . g) xs
prop_map3 f xs   = map f xs == (unpack . L.map f .  pack) xs

prop_filter1 c xs = (filter (/=c) xs) == (unpack $ L.filter (/=c) (pack xs))
prop_filter2 p xs = (filter p xs) == (unpack $ L.filter p (pack xs))

prop_reverse  xs = reverse xs          == (unpack . L.reverse . pack) xs
prop_reverse1 xs = L.reverse (pack xs) == pack (reverse xs)
prop_reverse2 xs = reverse (unpack xs) == (unpack . L.reverse) xs

prop_transpose xs = (transpose xs) == ((map unpack) . L.transpose . (map pack)) xs

prop_foldl f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char

prop_foldr f c xs = L.foldl f c (pack xs) == foldl f c xs
    where _ = c :: Char

prop_foldl_1 xs = L.foldl (\xs c -> c `L.cons` xs) L.empty xs == L.reverse xs
prop_foldr_1 xs = L.foldr (\c xs -> c `L.cons` xs) L.empty xs == id xs

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
prop_concat3    = forAll (sized $ \n -> resize (n `div` 2) arbitrary) $ \xss ->
                  L.concat (map pack xss) == pack (concat xss)

prop_concatMap xs = L.concatMap L.singleton xs == (pack . concatMap (:[]) . unpack) xs

prop_any xs a = (any (== a) xs) == (L.any (== a) (pack xs))
prop_all xs a = (all (== a) xs) == (L.all (== a) (pack xs))

prop_maximum xs = (not (null xs)) ==> (maximum xs) == (L.maximum ( pack xs ))
prop_minimum xs = (not (null xs)) ==> (minimum xs) == (L.minimum ( pack xs ))

prop_replicate1 c =
    forAll arbitrary $ \(Positive n) ->
    unpack (L.replicate (fromIntegral n) c) == replicate n c

prop_replicate2 c = unpack (L.replicate 0 c) == replicate 0 c

prop_take1 i xs = L.take (fromIntegral i) (pack xs) == pack (take i xs)
prop_takeEnd i xs = P.takeEnd i xs == P.drop (P.length xs - i) xs

prop_drop1 i xs = L.drop (fromIntegral i) (pack xs) == pack (drop i xs)
prop_dropEnd i xs = P.dropEnd i xs == P.take (P.length xs - i) xs

prop_splitAt i xs = --collect (i >= 0 && i < length xs) $
    L.splitAt (fromIntegral i) (pack xs) == let (a,b) = splitAt i xs in (pack a, pack b)

prop_takeWhile f xs = L.takeWhile f (pack xs) == pack (takeWhile f xs)
prop_dropWhile f xs = L.dropWhile f (pack xs) == pack (dropWhile f xs)
prop_takeWhileEnd f = P.takeWhileEnd f `eq1` (P.reverse . P.takeWhile f . P.reverse)
prop_dropWhileEnd f = P.dropWhileEnd f `eq1` (P.reverse . P.dropWhile f . P.reverse)

prop_break f xs = L.break f (pack xs) ==
    let (a,b) = break f xs in (pack a, pack b)

prop_breakspan xs c = L.break (==c) xs == L.span (/=c) xs

prop_span xs a = (span (/=a) xs) == (let (x,y) = L.span (/=a) (pack xs) in (unpack x, unpack y))

prop_split c xs = (map L.unpack . map checkInvariant . L.split c $ xs)
               == (map P.unpack . P.split c . P.pack . L.unpack $ xs)

prop_splitWith_empty f = L.splitWith f mempty == []

prop_splitWith f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map L.length splits) == L.length xs - l2
  where splits = L.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = L.length (L.filter f xs)

prop_splitWith_D_empty f = D.splitWith f mempty == []

prop_splitWith_D f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map D.length splits) == D.length xs - l2
  where splits = D.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = D.length (D.filter f xs)

prop_splitWith_C_empty f = C.splitWith f mempty == []

prop_splitWith_C f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map C.length splits) == C.length xs - l2
  where splits = C.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = C.length (C.filter f xs)

prop_split_empty c = L.split c mempty == []

prop_joinsplit c xs = L.intercalate (pack [c]) (L.split c xs) == id xs

prop_group xs       = group xs == (map unpack . L.group . pack) xs
prop_groupBy  f xs  = groupBy f xs == (map unpack . L.groupBy f . pack) xs

prop_groupBy_LC :: (Char8 -> Char8 -> Bool) -> String8 -> Bool
prop_groupBy_LC f' (String8 xs) =
    groupBy f xs == (map LC.unpack . LC.groupBy f .  LC.pack) xs
  where
    f :: Char -> Char -> Bool
    f = castFn f'

-- prop_joinjoinByte xs ys c = L.joinWithByte c xs ys == L.join (L.singleton c) [xs,ys]

prop_index xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == L.pack xs `L.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

prop_index_D (String8 xs) =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == D.pack xs `D.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

prop_index_C (String8 xs) =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == C.pack xs `C.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

-- | Test 'indexMaybe' for Lazy and Strict 'ByteString's.
--   If we are testing within the bounds it should return a 'Just' value.
--   If we are testing outside of the bounds it should return a 'Nothing' value.
prop_indexMaybe_Just_L xs =
  not (null xs) ==>
    forAll indices $ \i -> isJust (ys `L.indexMaybe` (fromIntegral i))
  where
    ys = L.pack xs
    indices = choose (0, length xs -1)

prop_indexMaybe_Just_P xs =
  not (null xs) ==>
    forAll indices $ \i -> isJust (ys `P.indexMaybe` (fromIntegral i))
  where
    ys = P.pack xs
    indices = choose (0, length xs -1)

prop_indexMaybe_Nothing_L xs =
  not (null xs) ==>
    forAll indices $ \i -> isNothing (ys `L.indexMaybe` (fromIntegral i))
  where
      ys = L.pack xs
      outOfBounds = choose (-100, length xs + 100)
      indices = suchThat outOfBounds (\n -> n < 0 || n >= length xs)

prop_indexMaybe_Nothing_P xs =
  not (null xs) ==>
    forAll indices $ \i -> isNothing (ys `P.indexMaybe` (fromIntegral i))
  where
    ys = P.pack xs
    outOfBounds = choose (-100, length xs + 100)
    indices = suchThat outOfBounds (\n -> n < 0 || n >= length xs)

prop_elemIndex xs c = (elemIndex c xs) == fmap fromIntegral (L.elemIndex c (pack xs))


prop_elemIndices xs c = elemIndices c xs == map fromIntegral (L.elemIndices c (pack xs))

prop_count c xs = length (L.elemIndices c xs) == fromIntegral (L.count c xs)

prop_findIndex xs f = (findIndex f xs) == fmap fromIntegral (L.findIndex f (pack xs))
prop_findIndexEnd xs f = (findIndexEnd f xs) == fmap fromIntegral (L.findIndexEnd f (pack xs))
prop_findIndicies xs f = (findIndices f xs) == map fromIntegral (L.findIndices f (pack xs))

prop_elem    xs c = (c `elem` xs)    == (c `L.elem` (pack xs))
prop_notElem xs c = (c `notElem` xs) == (L.notElem c (pack xs))
prop_elem_notelem xs c = c `L.elem` xs == not (c `L.notElem` xs)

-- prop_filterByte  xs c = L.filterByte c xs == L.filter (==c) xs
-- prop_filterByte2 xs c = unpack (L.filterByte c xs) == filter (==c) (unpack xs)

-- prop_filterNotByte  xs c = L.filterNotByte c xs == L.filter (/=c) xs
-- prop_filterNotByte2 xs c = unpack (L.filterNotByte c xs) == filter (/=c) (unpack xs)

prop_find p xs = find p xs == L.find p (pack xs)

prop_find_findIndex p xs =
    L.find p xs == case L.findIndex p xs of
                                Just n -> Just (xs `L.index` n)
                                _      -> Nothing

prop_isPrefixOf xs ys = isPrefixOf xs ys == (pack xs `L.isPrefixOf` pack ys)
prop_stripPrefix xs ys = (pack <$> stripPrefix xs ys) == (pack xs `L.stripPrefix` pack ys)

prop_isSuffixOf xs ys = isSuffixOf xs ys == (pack xs `L.isSuffixOf` pack ys)
prop_stripSuffix xs ys = (pack <$> stripSuffix xs ys) == (pack xs `L.stripSuffix` pack ys)

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

-}

prop_isSpaceWord8 (w :: Word8) = isSpace c == P.isSpaceChar8 c
   where c = chr (fromIntegral w)
