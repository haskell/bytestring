#!/usr/bin/env runhaskell
{-# OPTIONS_GHC -fglasgow-exts #-}

import Test.QuickCheck.Batch
import Test.QuickCheck
import Text.Show.Functions
import System.Random
import Data.List
import Data.Char
import Data.Word
import Data.Maybe
import Text.Printf
import System.Environment

import Data.ByteString.Char8 (ByteString, pack , unpack)
import qualified Data.ByteString.Char8 as P

instance Arbitrary Char where
  arbitrary = oneof $ map return
                (['a'..'z']++['A'..'Z']++['1'..'9']++['\n','\t','0','~','.',',','-','/'])
  coarbitrary c = variant (ord c `rem` 16)

instance Arbitrary Word8 where
  arbitrary = choose (minBound, maxBound)
  coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 16))

instance Random Word8 where
  randomR (a,b) g = case randomR (fromIntegral a :: Integer
                                 ,fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x :: Word8, g)

  random g        = randomR (minBound,maxBound) g

instance Arbitrary ByteString where
  arbitrary = P.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (P.unpack s)

------------------------------------------------------------------------
-- Simon's functions:

prop_pack string = string == P.unpack (P.pack string)

prop_nil1 = P.length P.empty == 0
prop_nil2 = P.unpack P.empty == ""

prop_cons c xs = P.unpack (P.cons c (P.pack xs)) == (c:xs)

prop_headS xs = not (P.null xs) ==> P.head xs == head (P.unpack xs)

prop_tailS xs = not (P.null xs) ==> P.tail xs == P.pack (tail (P.unpack xs))

prop_null xs = null (P.unpack xs) == P.null xs

prop_append xs ys = P.append xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_lengthS xs = length xs == P.length (P.pack xs)

prop_index xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.index` i
  where indices = choose (0, length xs -1)

prop_unsafeIndex xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_mapS f xs = P.map f (P.pack xs) == P.pack (map f xs)

prop_filter f xs = P.filter f (P.pack xs) == P.pack (filter f xs)

prop_reverseS xs = P.reverse (P.pack xs) == P.pack (reverse xs)

prop_concat xss = P.concat (map P.pack xss) == P.pack (concat xss)

prop_elemS x xs = P.elem x (P.pack xs) == elem x xs

prop_takeS i xs = P.take i (P.pack xs) == P.pack (take i xs)

prop_dropS i xs = P.drop i (P.pack xs) == P.pack (drop i xs)

prop_splitAtS i xs = collect (i >= 0 && i < length xs) $
    P.splitAt i (P.pack xs) ==
    let (a,b) = splitAt i xs in (P.pack a, P.pack b)

prop_foldl f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_foldr f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_takeWhileS f xs = P.takeWhile f (P.pack xs) == P.pack (takeWhile f xs)

prop_dropWhileS f xs = P.dropWhile f (P.pack xs) == P.pack (dropWhile f xs)

prop_spanS f xs = P.span f (P.pack xs) ==
    let (a,b) = span f xs in (P.pack a, P.pack b)

prop_breakS f xs = P.break f (P.pack xs) ==
    let (a,b) = break f xs in (P.pack a, P.pack b)

prop_linesS xs = P.lines (P.pack xs) == map P.pack (lines xs)

prop_unlinesS xss = P.unlines (map P.pack xss) == P.pack (unlines xss)

prop_wordsS xs = P.words (P.pack xs) == map P.pack (words xs)

prop_unwordsS xss = P.unwords (map P.pack xss) == P.pack (unwords xss)

prop_splitWith f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map P.length splits) == P.length xs - l2
  where splits = P.splitWith f xs
        l1 = length splits
        l2 = P.length (P.filter f xs)

prop_joinsplit c xs = P.join (P.pack [c]) (P.split c xs) == xs

prop_linessplit xs =
    (not . P.null) xs ==>
    P.lines' xs == P.split '\n' xs

prop_linessplit2 xs =
    P.lines xs == P.split '\n' xs ++ (if P.last xs == '\n' then [P.empty] else [])

prop_splitsplitWith c xs = P.split c xs == P.splitWith (== c) xs

prop_bijection  c = (P.w2c . P.c2w) c == id c
prop_bijection' w = (P.c2w . P.w2c) w == id w

------------------------------------------------------------------------
-- at first we just check the correspondence to List functions

prop_eq1 xs      = xs            == (unpack . pack $ xs)

prop_compare1 xs  = (pack xs         `compare` pack xs) == EQ
prop_compare2 xs  = (pack (xs++"X")  `compare` pack xs) == GT
prop_compare3 xs  = (pack xs  `compare` pack (xs++"X")) == LT
prop_compare4 xs  = (not (null xs)) ==> (pack xs  `compare` P.empty) == GT
prop_compare5 xs  = (not (null xs)) ==> (P.empty `compare` pack xs) == LT
prop_compare6 xs ys= (not (null ys)) ==> (pack (xs++ys)  `compare` pack xs) == GT

-- prop_nil1 xs = (null xs) ==> pack xs == P.empty
-- prop_nil2 xs = (null xs) ==> xs == unpack P.empty

prop_cons1 xs = 'X' : xs == unpack ('X' `P.cons` (pack xs))

prop_cons2 xs c = c : xs == unpack (c `P.cons` (pack xs))

prop_snoc1 xs c = xs ++ [c] == unpack ((pack xs) `P.snoc` c)

prop_head xs     = (not (null xs)) ==> head  xs  == (P.head . pack) xs
prop_head1 xs    = (not (null xs)) ==> head xs == (P.unsafeHead . pack) xs

prop_tail xs     = (not (null xs)) ==> tail xs    == (unpack . P.tail . pack) xs
prop_tail1 xs    = (not (null xs)) ==> tail xs    == (unpack . P.unsafeTail. pack) xs

prop_init xs     =
    (not (null xs)) ==>
    init xs    == (unpack . P.init . pack) xs

-- prop_null xs = (null xs) ==> null xs == (nullPS (pack xs))

prop_length xs = length xs == P.length (pack xs)

prop_append1 xs = (xs ++ xs) == (unpack $ pack xs `P.append` pack xs)
prop_append2 xs ys = (xs ++ ys) == (unpack $ pack xs `P.append` pack ys)

prop_map   xs = map toLower xs == (unpack . (P.map toLower) .  pack) xs

prop_filter1 xs   = (filter (=='X') xs) == (unpack $ P.filter (=='X') (pack xs))
prop_filter2 xs c = (filter (==c) xs) == (unpack $ P.filter (==c) (pack xs))

prop_find xs c = find (==c) xs == P.find (==c) (pack xs)

prop_foldl1 xs = ((foldl (\x c -> if c == 'a' then x else c:x) [] xs)) ==
                (unpack $ P.foldl (\x c -> if c == 'a' then x else c `P.cons` x) P.empty (pack xs))

prop_foldl2 xs = P.foldl (\xs c -> c `P.cons` xs) P.empty (pack xs) == P.reverse (pack xs)

prop_foldr1 xs = ((foldr (\c x -> if c == 'a' then x else c:x) [] xs)) ==
                (unpack $ P.foldr (\c x -> if c == 'a' then x else c `P.cons` x)
                    P.empty (pack xs))

prop_foldr2 xs = P.foldr (\c xs -> c `P.cons` xs) P.empty (pack xs) == (pack xs)

prop_foldl1_1 xs =
    (not . P.null) xs ==>
    P.foldl1 (\x c -> if c > x then c else x)      xs ==
    P.foldl  (\x c -> if c > x then c else x) '\0' xs

prop_foldr1_1 xs =
    (not . P.null) xs ==>
    P.foldr1 (\c x -> if c > x then c else x)      xs ==
    P.foldr  (\c x -> if c > x then c else x) '\0' xs

prop_takeWhile xs = (takeWhile (/= 'X') xs) == (unpack . (P.takeWhile (/= 'X')) . pack) xs

prop_dropWhile xs = (dropWhile (/= 'X') xs) == (unpack . (P.dropWhile (/= 'X')) . pack) xs

prop_take xs = (take 10 xs) == (unpack . (P.take 10) . pack) xs

prop_drop xs = (drop 10 xs) == (unpack . (P.drop 10) . pack) xs

prop_splitAt i xs = collect (i >= 0 && i < length xs) $
    splitAt i xs ==
    let (x,y) = P.splitAt i (pack xs) in (unpack x, unpack y)

prop_span xs = (span (/='X') xs) == (let (x,y) = P.span (/='X') (pack xs)
                                     in (unpack x, unpack y))

prop_break xs = (break (/='X') xs) == (let (x,y) = P.break (/='X') (pack xs)
                                       in (unpack x, unpack y))

prop_reverse xs = (reverse xs) == (unpack . P.reverse . pack) xs

prop_elem xs = ('X' `elem` xs) == ('X' `P.elem` (pack xs))

prop_notElem c xs = P.notElem c (P.pack xs) == notElem c xs

-- should try to stress it
prop_concat1 xs = (concat [xs,xs]) == (unpack $ P.concat [pack xs, pack xs])

prop_concat2 xs = (concat [xs,[]]) == (unpack $ P.concat [pack xs, pack []])

prop_any xs = (any (== 'X') xs) == (P.any (== 'X') (pack xs))
prop_all xs = (all (== 'X') xs) == (P.all (== 'X') (pack xs))

prop_lines xs = (lines xs) == ((map unpack) . P.lines . pack) xs

prop_unlines xs = (unlines.lines) xs == (unpack. P.unlines . P.lines .pack) xs

prop_words xs = (words xs) == ((map unpack) . P.words . pack) xs
prop_wordstokens xs = P.words xs == P.tokens isSpace xs

prop_unwords xs = (pack.unwords.words) xs == (P.unwords . P.words .pack) xs

prop_join xs = (concat . (intersperse "XYX") . lines) xs ==
               (unpack $ P.join (pack "XYX") (P.lines (pack xs)))

prop_elemIndex1 xs   = (elemIndex 'X' xs) == (P.elemIndex 'X' (pack xs))
prop_elemIndex2 xs c = (elemIndex c xs) == (P.elemIndex c (pack xs))

prop_lineIndices1 xs = P.elemIndices '\n' xs == P.lineIndices xs

prop_count c xs = length (P.elemIndices c xs) == P.count c xs

prop_elemIndexLast1 c xs = (P.elemIndexLast c (pack xs)) ==
                           (case P.elemIndex c (pack (reverse xs)) of
                                Nothing -> Nothing
                                Just i  -> Just (length xs -1 -i))

prop_elemIndexLast2 c xs = (P.elemIndexLast c (pack xs)) ==
                           ((-) (length xs - 1) `fmap` P.elemIndex c (pack $ reverse xs))

prop_elemIndices xs c = elemIndices c xs == P.elemIndices c (pack xs)

prop_findIndex xs = (findIndex (=='X') xs) == (P.findIndex (=='X') (pack xs))

prop_findIndicies xs c = (findIndices (==c) xs) == (P.findIndices (==c) (pack xs))

-- example properties from QuickCheck.Batch
prop_sort1 xs = sort xs == (unpack . P.sort . pack) xs
prop_sort2 xs = (not (null xs)) ==> (P.head . P.sort . pack $ xs) == minimum xs
prop_sort3 xs = (not (null xs)) ==> (P.last . P.sort . pack $ xs) == maximum xs
prop_sort4 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.head . P.sort) (P.append (pack xs) (pack ys)) == min (minimum xs) (minimum ys)
prop_sort5 xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.last . P.sort) (P.append (pack xs) (pack ys)) == max (maximum xs) (maximum ys)

prop_intersperse c xs = (intersperse c xs) == (unpack $ P.intersperse c (pack xs))

prop_transpose xs = (transpose xs) == ((map unpack) . P.transpose . (map pack)) xs

prop_maximum xs = (not (null xs)) ==> (maximum xs) == (P.maximum ( pack xs ))
prop_minimum xs = (not (null xs)) ==> (minimum xs) == (P.minimum ( pack xs ))

------------------------------------------------------------------------

prop_dropSpace xs    = dropWhile isSpace xs == unpack (P.dropSpace (pack xs))
prop_dropSpaceEnd xs = (P.reverse . (P.dropWhile isSpace) . P.reverse) (pack xs) ==
                       (P.dropSpaceEnd (pack xs))

prop_breakSpace xs = (let (x,y) = P.breakSpace (pack xs)
                      in (unpack x, unpack y)) == (break isSpace xs)

prop_spanEnd xs =
        (P.spanEnd (not . isSpace) (pack xs)) ==
        (let (x,y) = P.span (not.isSpace) (P.reverse (pack xs)) in (P.reverse y,P.reverse x))

prop_breakChar c xs =
        (break (==c) xs) ==
        (let (x,y) = P.breakChar c (pack xs) in (unpack x, unpack y))

prop_breakFirst c xs = (let (x,y) = break (==c) xs
                        in if null y then Nothing
                                     else Just (pack x, pack $ drop 1 y)) ==
                       (P.breakFirst c (pack xs))

prop_breakLast c xs = (let (x,y) = break (==c) (reverse xs)
                       in if null y then Nothing
                                    else Just (pack (reverse $ drop 1 y), pack (reverse x))) ==
                       (P.breakLast c (pack xs))

prop_words' xs = (unpack . P.unwords  . P.words' . pack) xs ==
                 (map (\c -> if isSpace c then ' ' else c) xs)
prop_lines' xs = (unpack . P.unlines' . P.lines' . pack) xs == (xs)

prop_unfoldr c =
    (P.unfoldrN 100 (\x -> Just (x, chr (ord x + 1))) c) ==
    (pack $ take 100 $ unfoldr (\x -> Just (x, chr (ord x + 1))) c)

prop_addr = let s = "my\nstring\nhaskell"# in P.length (P.packAddress s) == 17
prop_addr2 = let s = "my\nstring\nhaskell"# in P.unsafePackAddress 17 s == P.packAddress s

prop_prefix xs ys = isPrefixOf xs ys == (P.pack xs `P.isPrefixOf` P.pack ys)
prop_suffix xs ys = isSuffixOf xs ys == (P.pack xs `P.isSuffixOf` P.pack ys)

prop_copy xs = let p = P.pack xs in P.copy p == p

prop_inits xs = inits xs == map P.unpack (P.inits (P.pack xs))

prop_tails xs = tails xs == map P.unpack (P.tails (P.pack xs))

prop_findSubstrings s x l
    = P.findSubstrings (P.pack p) (P.pack s) == naive_findSubstrings p s
    where
    -- we look for some random substring of the test string
    p = take (abs l) $ drop (abs x) s
    -- naive reference implementation
    naive_findSubstrings :: String -> String -> [Int]
    naive_findSubstrings p s = [x | x <- [0..length s], p `isPrefixOf` drop x s]

prop_replicate1 n c =
    (n >= 0) ==>
    unpack (P.replicate n c) == replicate n c

prop_replicate2 n c =
    (n >= 0) ==>
    P.replicate n c == P.unfoldrN n (\u -> Just (u,u)) c

prop_replicate3 c = unpack (P.replicate 0 c) == replicate 0 c

prop_readint (n::Int) = (fst . fromJust . P.readInt . pack . show) n == n

prop_readint2 s =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in P.readInt (pack s') == Nothing

prop_filterChar1 c xs = (filter (==c) xs) == ((P.unpack . P.filterChar c . P.pack) xs)
prop_filterChar2 c xs = (P.filter (==c) (P.pack xs)) == (P.filterChar c (P.pack xs))
prop_filterChar3 c xs = P.filterChar c xs == P.replicate (P.count c xs) c

prop_filterNotChar1 c xs = (filter (/=c) xs) == ((P.unpack . P.filterNotChar c . P.pack) xs)
prop_filterNotChar2 c xs = (P.filter (/=c) (P.pack xs)) == (P.filterNotChar c (P.pack xs))

prop_joinjoinpath xs ys = P.joinWithChar ' ' xs ys == P.join (P.packChar ' ') [xs,ys]

prop_zip  xs ys = zip xs ys == P.zip (pack xs) (pack ys)
prop_zip1 xs ys = P.zip xs ys == zip (P.unpack xs) (P.unpack ys)

prop_zipWith xs ys = P.zipWith (,) xs ys == P.zip xs ys

prop_unzip x = let (xs,ys) = unzip x in (pack xs, pack ys) == P.unzip x

------------------------------------------------------------------------

main = do
    x <- getArgs
    let n = if null x then 200 else read . head $ x
    do mapM_ (runTests "test" (defOpt { no_of_tests = n, length_of_tests = 10 })) $
         breakUp 25 tests []

 where
    breakUp _ [] acc = reverse acc
    breakUp n xs acc = let h = take n xs
                           r = drop n xs
                       in breakUp n r (h:acc)

    tests = [
                 run prop_bijection
            ,    run prop_bijection'
            ,    run prop_eq1
            ,    run prop_compare1
            ,    run prop_compare2
            ,    run prop_compare3
            ,    run prop_compare4
            ,    run prop_compare5
            ,    run prop_compare6
            ,    run prop_cons1
            ,    run prop_cons2
            ,    run prop_snoc1
            ,    run prop_head
            ,    run prop_head1
            ,    run prop_tail
            ,    run prop_tail1
            ,    run prop_init
            ,    run prop_length
            ,    run prop_append1
            ,    run prop_append2
            ,    run prop_map
            ,    run prop_filter1
            ,    run prop_filter2
            ,    run prop_foldl1
            ,    run prop_foldl2
            ,    run prop_foldr1
            ,    run prop_foldr2
            ,    run prop_all
            ,    run prop_take
            ,    run prop_drop
            ,    run prop_takeWhile
            ,    run prop_dropWhile
            ,    run prop_splitAt
            ,    run prop_span
            ,    run prop_break
            ,    run prop_reverse
            ,    run prop_elem
            ,    run prop_notElem
            ,    run prop_concat1
            ,    run prop_concat2
            ,    run prop_any
            ,    run prop_lines
            ,    run prop_unlines
            ,    run prop_words
            ,    run prop_unwords
            ,    run prop_join
            ,    run prop_elemIndex1
            ,    run prop_elemIndex2
            ,    run prop_findIndex
            ,    run prop_findIndicies
            ,    run prop_elemIndices

            ,    run prop_find
            ,    run prop_sort1
            ,    run prop_sort2
            ,    run prop_sort3
            ,    run prop_sort4
            ,    run prop_sort5
            ,    run prop_intersperse
            ,    run prop_maximum
            ,    run prop_minimum
            ,    run prop_breakChar
            ,    run prop_breakSpace
            ,    run prop_dropSpace
            ,    run prop_spanEnd
            ,    run prop_breakFirst
            ,    run prop_breakLast
            ,    run prop_elemIndexLast1
            ,    run prop_elemIndexLast2
            ,    run prop_words'
            ,    run prop_lines'
            ,    run prop_dropSpaceEnd
            ,    run prop_unfoldr
            ,    run prop_addr
            ,    run prop_addr2
            ,    run prop_prefix
            ,    run prop_suffix
            ,    run prop_copy
            ,    run prop_inits
            ,    run prop_tails
            ,    run prop_findSubstrings
            ,    run prop_replicate1
            ,    run prop_replicate2
            ,    run prop_replicate3
            ,    run prop_readint
            ,    run prop_readint2
            ,    run prop_filterChar1
            ,    run prop_filterChar2
            ,    run prop_filterChar3
            ,    run prop_filterNotChar1
            ,    run prop_filterNotChar2
            ,    run prop_pack
            ,    run prop_nil1
            ,    run prop_nil2
            ,    run prop_cons
            ,    run prop_headS
            ,    run prop_lengthS
            ,    run prop_tailS
            ,    run prop_null
            ,    run prop_append
            ,    run prop_index
            ,    run prop_unsafeIndex
            ,    run prop_mapS
            ,    run prop_filter
            ,    run prop_reverseS
            ,    run prop_concat
            ,    run prop_elemS
    --      ,    run prop_substr
            ,    run prop_takeS
            ,    run prop_dropS
            ,    run prop_splitAtS
            ,    run prop_foldl
            ,    run prop_foldr
            ,    run prop_takeWhileS
            ,    run prop_dropWhileS
            ,    run prop_spanS
            ,    run prop_breakS
            ,    run prop_linesS
            ,    run prop_unlinesS
            ,    run prop_wordsS
            ,    run prop_unwordsS
            ,    run prop_wordstokens
            ,    run prop_splitWith
            ,    run prop_joinsplit
            ,    run prop_lineIndices1
            ,    run prop_count
            ,    run prop_linessplit
            ,    run prop_splitsplitWith
            ,    run prop_joinjoinpath
            ,    run prop_zip
            ,    run prop_zip1
            ,    run prop_zipWith
            ,    run prop_unzip
            ]
