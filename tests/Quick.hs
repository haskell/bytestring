
import Data.Char
import Data.List
import Data.Maybe
import Data.FastPackedString (pack,unpack)
import qualified Data.FastPackedString as P

import Test.QuickCheck.Batch
import Test.QuickCheck

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
prop_head1 xs    = (not (null xs)) ==> head xs == (P.head1 . pack) xs

prop_tail xs     = (not (null xs)) ==> tail xs    == (unpack . P.tail . pack) xs
prop_tail1 xs    = (not (null xs)) ==> tail xs    == (unpack . P.tail1. pack) xs

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

prop_foldl11 xs = 
    (not (null xs)) ==> 
    (P.foldl1 (\x y -> chr $ ord x + ord y)   (pack xs)) == 
    (P.foldl  (\x y -> chr $ ord x + ord y) '\0' (pack xs))

prop_foldr1 xs = ((foldr (\c x -> if c == 'a' then x else c:x) [] xs)) ==  
                (unpack $ P.foldr (\c x -> if c == 'a' then x else c `P.cons` x) 
                    P.empty (pack xs))

prop_foldr2 xs = P.foldr (\c xs -> c `P.cons` xs) P.empty (pack xs) == (pack xs)

prop_foldr11 xs = 
    (not (null xs)) ==> 
    (P.foldr1 (\x y -> chr $ ord x + ord y) (pack xs)) == 
    (P.foldr  (\x y -> chr $ ord x + ord y) '\0' (pack xs))

prop_takeWhile xs = (takeWhile (/= 'X') xs) == (unpack . (P.takeWhile (/= 'X')) . pack) xs

prop_dropWhile xs = (dropWhile (/= 'X') xs) == (unpack . (P.dropWhile (/= 'X')) . pack) xs

prop_take xs = (take 10 xs) == (unpack . (P.take 10) . pack) xs

prop_drop xs = (drop 10 xs) == (unpack . (P.drop 10) . pack) xs

prop_splitAt xs = (splitAt 1000 xs) == (let (x,y) = P.splitAt 1000 (pack xs) 
                                      in (unpack x, unpack y))

prop_span xs = (span (/='X') xs) == (let (x,y) = P.span (/='X') (pack xs) 
                                     in (unpack x, unpack y))

prop_break xs = (break (/='X') xs) == (let (x,y) = P.break (/='X') (pack xs) 
                                       in (unpack x, unpack y))

prop_reverse xs = (reverse xs) == (unpack . P.reverse . pack) xs

prop_elem xs = ('X' `elem` xs) == ('X' `P.elem` (pack xs))

-- should try to stress it
prop_concat1 xs = (concat [xs,xs]) == (unpack $ P.concat [pack xs, pack xs])

prop_concat2 xs = (concat [xs,[]]) == (unpack $ P.concat [pack xs, pack []])

prop_any xs = (any (== 'X') xs) == (P.any (== 'X') (pack xs))
prop_all xs = (all (== 'X') xs) == (P.all (== 'X') (pack xs))


prop_lines xs = (lines xs) == ((map unpack) . P.lines . pack) xs

prop_unlines xs = (unlines.lines) xs == (unpack. P.unlines . P.lines .pack) xs

prop_words xs = (words xs) == ((map unpack) . P.words . pack) xs

prop_unwords xs = (pack.unwords.words) xs == (P.unwords . P.words .pack) xs

prop_join xs = (concat . (intersperse "XYX") . lines) xs ==
               (unpack $ P.join (pack "XYX") (P.lines (pack xs)))

prop_elemIndex1 xs   = (elemIndex 'X' xs) == (P.elemIndex 'X' (pack xs))
prop_elemIndex2 xs c = (elemIndex c xs) == (P.elemIndex c (pack xs))

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

prop_dropSpace xs = dropWhile isSpace xs == unpack (P.dropSpace (pack xs))

prop_breakSpace xs = (let (x,y) = P.breakSpace (pack xs)
                      in (unpack x, unpack y)) == (break isSpace xs)

prop_spanEnd xs = (P.spanEnd (not . isSpace) (pack xs)) ==
                  (let (x,y) = P.span (not.isSpace) (P.reverse (pack xs)) 
                   in (P.reverse y,P.reverse x))

prop_breakOn c xs = (break (==c) xs) == 
                    (let (x,y) = P.breakOn c (pack xs) in (unpack x, unpack y))

prop_split xs = (map unpack (P.split '\n' (pack xs))) == lines xs

prop_breakFirst c xs = (let (x,y) = break (==c) xs
                        in if null y then Nothing
                                     else Just (pack x, pack $ drop 1 y)) ==
                       (P.breakFirst c (pack xs))

prop_breakLast c xs = (let (x,y) = break (==c) (reverse xs)
                       in if null x then Nothing
                                    else Just (pack (drop 1 y), pack x)) ==
                       (P.breakLast c (pack xs))

------------------------------------------------------------------------

main = do
    runTests "fps" (defOpt { no_of_tests = 200, length_of_tests= 10 } )
        [   run prop_eq1
        ,   run prop_compare1
        ,   run prop_compare2
        ,   run prop_compare3
        ,   run prop_compare4
        ,   run prop_compare5
        ,   run prop_compare6
    --  ,   run prop_nil1
    --  ,   run prop_nil2
        ,   run prop_cons1
        ,   run prop_cons2
        ,   run prop_snoc1
        ,   run prop_head
        ,   run prop_head1
        ,   run prop_tail
        ,   run prop_tail1
        ,   run prop_init
    --  ,   run prop_null
        ,   run prop_length
        ,   run prop_append1
        ,   run prop_append2
        ,   run prop_map
        ,   run prop_filter1
        ,   run prop_filter2
        ,   run prop_foldl1
        ,   run prop_foldl2
        ,   run prop_foldr1
        ,   run prop_foldr2
        ,   run prop_foldl11
        ,   run prop_foldr11
        ,   run prop_take
        ,   run prop_drop
        ,   run prop_takeWhile
        ,   run prop_dropWhile
        ,   run prop_splitAt
        ,   run prop_span
        ,   run prop_break
        ,   run prop_reverse
        ,   run prop_elem
        ,   run prop_concat1
        ,   run prop_concat2
        ,   run prop_any
        ,   run prop_all
        ,   run prop_lines
        ,   run prop_unlines
        ,   run prop_words
        ,   run prop_unwords
        ,   run prop_join
        ,   run prop_elemIndex1
        ,   run prop_elemIndex2
        ,   run prop_elemIndices
        ,   run prop_findIndex
        ,   run prop_findIndicies
        ,   run prop_find
        ,   run prop_sort1
        ,   run prop_sort2
        ,   run prop_sort3
        ,   run prop_sort4
        ,   run prop_sort5
        ,   run prop_intersperse
        ,   run prop_maximum
        ,   run prop_minimum
        ,   run prop_breakOn
        ,   run prop_breakSpace
        ,   run prop_dropSpace
        ,   run prop_spanEnd
        ,   run prop_split
        ,   run prop_breakFirst
        ]

instance Arbitrary Char where
  arbitrary = oneof $ map return
                (['a'..'z']++['A'..'Z']++['1'..'9']++['\n','\t','0','~','.',',','-','/'])
  coarbitrary c = coarbitrary (ord c)
