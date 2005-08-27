
import Data.Char
import Data.List
import Data.Maybe
import Data.FastPackedString

import Test.QuickCheck.Batch
import Test.QuickCheck

------------------------------------------------------------------------
-- at first we just check the correspondence to List functions

prop_eq1 xs      = xs            == (unpackPS . packString $ xs) 
prop_eq2 xs      = packString xs == packString xs
prop_eq3 xs      = (packString . unpackPS . packString $ xs) == packString xs

prop_compare1 xs  = (packString xs         `compare` packString xs) == EQ
prop_compare2 xs  = (packString (xs++"X")  `compare` packString xs) == GT
prop_compare3 xs  = (packString xs  `compare` packString (xs++"X")) == LT
prop_compare4 xs  = (not (null xs)) ==> (packString xs  `compare` nilPS) == GT
prop_compare5 xs  = (not (null xs)) ==> (nilPS `compare` packString xs) == LT

-- prop_nil1 xs = (null xs) ==> packString xs == nilPS
-- prop_nil2 xs = (null xs) ==> xs == unpackPS nilPS

prop_cons1 xs = 'X' : xs == unpackPS ('X' `consPS` (packString xs))

prop_head xs     = 
    (not (null xs)) ==>
        head xs  == headPS (packString xs)

prop_tail xs     = 
    (not (null xs)) ==>
    packString (tail xs)    == tailPS (packString xs)

prop_init xs     = 
    (not (null xs)) ==>
    packString (init xs)    == initPS (packString xs)

-- prop_null xs = (null xs) ==> null xs == (nullPS (packString xs))

prop_length xs = length xs == lengthPS (packString xs)

prop_append xs = packString (xs ++ xs) == packString xs `appendPS` packString xs

prop_map   xs = packString (map toUpper xs) == mapPS toUpper (packString xs)

prop_filter xs = packString (filter (=='X') xs) ==
                 filterPS (=='X') (packString xs)

prop_foldl xs = (packString (foldl (\x c -> if c == 'a' then x else c:x) [] xs)) ==  
                (foldlPS (\x c -> if c == 'a' then x else c `consPS` x) nilPS (packString xs))

prop_foldr xs = (packString (foldr (\c x -> if c == 'a' then x else c:x) [] xs)) ==  
                (foldrPS (\c x -> if c == 'a' then x else c `consPS` x) nilPS (packString xs))

prop_takeWhile xs = packString (takeWhile (/= 'X') xs) == takeWhilePS (/= 'X') (packString xs)

prop_dropWhile xs = packString (dropWhile (/= 'X') xs) == dropWhilePS (/= 'X') (packString xs)

prop_take xs = packString (take 10 xs) == takePS 10 (packString xs)

prop_drop xs = packString (drop 10 xs) == dropPS 10 (packString xs)

prop_splitAt xs = (splitAt 1000 xs) == (let (x,y) = splitAtPS 1000 (packString xs) 
                                      in (unpackPS x, unpackPS y))

prop_span xs = (span (/='X') xs) == (let (x,y) = spanPS (/='X') (packString xs) 
                                     in (unpackPS x, unpackPS y))

prop_break xs = (break (/='X') xs) == (let (x,y) = breakPS (/='X') (packString xs) 
                                       in (unpackPS x, unpackPS y))

prop_reverse xs = packString (reverse xs) == reversePS (packString xs)

prop_elem xs = ('X' `elem` xs) == ('X' `elemPS` (packString xs))

-- should try to stress it
prop_concat1 xs = packString (concat [xs,xs]) == concatPS [packString xs, packString xs]

prop_concat2 xs = packString (concat [xs,[]]) == concatPS [packString xs, packString []]

prop_any xs = (any (== 'X') xs) == (anyPS (== 'X') (packString xs))

prop_lines xs = map packString (lines xs) == linesPS (packString xs)

prop_unlines xs = (packString.unlines.lines) xs == (unlinesPS . linesPS .packString) xs

prop_words xs = map packString (words xs) == wordsPS (packString xs)

prop_unwords xs = (packString.unwords.words) xs == (unwordsPS . wordsPS .packString) xs

prop_join xs = (packString . concat . (intersperse "XYX") . lines) xs ==
               (joinPS (packString "XYX") (linesPS (packString xs)))

prop_elemIndex xs = (elemIndex 'X' xs) == (elemIndexPS 'X' (packString xs))

prop_findIndex xs = (fromMaybe (length xs) (findIndex (=='X') xs)) ==
                    (findIndexPS (=='X') (packString xs))

------------------------------------------------------------------------

main = do
    runTests "fps" (defOpt { no_of_tests = 1000, length_of_tests= 10 } )
        [   run prop_eq1
        ,   run prop_eq2
        ,   run prop_eq3
        ,   run prop_compare1
        ,   run prop_compare2
        ,   run prop_compare3
        ,   run prop_compare4
        ,   run prop_compare5
    --  ,   run prop_nil1
    --  ,   run prop_nil2
        ,   run prop_cons1
        ,   run prop_head
        ,   run prop_tail
        ,   run prop_init
    --  ,   run prop_null
        ,   run prop_length
        ,   run prop_append
        ,   run prop_map
        ,   run prop_filter
        ,   run prop_foldl
        ,   run prop_foldr
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
        ,   run prop_lines
        ,   run prop_unlines
        ,   run prop_words
        ,   run prop_unwords
        ,   run prop_join
        ,   run prop_elemIndex
        ,   run prop_findIndex
        ]

instance Arbitrary Char where
  arbitrary     = chr `fmap` choose (0,255)
  coarbitrary c = coarbitrary (ord c)

