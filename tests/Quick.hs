
import Data.Char
import Data.List
import Data.Maybe
import Data.FastPackedString

import Test.QuickCheck.Batch
import Test.QuickCheck

------------------------------------------------------------------------
-- at first we just check the correspondence to List functions

prop_eq1 xs      = xs            == (unpackPS . packString $ xs) 

prop_compare1 xs  = (packString xs         `compare` packString xs) == EQ
prop_compare2 xs  = (packString (xs++"X")  `compare` packString xs) == GT
prop_compare3 xs  = (packString xs  `compare` packString (xs++"X")) == LT
prop_compare4 xs  = (not (null xs)) ==> (packString xs  `compare` nilPS) == GT
prop_compare5 xs  = (not (null xs)) ==> (nilPS `compare` packString xs) == LT

-- prop_nil1 xs = (null xs) ==> packString xs == nilPS
-- prop_nil2 xs = (null xs) ==> xs == unpackPS nilPS

prop_cons1 xs = 'X' : xs == unpackPS ('X' `consPS` (packString xs))

prop_head xs     = 
    (not (null xs)) ==> head xs  == (headPS . packString) xs

prop_tail xs     = 
    (not (null xs)) ==>
    tail xs    == (unpackPS . tailPS . packString) xs

prop_init xs     = 
    (not (null xs)) ==>
    init xs    == (unpackPS . initPS . packString) xs

-- prop_null xs = (null xs) ==> null xs == (nullPS (packString xs))

prop_length xs = length xs == lengthPS (packString xs)

prop_append xs = (xs ++ xs) == (unpackPS $ packString xs `appendPS` packString xs)

prop_map   xs = map toLower xs == (unpackPS . (mapPS toLower) .  packString) xs

prop_filter xs = (filter (=='X') xs) == (unpackPS $ filterPS (=='X') (packString xs))

prop_foldl xs = ((foldl (\x c -> if c == 'a' then x else c:x) [] xs)) ==  
                (unpackPS $ foldlPS (\x c -> if c == 'a' then x else c `consPS` x) nilPS (packString xs))

prop_foldr xs = ((foldr (\c x -> if c == 'a' then x else c:x) [] xs)) ==  
                (unpackPS $ foldrPS (\c x -> if c == 'a' then x else c `consPS` x) 
                    nilPS (packString xs))

prop_takeWhile xs = (takeWhile (/= 'X') xs) == (unpackPS . (takeWhilePS (/= 'X')) . packString) xs

prop_dropWhile xs = (dropWhile (/= 'X') xs) == (unpackPS . (dropWhilePS (/= 'X')) . packString) xs

prop_take xs = (take 10 xs) == (unpackPS . (takePS 10) . packString) xs

prop_drop xs = (drop 10 xs) == (unpackPS . (dropPS 10) . packString) xs

prop_splitAt xs = (splitAt 1000 xs) == (let (x,y) = splitAtPS 1000 (packString xs) 
                                      in (unpackPS x, unpackPS y))

prop_span xs = (span (/='X') xs) == (let (x,y) = spanPS (/='X') (packString xs) 
                                     in (unpackPS x, unpackPS y))

prop_break xs = (break (/='X') xs) == (let (x,y) = breakPS (/='X') (packString xs) 
                                       in (unpackPS x, unpackPS y))

prop_reverse xs = (reverse xs) == (unpackPS . reversePS . packString) xs

prop_elem xs = ('X' `elem` xs) == ('X' `elemPS` (packString xs))

-- should try to stress it
prop_concat1 xs = (concat [xs,xs]) == (unpackPS $ concatPS [packString xs, packString xs])

prop_concat2 xs = (concat [xs,[]]) == (unpackPS $ concatPS [packString xs, packString []])

prop_any xs = (any (== 'X') xs) == (anyPS (== 'X') (packString xs))

prop_lines xs = (lines xs) == ((map unpackPS) . linesPS . packString) xs

prop_unlines xs = (unlines.lines) xs == (unpackPS. unlinesPS . linesPS .packString) xs

prop_words xs = (words xs) == ((map unpackPS) . wordsPS . packString) xs

prop_unwords xs = (packString.unwords.words) xs == (unwordsPS . wordsPS .packString) xs

prop_join xs = (concat . (intersperse "XYX") . lines) xs ==
               (unpackPS $ joinPS (packString "XYX") (linesPS (packString xs)))

prop_elemIndex xs = (elemIndex 'X' xs) == (elemIndexPS 'X' (packString xs))

prop_findIndex xs = (fromMaybe (length xs) (findIndex (=='X') xs)) ==
                    (findIndexPS (=='X') (packString xs))

------------------------------------------------------------------------

main = do
    runTests "fps" (defOpt { no_of_tests = 500, length_of_tests= 10 } )
        [   run prop_eq1
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
  arbitrary = oneof $ map return
                (['a'..'z']++['A'..'Z']++['1'..'9']++['0','~','.',',','-','/'])
  coarbitrary c = coarbitrary (ord c)

