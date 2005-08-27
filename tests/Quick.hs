
import Data.Char
import Data.List
import Data.Maybe
import Data.FastPackedString

import Test.QuickCheck.Batch
import Test.QuickCheck

-- at first we just check the correspondence to List functions

prop_eq1 xs      = xs            == (unpackPS . packString $ xs) 
prop_eq2 xs      = packString xs == packString xs
prop_eq3 xs      = (packString . unpackPS . packString $ xs) == packString xs

prop_compare xs  = (packString xs `compare` packString xs) == EQ

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

prop_reverse1 xs = packString (reverse xs) == reversePS (packString xs)


main = runTests "fps" (defOpt { no_of_tests = 1000 } )
    [   run prop_eq1
    ,   run prop_eq2
    ,   run prop_eq3
    ,   run prop_compare
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
    ,   run prop_reverse1
    ]

instance Arbitrary Char where
  arbitrary     = chr `fmap` choose (0,255)
  coarbitrary c = coarbitrary (ord c)

