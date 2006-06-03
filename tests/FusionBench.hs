{-# OPTIONS -cpp #-}
--
-- Test the results of fusion
--

--
-- N.B. make sure to disable down fusion when using only loopU fusion
--

import BenchUtils
import Text.Printf
import qualified Data.ByteString      as P

-- minimum pipelines to trigger the various fusion forms
tests =
 [("map/map",         [F  ({-# SCC "map/map" #-}P.map (*2) . P.map (+4)                                           $ fps)])
 ,("filter/filter",   [F  ({-# SCC "filter/filter" #-}  P.filter (/=101) . P.filter (/=102)                               $ fps)])
 ,("filter/map",      [F  ({-# SCC "filter/map" #-}     P.filter (/=103) . P.map (+5)                                     $ fps)])
 ,("map/filter",      [F  ({-# SCC "map/filter" #-}     P.map (*3) . P.filter (/=104)                                     $ fps)])
 ,("up/up",           [F  ({-# SCC "up/up" #-}          P.foldl (const.(+1)) (0::X) . P.scanl (flip const) (0::W)         $ fps)])
 ,("map/up",          [F  ({-# SCC "map/up" #-}         P.foldl (const.(+6)) (0::X) . P.map (*4)                          $ fps)])
 ,("up/map",          [F  ({-# SCC "up/map" #-}         P.map (+7) . P.scanl const (0::W)                                 $ fps)])
 ,("filter/up",       [F  ({-# SCC "filter/up" #-}      P.foldl (const.(+8)) (0::X) . P.filter (/=105)                    $ fps)])
 ,("up/filter",       [F  ({-# SCC "up/filter" #-}      P.filter (/=106) . P.scanl (flip const) (0::W)                    $ fps)])
 ,("noacc/noacc",     [F  ({-# SCC "noacc/noacc" #-}    (P.map (*3) . P.filter (/=108)) . (P.map (*4) . P.filter (/=109)) $ fps)])
 ,("noacc/up",        [F  ({-# SCC "noacc/up" #-}       P.foldl (const.(+1)) (0::W) . (P.map (+1) . P.filter (/=110))     $ fps)])
 ,("up/noacc",        [F  ({-# SCC "up/noacc" #-}       (P.map (+1) . P.filter (/=111)) . P.scanl (flip const) (0::W)     $ fps)])
 ,("map/noacc",       [F  ({-# SCC "map/noacc" #-}      (P.map (+1) . P.filter (/=112)) . P.map (*2)                      $ fps)])
 ,("noacc/map",       [F  ({-# SCC "filter/noacc" #-}   P.map (+1) . (P.map (+2) . P.filter (/=113))                      $ fps)])
 ,("filter/noacc",    [F  ({-# SCC "noacc/filter"#-}    (P.map (+1) . P.filter (/=101)) . P.filter (/=114)                $ fps)])
 ,("noacc/filter",    [F  ({-# SCC "noacc/filter"#-}    P.filter (/=101) . (P.map (*2) . P.filter (/=115))                $ fps)])
#if !defined(LOOPU_FUSION)
 ,("down/down",       [F  ({-# SCC "map/map"    #-}     P.foldr (const.(+9))  (0::W) . P.scanr const (0::W)               $ fps)])
 ,("map/down",        [F  ({-# SCC "map/down"   #-}     P.foldr (const.(+10)) (0::W) . P.map (*2)                         $ fps)])
 ,("filter/down",     [F  ({-# SCC "filter/down"#-}     P.foldr (const.(+11)) (0::W) . P.filter (/=106)                   $ fps)])
 ,("down/filter",     [F  ({-# SCC "down/filter"#-}     P.filter (/=107) . P.scanr const (0::W)                           $ fps)])
 ,("noacc/down",      [F  ({-# SCC "noacc/down" #-}     P.foldr (const.(+1)) (0::W) . (P.map (+1) . P.filter (/=116))     $ fps)])
 ,("down/noacc",      [F  ({-# SCC "down/noacc" #-}     (P.map (+1) . P.filter (/=101)) . P.scanr const (0::W)            $ fps)])
 ,("length/loop",     [F  ({-# SCC "length/loop"#-}     P.length  . P.filter (/=105)                                      $ fps)])
 ,("maximum/loop",    [F  ({-# SCC "maximum/map"#-}     P.maximum . P.map (*4)                                            $ fps)])
 ,("minimum/loop",    [F  ({-# SCC "minimum/map"#-}     P.minimum . P.map (+6)                                            $ fps)])
#endif
 ]

-- and some longer ones to see the full effect
bigtests =
 [("big map/map",
    [F  ({-# SCC "map/map"#-}P.map (subtract 3). P.map (+7) . P.map (*2) . P.map (+4)                          $ fps)])
 ,("big filter/filter",
    [F  ({-# SCC "filter/filter"#-}P.filter (/=103) . P.filter (/=104) . P.filter (/=101) . P.filter (/=102)         $ fps)])
 ,("big filter/map",
    [F  ({-# SCC "filter/map"#-}P.map (*2) . P.filter (/=104) . P.map (+6) . P.filter (/=103) . P.map (+5)        $ fps)])
 ]

main = do
    force (fps,fps')
    printf "# Size of test data: %dk\n" ((floor $ (fromIntegral (P.length fps)) / 1024) :: Int)
    printf "#Byte\n"
    run (tests ++ bigtests)
