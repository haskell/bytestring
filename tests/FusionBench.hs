{-# OPTIONS -cpp #-}
--
-- Test the results of fusion
--

import BenchUtils
import Text.Printf
import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L

--
-- loopU fusoin only supported:
--      map, foldl, mapAccumL, scanl, filter
--
--

-- minimum pipelines to trigger the various fusion forms
tests =
 [("map/map",         [F  (P.map (*2) . P.map (+4)                                           $ fps)])
 ,("filter/filter",   [F  (P.filter (/=101) . P.filter (/=102)                               $ fps)])
 ,("filter/map",      [F  (P.filter (/=103) . P.map (+5)                                     $ fps)])
 ,("map/filter",      [F  (P.map (*3) . P.filter (/=104)                                     $ fps)])
 ,("up/up",           [F  (P.foldl (const.(+1)) (0::X) . P.scanl (flip const) (0::W)         $ fps)])
 ,("map/up",          [F  (P.foldl (const.(+6)) (0::X) . P.map (*4)                          $ fps)])
 ,("up/map",          [F  (P.map (+7) . P.scanl const (0::W)                                 $ fps)])
 ,("filter/up",       [F  (P.foldl (const.(+8)) (0::X) . P.filter (/=105)                    $ fps)])
 ,("up/filter",       [F  (P.filter (/=106) . P.scanl (flip const) (0::W)                    $ fps)])
 ,("noacc/noacc",     [F  ((P.map (*3) . P.filter (/=108)) . (P.map (*4) . P.filter (/=109)) $ fps)])
 ,("noacc/up",        [F  (P.foldl (const.(+1)) (0::W) . (P.map (+1) . P.filter (/=110))     $ fps)])
 ,("up/noacc",        [F  ((P.map (+1) . P.filter (/=111)) . P.scanl (flip const) (0::W)     $ fps)])
 ,("map/noacc",       [F  ((P.map (+1) . P.filter (/=112)) . P.map (*2)                      $ fps)])
 ,("noacc/map",       [F  (P.map (+1) . (P.map (+2) . P.filter (/=113))                      $ fps)])
 ,("filter/noacc",    [F  ((P.map (+1) . P.filter (/=101)) . P.filter (/=114)                $ fps)])
 ,("noacc/filter",    [F  (P.filter (/=101) . (P.map (*2) . P.filter (/=115))                $ fps)])
#if !defined(LOOPU_FUSION)
 ,("down/down",       [F  (P.foldr (const.(+9))  (0::W) . P.scanr const (0::W)               $ fps)])
 ,("map/down",        [F  (P.foldr (const.(+10)) (0::W) . P.map (*2)                         $ fps)])
 ,("filter/down",     [F  (P.foldr (const.(+11)) (0::W) . P.filter (/=106)                   $ fps)])
 ,("down/filter",     [F  (P.filter (/=107) . P.scanr const (0::W)                           $ fps)])
 ,("noacc/down",      [F  (P.foldr (const.(+1)) (0::W) . (P.map (+1) . P.filter (/=116))     $ fps)])
 ,("down/noacc",      [F  ((P.map (+1) . P.filter (/=101)) . P.scanr const (0::W)            $ fps)])
 ,("length/loop",     [F  (P.length  . P.filter (/=105)                                      $ fps)])
 ,("maximum/loop",    [F  (P.maximum . P.map (*4)                                            $ fps)])
 ,("minimum/loop",    [F  (P.minimum . P.map (+6)                                            $ fps)])
#endif
 ]

-- and some longer ones to see the full effect
bigtests =
 [("big map/map",
    [F  (P.map (subtract 3). P.map (+7) . P.map (*2) . P.map (+4)                          $ fps)])
 ,("big filter/filter",
    [F  (P.filter (/=103) . P.filter (/=104) . P.filter (/=101) . P.filter (/=102)         $ fps)])
 ,("big filter/map",
    [F  (P.map (*2) . P.filter (/=104) . P.map (+6) . P.filter (/=103) . P.map (+5)        $ fps)])
 ]

main = do
    force (fps,fps') >> force (lps,lps')
    printf "# Size of test data: %dk\n" ((floor $ (fromIntegral (P.length fps)) / 1024) :: Int)
    printf "#Byte\n"
    run (tests ++ bigtests)
