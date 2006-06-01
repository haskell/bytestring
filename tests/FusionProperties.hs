--
-- You want to compile with, and with -O2, checking the rules are firing.
--

import Test.QuickCheck
import QuickCheckUtils
import System.Environment
import Text.Printf

import Data.ByteString.Fusion
import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L

main = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests

--
-- Test that, after loop fusion, our code behaves the same as the
-- unfused lazy or list models. Use -ddump-simpl to also check that
-- rules are firing for each case.
--
tests =                           -- 29/5/06, all tests are fusing:
    [("down/down     list", mytest prop_downdown_list)          -- checked
    ,("down/filter   list", mytest prop_downfilter_list)        -- checked
    ,("down/map      list", mytest prop_downmap_list)           -- checked
    ,("filter/down   lazy", mytest prop_filterdown_lazy)        -- checked
    ,("filter/down   list", mytest prop_filterdown_list)        -- checked
    ,("filter/filter lazy", mytest prop_filterfilter_lazy)      -- checked
    ,("filter/filter list", mytest prop_filterfilter_list)      -- checked
    ,("filter/map    lazy", mytest prop_filtermap_lazy)         -- checked
    ,("filter/map    list", mytest prop_filtermap_list)         -- checked
    ,("filter/up     lazy", mytest prop_filterup_lazy)          -- checked
    ,("filter/up     list", mytest prop_filterup_list)          -- checked
    ,("map/down      lazy", mytest prop_mapdown_lazy)           -- checked
    ,("map/down      list", mytest prop_mapdown_list)           -- checked
    ,("map/filter    lazy", mytest prop_mapfilter_lazy)         -- checked
    ,("map/filter    list", mytest prop_mapfilter_list)         -- checked
    ,("map/map       lazy", mytest prop_mapmap_lazy)            -- checked
    ,("map/map       list", mytest prop_mapmap_list)            -- checked
    ,("map/up        lazy", mytest prop_mapup_lazy)             -- checked
    ,("map/up        list", mytest prop_mapup_list)             -- checked
    ,("up/filter     lazy", mytest prop_upfilter_lazy)          -- checked
    ,("up/filter     list", mytest prop_upfilter_list)          -- checked
    ,("up/map        lazy", mytest prop_upmap_lazy)             -- checked
    ,("up/map        list", mytest prop_upmap_list)             -- checked
    ,("up/up         lazy", mytest prop_upup_lazy)              -- checked
    ,("up/up         list", mytest prop_upup_list)              -- checked
    ,("noacc/noacc   lazy", mytest prop_noacc_noacc_lazy)       -- checked
    ,("noacc/noacc   list", mytest prop_noacc_noacc_list)       -- checked
    ,("noacc/up      lazy", mytest prop_noacc_up_lazy)          -- checked
    ,("noacc/up      list", mytest prop_noacc_up_list)          -- checked
    ,("up/noacc      lazy", mytest prop_up_noacc_lazy)          -- checked
    ,("up/noacc      list", mytest prop_up_noacc_list)          -- checked
    ,("map/noacc     lazy", mytest prop_map_noacc_lazy)         -- checked
    ,("map/noacc     list", mytest prop_map_noacc_list)         -- checked
    ,("noacc/map     lazy", mytest prop_noacc_map_lazy)         -- checked
    ,("noacc/map     list", mytest prop_noacc_map_list)         -- checked
    ,("filter/noacc  lazy", mytest prop_filter_noacc_lazy)      -- checked
    ,("filter/noacc  list", mytest prop_filter_noacc_list)      -- checked
    ,("noacc/filter  lazy", mytest prop_noacc_filter_lazy)      -- checked
    ,("noacc/filter  list", mytest prop_noacc_filter_list)      -- checked
    ,("noacc/down    lazy", mytest prop_noacc_down_lazy)        -- checked
    ,("noacc/down    list", mytest prop_noacc_down_list)        -- checked
--  ,("down/noacc    lazy", mytest prop_down_noacc_lazy)        -- checked
    ,("down/noacc    list", mytest prop_down_noacc_list)        -- checked


    ,("length/loop   list", mytest prop_lengthloop_list)
--  ,("length/loop   lazy", mytest prop_lengthloop_lazy)
    ,("maximum/loop  list", mytest prop_maximumloop_list)
--  ,("maximum/loop  lazy", mytest prop_maximumloop_lazy)
    ,("minimum/loop  list", mytest prop_minimumloop_list)
--  ,("minimum/loop  lazy", mytest prop_minimumloop_lazy)

    ]

prop_upup_list = eq3
     (\f g  -> P.foldl f (0::Int) . P.scanl g (0::W))
     ((\f g ->   foldl f (0::Int) .   scanl g (0::W)) :: (X -> W -> X) -> (W -> W -> W) -> [W] -> X)

prop_upup_lazy = eq3
     (\f g  -> L.foldl f (0::X) . L.scanl g (0::W))
     (\f g  -> P.foldl f (0::X) . P.scanl g (0::W))

prop_mapmap_list = eq3
     (\f g  -> P.map f . P.map g)
     ((\f g ->   map f .   map g) :: (W -> W) -> (W -> W) -> [W] -> [W])

prop_mapmap_lazy = eq3
     (\f g  -> L.map f . L.map g)
     (\f g  -> P.map f . P.map g)

prop_filterfilter_list = eq3
     (\f g  -> P.filter f . P.filter g)
     ((\f g ->   filter f .   filter g) :: (W -> Bool) -> (W -> Bool) -> [W] -> [W])

prop_filterfilter_lazy = eq3
     (\f g  -> L.filter f . L.filter g)
     (\f g  -> P.filter f . P.filter g)

prop_mapfilter_list = eq3
     (\f g  -> P.filter f . P.map g)
     ((\f g ->   filter f .   map g) :: (W -> Bool) -> (W -> W) -> [W] -> [W])

prop_mapfilter_lazy = eq3
     (\f g  -> L.filter f . L.map g)
     (\f g  -> P.filter f . P.map g)

prop_filtermap_list = eq3
     (\f g  -> P.map f . P.filter g)
     ((\f g ->   map f .   filter g) :: (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_filtermap_lazy = eq3
     (\f g  -> L.map f . L.filter g)
     (\f g  -> P.map f . P.filter g)

prop_mapup_list = eq3
     (\f g  -> P.foldl g (0::W) . P.map f)
     ((\f g ->   foldl g (0::W) .   map f) :: (W -> W) -> (W -> W -> W) -> [W] -> W)

prop_mapup_lazy = eq3
     (\f g -> L.foldl g (0::W) . L.map f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldl g (0::W) . P.map f)

prop_upmap_list = eq3
     (\f g  -> P.map f . P.scanl g (0::W))
     ((\f g ->   map f .   scanl g (0::W)) :: (W -> W) -> (W -> W -> W) -> [W] -> [W])

prop_upmap_lazy = eq3
     (\f g -> L.map f . L.scanl g (0::W))
     (\f g -> P.map f . P.scanl g (0::W))

prop_filterup_list = eq3
     (\f g  -> P.foldl g (0::W) . P.filter f)
     ((\f g ->   foldl g (0::W) .   filter f) :: (W -> Bool) -> (W -> W -> W) -> [W] -> W)

prop_filterup_lazy = eq3
     (\f g -> L.foldl g (0::W) . L.filter f)
     (\f g -> P.foldl g (0::W) . P.filter f)

prop_upfilter_list = eq3
     (\f g  -> P.filter f . P.scanl g (0::W))
     ((\f g ->   filter f .   scanl g (0::W)) :: (W -> Bool) -> (W -> W -> W) -> [W] -> [W])

prop_upfilter_lazy = eq3
     (\f g -> L.filter f . L.scanl g (0::W))
     (\f g -> P.filter f . P.scanl g (0::W))

prop_downdown_list = eq3
     (\f g  -> P.foldr f (0::X) . P.scanr g (0::W))
     ((\f g ->   foldr f (0::X) .   scanr g (0::W)) :: (W -> X -> X) -> (W -> W -> W) -> [W] -> X)

{-
-- no lazy scanr yet
prop_downdown_lazy = eq3
     (\f g  -> L.foldr f (0::X) . L.scanr g (0::W))
     (\f g  -> P.foldr f (0::X) . P.scanr g (0::W))
-}

prop_mapdown_list = eq3
     (\f g  -> P.foldr g (0::W) . P.map f)
     ((\f g ->   foldr g (0::W) .   map f) :: (W -> W) -> (W -> W -> W) -> [W] -> W)

prop_mapdown_lazy = eq3
     (\f g -> L.foldr g (0::W) . L.map f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldr g (0::W) . P.map f)

prop_downmap_list = eq3
     (\f g  -> P.map f . P.scanr g (0::W))
     ((\f g ->   map f .   scanr g (0::W)) :: (W -> W) -> (W -> W -> W) -> [W] -> [W])

{-
prop_downmap_lazy = eq3
     (\f g -> L.map f . L.scanr g (0::W))
     (\f g -> P.map f . P.scanr g (0::W))
-}

prop_filterdown_list = eq3
     (\f g  -> P.foldr g (0::W) . P.filter f)
     ((\f g ->   foldr g (0::W) .   filter f) :: (W -> Bool) -> (W -> W -> W) -> [W] -> W)

prop_filterdown_lazy = eq3
     (\f g -> L.foldr g (0::W) . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldr g (0::W) . P.filter f)

prop_downfilter_list = eq3
     (\f g  -> P.filter f . P.scanr g (0::W))
     ((\f g ->   filter f .   scanr g (0::W)) :: (W -> Bool) -> (W -> W -> W) -> [W] -> [W])

{-
prop_downfilter_lazy = eq3
     (\f g -> L.filter f . L.scanr g (0::W))
     (\f g -> P.filter f . P.scanr g (0::W))
-}

prop_noacc_noacc_list = eq5
    (\f g h i -> (P.map f . P.filter g) . (P.map h . P.filter i))
    ((\f g h i -> (  map f .   filter g) . (  map h .   filter i))
        :: (W -> W) -> (W -> Bool) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_noacc_noacc_lazy = eq5
     (\f g h i -> (L.map f . L.filter g) . (L.map h . L.filter i))
     (\f g h i -> (P.map f . P.filter g) . (P.map h . P.filter i))

prop_noacc_up_list = eq4
    ( \g h i -> P.foldl g (0::W) . (P.map h . P.filter i))
    ((\g h i ->   foldl g (0::W) . (  map h .   filter i))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> W)

prop_noacc_up_lazy = eq4
    (\g h i -> L.foldl g (0::W) . (L.map h . L.filter i))
    (\g h i -> P.foldl g (0::W) . (P.map h . P.filter i))

prop_up_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.scanl g (0::W))
    ((\g h i -> (  map h .   filter i) .   scanl g (0::W))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_up_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.scanl g (0::W))
    (\g h i -> (P.map h . P.filter i) . P.scanl g (0::W))

prop_map_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.map g)
    ((\g h i -> (  map h .   filter i) .   map g)
        :: (W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_map_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.map g)
    (\g h i -> (P.map h . P.filter i) . P.map g)

prop_noacc_map_list = eq4
    ( \g h i -> P.map g . (P.map h . P.filter i))
    ((\g h i ->   map g . (  map h .   filter i))
        :: (W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_noacc_map_lazy = eq4
    (\g h i -> L.map g . (L.map h . L.filter i))
    (\g h i -> P.map g . (P.map h . P.filter i))

prop_filter_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.filter g)
    ((\g h i -> (  map h .   filter i) .   filter g)
        :: (W -> Bool) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_filter_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.filter g)
    (\g h i -> (P.map h . P.filter i) . P.filter g)

prop_noacc_filter_list = eq4
    ( \g h i -> P.filter g . (P.map h . P.filter i))
    ((\g h i ->   filter g . (  map h .   filter i))
        :: (W -> Bool) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_noacc_filter_lazy = eq4
    (\g h i -> L.filter g . (L.map h . L.filter i))
    (\g h i -> P.filter g . (P.map h . P.filter i))

prop_noacc_down_list = eq4
    ( \g h i -> P.foldr g (0::W) . (P.map h . P.filter i))
    ((\g h i ->   foldr g (0::W) . (  map h .   filter i))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> W)

prop_noacc_down_lazy = eq4
    (\g h i -> L.foldr g (0::W) . (L.map h . L.filter i))
    (\g h i -> P.foldr g (0::W) . (P.map h . P.filter i))

prop_down_noacc_list = eq4
    ( \g h i -> (P.map h . P.filter i) . P.scanr g (0::W))
    ((\g h i -> (  map h .   filter i) .   scanr g (0::W))
        :: (W -> W -> W) -> (W -> W) -> (W -> Bool) -> [W] -> [W])

{-
prop_down_noacc_lazy = eq4
    (\g h i -> (L.map h . L.filter i) . L.scanl g (0::W))
    (\g h i -> (P.map h . P.filter i) . P.scanl g (0::W))
-}

------------------------------------------------------------------------

prop_lengthloop_list = eq2
     (\f  -> P.length . P.filter f)
     ((\f ->   length .   filter f) :: (W -> Bool) -> [W] -> X)

{-
prop_lengthloop_lazy = eq2
     (\f g -> L.length . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.length . P.filter f)
-}

prop_maximumloop_list = eqnotnull2
     (\f  -> P.maximum . P.map f)   -- so we don't get null strings
     ((\f ->   maximum .   map f) :: (W -> W) -> [W] -> W)

{-
prop_maximumloop_lazy = eq2
     (\f g -> L.maximum . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.maximum . P.filter f)
-}

prop_minimumloop_list = eqnotnull2
     (\f  -> P.minimum . P.map f)
     ((\f ->   minimum .   map f) :: (W -> W) -> [W] -> W)

{-
prop_minimumloop_lazy = eq2
     (\f g -> L.minimum . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.minimum . P.filter f)
-}

