#!/usr/bin/env runhaskell
--
-- You want to compile with, and with -O2, checking the rules are firing.
--
module Main where

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
    ]

prop_upup_list = compare3
     (\f g  -> P.foldl f (0::Int) . P.scanl g (0::W))
     ((\f g ->   foldl f (0::Int) .   scanl g (0::W)) :: (X -> W -> X) -> (W -> W -> W) -> [W] -> X)

prop_upup_lazy = compare3
     (\f g  -> L.foldl f (0::X) . L.scanl g (0::W))
     (\f g  -> P.foldl f (0::X) . P.scanl g (0::W))

prop_mapmap_list = compare3
     (\f g  -> P.map f . P.map g)
     ((\f g ->   map f .   map g) :: (W -> W) -> (W -> W) -> [W] -> [W])

prop_mapmap_lazy = compare3
     (\f g  -> L.map f . L.map g)
     (\f g  -> P.map f . P.map g)

prop_filterfilter_list = compare3
     (\f g  -> P.filter f . P.filter g)
     ((\f g ->   filter f .   filter g) :: (W -> Bool) -> (W -> Bool) -> [W] -> [W])

prop_filterfilter_lazy = compare3
     (\f g  -> L.filter f . L.filter g)
     (\f g  -> P.filter f . P.filter g)

prop_mapfilter_list = compare3
     (\f g  -> P.filter f . P.map g)
     ((\f g ->   filter f .   map g) :: (W -> Bool) -> (W -> W) -> [W] -> [W])

prop_mapfilter_lazy = compare3
     (\f g  -> L.filter f . L.map g)
     (\f g  -> P.filter f . P.map g)

prop_filtermap_list = compare3
     (\f g  -> P.map f . P.filter g)
     ((\f g ->   map f .   filter g) :: (W -> W) -> (W -> Bool) -> [W] -> [W])

prop_filtermap_lazy = compare3
     (\f g  -> L.map f . L.filter g)
     (\f g  -> P.map f . P.filter g)

prop_mapup_list = compare3
     (\f g  -> P.foldl g (0::W) . P.map f)
     ((\f g ->   foldl g (0::W) .   map f) :: (W -> W) -> (W -> W -> W) -> [W] -> W)

prop_mapup_lazy = compare3
     (\f g -> L.foldl g (0::W) . L.map f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldl g (0::W) . P.map f)

prop_upmap_list = compare3
     (\f g  -> P.map f . P.scanl g (0::W))
     ((\f g ->   map f .   scanl g (0::W)) :: (W -> W) -> (W -> W -> W) -> [W] -> [W])

prop_upmap_lazy = compare3
     (\f g -> L.map f . L.scanl g (0::W))
     (\f g -> P.map f . P.scanl g (0::W))

prop_filterup_list = compare3
     (\f g  -> P.foldl g (0::W) . P.filter f)
     ((\f g ->   foldl g (0::W) .   filter f) :: (W -> Bool) -> (W -> W -> W) -> [W] -> W)

prop_filterup_lazy = compare3
     (\f g -> L.foldl g (0::W) . L.filter f) 
     (\f g -> P.foldl g (0::W) . P.filter f)

prop_upfilter_list = compare3
     (\f g  -> P.filter f . P.scanl g (0::W))
     ((\f g ->   filter f .   scanl g (0::W)) :: (W -> Bool) -> (W -> W -> W) -> [W] -> [W])

prop_upfilter_lazy = compare3
     (\f g -> L.filter f . L.scanl g (0::W))
     (\f g -> P.filter f . P.scanl g (0::W))

prop_downdown_list = compare3
     (\f g  -> P.foldr f (0::X) . P.scanr g (0::W))
     ((\f g ->   foldr f (0::X) .   scanr g (0::W)) :: (W -> X -> X) -> (W -> W -> W) -> [W] -> X)

{-
-- no lazy scanr yet
prop_downdown_lazy = compare3
     (\f g  -> L.foldr f (0::X) . L.scanr g (0::W))
     (\f g  -> P.foldr f (0::X) . P.scanr g (0::W))
-}

prop_mapdown_list = compare3
     (\f g  -> P.foldr g (0::W) . P.map f)
     ((\f g ->   foldr g (0::W) .   map f) :: (W -> W) -> (W -> W -> W) -> [W] -> W)

prop_mapdown_lazy = compare3
     (\f g -> L.foldr g (0::W) . L.map f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldr g (0::W) . P.map f)

prop_downmap_list = compare3
     (\f g  -> P.map f . P.scanr g (0::W))
     ((\f g ->   map f .   scanr g (0::W)) :: (W -> W) -> (W -> W -> W) -> [W] -> [W])

{-
prop_downmap_lazy = compare3
     (\f g -> L.map f . L.scanr g (0::W))
     (\f g -> P.map f . P.scanr g (0::W))
-}

prop_filterdown_list = compare3
     (\f g  -> P.foldr g (0::W) . P.filter f)
     ((\f g ->   foldr g (0::W) .   filter f) :: (W -> Bool) -> (W -> W -> W) -> [W] -> W)

prop_filterdown_lazy = compare3
     (\f g -> L.foldr g (0::W) . L.filter f) -- n.b. scan doesn't fuse here, atm
     (\f g -> P.foldr g (0::W) . P.filter f)

prop_downfilter_list = compare3
     (\f g  -> P.filter f . P.scanr g (0::W))
     ((\f g ->   filter f .   scanr g (0::W)) :: (W -> Bool) -> (W -> W -> W) -> [W] -> [W])

{-
prop_downfilter_lazy = compare3
     (\f g -> L.filter f . L.scanr g (0::W))
     (\f g -> P.filter f . P.scanr g (0::W))
-}

------------------------------------------------------------------------
-- no functions written with this yet..

-- noAcc/noAcc
-- noAcc/up
-- up/noAcc
-- filter/noAcc
-- noAcc/filter
-- noAcc/down
-- down/noAcc
