--
-- Must have rules off, otherwise the fusion rules will replace the rhs
-- with the lhs, and we only end up testing lhs == lhs
--

--
-- -fhpc interferes with rewrite rules firing.
--

import Foreign
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import GHC.Ptr
import Test.QuickCheck
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Directory

import Data.List
import Data.Char
import Data.Word
import Data.Maybe
import Data.Int (Int64)
import Data.Monoid

import Text.Printf
import Debug.Trace
import Data.String

import System.Environment
import System.IO
import System.IO.Unsafe
import System.Random

import Foreign.Ptr

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..))

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
import qualified Data.ByteString.Char8      as C

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.Char8 as D

import qualified Data.ByteString.Lazy.Internal as LP
import Data.ByteString.Fusion
import Prelude hiding (abs)

import QuickCheckUtils

f = C.dropWhile isSpace

--
-- ByteString.Lazy.Char8 <=> ByteString.Char8
--

prop_concatCC       = D.concat      `eq1`  C.concat
prop_nullCC         = D.null        `eq1`  C.null
prop_reverseCC      = D.reverse     `eq1`  C.reverse
prop_transposeCC    = D.transpose   `eq1`  C.transpose
prop_groupCC        = D.group       `eq1`  C.group
prop_initsCC        = D.inits       `eq1`  C.inits
prop_tailsCC        = D.tails       `eq1`  C.tails
prop_allCC          = D.all         `eq2`  C.all
prop_anyCC          = D.any         `eq2`  C.any
prop_appendCC       = D.append      `eq2`  C.append
prop_breakCC        = D.break       `eq2`  C.break
prop_concatMapCC    = D.concatMap   `eq2`  C.concatMap
prop_consCC         = D.cons        `eq2`  C.cons
prop_unconsCC       = D.uncons      `eq1`  C.uncons
prop_countCC        = D.count       `eq2`  C.count
prop_dropCC         = D.drop        `eq2`  C.drop
prop_dropWhileCC    = D.dropWhile   `eq2`  C.dropWhile
prop_filterCC       = D.filter      `eq2`  C.filter
prop_findCC         = D.find        `eq2`  C.find
prop_findIndexCC    = D.findIndex   `eq2`  C.findIndex
prop_findIndicesCC  = D.findIndices `eq2`  C.findIndices
prop_isPrefixOfCC   = D.isPrefixOf  `eq2`  C.isPrefixOf
prop_mapCC          = D.map         `eq2`  C.map
prop_replicateCC    = D.replicate   `eq2`  C.replicate
prop_snocCC         = D.snoc        `eq2`  C.snoc
prop_spanCC         = D.span        `eq2`  C.span
prop_splitCC        = D.split       `eq2`  C.split
prop_splitAtCC      = D.splitAt     `eq2`  C.splitAt
prop_takeCC         = D.take        `eq2`  C.take
prop_takeWhileCC    = D.takeWhile   `eq2`  C.takeWhile
prop_elemCC         = D.elem        `eq2`  C.elem
prop_notElemCC      = D.notElem     `eq2`  C.notElem
prop_elemIndexCC    = D.elemIndex   `eq2`  C.elemIndex
prop_elemIndicesCC  = D.elemIndices `eq2`  C.elemIndices
prop_lengthCC       = D.length      `eq1`  (fromIntegral . C.length :: C.ByteString -> Int64)

prop_headCC         = D.head        `eqnotnull1` C.head
prop_initCC         = D.init        `eqnotnull1` C.init
prop_lastCC         = D.last        `eqnotnull1` C.last
prop_maximumCC      = D.maximum     `eqnotnull1` C.maximum
prop_minimumCC      = D.minimum     `eqnotnull1` C.minimum
prop_tailCC         = D.tail        `eqnotnull1` C.tail
prop_foldl1CC       = D.foldl1      `eqnotnull2` C.foldl1
prop_foldl1CC'      = D.foldl1'     `eqnotnull2` C.foldl1'
prop_foldr1CC       = D.foldr1      `eqnotnull2` C.foldr1
prop_foldr1CC'      = D.foldr1      `eqnotnull2` C.foldr1'
prop_scanlCC        = D.scanl       `eqnotnull3` C.scanl

prop_intersperseCC = D.intersperse  `eq2` C.intersperse

prop_foldlCC     = eq3
    (D.foldl     :: (X -> Char -> X) -> X -> B -> X)
    (C.foldl     :: (X -> Char -> X) -> X -> P -> X)
prop_foldlCC'    = eq3
    (D.foldl'    :: (X -> Char -> X) -> X -> B -> X)
    (C.foldl'    :: (X -> Char -> X) -> X -> P -> X)
prop_foldrCC     = eq3
    (D.foldr     :: (Char -> X -> X) -> X -> B -> X)
    (C.foldr     :: (Char -> X -> X) -> X -> P -> X)
prop_foldrCC'    = eq3
    (D.foldr     :: (Char -> X -> X) -> X -> B -> X)
    (C.foldr'    :: (Char -> X -> X) -> X -> P -> X)
prop_mapAccumLCC = eq3
    (D.mapAccumL :: (X -> Char -> (X,Char)) -> X -> B -> (X, B))
    (C.mapAccumL :: (X -> Char -> (X,Char)) -> X -> P -> (X, P))

prop_mapIndexedCC = D.mapIndexed `eq2` C.mapIndexed

--
-- ByteString.Lazy <=> ByteString
--

prop_concatBP       = L.concat      `eq1`  P.concat
prop_nullBP         = L.null        `eq1`  P.null
prop_reverseBP      = L.reverse     `eq1`  P.reverse

prop_transposeBP    = L.transpose   `eq1`  P.transpose
prop_groupBP        = L.group       `eq1`  P.group
prop_initsBP        = L.inits       `eq1`  P.inits
prop_tailsBP        = L.tails       `eq1`  P.tails
prop_allBP          = L.all         `eq2`  P.all
prop_anyBP          = L.any         `eq2`  P.any
prop_appendBP       = L.append      `eq2`  P.append
prop_breakBP        = L.break       `eq2`  P.break
prop_concatMapBP    = L.concatMap   `eq2`  P.concatMap
prop_consBP         = L.cons        `eq2`  P.cons
prop_consBP'        = L.cons'       `eq2`  P.cons
prop_consLP'        = LC.cons'      `eq2`  P.cons
prop_unconsBP       = L.uncons      `eq1`  P.uncons
prop_countBP        = L.count       `eq2`  P.count
prop_dropBP         = L.drop        `eq2`  P.drop
prop_dropWhileBP    = L.dropWhile   `eq2`  P.dropWhile
prop_filterBP       = L.filter      `eq2`  P.filter
prop_findBP         = L.find        `eq2`  P.find
prop_findIndexBP    = L.findIndex   `eq2`  P.findIndex
prop_findIndicesBP  = L.findIndices `eq2`  P.findIndices
prop_isPrefixOfBP   = L.isPrefixOf  `eq2`  P.isPrefixOf
prop_mapBP          = L.map         `eq2`  P.map
prop_replicateBP    = L.replicate   `eq2`  P.replicate
prop_snocBP         = L.snoc        `eq2`  P.snoc
prop_spanBP         = L.span        `eq2`  P.span
prop_splitBP        = L.split       `eq2`  P.split
prop_splitAtBP      = L.splitAt     `eq2`  P.splitAt
prop_takeBP         = L.take        `eq2`  P.take
prop_takeWhileBP    = L.takeWhile   `eq2`  P.takeWhile
prop_elemBP         = L.elem        `eq2`  P.elem
prop_notElemBP      = L.notElem     `eq2`  P.notElem
prop_elemIndexBP    = L.elemIndex   `eq2`  P.elemIndex
prop_elemIndicesBP  = L.elemIndices `eq2`  P.elemIndices
prop_intersperseBP  = L.intersperse  `eq2` P.intersperse
prop_lengthBP       = L.length      `eq1`  (fromIntegral . P.length :: P.ByteString -> Int64)
prop_readIntBP      = D.readInt     `eq1`  C.readInt
prop_linesBP        = D.lines       `eq1`  C.lines

-- double check:
-- Currently there's a bug in the lazy bytestring version of lines, this
-- catches it:
prop_linesNLBP      = eq1 D.lines C.lines x
    where x = D.pack "one\ntwo\n\n\nfive\n\nseven\n"

prop_headBP         = L.head        `eqnotnull1` P.head
prop_initBP         = L.init        `eqnotnull1` P.init
prop_lastBP         = L.last        `eqnotnull1` P.last
prop_maximumBP      = L.maximum     `eqnotnull1` P.maximum
prop_minimumBP      = L.minimum     `eqnotnull1` P.minimum
prop_tailBP         = L.tail        `eqnotnull1` P.tail
prop_foldl1BP       = L.foldl1      `eqnotnull2` P.foldl1
prop_foldl1BP'      = L.foldl1'     `eqnotnull2` P.foldl1'
prop_foldr1BP       = L.foldr1      `eqnotnull2` P.foldr1
prop_foldr1BP'      = L.foldr1      `eqnotnull2` P.foldr1'
prop_scanlBP        = L.scanl       `eqnotnull3` P.scanl


prop_eqBP        = eq2
    ((==) :: B -> B -> Bool)
    ((==) :: P -> P -> Bool)
prop_compareBP   = eq2
    ((compare) :: B -> B -> Ordering)
    ((compare) :: P -> P -> Ordering)
prop_foldlBP     = eq3
    (L.foldl     :: (X -> W -> X) -> X -> B -> X)
    (P.foldl     :: (X -> W -> X) -> X -> P -> X)
prop_foldlBP'    = eq3
    (L.foldl'    :: (X -> W -> X) -> X -> B -> X)
    (P.foldl'    :: (X -> W -> X) -> X -> P -> X)
prop_foldrBP     = eq3
    (L.foldr     :: (W -> X -> X) -> X -> B -> X)
    (P.foldr     :: (W -> X -> X) -> X -> P -> X)
prop_foldrBP'    = eq3
    (L.foldr     :: (W -> X -> X) -> X -> B -> X)
    (P.foldr'    :: (W -> X -> X) -> X -> P -> X)
prop_mapAccumLBP = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B -> (X, B))
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))

prop_unfoldrBP   = eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                     fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)

prop_unfoldr2BP   = eq2
    ((\n a -> P.take (n*100) $
        P.unfoldr    (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)
    ((\n a ->                     fst $
        P.unfoldrN (n*100) (\x -> if x <= (n*100) then Just (fromIntegral x, x + 1) else Nothing) a)
                :: Int -> Int -> P)


prop_unfoldrLC   = eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.unfoldr    f a) :: Int -> (X -> Maybe (Char,X)) -> X -> B)
    ((\n f a ->                     fst $
        C.unfoldrN n f a) :: Int -> (X -> Maybe (Char,X)) -> X -> P)


prop_iterateLC   = eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.iterate  f a) :: Int -> (Char -> Char) -> Char -> B)
    ((\n f a -> fst $
        C.unfoldrN n (\a -> Just (f a, f a)) a) :: Int -> (Char -> Char) -> Char -> P)

prop_iterateLC_2   = eq3
    ((\n f a -> LC.take (fromIntegral n) $
        LC.iterate  f a) :: Int -> (Char -> Char) -> Char -> B)
    ((\n f a -> LC.take (fromIntegral n) $
        LC.unfoldr (\a -> Just (f a, f a)) a) :: Int -> (Char -> Char) -> Char -> B)

prop_iterateL   = eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.iterate  f a) :: Int -> (W -> W) -> W -> B)
    ((\n f a -> fst $
        P.unfoldrN n (\a -> Just (f a, f a)) a) :: Int -> (W -> W) -> W -> P)

prop_repeatLC   = eq2
    ((\n a -> LC.take (fromIntegral n) $
        LC.repeat a) :: Int -> Char -> B)
    ((\n a -> fst $
        C.unfoldrN n (\a -> Just (a, a)) a) :: Int -> Char -> P)

prop_repeatL   = eq2
    ((\n a -> L.take (fromIntegral n) $
        L.repeat a) :: Int -> W -> B)
    ((\n a -> fst $
        P.unfoldrN n (\a -> Just (a, a)) a) :: Int -> W -> P)

--
-- properties comparing ByteString.Lazy `eq1` List
--

prop_concatBL       = L.concat      `eq1` (concat    :: [[W]] -> [W])
prop_lengthBL       = L.length      `eq1` (length    :: [W] -> Int)
prop_nullBL         = L.null        `eq1` (null      :: [W] -> Bool)
prop_reverseBL      = L.reverse     `eq1` (reverse   :: [W] -> [W])
prop_transposeBL    = L.transpose   `eq1` (transpose :: [[W]] -> [[W]])
prop_groupBL        = L.group       `eq1` (group     :: [W] -> [[W]])
prop_initsBL        = L.inits       `eq1` (inits     :: [W] -> [[W]])
prop_tailsBL        = L.tails       `eq1` (tails     :: [W] -> [[W]])
prop_allBL          = L.all         `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyBL          = L.any         `eq2` (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendBL       = L.append      `eq2` ((++)      :: [W] -> [W] -> [W])
prop_breakBL        = L.break       `eq2` (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatMapBL    = L.concatMap   `eq2` (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consBL         = L.cons        `eq2` ((:)       :: W -> [W] -> [W])
prop_dropBL         = L.drop        `eq2` (drop      :: Int -> [W] -> [W])
prop_dropWhileBL    = L.dropWhile   `eq2` (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterBL       = L.filter      `eq2` (filter    :: (W -> Bool ) -> [W] -> [W])
prop_findBL         = L.find        `eq2` (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndicesBL  = L.findIndices `eq2` (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_findIndexBL    = L.findIndex   `eq2` (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfBL   = L.isPrefixOf  `eq2` (isPrefixOf:: [W] -> [W] -> Bool)
prop_mapBL          = L.map         `eq2` (map       :: (W -> W) -> [W] -> [W])
prop_replicateBL    = L.replicate   `eq2` (replicate :: Int -> W -> [W])
prop_snocBL         = L.snoc        `eq2` ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanBL         = L.span        `eq2` (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtBL      = L.splitAt     `eq2` (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takeBL         = L.take        `eq2` (take      :: Int -> [W] -> [W])
prop_takeWhileBL    = L.takeWhile   `eq2` (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemBL         = L.elem        `eq2` (elem      :: W -> [W] -> Bool)
prop_notElemBL      = L.notElem     `eq2` (notElem   :: W -> [W] -> Bool)
prop_elemIndexBL    = L.elemIndex   `eq2` (elemIndex :: W -> [W] -> Maybe Int)
prop_elemIndicesBL  = L.elemIndices `eq2` (elemIndices:: W -> [W] -> [Int])
prop_linesBL        = D.lines       `eq1` (lines     :: String -> [String])

prop_foldl1BL       = L.foldl1  `eqnotnull2` (foldl1    :: (W -> W -> W) -> [W] -> W)
prop_foldl1BL'      = L.foldl1' `eqnotnull2` (foldl1'   :: (W -> W -> W) -> [W] -> W)
prop_foldr1BL       = L.foldr1  `eqnotnull2` (foldr1    :: (W -> W -> W) -> [W] -> W)
prop_headBL         = L.head    `eqnotnull1` (head      :: [W] -> W)
prop_initBL         = L.init    `eqnotnull1` (init      :: [W] -> [W])
prop_lastBL         = L.last    `eqnotnull1` (last      :: [W] -> W)
prop_maximumBL      = L.maximum `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumBL      = L.minimum `eqnotnull1` (minimum   :: [W] -> W)
prop_tailBL         = L.tail    `eqnotnull1` (tail      :: [W] -> [W])

prop_eqBL         = eq2
    ((==) :: B   -> B   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_compareBL    = eq2
    ((compare) :: B   -> B   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlBL      = eq3
    (L.foldl  :: (X -> W -> X) -> X -> B   -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W] -> X)
prop_foldlBL'     = eq3
    (L.foldl' :: (X -> W -> X) -> X -> B   -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W] -> X)
prop_foldrBL      = eq3
    (L.foldr  :: (W -> X -> X) -> X -> B   -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W] -> X)
prop_mapAccumLBL  = eq3
    (L.mapAccumL :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

prop_mapAccumRBL  = eq3
    (L.mapAccumR :: (X -> W -> (X,W)) -> X -> B   -> (X, B))
    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))

prop_mapAccumRCC  = eq3
    (C.mapAccumR :: (X -> Char -> (X,Char)) -> X -> P   -> (X, P))
    (  mapAccumR :: (X -> Char -> (X,Char)) -> X -> [Char] -> (X, [Char]))

prop_unfoldrBL = eq3
    ((\n f a -> L.take (fromIntegral n) $
        L.unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> B)
    ((\n f a ->                  take n $
          unfoldr f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

--
-- And finally, check correspondance between Data.ByteString and List
--

prop_lengthPL     = (fromIntegral.P.length :: P -> Int) `eq1` (length :: [W] -> Int)
prop_nullPL       = P.null      `eq1` (null      :: [W] -> Bool)
prop_reversePL    = P.reverse   `eq1` (reverse   :: [W] -> [W])
prop_transposePL  = P.transpose `eq1` (transpose :: [[W]] -> [[W]])
prop_groupPL      = P.group     `eq1` (group     :: [W] -> [[W]])
prop_initsPL      = P.inits     `eq1` (inits     :: [W] -> [[W]])
prop_tailsPL      = P.tails     `eq1` (tails     :: [W] -> [[W]])
prop_concatPL     = P.concat    `eq1` (concat    :: [[W]] -> [W])
prop_allPL        = P.all       `eq2` (all       :: (W -> Bool) -> [W] -> Bool)
prop_anyPL        = P.any       `eq2`    (any       :: (W -> Bool) -> [W] -> Bool)
prop_appendPL     = P.append    `eq2`    ((++)      :: [W] -> [W] -> [W])
prop_breakPL      = P.break     `eq2`    (break     :: (W -> Bool) -> [W] -> ([W],[W]))
prop_concatMapPL  = P.concatMap `eq2`    (concatMap :: (W -> [W]) -> [W] -> [W])
prop_consPL       = P.cons      `eq2`    ((:)       :: W -> [W] -> [W])
prop_dropPL       = P.drop      `eq2`    (drop      :: Int -> [W] -> [W])
prop_dropWhilePL  = P.dropWhile `eq2`    (dropWhile :: (W -> Bool) -> [W] -> [W])
prop_filterPL     = P.filter    `eq2`    (filter    :: (W -> Bool ) -> [W] -> [W])
prop_filterPL_rule= (\x -> P.filter ((==) x))  `eq2` -- test rules
                    ((\x -> filter ((==) x)) :: W -> [W] -> [W])

-- under lambda doesn't fire?
prop_filterLC_rule= (f)  `eq2` -- test rules
                    ((\x -> filter ((==) x)) :: Char -> [Char] -> [Char])
    where
         f x s = LC.filter ((==) x) s

prop_partitionPL  = P.partition `eq2`    (partition :: (W -> Bool ) -> [W] -> ([W],[W]))
prop_partitionLL  = L.partition `eq2`    (partition :: (W -> Bool ) -> [W] -> ([W],[W]))
prop_findPL       = P.find      `eq2`    (find      :: (W -> Bool) -> [W] -> Maybe W)
prop_findIndexPL  = P.findIndex `eq2`    (findIndex :: (W -> Bool) -> [W] -> Maybe Int)
prop_isPrefixOfPL = P.isPrefixOf`eq2`    (isPrefixOf:: [W] -> [W] -> Bool)
prop_isInfixOfPL = P.isInfixOf`eq2`       (isInfixOf:: [W] -> [W] -> Bool)
prop_mapPL        = P.map       `eq2`    (map       :: (W -> W) -> [W] -> [W])
prop_replicatePL  = P.replicate `eq2`    (replicate :: Int -> W -> [W])
prop_snocPL       = P.snoc      `eq2`    ((\xs x -> xs ++ [x]) :: [W] -> W -> [W])
prop_spanPL       = P.span      `eq2`    (span      :: (W -> Bool) -> [W] -> ([W],[W]))
prop_splitAtPL    = P.splitAt   `eq2`    (splitAt   :: Int -> [W] -> ([W],[W]))
prop_takePL       = P.take      `eq2`    (take      :: Int -> [W] -> [W])
prop_takeWhilePL  = P.takeWhile `eq2`    (takeWhile :: (W -> Bool) -> [W] -> [W])
prop_elemPL       = P.elem      `eq2`    (elem      :: W -> [W] -> Bool)
prop_notElemPL    = P.notElem   `eq2`    (notElem   :: W -> [W] -> Bool)
prop_elemIndexPL  = P.elemIndex `eq2`    (elemIndex :: W -> [W] -> Maybe Int)
prop_linesPL      = C.lines     `eq1`    (lines     :: String -> [String])
prop_findIndicesPL= P.findIndices`eq2`   (findIndices:: (W -> Bool) -> [W] -> [Int])
prop_elemIndicesPL= P.elemIndices`eq2`   (elemIndices:: W -> [W] -> [Int])
prop_zipPL        = P.zip        `eq2`   (zip :: [W] -> [W] -> [(W,W)])
prop_zipCL        = C.zip        `eq2`   (zip :: [Char] -> [Char] -> [(Char,Char)])
prop_zipLL        = L.zip        `eq2`   (zip :: [W] -> [W] -> [(W,W)])
prop_unzipPL      = P.unzip      `eq1`   (unzip :: [(W,W)] -> ([W],[W]))
prop_unzipLL      = L.unzip      `eq1`   (unzip :: [(W,W)] -> ([W],[W]))
prop_unzipCL      = C.unzip      `eq1`   (unzip :: [(Char,Char)] -> ([Char],[Char]))

prop_foldl1PL     = P.foldl1    `eqnotnull2` (foldl1   :: (W -> W -> W) -> [W] -> W)
prop_foldl1PL'    = P.foldl1'   `eqnotnull2` (foldl1' :: (W -> W -> W) -> [W] -> W)
prop_foldr1PL     = P.foldr1    `eqnotnull2` (foldr1 :: (W -> W -> W) -> [W] -> W)
prop_scanlPL      = P.scanl     `eqnotnull3` (scanl  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanl1PL     = P.scanl1    `eqnotnull2` (scanl1 :: (W -> W -> W) -> [W] -> [W])
prop_scanrPL      = P.scanr     `eqnotnull3` (scanr  :: (W -> W -> W) -> W -> [W] -> [W])
prop_scanr1PL     = P.scanr1    `eqnotnull2` (scanr1 :: (W -> W -> W) -> [W] -> [W])
prop_headPL       = P.head      `eqnotnull1` (head      :: [W] -> W)
prop_initPL       = P.init      `eqnotnull1` (init      :: [W] -> [W])
prop_lastPL       = P.last      `eqnotnull1` (last      :: [W] -> W)
prop_maximumPL    = P.maximum   `eqnotnull1` (maximum   :: [W] -> W)
prop_minimumPL    = P.minimum   `eqnotnull1` (minimum   :: [W] -> W)
prop_tailPL       = P.tail      `eqnotnull1` (tail      :: [W] -> [W])

-- prop_zipWithPL'   = P.zipWith'  `eq3` (zipWith :: (W -> W -> W) -> [W] -> [W] -> [W])

prop_zipWithPL    = (P.zipWith  :: (W -> W -> X) -> P   -> P   -> [X]) `eq3`
                      (zipWith  :: (W -> W -> X) -> [W] -> [W] -> [X])

prop_zipWithPL_rules   = (P.zipWith  :: (W -> W -> W) -> P -> P -> [W]) `eq3`
                         (zipWith    :: (W -> W -> W) -> [W] -> [W] -> [W])

prop_eqPL      = eq2
    ((==) :: P   -> P   -> Bool)
    ((==) :: [W] -> [W] -> Bool)
prop_comparePL = eq2
    ((compare) :: P   -> P   -> Ordering)
    ((compare) :: [W] -> [W] -> Ordering)
prop_foldlPL   = eq3
    (P.foldl  :: (X -> W -> X) -> X -> P        -> X)
    (  foldl  :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldlPL'  = eq3
    (P.foldl' :: (X -> W -> X) -> X -> P        -> X)
    (  foldl' :: (X -> W -> X) -> X -> [W]      -> X)
prop_foldrPL   = eq3
    (P.foldr  :: (W -> X -> X) -> X -> P        -> X)
    (  foldr  :: (W -> X -> X) -> X -> [W]      -> X)
prop_mapAccumLPL= eq3
    (P.mapAccumL :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumL :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_mapAccumRPL= eq3
    (P.mapAccumR :: (X -> W -> (X,W)) -> X -> P -> (X, P))
    (  mapAccumR :: (X -> W -> (X,W)) -> X -> [W] -> (X, [W]))
prop_unfoldrPL = eq3
    ((\n f a ->      fst $
        P.unfoldrN n f a) :: Int -> (X -> Maybe (W,X)) -> X -> P)
    ((\n f a ->   take n $
          unfoldr    f a) :: Int -> (X -> Maybe (W,X)) -> X -> [W])

------------------------------------------------------------------------
--
-- These are miscellaneous tests left over. Or else they test some
-- property internal to a type (i.e. head . sort == minimum), without
-- reference to a model type.
--

invariant :: L.ByteString -> Bool
invariant Empty       = True
invariant (Chunk c cs) = not (P.null c) && invariant cs

prop_invariant = invariant

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

prop_compare7LL x  y  = x  `compare` y  == (LC.singleton x `compare` LC.singleton y)

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
prop_concat3 xss = L.concat (map pack xss) == pack (concat xss)

prop_concatMap xs = L.concatMap L.singleton xs == (pack . concatMap (:[]) . unpack) xs

prop_any xs a = (any (== a) xs) == (L.any (== a) (pack xs))
prop_all xs a = (all (== a) xs) == (L.all (== a) (pack xs))

prop_maximum xs = (not (null xs)) ==> (maximum xs) == (L.maximum ( pack xs ))
prop_minimum xs = (not (null xs)) ==> (minimum xs) == (L.minimum ( pack xs ))

prop_replicate1 n c =
    (n >= 0) ==> unpack (L.replicate (fromIntegral n) c) == replicate n c

prop_replicate2 c = unpack (L.replicate 0 c) == replicate 0 c

prop_take1 i xs = L.take (fromIntegral i) (pack xs) == pack (take i xs)
prop_drop1 i xs = L.drop (fromIntegral i) (pack xs) == pack (drop i xs)

prop_splitAt i xs = collect (i >= 0 && i < length xs) $
    L.splitAt (fromIntegral i) (pack xs) == let (a,b) = splitAt i xs in (pack a, pack b)

prop_takeWhile f xs = L.takeWhile f (pack xs) == pack (takeWhile f xs)
prop_dropWhile f xs = L.dropWhile f (pack xs) == pack (dropWhile f xs)

prop_break f xs = L.break f (pack xs) ==
    let (a,b) = break f xs in (pack a, pack b)

prop_breakspan xs c = L.break (==c) xs == L.span (/=c) xs

prop_span xs a = (span (/=a) xs) == (let (x,y) = L.span (/=a) (pack xs) in (unpack x, unpack y))

-- prop_breakByte xs c = L.break (== c) xs == L.breakByte c xs

-- prop_spanByte c xs = (L.span (==c) xs) == L.spanByte c xs

prop_split c xs = (map L.unpack . map checkInvariant . L.split c $ xs)
               == (map P.unpack . P.split c . P.pack . L.unpack $ xs)

prop_splitWith f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map L.length splits) == L.length xs - l2
  where splits = L.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = L.length (L.filter f xs)

prop_splitWith_D f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map D.length splits) == D.length xs - l2
  where splits = D.splitWith f xs
        l1 = fromIntegral (length splits)
        l2 = D.length (D.filter f xs)

prop_joinsplit c xs = L.intercalate (pack [c]) (L.split c xs) == id xs

prop_group xs       = group xs == (map unpack . L.group . pack) xs
prop_groupBy  f xs  = groupBy f xs == (map unpack . L.groupBy f . pack) xs
prop_groupBy_LC  f xs  = groupBy f xs == (map LC.unpack . LC.groupBy f .  LC.pack) xs

-- prop_joinjoinByte xs ys c = L.joinWithByte c xs ys == L.join (L.singleton c) [xs,ys]

prop_index xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == L.pack xs `L.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

prop_index_D xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == D.pack xs `D.index` (fromIntegral i)
  where indices = choose (0, length xs -1)

prop_elemIndex xs c = (elemIndex c xs) == fmap fromIntegral (L.elemIndex c (pack xs))
prop_elemIndexCL xs c = (elemIndex c xs) == (C.elemIndex c (C.pack xs))

prop_elemIndices xs c = elemIndices c xs == map fromIntegral (L.elemIndices c (pack xs))

prop_count c xs = length (L.elemIndices c xs) == fromIntegral (L.count c xs)

prop_findIndex xs f = (findIndex f xs) == fmap fromIntegral (L.findIndex f (pack xs))
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

------------------------------------------------------------------------
-- Misc ByteString properties

prop_nil1BB = P.length P.empty == 0
prop_nil2BB = P.unpack P.empty == []
prop_nil1BB_monoid = P.length mempty == 0
prop_nil2BB_monoid = P.unpack mempty == []

prop_nil1LL_monoid = L.length mempty == 0
prop_nil2LL_monoid = L.unpack mempty == []

prop_tailSBB xs = not (P.null xs) ==> P.tail xs == P.pack (tail (P.unpack xs))

prop_nullBB xs = null (P.unpack xs) == P.null xs

prop_lengthBB xs = P.length xs == length1 xs
    where
        length1 ys
            | P.null ys = 0
            | otherwise = 1 + length1 (P.tail ys)

prop_lengthSBB xs = length xs == P.length (P.pack xs)

prop_indexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.index` i
  where indices = choose (0, length xs -1)

prop_unsafeIndexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_mapfusionBB f g xs = P.map f (P.map g xs) == P.map (f . g) xs

prop_filterBB f xs = P.filter f (P.pack xs) == P.pack (filter f xs)

prop_filterfusionBB f g xs = P.filter f (P.filter g xs) == P.filter (\c -> f c && g c) xs

prop_elemSBB x xs = P.elem x (P.pack xs) == elem x xs

prop_takeSBB i xs = P.take i (P.pack xs) == P.pack (take i xs)
prop_dropSBB i xs = P.drop i (P.pack xs) == P.pack (drop i xs)

prop_splitAtSBB i xs = -- collect (i >= 0 && i < length xs) $
    P.splitAt i (P.pack xs) ==
    let (a,b) = splitAt i xs in (P.pack a, P.pack b)

prop_foldlBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_scanlfoldlBB f z xs = not (P.null xs) ==> P.last (P.scanl f z xs) == P.foldl f z xs

prop_foldrBB f c xs = P.foldl f c (P.pack xs) == foldl f c xs
  where types = c :: Char

prop_takeWhileSBB f xs = P.takeWhile f (P.pack xs) == P.pack (takeWhile f xs)
prop_dropWhileSBB f xs = P.dropWhile f (P.pack xs) == P.pack (dropWhile f xs)

prop_spanSBB f xs = P.span f (P.pack xs) ==
    let (a,b) = span f xs in (P.pack a, P.pack b)

prop_breakSBB f xs = P.break f (P.pack xs) ==
    let (a,b) = break f xs in (P.pack a, P.pack b)

prop_breakspan_1BB xs c = P.break (== c) xs == P.span (/= c) xs

prop_linesSBB xs = C.lines (C.pack xs) == map C.pack (lines xs)

prop_unlinesSBB xss = C.unlines (map C.pack xss) == C.pack (unlines xss)

prop_wordsSBB xs =
    C.words (C.pack xs) == map C.pack (words xs)

prop_wordsLC xs =
    LC.words (LC.pack xs) == map LC.pack (words xs)

prop_unwordsSBB xss = C.unwords (map C.pack xss) == C.pack (unwords xss)
prop_unwordsSLC xss = LC.unwords (map LC.pack xss) == LC.pack (unwords xss)

prop_splitWithBB f xs = (l1 == l2 || l1 == l2+1) &&
        sum (map P.length splits) == P.length xs - l2
  where splits = P.splitWith f xs
        l1 = length splits
        l2 = P.length (P.filter f xs)

prop_joinsplitBB c xs = P.intercalate (P.pack [c]) (P.split c xs) == xs

prop_intercalatePL c x y =

    P.intercalate (P.singleton c) (x : y : []) ==
 --     intercalate (singleton c) (s1 : s2 : [])

    P.pack (intercalate [c] [P.unpack x,P.unpack y])

-- prop_linessplitBB xs =
--     (not . C.null) xs ==>
--     C.lines' xs == C.split '\n' xs

-- false:
{-
prop_linessplit2BB xs =
   (not . C.null) xs ==>
    C.lines xs == C.split '\n' xs ++ (if C.last xs == '\n' then [C.empty] else [])
-}

prop_splitsplitWithBB c xs = P.split c xs == P.splitWith (== c) xs

prop_bijectionBB  c = (P.w2c . P.c2w) c == id c
prop_bijectionBB' w = (P.c2w . P.w2c) w == id w

prop_packunpackBB  s = (P.unpack . P.pack) s == id s
prop_packunpackBB' s = (P.pack . P.unpack) s == id s

prop_eq1BB xs      = xs            == (P.unpack . P.pack $ xs)
prop_eq2BB xs      = xs == (xs :: P.ByteString)
prop_eq3BB xs ys   = (xs == ys) == (P.unpack xs == P.unpack ys)

prop_compare1BB xs  = (P.pack xs         `compare` P.pack xs) == EQ
prop_compare2BB xs c = (P.pack (xs++[c]) `compare` P.pack xs) == GT
prop_compare3BB xs c = (P.pack xs `compare` P.pack (xs++[c])) == LT

prop_compare4BB xs  = (not (null xs)) ==> (P.pack xs  `compare` P.empty) == GT
prop_compare5BB xs  = (not (null xs)) ==> (P.empty `compare` P.pack xs) == LT
prop_compare6BB xs ys= (not (null ys)) ==> (P.pack (xs++ys)  `compare` P.pack xs) == GT

prop_compare7BB x  y = x `compare` y == (C.singleton x `compare` C.singleton y)
prop_compare8BB xs ys = xs `compare` ys == (P.pack xs `compare` P.pack ys)

prop_consBB  c xs = P.unpack (P.cons c (P.pack xs)) == (c:xs)
prop_cons1BB xs   = 'X' : xs == C.unpack ('X' `C.cons` (C.pack xs))
prop_cons2BB xs c = c : xs == P.unpack (c `P.cons` (P.pack xs))
prop_cons3BB c    = C.unpack (C.singleton c) == (c:[])
prop_cons4BB c    = (c `P.cons` P.empty)  == P.pack (c:[])

prop_snoc1BB xs c = xs ++ [c] == P.unpack ((P.pack xs) `P.snoc` c)

prop_head1BB xs     = (not (null xs)) ==> head  xs  == (P.head . P.pack) xs
prop_head2BB xs    = (not (null xs)) ==> head xs   == (P.unsafeHead . P.pack) xs
prop_head3BB xs    = not (P.null xs) ==> P.head xs == head (P.unpack xs)

prop_tailBB xs     = (not (null xs)) ==> tail xs    == (P.unpack . P.tail . P.pack) xs
prop_tail1BB xs    = (not (null xs)) ==> tail xs    == (P.unpack . P.unsafeTail. P.pack) xs

prop_lastBB xs     = (not (null xs)) ==> last xs    == (P.last . P.pack) xs

prop_initBB xs     =
    (not (null xs)) ==>
    init xs    == (P.unpack . P.init . P.pack) xs

-- prop_null xs = (null xs) ==> null xs == (nullPS (pack xs))

prop_append1BB xs    = (xs ++ xs) == (P.unpack $ P.pack xs `P.append` P.pack xs)
prop_append2BB xs ys = (xs ++ ys) == (P.unpack $ P.pack xs `P.append` P.pack ys)
prop_append3BB xs ys = P.append xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_append1BB_monoid xs    = (xs ++ xs) == (P.unpack $ P.pack xs `mappend` P.pack xs)
prop_append2BB_monoid xs ys = (xs ++ ys) == (P.unpack $ P.pack xs `mappend` P.pack ys)
prop_append3BB_monoid xs ys = mappend xs ys == P.pack (P.unpack xs ++ P.unpack ys)

prop_append1LL_monoid xs    = (xs ++ xs) == (L.unpack $ L.pack xs `mappend` L.pack xs)
prop_append2LL_monoid xs ys = (xs ++ ys) == (L.unpack $ L.pack xs `mappend` L.pack ys)
prop_append3LL_monoid xs ys = mappend xs ys == L.pack (L.unpack xs ++ L.unpack ys)

prop_map1BB f xs   = P.map f (P.pack xs)    == P.pack (map f xs)
prop_map2BB f g xs = P.map f (P.map g xs) == P.map (f . g) xs
prop_map3BB f xs   = map f xs == (P.unpack . P.map f .  P.pack) xs
-- prop_mapBB' f xs   = P.map' f (P.pack xs) == P.pack (map f xs)

prop_filter1BB xs   = (filter (=='X') xs) == (C.unpack $ C.filter (=='X') (C.pack xs))
prop_filter2BB p xs = (filter p xs) == (P.unpack $ P.filter p (P.pack xs))

prop_findBB p xs = find p xs == P.find p (P.pack xs)

prop_find_findIndexBB p xs =
    P.find p xs == case P.findIndex p xs of
                                Just n -> Just (xs `P.unsafeIndex` n)
                                _      -> Nothing

prop_foldl1BB xs a = ((foldl (\x c -> if c == a then x else c:x) [] xs)) ==
                   (P.unpack $ P.foldl (\x c -> if c == a then x else c `P.cons` x) P.empty (P.pack xs)) 
prop_foldl2BB xs = P.foldl (\xs c -> c `P.cons` xs) P.empty (P.pack xs) == P.reverse (P.pack xs)

prop_foldr1BB xs a = ((foldr (\c x -> if c == a then x else c:x) [] xs)) ==
                (P.unpack $ P.foldr (\c x -> if c == a then x else c `P.cons` x)
                    P.empty (P.pack xs))

prop_foldr2BB xs = P.foldr (\c xs -> c `P.cons` xs) P.empty (P.pack xs) == (P.pack xs)

prop_foldl1_1BB xs =
    (not . P.null) xs ==>
    P.foldl1 (\x c -> if c > x then c else x)   xs ==
    P.foldl  (\x c -> if c > x then c else x) 0 xs

prop_foldl1_2BB xs =
    (not . P.null) xs ==>
    P.foldl1 const xs == P.head xs

prop_foldl1_3BB xs =
    (not . P.null) xs ==>
    P.foldl1 (flip const) xs == P.last xs

prop_foldr1_1BB xs =
    (not . P.null) xs ==>
    P.foldr1 (\c x -> if c > x then c else x)   xs ==
    P.foldr  (\c x -> if c > x then c else x) 0 xs

prop_foldr1_2BB xs =
    (not . P.null) xs ==>
    P.foldr1 (flip const) xs == P.last xs

prop_foldr1_3BB xs =
    (not . P.null) xs ==>
    P.foldr1 const xs == P.head xs

prop_takeWhileBB xs a = (takeWhile (/= a) xs) == (P.unpack . (P.takeWhile (/= a)) . P.pack) xs

prop_dropWhileBB xs a = (dropWhile (/= a) xs) == (P.unpack . (P.dropWhile (/= a)) . P.pack) xs

prop_dropWhileCC_isSpace xs =
        (dropWhile isSpace xs) ==
       (C.unpack .  (C.dropWhile isSpace) . C.pack) xs

prop_takeBB xs = (take 10 xs) == (P.unpack . (P.take 10) . P.pack) xs

prop_dropBB xs = (drop 10 xs) == (P.unpack . (P.drop 10) . P.pack) xs

prop_splitAtBB i xs = -- collect (i >= 0 && i < length xs) $
    splitAt i xs ==
    let (x,y) = P.splitAt i (P.pack xs) in (P.unpack x, P.unpack y)

prop_spanBB xs a = (span (/=a) xs) == (let (x,y) = P.span (/=a) (P.pack xs)
                                     in (P.unpack x, P.unpack y))

prop_breakBB xs a = (break (/=a) xs) == (let (x,y) = P.break (/=a) (P.pack xs)
                                       in (P.unpack x, P.unpack y))

prop_reverse1BB xs = (reverse xs) == (P.unpack . P.reverse . P.pack) xs
prop_reverse2BB xs = P.reverse (P.pack xs) == P.pack (reverse xs)
prop_reverse3BB xs = reverse (P.unpack xs) == (P.unpack . P.reverse) xs

prop_elemBB xs a = (a `elem` xs) == (a `P.elem` (P.pack xs))

prop_notElemBB c xs = P.notElem c (P.pack xs) == notElem c xs

-- should try to stress it
prop_concat1BB xs = (concat [xs,xs]) == (P.unpack $ P.concat [P.pack xs, P.pack xs])
prop_concat2BB xs = (concat [xs,[]]) == (P.unpack $ P.concat [P.pack xs, P.pack []])
prop_concatBB xss = P.concat (map P.pack xss) == P.pack (concat xss)

prop_concat1BB_monoid xs = (concat [xs,xs]) == (P.unpack $ mconcat [P.pack xs, P.pack xs])
prop_concat2BB_monoid xs = (concat [xs,[]]) == (P.unpack $ mconcat [P.pack xs, P.pack []])
prop_concatBB_monoid xss = mconcat (map P.pack xss) == P.pack (concat xss)

prop_concat1LL_monoid xs = (concat [xs,xs]) == (L.unpack $ mconcat [L.pack xs, L.pack xs])
prop_concat2LL_monoid xs = (concat [xs,[]]) == (L.unpack $ mconcat [L.pack xs, L.pack []])
prop_concatLL_monoid xss = mconcat (map L.pack xss) == L.pack (concat xss)

prop_concatMapBB xs = C.concatMap C.singleton xs == (C.pack . concatMap (:[]) . C.unpack) xs

prop_anyBB xs a = (any (== a) xs) == (P.any (== a) (P.pack xs))
prop_allBB xs a = (all (== a) xs) == (P.all (== a) (P.pack xs))

prop_linesBB xs = (lines xs) == ((map C.unpack) . C.lines . C.pack) xs

prop_unlinesBB xs = (unlines.lines) xs == (C.unpack. C.unlines . C.lines .C.pack) xs
prop_unlinesLC xs = (unlines.lines) xs == (LC.unpack. LC.unlines .  LC.lines .LC.pack) xs

prop_wordsBB xs =
    (words xs) == ((map C.unpack) . C.words . C.pack) xs
-- prop_wordstokensBB xs = C.words xs == C.tokens isSpace xs

prop_unwordsBB xs =
    (C.pack.unwords.words) xs == (C.unwords . C.words .C.pack) xs

prop_groupBB xs   = group xs == (map P.unpack . P.group . P.pack) xs

prop_groupByBB  xs = groupBy (==) xs == (map P.unpack . P.groupBy (==) . P.pack) xs
prop_groupByCC  xs = groupBy (==) xs == (map C.unpack . C.groupBy (==) . C.pack) xs
prop_groupBy1BB xs = groupBy (/=) xs == (map P.unpack . P.groupBy (/=) . P.pack) xs
prop_groupBy1CC xs = groupBy (/=) xs == (map C.unpack . C.groupBy (/=) . C.pack) xs

prop_joinBB xs ys = (concat . (intersperse ys) . lines) xs ==
               (C.unpack $ C.intercalate (C.pack ys) (C.lines (C.pack xs)))

prop_elemIndex1BB xs   = (elemIndex 'X' xs) == (C.elemIndex 'X' (C.pack xs))
prop_elemIndex2BB xs c = (elemIndex c xs) == (C.elemIndex c (C.pack xs))

-- prop_lineIndices1BB xs = C.elemIndices '\n' xs == C.lineIndices xs

prop_countBB c xs = length (P.elemIndices c xs) == P.count c xs

prop_elemIndexEnd1BB c xs = (P.elemIndexEnd c (P.pack xs)) ==
                           (case P.elemIndex c (P.pack (reverse xs)) of
                                Nothing -> Nothing
                                Just i  -> Just (length xs -1 -i))

prop_elemIndexEnd2BB c xs = (P.elemIndexEnd c (P.pack xs)) ==
                           ((-) (length xs - 1) `fmap` P.elemIndex c (P.pack $ reverse xs))

prop_elemIndicesBB xs c = elemIndices c xs == P.elemIndices c (P.pack xs)

prop_findIndexBB xs a = (findIndex (==a) xs) == (P.findIndex (==a) (P.pack xs))

prop_findIndiciesBB xs c = (findIndices (==c) xs) == (P.findIndices (==c) (P.pack xs))

-- example properties from QuickCheck.Batch
prop_sort1BB xs = sort xs == (P.unpack . P.sort . P.pack) xs
prop_sort2BB xs = (not (null xs)) ==> (P.head . P.sort . P.pack $ xs) == minimum xs
prop_sort3BB xs = (not (null xs)) ==> (P.last . P.sort . P.pack $ xs) == maximum xs
prop_sort4BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.head . P.sort) (P.append (P.pack xs) (P.pack ys)) == min (minimum xs) (minimum ys)
prop_sort5BB xs ys =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (P.last . P.sort) (P.append (P.pack xs) (P.pack ys)) == max (maximum xs) (maximum ys)

prop_intersperseBB c xs = (intersperse c xs) == (P.unpack $ P.intersperse c (P.pack xs))

-- prop_transposeBB xs = (transpose xs) == ((map P.unpack) . P.transpose .  (map P.pack)) xs

prop_maximumBB xs = (not (null xs)) ==> (maximum xs) == (P.maximum ( P.pack xs ))
prop_minimumBB xs = (not (null xs)) ==> (minimum xs) == (P.minimum ( P.pack xs ))

-- prop_dropSpaceBB xs    = dropWhile isSpace xs == C.unpack (C.dropSpace (C.pack xs))
-- prop_dropSpaceEndBB xs = (C.reverse . (C.dropWhile isSpace) . C.reverse) (C.pack xs) ==
--                        (C.dropSpaceEnd (C.pack xs))

-- prop_breakSpaceBB xs =
--     (let (x,y) = C.breakSpace (C.pack xs)
--      in (C.unpack x, C.unpack y)) == (break isSpace xs)

prop_spanEndBB xs =
        (C.spanEnd (not . isSpace) (C.pack xs)) ==
        (let (x,y) = C.span (not.isSpace) (C.reverse (C.pack xs)) in (C.reverse y,C.reverse x))

prop_breakEndBB p xs = P.breakEnd (not.p) xs == P.spanEnd p xs
prop_breakEndCC p xs = C.breakEnd (not.p) xs == C.spanEnd p xs

{-
prop_breakCharBB c xs =
        (break (==c) xs) ==
        (let (x,y) = C.breakChar c (C.pack xs) in (C.unpack x, C.unpack y))

prop_spanCharBB c xs =
        (break (/=c) xs) ==
        (let (x,y) = C.spanChar c (C.pack xs) in (C.unpack x, C.unpack y))

prop_spanChar_1BB c xs =
        (C.span (==c) xs) == C.spanChar c xs

prop_wordsBB' xs =
    (C.unpack . C.unwords  . C.words' . C.pack) xs ==
    (map (\c -> if isSpace c then ' ' else c) xs)

-- prop_linesBB' xs = (C.unpack . C.unlines' . C.lines' . C.pack) xs == (xs)
-}

prop_unfoldrBB c n =
    (fst $ C.unfoldrN n fn c) == (C.pack $ take n $ unfoldr fn c)
    where
      fn x = Just (x, chr (ord x + 1))

prop_prefixBB xs ys = isPrefixOf xs ys == (P.pack xs `P.isPrefixOf` P.pack ys)
prop_suffixBB xs ys = isSuffixOf xs ys == (P.pack xs `P.isSuffixOf` P.pack ys)
prop_suffixLL xs ys = isSuffixOf xs ys == (L.pack xs `L.isSuffixOf` L.pack ys)

prop_copyBB xs = let p = P.pack xs in P.copy p == p
prop_copyLL xs = let p = L.pack xs in L.copy p == p

prop_initsBB xs = inits xs == map P.unpack (P.inits (P.pack xs))

prop_tailsBB xs = tails xs == map P.unpack (P.tails (P.pack xs))

prop_findSubstringsBB s x l
    = C.findSubstrings (C.pack p) (C.pack s) == naive_findSubstrings p s
  where
    _ = l :: Int
    _ = x :: Int

    -- we look for some random substring of the test string
    p = take (model l) $ drop (model x) s

    -- naive reference implementation
    naive_findSubstrings :: String -> String -> [Int]
    naive_findSubstrings p s = [x | x <- [0..length s], p `isPrefixOf` drop x s]

prop_findSubstringBB s x l
    = C.findSubstring (C.pack p) (C.pack s) == naive_findSubstring p s
  where
    _ = l :: Int
    _ = x :: Int

    -- we look for some random substring of the test string
    p = take (model l) $ drop (model x) s

    -- naive reference implementation
    naive_findSubstring :: String -> String -> Maybe Int
    naive_findSubstring p s = listToMaybe [x | x <- [0..length s], p `isPrefixOf` drop x s]

-- correspondance between break and breakSubstring
prop_breakSubstringBB c l
    = P.break (== c) l == P.breakSubstring (P.singleton c) l

prop_breakSubstring_isInfixOf s l
    = P.isInfixOf s l == if P.null s then True
                                     else case P.breakSubstring s l of
                                            (x,y) | P.null y  -> False
                                                  | otherwise -> True

prop_breakSubstring_findSubstring s l
    = P.findSubstring s l == if P.null s then Just 0
                                       else case P.breakSubstring s l of
                                            (x,y) | P.null y  -> Nothing
                                                  | otherwise -> Just (P.length x)

prop_replicate1BB n c = P.unpack (P.replicate n c) == replicate n c
prop_replicate2BB n c = P.replicate n c == fst (P.unfoldrN n (\u -> Just (u,u)) c)

prop_replicate3BB c = P.unpack (P.replicate 0 c) == replicate 0 c

prop_readintBB n = (fst . fromJust . C.readInt . C.pack . show) n == (n :: Int)
prop_readintLL n = (fst . fromJust . D.readInt . D.pack . show) n == (n :: Int)

prop_readBB x = (read . show) x == (x :: P.ByteString)
prop_readLL x = (read . show) x == (x :: L.ByteString)

prop_readint2BB s =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in C.readInt (C.pack s') == Nothing

prop_readintegerBB n = (fst . fromJust . C.readInteger . C.pack . show) n == (n :: Integer)
prop_readintegerLL n = (fst . fromJust . D.readInteger . D.pack . show) n == (n :: Integer)

prop_readinteger2BB s =
    let s' = filter (\c -> c `notElem` ['0'..'9']) s
    in C.readInteger (C.pack s') == Nothing

-- prop_filterChar1BB c xs = (filter (==c) xs) == ((C.unpack . C.filterChar c . C.pack) xs)
-- prop_filterChar2BB c xs = (C.filter (==c) (C.pack xs)) == (C.filterChar c (C.pack xs))
-- prop_filterChar3BB c xs = C.filterChar c xs == C.replicate (C.count c xs) c

-- prop_filterNotChar1BB c xs = (filter (/=c) xs) == ((C.unpack . C.filterNotChar c . C.pack) xs)
-- prop_filterNotChar2BB c xs = (C.filter (/=c) (C.pack xs)) == (C.filterNotChar c (C.pack xs))

-- prop_joinjoinpathBB xs ys c = C.joinWithChar c xs ys == C.join (C.singleton c) [xs,ys]

prop_zipBB  xs ys = zip xs ys == P.zip (P.pack xs) (P.pack ys)
prop_zipLC  xs ys = zip xs ys == LC.zip (LC.pack xs) (LC.pack ys)
prop_zip1BB xs ys = P.zip xs ys == zip (P.unpack xs) (P.unpack ys)

prop_zipWithBB xs ys = P.zipWith (,) xs ys == P.zip xs ys
prop_zipWithCC xs ys = C.zipWith (,) xs ys == C.zip xs ys
prop_zipWithLC xs ys = LC.zipWith (,) xs ys == LC.zip xs ys
-- prop_zipWith'BB xs ys = P.pack (P.zipWith (+) xs ys) == P.zipWith' (+) xs ys

prop_unzipBB x = let (xs,ys) = unzip x in (P.pack xs, P.pack ys) == P.unzip x

------------------------------------------------------------------------
--
-- And check fusion RULES.
--

prop_lazylooploop em1 em2 start1 start2 arr =
    loopL em2 start2 (loopArr (loopL em1 start1 arr))             ==
    loopSndAcc (loopL (em1 `fuseEFL` em2) (start1 :*: start2) arr)
 where
   _ = start1 :: Int
   _ = start2 :: Int

prop_looploop em1 em2 start1 start2 arr =
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) ==
    loopSndAcc (loopU (em1 `fuseEFL` em2) (start1 :*: start2) arr)
 where
   _ = start1 :: Int
   _ = start2 :: Int

------------------------------------------------------------------------

-- check associativity of sequence loops
prop_sequenceloops_assoc n m o x y z a1 a2 a3 xs =

    k ((f * g) * h) == k (f * (g * h))  -- associativity

    where
       (*) = sequenceLoops
       f = (sel n)      x a1
       g = (sel m)      y a2
       h = (sel o)      z a3

       _ = a1 :: Int; _ = a2 :: Int; _ = a3 :: Int
       k g = loopArr (loopWrapper g xs)

-- check wrapper elimination
prop_loop_loop_wrapper_elimination n m x y a1 a2 xs =
  loopWrapper g (loopArr (loopWrapper f xs)) ==
    loopSndAcc (loopWrapper (sequenceLoops f g) xs)
  where
       f = (sel n) x a1
       g = (sel m) y a2
       _ = a1 :: Int; _ = a2 :: Int

sel :: Bool
       -> (acc -> Word8 -> PairS acc (MaybeS Word8))
       -> acc
       -> Ptr Word8
       -> Ptr Word8
       -> Int
       -> IO (PairS (PairS acc Int) Int)
sel False = doDownLoop
sel True  = doUpLoop

------------------------------------------------------------------------
--
-- Test fusion forms
--

prop_up_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doUpLoop f2 acc2)) ==
  k (doUpLoop (f1 `fuseAccAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_down_down_loop_fusion f1 f2 acc1 acc2 xs =
    k (sequenceLoops (doDownLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseAccAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int ; _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
  k (doNoAccLoop (f1 `fuseNoAccNoAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int ; _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doUpLoop f2 acc2)) ==
  k (doUpLoop (f1 `fuseNoAccAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_up_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
  k (doUpLoop (f1 `fuseAccNoAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_noAcc_down_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseNoAccAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_down_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doDownLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
  k (doDownLoop (f1 `fuseAccNoAccEFL` f2) (acc1 :*: acc2))
  where _ = acc1 :: Int; _ = acc2 :: Int; k g = loopWrapper g xs

prop_map_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doMapLoop (f1 `fuseMapMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doFilterLoop (f1 `fuseFilterFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseMapFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseFilterMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseMapNoAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseNoAccMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doUpLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseMapAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_up_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseAccMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_map_down_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doMapLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseMapAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_down_map_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doDownLoop f1 acc1) (doMapLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseAccMapEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_noAcc_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doNoAccLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseFilterNoAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_noAcc_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doNoAccLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doNoAccLoop (f1 `fuseNoAccFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_up_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doUpLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseFilterAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_up_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doUpLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doUpLoop (f1 `fuseAccFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_filter_down_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doFilterLoop f1 acc1) (doDownLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseFilterAccEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

prop_down_filter_loop_fusion f1 f2 acc1 acc2 xs =
  k (sequenceLoops (doDownLoop f1 acc1) (doFilterLoop f2 acc2)) ==
    k (doDownLoop (f1 `fuseAccFilterEFL` f2) (acc1 :*: acc2))
    where _ = acc1 :: Int;  _ = acc2 :: Int ; k g = loopWrapper g xs

------------------------------------------------------------------------

{-
prop_length_loop_fusion_1 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doUpLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doUpLoop f1 acc1) xs))
  where _ = acc1 :: Int

prop_length_loop_fusion_2 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doDownLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doDownLoop f1 acc1) xs))
  where _ = acc1 :: Int

prop_length_loop_fusion_3 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doMapLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doMapLoop f1 acc1) xs))
  where _ = acc1 :: Int

prop_length_loop_fusion_4 f1 acc1 xs =
  P.length  (loopArr (loopWrapper (doFilterLoop f1 acc1) xs)) ==
  P.lengthU (loopArr (loopWrapper (doFilterLoop f1 acc1) xs))
  where _ = acc1 :: Int
-}

-- prop_zipwith_spec f p q =
--   P.pack (P.zipWith f p q) == P.zipWith' f p q
--   where _ = f :: Word8 -> Word8 -> Word8

-- prop_join_spec c s1 s2 =
--  P.join (P.singleton c) (s1 : s2 : []) == P.joinWithByte c s1 s2

-- prop_break_spec x s =
--     P.break ((==) x) s == P.breakByte x s

-- prop_span_spec x s =
--     P.span ((==) x) s == P.spanByte x s

------------------------------------------------------------------------

-- Test IsString
prop_isstring x = C.unpack (fromString x :: C.ByteString) == x
prop_isstring_lc x = LC.unpack (fromString x :: LC.ByteString) == x

------------------------------------------------------------------------
-- Unsafe functions

-- Test unsafePackAddress
prop_unsafePackAddress x = unsafePerformIO $ do
        let (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddress addr
        return (y == x)

-- Test unsafePackAddressLen
prop_unsafePackAddressLen x = unsafePerformIO $ do
        let i = P.length x
            (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddressLen i addr
        return (y == x)

prop_unsafeUseAsCString x = unsafePerformIO $ do
        let n = P.length x
        y <- P.unsafeUseAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_unsafeUseAsCStringLen x = unsafePerformIO $ do
        let n = P.length x
        y <- P.unsafeUseAsCStringLen x $ \(cstr,_) ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_internal_invariant x = LP.invariant x

prop_useAsCString x = unsafePerformIO $ do
        let n = P.length x
        y <- P.useAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_packCString x = unsafePerformIO $ do
        y <- P.useAsCString x $ P.unsafePackCString
        return (y == x)

prop_packCStringLen x = unsafePerformIO $ do
        y <- P.useAsCStringLen x $ P.unsafePackCStringLen
        return (y == x && P.length y == P.length x)

prop_packMallocCString x = unsafePerformIO $ do

         let (fp,_,_) = P.toForeignPtr x
         ptr <- mallocArray0 (P.length x) :: IO (Ptr Word8)
         forM_ [0 .. P.length x] $ \n -> pokeElemOff ptr n 0
         withForeignPtr fp $ \qtr -> copyArray ptr qtr (P.length x)
         y   <- P.unsafePackMallocCString (castPtr ptr)

         let !z = y == x
         free ptr `seq` return z

prop_unsafeFinalize    x = unsafePerformIO $ do
        x <- P.unsafeFinalize x
        return (x == ())

prop_packCStringFinaliser x = unsafePerformIO $ do
        y <- P.useAsCString x $ \cstr -> P.unsafePackCStringFinalizer (castPtr cstr) (P.length x) (return ())
        return (y == x)

prop_show x = show x == show (C.unpack x)

prop_fromForeignPtr x = (let (a,b,c) = (P.toForeignPtr x)
                                in P.fromForeignPtr a b c) == x

------------------------------------------------------------------------
-- IO

prop_read_write_file_P x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do P.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- P.readFile f
                    return (x==y))

prop_read_write_file_C x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do C.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- C.readFile f
                    return (x==y))

prop_read_write_file_L x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do L.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- L.readFile f
                    return (x==y))

prop_read_write_file_D x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do D.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- D.readFile f
                    return (x==y))

------------------------------------------------------------------------

prop_append_file_P x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do P.writeFile f x
            P.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- P.readFile f
                    return (z==(x `P.append` y)))

prop_append_file_C x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do C.writeFile f x
            C.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- C.readFile f
                    return (z==(x `C.append` y)))

prop_append_file_L x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do L.writeFile f x
            L.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- L.readFile f
                    return (z==(x `L.append` y)))

prop_append_file_D x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do D.writeFile f x
            D.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- D.readFile f
                    return (z==(x `D.append` y)))

prop_packAddress = C.pack "this is a test" 
            ==
                   C.pack "this is a test" 

------------------------------------------------------------------------
-- The entry point

main :: IO ()
main = run tests

run :: [(String, Int -> IO (Bool,Int))] -> IO ()
run tests = do
    x <- getArgs
    let n = if null x then 100 else read . head $ x
    (results, passed) <- liftM unzip $ mapM (\(s,a) -> printf "%-40s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"

--
-- And now a list of all the properties to test.
--

tests = misc_tests
     ++ bl_tests
     ++ cc_tests
     ++ bp_tests
     ++ pl_tests
     ++ bb_tests
     ++ ll_tests
     ++ fusion_tests
     ++ io_tests

--
-- 'morally sound' IO
--
io_tests =
    [("readFile.writeFile", mytest prop_read_write_file_P)
    ,("readFile.writeFile", mytest prop_read_write_file_C)
    ,("readFile.writeFile", mytest prop_read_write_file_L)
    ,("readFile.writeFile", mytest prop_read_write_file_D)

    ,("appendFile        ", mytest prop_append_file_P)
    ,("appendFile        ", mytest prop_append_file_C)
    ,("appendFile        ", mytest prop_append_file_L)
    ,("appendFile        ", mytest prop_append_file_D)

    ,("packAddress       ", mytest prop_packAddress)

    ]

misc_tests =
    [("invariant",              mytest prop_invariant)
    ,("unsafe pack address",    mytest prop_unsafePackAddress)
    ,("unsafe pack address len",mytest prop_unsafePackAddressLen)
    ,("unsafeUseAsCString",     mytest prop_unsafeUseAsCString)
    ,("unsafeUseAsCStringLen",     mytest prop_unsafeUseAsCStringLen)
    ,("useAsCString",           mytest prop_useAsCString)
    ,("packCString",            mytest prop_packCString)
    ,("packCStringLen",         mytest prop_packCStringLen)
    ,("packCStringFinaliser",   mytest prop_packCStringFinaliser)
    ,("packMallocString",       mytest prop_packMallocCString)
    ,("unsafeFinalise",         mytest prop_unsafeFinalize)
    ,("invariant",              mytest prop_internal_invariant)
    ,("show",                   mytest prop_show)
    ,("fromForeignPtr",         mytest prop_fromForeignPtr)
    ]

------------------------------------------------------------------------
-- ByteString.Lazy <=> List

bl_tests =
    [("all",         mytest prop_allBL)
    ,("any",         mytest prop_anyBL)
    ,("append",      mytest prop_appendBL)
    ,("compare",     mytest prop_compareBL)
    ,("concat",      mytest prop_concatBL)
    ,("cons",        mytest prop_consBL)
    ,("eq",          mytest prop_eqBL)
    ,("filter",      mytest prop_filterBL)
    ,("find",        mytest prop_findBL)
    ,("findIndex",   mytest prop_findIndexBL)
    ,("findIndices", mytest prop_findIndicesBL)
    ,("foldl",       mytest prop_foldlBL)
    ,("foldl'",      mytest prop_foldlBL')
    ,("foldl1",      mytest prop_foldl1BL)
    ,("foldl1'",     mytest prop_foldl1BL')
    ,("foldr",       mytest prop_foldrBL)
    ,("foldr1",      mytest prop_foldr1BL)
    ,("mapAccumL",   mytest prop_mapAccumLBL)
    ,("mapAccumR",   mytest prop_mapAccumRBL)
    ,("mapAccumR",   mytest prop_mapAccumRCC)
    ,("unfoldr",     mytest prop_unfoldrBL)
    ,("unfoldr",     mytest prop_unfoldrLC)
    ,("iterate",     mytest prop_iterateLC)
    ,("iterate",     mytest prop_iterateLC_2)
    ,("iterate",     mytest prop_iterateL)
    ,("repeat",      mytest prop_repeatLC)
    ,("repeat",      mytest prop_repeatL)
    ,("head",        mytest prop_headBL)
    ,("init",        mytest prop_initBL)
    ,("isPrefixOf",  mytest prop_isPrefixOfBL)
    ,("last",        mytest prop_lastBL)
    ,("length",      mytest prop_lengthBL)
    ,("map",         mytest prop_mapBL)
    ,("maximum",     mytest prop_maximumBL)
    ,("minimum",     mytest prop_minimumBL)
    ,("null",        mytest prop_nullBL)
    ,("reverse",     mytest prop_reverseBL)
    ,("snoc",        mytest prop_snocBL)
    ,("tail",        mytest prop_tailBL)
    ,("transpose",   mytest prop_transposeBL)
    ,("replicate",   mytest prop_replicateBL)
    ,("take",        mytest prop_takeBL)
    ,("drop",        mytest prop_dropBL)
    ,("splitAt",     mytest prop_splitAtBL)
    ,("takeWhile",   mytest prop_takeWhileBL)
    ,("dropWhile",   mytest prop_dropWhileBL)
    ,("break",       mytest prop_breakBL)
    ,("span",        mytest prop_spanBL)
    ,("group",       mytest prop_groupBL)
    ,("inits",       mytest prop_initsBL)
    ,("tails",       mytest prop_tailsBL)
    ,("elem",        mytest prop_elemBL)
    ,("notElem",     mytest prop_notElemBL)
    ,("lines",       mytest prop_linesBL)
    ,("elemIndex",   mytest prop_elemIndexBL)
    ,("elemIndices", mytest prop_elemIndicesBL)
    ,("concatMap",   mytest prop_concatMapBL)
    ]

------------------------------------------------------------------------
-- ByteString.Lazy <=> ByteString

cc_tests =
    [("prop_concatCC", mytest prop_concatCC)
    ,("prop_nullCC", mytest prop_nullCC)
    ,("prop_reverseCC", mytest prop_reverseCC)
    ,("prop_transposeCC", mytest prop_transposeCC)
    ,("prop_groupCC", mytest prop_groupCC)
    ,("prop_initsCC", mytest prop_initsCC)
    ,("prop_tailsCC", mytest prop_tailsCC)
    ,("prop_allCC", mytest prop_allCC)
    ,("prop_anyCC", mytest prop_anyCC)
    ,("prop_appendCC", mytest prop_appendCC)
    ,("prop_breakCC", mytest prop_breakCC)
    ,("prop_concatMapCC", mytest prop_concatMapCC)
    ,("prop_consCC", mytest prop_consCC)
    ,("prop_unconsCC", mytest prop_unconsCC)
    ,("prop_countCC", mytest prop_countCC)
    ,("prop_dropCC", mytest prop_dropCC)
    ,("prop_dropWhileCC", mytest prop_dropWhileCC)
    ,("prop_filterCC", mytest prop_filterCC)
    ,("prop_findCC", mytest prop_findCC)
    ,("prop_findIndexCC", mytest prop_findIndexCC)
    ,("prop_findIndicesCC", mytest prop_findIndicesCC)
    ,("prop_isPrefixOfCC", mytest prop_isPrefixOfCC)
    ,("prop_mapCC", mytest prop_mapCC)
    ,("prop_replicateCC", mytest prop_replicateCC)
    ,("prop_snocCC", mytest prop_snocCC)
    ,("prop_spanCC", mytest prop_spanCC)
    ,("prop_splitCC", mytest prop_splitCC)
    ,("prop_splitAtCC", mytest prop_splitAtCC)
    ,("prop_takeCC", mytest prop_takeCC)
    ,("prop_takeWhileCC", mytest prop_takeWhileCC)
    ,("prop_elemCC", mytest prop_elemCC)
    ,("prop_notElemCC", mytest prop_notElemCC)
    ,("prop_elemIndexCC", mytest prop_elemIndexCC)
    ,("prop_elemIndicesCC", mytest prop_elemIndicesCC)
    ,("prop_lengthCC", mytest prop_lengthCC)
    ,("prop_headCC", mytest prop_headCC)
    ,("prop_initCC", mytest prop_initCC)
    ,("prop_lastCC", mytest prop_lastCC)
    ,("prop_maximumCC", mytest prop_maximumCC)
    ,("prop_minimumCC", mytest prop_minimumCC)
    ,("prop_tailCC", mytest prop_tailCC)
    ,("prop_foldl1CC", mytest prop_foldl1CC)
    ,("prop_foldl1CC'", mytest prop_foldl1CC')
    ,("prop_foldr1CC", mytest prop_foldr1CC)
    ,("prop_foldr1CC'", mytest prop_foldr1CC')
    ,("prop_scanlCC", mytest prop_scanlCC)
    ,("prop_intersperseCC", mytest prop_intersperseCC)

    ,("prop_foldlCC", mytest prop_foldlCC)
    ,("prop_foldlCC'", mytest prop_foldlCC')
    ,("prop_foldrCC", mytest prop_foldrCC)
    ,("prop_foldrCC'", mytest prop_foldrCC')
    ,("prop_mapAccumLCC", mytest prop_mapAccumLCC)
    ,("prop_mapIndexedCC", mytest prop_mapIndexedCC)

    ]

bp_tests =
    [("all",         mytest prop_allBP)
    ,("any",         mytest prop_anyBP)
    ,("append",      mytest prop_appendBP)
    ,("compare",     mytest prop_compareBP)
    ,("concat",      mytest prop_concatBP)
    ,("cons",        mytest prop_consBP)
    ,("cons'",       mytest prop_consBP')
    ,("cons'",       mytest prop_consLP')
    ,("uncons",      mytest prop_unconsBP)
    ,("eq",          mytest prop_eqBP)
    ,("filter",      mytest prop_filterBP)
    ,("find",        mytest prop_findBP)
    ,("findIndex",   mytest prop_findIndexBP)
    ,("findIndices", mytest prop_findIndicesBP)
    ,("foldl",       mytest prop_foldlBP)
    ,("foldl'",      mytest prop_foldlBP')
    ,("foldl1",      mytest prop_foldl1BP)
    ,("foldl1'",     mytest prop_foldl1BP')
    ,("foldr",       mytest prop_foldrBP)
    ,("foldr'",       mytest prop_foldrBP')
    ,("foldr1",      mytest prop_foldr1BP)
    ,("foldr1'",      mytest prop_foldr1BP')
    ,("mapAccumL",   mytest prop_mapAccumLBP)
    ,("unfoldr",     mytest prop_unfoldrBP)
    ,("unfoldr 2",   mytest prop_unfoldr2BP)
    ,("head",        mytest prop_headBP)
    ,("init",        mytest prop_initBP)
    ,("isPrefixOf",  mytest prop_isPrefixOfBP)
    ,("last",        mytest prop_lastBP)
    ,("length",      mytest prop_lengthBP)
    ,("readInt",     mytest prop_readIntBP)
    ,("lines",       mytest prop_linesBP)
    ,("lines \\n",   mytest prop_linesNLBP)
    ,("map",         mytest prop_mapBP)
    ,("maximum   ",  mytest prop_maximumBP)
    ,("minimum"   ,  mytest prop_minimumBP)
    ,("null",        mytest prop_nullBP)
    ,("reverse",     mytest prop_reverseBP)
    ,("snoc",        mytest prop_snocBP)
    ,("tail",        mytest prop_tailBP)
    ,("scanl",       mytest prop_scanlBP)
    ,("transpose",   mytest prop_transposeBP)
    ,("replicate",   mytest prop_replicateBP)
    ,("take",        mytest prop_takeBP)
    ,("drop",        mytest prop_dropBP)
    ,("splitAt",     mytest prop_splitAtBP)
    ,("takeWhile",   mytest prop_takeWhileBP)
    ,("dropWhile",   mytest prop_dropWhileBP)
    ,("break",       mytest prop_breakBP)
    ,("span",        mytest prop_spanBP)
    ,("split",       mytest prop_splitBP)
    ,("count",       mytest prop_countBP)
    ,("group",       mytest prop_groupBP)
    ,("inits",       mytest prop_initsBP)
    ,("tails",       mytest prop_tailsBP)
    ,("elem",        mytest prop_elemBP)
    ,("notElem",     mytest prop_notElemBP)
    ,("elemIndex",   mytest prop_elemIndexBP)
    ,("elemIndices", mytest prop_elemIndicesBP)
    ,("intersperse", mytest prop_intersperseBP)
    ,("concatMap",   mytest prop_concatMapBP)
    ]

------------------------------------------------------------------------
-- ByteString <=> List

pl_tests =
    [("all",         mytest prop_allPL)
    ,("any",         mytest prop_anyPL)
    ,("append",      mytest prop_appendPL)
    ,("compare",     mytest prop_comparePL)
    ,("concat",      mytest prop_concatPL)
    ,("cons",        mytest prop_consPL)
    ,("eq",          mytest prop_eqPL)
    ,("filter",      mytest prop_filterPL)
    ,("filter rules",mytest prop_filterPL_rule)
    ,("filter rules",mytest prop_filterLC_rule)
    ,("partition",   mytest prop_partitionPL)
    ,("partition",   mytest prop_partitionLL)
    ,("find",        mytest prop_findPL)
    ,("findIndex",   mytest prop_findIndexPL)
    ,("findIndices", mytest prop_findIndicesPL)
    ,("foldl",       mytest prop_foldlPL)
    ,("foldl'",      mytest prop_foldlPL')
    ,("foldl1",      mytest prop_foldl1PL)
    ,("foldl1'",     mytest prop_foldl1PL')
    ,("foldr1",      mytest prop_foldr1PL)
    ,("foldr",       mytest prop_foldrPL)
    ,("mapAccumL",   mytest prop_mapAccumLPL)
    ,("mapAccumR",   mytest prop_mapAccumRPL)
    ,("unfoldr",     mytest prop_unfoldrPL)
    ,("scanl",       mytest prop_scanlPL)
    ,("scanl1",      mytest prop_scanl1PL)
    ,("scanr",       mytest prop_scanrPL)
    ,("scanr1",      mytest prop_scanr1PL)
    ,("head",        mytest prop_headPL)
    ,("init",        mytest prop_initPL)
    ,("last",        mytest prop_lastPL)
    ,("maximum",     mytest prop_maximumPL)
    ,("minimum",     mytest prop_minimumPL)
    ,("tail",        mytest prop_tailPL)
    ,("zip",         mytest prop_zipPL)
    ,("zip",         mytest prop_zipLL)
    ,("zip",         mytest prop_zipCL)
    ,("unzip",       mytest prop_unzipPL)
    ,("unzip",       mytest prop_unzipLL)
    ,("unzip",       mytest prop_unzipCL)
    ,("zipWith",          mytest prop_zipWithPL)
--  ,("zipWith",          mytest prop_zipWithCL)
    ,("zipWith rules",   mytest prop_zipWithPL_rules)
--     ,("zipWith/zipWith'", mytest prop_zipWithPL')

    ,("isPrefixOf",  mytest prop_isPrefixOfPL)
    ,("isInfixOf",   mytest prop_isInfixOfPL)
    ,("length",      mytest prop_lengthPL)
    ,("map",         mytest prop_mapPL)
    ,("null",        mytest prop_nullPL)
    ,("reverse",     mytest prop_reversePL)
    ,("snoc",        mytest prop_snocPL)
    ,("transpose",   mytest prop_transposePL)
    ,("replicate",   mytest prop_replicatePL)
    ,("take",        mytest prop_takePL)
    ,("drop",        mytest prop_dropPL)
    ,("splitAt",     mytest prop_splitAtPL)
    ,("takeWhile",   mytest prop_takeWhilePL)
    ,("dropWhile",   mytest prop_dropWhilePL)
    ,("break",       mytest prop_breakPL)
    ,("span",        mytest prop_spanPL)
    ,("group",       mytest prop_groupPL)
    ,("inits",       mytest prop_initsPL)
    ,("tails",       mytest prop_tailsPL)
    ,("elem",        mytest prop_elemPL)
    ,("notElem",     mytest prop_notElemPL)
    ,("lines",       mytest prop_linesPL)
    ,("elemIndex",   mytest prop_elemIndexPL)
    ,("elemIndex",   mytest prop_elemIndexCL)
    ,("elemIndices", mytest prop_elemIndicesPL)
    ,("concatMap",   mytest prop_concatMapPL)
    ,("IsString",    mytest prop_isstring)
    ,("IsString LC",    mytest prop_isstring_lc)
    ]

------------------------------------------------------------------------
-- extra ByteString properties

bb_tests =
    [    ("bijection",      mytest prop_bijectionBB)
    ,    ("bijection'",     mytest prop_bijectionBB')
    ,    ("pack/unpack",    mytest prop_packunpackBB)
    ,    ("unpack/pack",    mytest prop_packunpackBB')
    ,    ("eq 1",           mytest prop_eq1BB)
    ,    ("eq 2",           mytest prop_eq2BB)
    ,    ("eq 3",           mytest prop_eq3BB)
    ,    ("compare 1",      mytest prop_compare1BB)
    ,    ("compare 2",      mytest prop_compare2BB)
    ,    ("compare 3",      mytest prop_compare3BB)
    ,    ("compare 4",      mytest prop_compare4BB)
    ,    ("compare 5",      mytest prop_compare5BB)
    ,    ("compare 6",      mytest prop_compare6BB)
    ,    ("compare 7",      mytest prop_compare7BB)
    ,    ("compare 7",      mytest prop_compare7LL)
    ,    ("compare 8",      mytest prop_compare8BB)
    ,    ("empty 1",        mytest prop_nil1BB)
    ,    ("empty 2",        mytest prop_nil2BB)
    ,    ("empty 1 monoid", mytest prop_nil1LL_monoid)
    ,    ("empty 2 monoid", mytest prop_nil2LL_monoid)
    ,    ("empty 1 monoid", mytest prop_nil1BB_monoid)
    ,    ("empty 2 monoid", mytest prop_nil2BB_monoid)

    ,    ("null",           mytest prop_nullBB)
    ,    ("length 1",       mytest prop_lengthBB)
    ,    ("length 2",       mytest prop_lengthSBB)
    ,    ("cons 1",         mytest prop_consBB)
    ,    ("cons 2",         mytest prop_cons1BB)
    ,    ("cons 3",         mytest prop_cons2BB)
    ,    ("cons 4",         mytest prop_cons3BB)
    ,    ("cons 5",         mytest prop_cons4BB)
    ,    ("snoc",           mytest prop_snoc1BB)
    ,    ("head 1",         mytest prop_head1BB)
    ,    ("head 2",         mytest prop_head2BB)
    ,    ("head 3",         mytest prop_head3BB)
    ,    ("tail",           mytest prop_tailBB)
    ,    ("tail 1",         mytest prop_tail1BB)
    ,    ("last",           mytest prop_lastBB)
    ,    ("init",           mytest prop_initBB)
    ,    ("append 1",       mytest prop_append1BB)
    ,    ("append 2",       mytest prop_append2BB)
    ,    ("append 3",       mytest prop_append3BB)
    ,    ("mappend 1",       mytest prop_append1BB_monoid)
    ,    ("mappend 2",       mytest prop_append2BB_monoid)
    ,    ("mappend 3",       mytest prop_append3BB_monoid)

    ,    ("map 1",          mytest prop_map1BB)
    ,    ("map 2",          mytest prop_map2BB)
    ,    ("map 3",          mytest prop_map3BB)
    ,    ("filter1",        mytest prop_filter1BB)
    ,    ("filter2",        mytest prop_filter2BB)
    ,    ("map fusion",     mytest prop_mapfusionBB)
    ,    ("filter fusion",  mytest prop_filterfusionBB)
    ,    ("reverse 1",      mytest prop_reverse1BB)
    ,    ("reverse 2",      mytest prop_reverse2BB)
    ,    ("reverse 3",      mytest prop_reverse3BB)
    ,    ("foldl 1",        mytest prop_foldl1BB)
    ,    ("foldl 2",        mytest prop_foldl2BB)
    ,    ("foldr 1",        mytest prop_foldr1BB)
    ,    ("foldr 2",        mytest prop_foldr2BB)
    ,    ("foldl1 1",       mytest prop_foldl1_1BB)
    ,    ("foldl1 2",       mytest prop_foldl1_2BB)
    ,    ("foldl1 3",       mytest prop_foldl1_3BB)
    ,    ("foldr1 1",       mytest prop_foldr1_1BB)
    ,    ("foldr1 2",       mytest prop_foldr1_2BB)
    ,    ("foldr1 3",       mytest prop_foldr1_3BB)
    ,    ("scanl/foldl",    mytest prop_scanlfoldlBB)
    ,    ("all",            mytest prop_allBB)
    ,    ("any",            mytest prop_anyBB)
    ,    ("take",           mytest prop_takeBB)
    ,    ("drop",           mytest prop_dropBB)
    ,    ("takeWhile",      mytest prop_takeWhileBB)
    ,    ("dropWhile",      mytest prop_dropWhileBB)
    ,    ("dropWhile",      mytest prop_dropWhileCC_isSpace)
    ,    ("splitAt",        mytest prop_splitAtBB)
    ,    ("span",           mytest prop_spanBB)
    ,    ("break",          mytest prop_breakBB)
    ,    ("elem",           mytest prop_elemBB)
    ,    ("notElem",        mytest prop_notElemBB)

    ,    ("concat 1",       mytest prop_concat1BB)
    ,    ("concat 2",       mytest prop_concat2BB)
    ,    ("concat 3",       mytest prop_concatBB)
    ,    ("mconcat 1",       mytest prop_concat1BB_monoid)
    ,    ("mconcat 2",       mytest prop_concat2BB_monoid)
    ,    ("mconcat 3",       mytest prop_concatBB_monoid)

    ,    ("mconcat 1",       mytest prop_concat1LL_monoid)
    ,    ("mconcat 2",       mytest prop_concat2LL_monoid)
    ,    ("mconcat 3",       mytest prop_concatLL_monoid)

    ,    ("lines",          mytest prop_linesBB)
    ,    ("unlines",        mytest prop_unlinesBB)
    ,    ("unlines",        mytest prop_unlinesLC)
    ,    ("words",          mytest prop_wordsBB)
    ,    ("words",          mytest prop_wordsLC)
    ,    ("unwords",        mytest prop_unwordsBB)
    ,    ("group",          mytest prop_groupBB)
    ,    ("groupBy",        mytest prop_groupByBB)
    ,    ("groupBy",        mytest prop_groupByCC)
    ,    ("groupBy 1",      mytest prop_groupBy1BB)
    ,    ("groupBy 1",      mytest prop_groupBy1CC)
    ,    ("join",           mytest prop_joinBB)
    ,    ("elemIndex 1",    mytest prop_elemIndex1BB)
    ,    ("elemIndex 2",    mytest prop_elemIndex2BB)
    ,    ("findIndex",      mytest prop_findIndexBB)
    ,    ("findIndicies",   mytest prop_findIndiciesBB)
    ,    ("elemIndices",    mytest prop_elemIndicesBB)
    ,    ("find",           mytest prop_findBB)
    ,    ("find/findIndex", mytest prop_find_findIndexBB)
    ,    ("sort 1",         mytest prop_sort1BB)
    ,    ("sort 2",         mytest prop_sort2BB)
    ,    ("sort 3",         mytest prop_sort3BB)
    ,    ("sort 4",         mytest prop_sort4BB)
    ,    ("sort 5",         mytest prop_sort5BB)
    ,    ("intersperse",    mytest prop_intersperseBB)
    ,    ("maximum",        mytest prop_maximumBB)
    ,    ("minimum",        mytest prop_minimumBB)
--  ,    ("breakChar",      mytest prop_breakCharBB)
--  ,    ("spanChar 1",     mytest prop_spanCharBB)
--  ,    ("spanChar 2",     mytest prop_spanChar_1BB)
--  ,    ("breakSpace",     mytest prop_breakSpaceBB)
--  ,    ("dropSpace",      mytest prop_dropSpaceBB)
    ,    ("spanEnd",        mytest prop_spanEndBB)
    ,    ("breakEnd",       mytest prop_breakEndBB)
    ,    ("breakEnd",       mytest prop_breakEndCC)
    ,    ("elemIndexEnd 1",mytest prop_elemIndexEnd1BB)
    ,    ("elemIndexEnd 2",mytest prop_elemIndexEnd2BB)
--  ,    ("words'",         mytest prop_wordsBB')
--     ,    ("lines'",         mytest prop_linesBB')
--  ,    ("dropSpaceEnd",   mytest prop_dropSpaceEndBB)
    ,    ("unfoldr",        mytest prop_unfoldrBB)
    ,    ("prefix",         mytest prop_prefixBB)
    ,    ("suffix",         mytest prop_suffixBB)
    ,    ("suffix",         mytest prop_suffixLL)
    ,    ("copy",           mytest prop_copyBB)
    ,    ("copy",           mytest prop_copyLL)
    ,    ("inits",          mytest prop_initsBB)
    ,    ("tails",          mytest prop_tailsBB)
    ,    ("findSubstrings ",mytest prop_findSubstringsBB)
    ,    ("findSubstring ",mytest prop_findSubstringBB)
    ,    ("breakSubstring 1",mytest prop_breakSubstringBB)
    ,    ("breakSubstring 2",mytest prop_breakSubstring_findSubstring)
    ,    ("breakSubstring 3",mytest prop_breakSubstring_isInfixOf)

    ,    ("replicate1",     mytest prop_replicate1BB)
    ,    ("replicate2",     mytest prop_replicate2BB)
    ,    ("replicate3",     mytest prop_replicate3BB)
    ,    ("readInt",        mytest prop_readintBB)
    ,    ("readInt 2",      mytest prop_readint2BB)
    ,    ("readInteger",    mytest prop_readintegerBB)
    ,    ("readInteger 2",  mytest prop_readinteger2BB)
    ,    ("read",  mytest prop_readLL)
    ,    ("read",  mytest prop_readBB)
    ,    ("Lazy.readInt",   mytest prop_readintLL)
    ,    ("Lazy.readInt",   mytest prop_readintLL)
    ,    ("Lazy.readInteger", mytest prop_readintegerLL)
    ,    ("mconcat 1",       mytest prop_append1LL_monoid)
    ,    ("mconcat 2",       mytest prop_append2LL_monoid)
    ,    ("mconcat 3",       mytest prop_append3LL_monoid)
--  ,    ("filterChar1",    mytest prop_filterChar1BB)
--  ,    ("filterChar2",    mytest prop_filterChar2BB)
--  ,    ("filterChar3",    mytest prop_filterChar3BB)
--  ,    ("filterNotChar1", mytest prop_filterNotChar1BB)
--  ,    ("filterNotChar2", mytest prop_filterNotChar2BB)
    ,    ("tail",           mytest prop_tailSBB)
    ,    ("index",          mytest prop_indexBB)
    ,    ("unsafeIndex",    mytest prop_unsafeIndexBB)
--  ,    ("map'",           mytest prop_mapBB')
    ,    ("filter",         mytest prop_filterBB)
    ,    ("elem",           mytest prop_elemSBB)
    ,    ("take",           mytest prop_takeSBB)
    ,    ("drop",           mytest prop_dropSBB)
    ,    ("splitAt",        mytest prop_splitAtSBB)
    ,    ("foldl",          mytest prop_foldlBB)
    ,    ("foldr",          mytest prop_foldrBB)
    ,    ("takeWhile ",     mytest prop_takeWhileSBB)
    ,    ("dropWhile ",     mytest prop_dropWhileSBB)
    ,    ("span ",          mytest prop_spanSBB)
    ,    ("break ",         mytest prop_breakSBB)
    ,    ("breakspan",      mytest prop_breakspan_1BB)
    ,    ("lines ",         mytest prop_linesSBB)
    ,    ("unlines ",       mytest prop_unlinesSBB)
    ,    ("words ",         mytest prop_wordsSBB)
    ,    ("unwords ",       mytest prop_unwordsSBB)
    ,    ("unwords ",       mytest prop_unwordsSLC)
--     ,    ("wordstokens",    mytest prop_wordstokensBB)
    ,    ("splitWith",      mytest prop_splitWithBB)
    ,    ("joinsplit",      mytest prop_joinsplitBB)
    ,    ("intercalate",    mytest prop_intercalatePL)
--     ,    ("lineIndices",    mytest prop_lineIndices1BB)
    ,    ("count",          mytest prop_countBB)
--  ,    ("linessplit",     mytest prop_linessplit2BB)
    ,    ("splitsplitWith", mytest prop_splitsplitWithBB)
--  ,    ("joinjoinpath",   mytest prop_joinjoinpathBB)
    ,    ("zip",            mytest prop_zipBB)
    ,    ("zip",            mytest prop_zipLC)
    ,    ("zip1",           mytest prop_zip1BB)
    ,    ("zipWith",        mytest prop_zipWithBB)
    ,    ("zipWith",        mytest prop_zipWithCC)
    ,    ("zipWith",        mytest prop_zipWithLC)
--     ,    ("zipWith'",       mytest prop_zipWith'BB)
    ,    ("unzip",          mytest prop_unzipBB)
    ,    ("concatMap",      mytest prop_concatMapBB)
--  ,    ("join/joinByte",  mytest prop_join_spec)
--  ,    ("span/spanByte",  mytest prop_span_spec)
--  ,    ("break/breakByte",mytest prop_break_spec)
    ]

------------------------------------------------------------------------
-- Fusion rules

fusion_tests =
-- v1 fusion
    [    ("lazy loop/loop fusion", mytest prop_lazylooploop)
    ,    ("loop/loop fusion",      mytest prop_looploop)

-- v2 fusion
    ,("loop/loop wrapper elim",       mytest prop_loop_loop_wrapper_elimination)
    ,("sequence association",         mytest prop_sequenceloops_assoc)

    ,("up/up         loop fusion",    mytest prop_up_up_loop_fusion)
    ,("down/down     loop fusion",    mytest prop_down_down_loop_fusion)
    ,("noAcc/noAcc   loop fusion",    mytest prop_noAcc_noAcc_loop_fusion)
    ,("noAcc/up      loop fusion",    mytest prop_noAcc_up_loop_fusion)
    ,("up/noAcc      loop fusion",    mytest prop_up_noAcc_loop_fusion)
    ,("noAcc/down    loop fusion",    mytest prop_noAcc_down_loop_fusion)
    ,("down/noAcc    loop fusion",    mytest prop_down_noAcc_loop_fusion)
    ,("map/map       loop fusion",    mytest prop_map_map_loop_fusion)
    ,("filter/filter loop fusion",    mytest prop_filter_filter_loop_fusion)
    ,("map/filter    loop fusion",    mytest prop_map_filter_loop_fusion)
    ,("filter/map    loop fusion",    mytest prop_filter_map_loop_fusion)
    ,("map/noAcc     loop fusion",    mytest prop_map_noAcc_loop_fusion)
    ,("noAcc/map     loop fusion",    mytest prop_noAcc_map_loop_fusion)
    ,("map/up        loop fusion",    mytest prop_map_up_loop_fusion)
    ,("up/map        loop fusion",    mytest prop_up_map_loop_fusion)
    ,("map/down      loop fusion",    mytest prop_map_down_fusion)
    ,("down/map      loop fusion",    mytest prop_down_map_loop_fusion)
    ,("filter/noAcc  loop fusion",    mytest prop_filter_noAcc_loop_fusion)
    ,("noAcc/filter  loop fusion",    mytest prop_noAcc_filter_loop_fusion)
    ,("filter/up     loop fusion",    mytest prop_filter_up_loop_fusion)
    ,("up/filter     loop fusion",    mytest prop_up_filter_loop_fusion)
    ,("filter/down   loop fusion",    mytest prop_filter_down_fusion)
    ,("down/filter   loop fusion",    mytest prop_down_filter_loop_fusion)

{-
    ,("length/loop   fusion",          mytest prop_length_loop_fusion_1)
    ,("length/loop   fusion",          mytest prop_length_loop_fusion_2)
    ,("length/loop   fusion",          mytest prop_length_loop_fusion_3)
    ,("length/loop   fusion",          mytest prop_length_loop_fusion_4)
-}

--  ,("zipwith/spec",                  mytest prop_zipwith_spec)
    ]


------------------------------------------------------------------------
-- Extra lazy properties

ll_tests =
    [("eq 1",               mytest prop_eq1)
    ,("eq 2",               mytest prop_eq2)
    ,("eq 3",               mytest prop_eq3)
    ,("eq refl",            mytest prop_eq_refl)
    ,("eq symm",            mytest prop_eq_symm)
    ,("compare 1",          mytest prop_compare1)
    ,("compare 2",          mytest prop_compare2)
    ,("compare 3",          mytest prop_compare3)
    ,("compare 4",          mytest prop_compare4)
    ,("compare 5",          mytest prop_compare5)
    ,("compare 6",          mytest prop_compare6)
    ,("compare 7",          mytest prop_compare7)
    ,("compare 8",          mytest prop_compare8)
    ,("empty 1",            mytest prop_empty1)
    ,("empty 2",            mytest prop_empty2)
    ,("pack/unpack",        mytest prop_packunpack)
    ,("unpack/pack",        mytest prop_unpackpack)
    ,("null",               mytest prop_null)
    ,("length 1",           mytest prop_length1)
    ,("length 2",           mytest prop_length2)
    ,("cons 1"    ,         mytest prop_cons1)
    ,("cons 2"    ,         mytest prop_cons2)
    ,("cons 3"    ,         mytest prop_cons3)
    ,("cons 4"    ,         mytest prop_cons4)
    ,("snoc"    ,           mytest prop_snoc1)
    ,("head/pack",          mytest prop_head)
    ,("head/unpack",        mytest prop_head1)
    ,("tail/pack",          mytest prop_tail)
    ,("tail/unpack",        mytest prop_tail1)
    ,("last",               mytest prop_last)
    ,("init",               mytest prop_init)
    ,("append 1",           mytest prop_append1)
    ,("append 2",           mytest prop_append2)
    ,("append 3",           mytest prop_append3)
    ,("map 1",              mytest prop_map1)
    ,("map 2",              mytest prop_map2)
    ,("map 3",              mytest prop_map3)
    ,("filter 1",           mytest prop_filter1)
    ,("filter 2",           mytest prop_filter2)
    ,("reverse",            mytest prop_reverse)
    ,("reverse1",           mytest prop_reverse1)
    ,("reverse2",           mytest prop_reverse2)
    ,("transpose",          mytest prop_transpose)
    ,("foldl",              mytest prop_foldl)
    ,("foldl/reverse",      mytest prop_foldl_1)
    ,("foldr",              mytest prop_foldr)
    ,("foldr/id",           mytest prop_foldr_1)
    ,("foldl1/foldl",       mytest prop_foldl1_1)
    ,("foldl1/head",        mytest prop_foldl1_2)
    ,("foldl1/tail",        mytest prop_foldl1_3)
    ,("foldr1/foldr",       mytest prop_foldr1_1)
    ,("foldr1/last",        mytest prop_foldr1_2)
    ,("foldr1/head",        mytest prop_foldr1_3)
    ,("concat 1",           mytest prop_concat1)
    ,("concat 2",           mytest prop_concat2)
    ,("concat/pack",        mytest prop_concat3)
    ,("any",                mytest prop_any)
    ,("all",                mytest prop_all)
    ,("maximum",            mytest prop_maximum)
    ,("minimum",            mytest prop_minimum)
    ,("replicate 1",        mytest prop_replicate1)
    ,("replicate 2",        mytest prop_replicate2)
    ,("take",               mytest prop_take1)
    ,("drop",               mytest prop_drop1)
    ,("splitAt",            mytest prop_drop1)
    ,("takeWhile",          mytest prop_takeWhile)
    ,("dropWhile",          mytest prop_dropWhile)
    ,("break",              mytest prop_break)
    ,("span",               mytest prop_span)
    ,("splitAt",               mytest prop_splitAt)
    ,("break/span",         mytest prop_breakspan)
--     ,("break/breakByte",    mytest prop_breakByte)
--     ,("span/spanByte",      mytest prop_spanByte)
    ,("split",              mytest prop_split)
    ,("splitWith",          mytest prop_splitWith)
    ,("splitWith",          mytest prop_splitWith_D)
    ,("join.split/id",      mytest prop_joinsplit)
--  ,("join/joinByte",      mytest prop_joinjoinByte)
    ,("group",              mytest prop_group)
    ,("groupBy",            mytest prop_groupBy)
    ,("groupBy",            mytest prop_groupBy_LC)
    ,("index",              mytest prop_index)
    ,("index",              mytest prop_index_D)
    ,("elemIndex",          mytest prop_elemIndex)
    ,("elemIndices",        mytest prop_elemIndices)
    ,("count/elemIndices",  mytest prop_count)
    ,("findIndex",          mytest prop_findIndex)
    ,("findIndices",        mytest prop_findIndicies)
    ,("find",               mytest prop_find)
    ,("find/findIndex",     mytest prop_find_findIndex)
    ,("elem",               mytest prop_elem)
    ,("notElem",            mytest prop_notElem)
    ,("elem/notElem",       mytest prop_elem_notelem)
--  ,("filterByte 1",       mytest prop_filterByte)
--  ,("filterByte 2",       mytest prop_filterByte2)
--  ,("filterNotByte 1",    mytest prop_filterNotByte)
--  ,("filterNotByte 2",    mytest prop_filterNotByte2)
    ,("isPrefixOf",         mytest prop_isPrefixOf)
    ,("concatMap",          mytest prop_concatMap)
    ]
