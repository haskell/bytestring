--
-- Test fast packed string functions against their list cousins
--

module Tests where

import Data.Char
import Data.List
import Data.FastPackedString

import TestFramework
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory

import System.IO.Unsafe
import qualified Control.Exception

------------------------------------------------------------------------

pstr = unsafePerformIO $ mmapFilePS "data"

str  = unsafePerformIO $ readFile "data"
{-# NOINLINE str #-}

$(tests "fps" [d| 

    test_lengthPS = assertEqual (length str)    (lengthPS pstr)  
    test_nilPS    = assertEqual (length [])     (lengthPS nilPS) 
    test_unpackPS = assertEqual (str)           (unpackPS pstr) 
    test_consPS   = assertEqual ('X' : str)     (unpackPS $ 'X' `consPS` pstr)
    test_tailPS   = assertEqual (tail str)      (unpackPS (tailPS pstr))
    test_initPS   = assertEqual (init str)      (unpackPS (initPS pstr))
    test_lastPS   = assertEqual (last str)      (lastPS pstr)
    test_nullPS   = do assertEqual (null [])    (nullPS nilPS)
                       assertEqual (null str)   (nullPS pstr)
    test_appendPS = assertEqual (str ++ str)    (unpackPS $ pstr `appendPS` pstr)
    test_indexPS  = assertEqual (str !! 1000)   (pstr `indexPS` 1000)
    test_mapPS    = assertEqual (map toUpper str) (unpackPS $ mapPS toUpper pstr)
    test_reversePS = assertEqual (reverse str)  (unpackPS $ reversePS pstr)
    test_foldlPS  = assertEqual (foldl (\x c -> if c == 'a' then x + 1 else x)  0 str)
                                (foldlPS (\x c -> if c == 'a' then x + 1 else x)  0 pstr)
    test_foldrPS  = assertEqual (foldr (\c x -> if c == 'a' then x + 1 else x)  0 str)
                                (foldrPS (\c x -> if c == 'a' then x + 1 else x)  0 pstr)
    test_takePS  = assertEqual (take 1000 str) (unpackPS $ takePS 1000 pstr)
    test_dropPS  = assertEqual (drop 1000 str) (unpackPS $ dropPS 1000 pstr)
    test_splitAtPS = assertEqual (splitAt 1000 str) 
                                (let (x,y) = splitAtPS 1000 pstr in (unpackPS x, unpackPS y))
    test_anyPS   = do assertEqual (any (== 'X') str) (anyPS (== 'X') pstr)
                      assertEqual (any (== '~') str) (anyPS (== '~') pstr)
    test_linesPS    = do assertEqual (lines str) (map unpackPS $ linesPS pstr)
    test_unlinesPS  = do assertEqual (unlines . lines $ str) 
                                     (unpackPS . unlinesPS . linesPS $ pstr)

 |])
    
