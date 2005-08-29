--
-- Test fast packed string functions against their list cousins
--

module Tests where

import Data.Char
import Data.List
import Data.Maybe
import Data.FastPackedString

import TestFramework
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory

import System.IO.Unsafe
import qualified Control.Exception

------------------------------------------------------------------------
    
pstr = unsafePerformIO $ mmapFilePS "data"
{-# NOINLINE pstr #-}
qstr = unsafePerformIO $ mmapFilePS "Makefile"
{-# NOINLINE qstr #-}
str  = unsafePerformIO $ readFile "data"
{-# NOINLINE str #-}
str' = unsafePerformIO $ readFile "Makefile"
{-# NOINLINE str' #-}

------------------------------------------------------------------------

$(tests "fps" [d| 
    test_unpackPS = assertEqual (str) (unpackPS pstr) 

    test_eqPS     = do assert (pstr `eqPS` pstr)
                       assert (nilPS `eqPS` nilPS)
                       assert (not (pstr `eqPS` nilPS))
                       assert (not (pstr `eqPS` qstr))

    test_comparePS = do assert (pstr `comparePS` pstr == EQ)
                        assert (qstr `comparePS` pstr == LT)
                        assert (pstr `comparePS` qstr == GT)
                        assert (pstr `comparePS` nilPS == GT)
                        assert (nilPS `comparePS` pstr == LT)
                        assert (nilPS `comparePS` nilPS == EQ)
                        let a = packString "x"
                            b = packString "xy"
                        assert (a `compare` b == LT)
                        assert (b `compare` a == GT)
                        assert (a `compare` a == EQ)
                        assert (b `compare` b == EQ)

    test_nilPS = do assertEqual (length [])     (lengthPS nilPS) 
    test_consPS   = assertEqual ('X' : str)     (unpackPS $ 'X' `consPS` pstr)
    test_headPS   = assertEqual (head str)      (headPS pstr)
    test_tailPS   = assertEqual (tail str)      (unpackPS (tailPS pstr))
    test_lastPS   = assertEqual (last str)      (lastPS pstr)
    test_initPS   = assertEqual (init str)      (unpackPS (initPS pstr))
    test_nullPS   = do assertEqual (null [])    (nullPS nilPS)
                       assertEqual (null str)   (nullPS pstr)
    test_lengthPS = assertEqual (length str)    (lengthPS pstr)  
    test_appendPS = assertEqual (str ++ str)    (unpackPS $ pstr `appendPS` pstr)
    test_mapPS    = do assertEqual (map toUpper str) (unpackPS $ mapPS toUpper pstr)
                       assertEqual (map toUpper []) (unpackPS $ mapPS toUpper nilPS)
    test_filterPS = do assertEqual (filter (=='X') str) (unpackPS (filterPS (=='X') pstr))

    test_foldlPS  = assertEqual (foldl (\x c -> if c == 'a' then x + 1 else x)  0 str)
                                (foldlPS (\x c -> if c == 'a' then x + 1 else x)  0 pstr)
    test_foldrPS  = assertEqual (foldr (\c x -> if c == 'a' then x + 1 else x)  0 str)
                                (foldrPS (\c x -> if c == 'a' then x + 1 else x)  0 pstr)

    test_takeWhilePS  = do assertEqual (takeWhile (/= 'X') str) 
                                       (unpackPS $ takeWhilePS (/= 'X') pstr)
                           assertEqual (takeWhile (/= 'X') [])
                                       (unpackPS $ takeWhilePS (/= 'X') nilPS)
    test_dropWhilePS  = do assertEqual (dropWhile (/= 'X') str)
                                       (unpackPS $ dropWhilePS (/= 'X') pstr)
                           assertEqual (dropWhile (/= 'X') [])
                                       (unpackPS $ dropWhilePS (/= 'X') nilPS)
    test_takePS  = do assertEqual (take 1000 str) (unpackPS $ takePS 1000 pstr)
                      assertEqual (take 1000 []) (unpackPS $ takePS 1000 nilPS)
    test_dropPS  = do assertEqual (drop 1000 str) (unpackPS $ dropPS 1000 pstr)
                      assertEqual (drop 1000 []) (unpackPS $ dropPS 1000 nilPS)

    test_splitAtPS = assertEqual (splitAt 1000 str) 
                                (let (x,y) = splitAtPS 1000 pstr in (unpackPS x, unpackPS y))

    test_spanPS = do assertEqual (span (/= 'X') str) 
                                (let (x,y) = spanPS (/= 'X') pstr in (unpackPS x, unpackPS y))
                     assertEqual (span (/= 'X') []) 
                                (let (x,y) = spanPS (/= 'X') nilPS in (unpackPS x, unpackPS y))

    test_breakPS = do assertEqual (break (/= 'X') str) 
                                (let (x,y) = breakPS (/= 'X') pstr in (unpackPS x, unpackPS y))
                      assertEqual (break (/= 'X') []) 
                                (let (x,y) = breakPS (/= 'X') nilPS in (unpackPS x, unpackPS y))

    test_reversePS = do assertEqual (reverse str)  (unpackPS $ reversePS pstr)
                        assertEqual (reverse [])  (unpackPS $ reversePS nilPS)

    test_elemPS  = do assertEqual ('X' `elem` str)   ('X' `elemPS` pstr)
                      assertEqual ('X' `elem` [])   ('X' `elemPS` nilPS)

    test_concatPS = do
        assertEqual (concat [str,str'])       (unpackPS $ concatPS [pstr,qstr])
        assertEqual (concat [str,[]])         (unpackPS $ concatPS [pstr,nilPS])
        assertEqual (concat [[],str])         (unpackPS $ concatPS [nilPS, pstr])
        assertEqual (concat [str',[],str])    (unpackPS $ concatPS [qstr, nilPS, pstr])
        assertEqual (concat [str,[],str'])    (unpackPS $ concatPS [pstr, nilPS, qstr])
        assertEqual (concat [str,str',[]])    (unpackPS $ concatPS [pstr, qstr, nilPS])
        assertEqual (concat [[],str,str',[]]) (unpackPS $ concatPS [nilPS,pstr, qstr])

    test_indexPS  = do 
        assertEqual (str !! 1000)   (pstr `indexPS` 1000)
        e <- Control.Exception.catch 
                  (Control.Exception.evaluate $ [] !! 1000)           
                  (\_ -> return (chr 0))
        f <- Control.Exception.catch
                  (Control.Exception.evaluate $ nilPS `indexPS` 1000) 
                  (\_ -> return (chr 0))
        assertEqual e f

    test_indexWord8PS  = do
        assertEqual (str !! 1000)   (chr . fromIntegral $ pstr `indexWord8PS` 1000)
        e <- Control.Exception.catch 
                      (Control.Exception.evaluate $ [] !! 1000)           
                      (\_ -> return (chr 0))
        f <- Control.Exception.catch
                      (Control.Exception.evaluate $ (chr .fromIntegral $ nilPS `indexWord8PS` 1000) )
                      (\_ -> return (chr 0))
        assertEqual e f

    test_anyPS   = do assertEqual (any (== 'X') str) (anyPS (== 'X') pstr)
                      assertEqual (any (== '~') str) (anyPS (== '~') pstr)

    test_linesPS    = do 
        assertEqual (lines str)  (map unpackPS $ linesPS pstr)
        assertEqual (lines [])   (map unpackPS  $ linesPS nilPS)
        assertEqual (lines str') (map unpackPS  $ linesPS qstr)
        assertEqual (lines "a\nb\n") (map unpackPS  $ linesPS (packString "a\nb\n"))
        assertEqual (lines "a\nb") (map unpackPS  $ linesPS (packString "a\nb"))

    test_unlinesPS  = do 
        assertEqual (unlines . lines $ str)      (unpackPS . unlinesPS . linesPS $ pstr)
        assertEqual (unlines . lines $ str')     (unpackPS . unlinesPS . linesPS $ qstr)
        assertEqual (unlines . lines $ [])       (unpackPS . unlinesPS .  linesPS $ nilPS)
        assertEqual (unlines . lines $ "a\nb\n") 
                    (unpackPS . unlinesPS . linesPS $ packString "a\nb\n")
        assertEqual (unlines . lines $ "a\nb")
                    (unpackPS . unlinesPS . linesPS $ packString "a\nb")

    test_wordsPS    = do
        assertEqual (words "a b ") 
                    (map unpackPS $ wordsPS $ packString "a b ")
        assertEqual (words "a b") 
                    (map unpackPS $ wordsPS $ packString "a b")
        assertEqual (words str') (map unpackPS $ wordsPS qstr)

    test_unwordsPS  = do
        assertEqual (unwords . words $ "a b ") 
                    (unpackPS . unwordsPS . wordsPS $ packString "a b ")
        assertEqual (unwords $ words str) (unpackPS .  unwordsPS . wordsPS $ pstr)

    test_joinPS = do
        assertEqual (concat $ intersperse "XYX" (lines str))
                    (unpackPS $ joinPS (packString "XYX") (linesPS pstr))

    test_elemIndexPS = do
        assertEqual (elemIndex 'X' str) (elemIndexPS 'X' pstr)

    test_findIndexPS = do
        assertEqual (findIndex (=='X') str)
                    (findIndexPS (=='X') pstr)
        assertEqual (findIndex (=='X') [])
                    (findIndexPS (=='X') nilPS)

    test_sortPS = assertEqual (sort str) (unpackPS . sortPS $ pstr)

 |])
