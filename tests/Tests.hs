--
-- Test fast packed string functions against their list cousins
--

module Tests where

import Data.Char
import Data.List
import Data.Maybe

import qualified Data.ByteString.Char8 as P
import Data.ByteString.Char8 (pack,unpack,ByteString,mmapFile)

import TestFramework
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory

import System.IO.Unsafe
import qualified Control.Exception

------------------------------------------------------------------------
    
pstr = unsafePerformIO $ mmapFile "data"
{-# NOINLINE pstr #-}
qstr = unsafePerformIO $ mmapFile "Makefile"
{-# NOINLINE qstr #-}
str  = unsafePerformIO $ readFile "data"
{-# NOINLINE str #-}
str' = unsafePerformIO $ readFile "Makefile"
{-# NOINLINE str' #-}

------------------------------------------------------------------------

$(tests "fps" [d|
    test_unpack = assertEqual (str) (unpack pstr)

    test_eqPS     = do assert (pstr == pstr)
                       assert (P.empty == P.empty)
                       assert (not (pstr == P.empty))
                       assert (not (pstr == qstr))

    test_compare = do assert (pstr `compare` pstr == EQ)
                      assert (qstr `compare` pstr == LT)
                      assert (pstr `compare` qstr == GT)
                      assert (pstr `compare` P.empty == GT)
                      assert (P.empty `compare` pstr == LT)
                      assert (P.empty `compare` P.empty == EQ)
                      let a = pack "x"
                          b = pack "xy"
                      assert (a `compare` b == LT)
                      assert (b `compare` a == GT)
                      assert (a `compare` a == EQ)
                      assert (b `compare` b == EQ)

    test_empty = do assertEqual (length [])   (P.length P.empty)
    test_cons   = assertEqual ('X' : str)     (unpack $ 'X' `P.cons` pstr)
    test_head   = assertEqual (head str)      (P.head pstr)
    test_tail   = assertEqual (tail str)      (unpack (P.tail pstr))
    test_last   = assertEqual (last str)      (P.last pstr)
    test_init   = assertEqual (init str)      (unpack (P.init pstr))
    test_null   = do assertEqual (null [])    (P.null P.empty)
                     assertEqual (null str)   (P.null pstr)
    test_length = assertEqual (length str)    (P.length pstr)
    test_append = assertEqual (str ++ str)    (unpack $ pstr `P.append` pstr)

    test_map    = do assertEqual (map toUpper str) (unpack $ P.map toUpper pstr)
                     assertEqual (map toUpper []) (unpack $ P.map toUpper P.empty)

    test_filter = do assertEqual (filter (=='X') str) (unpack (P.filter (=='X') pstr))

    test_foldl  = assertEqual (foldl (\x c -> if c == 'a' then x + 1 else x)  0 str)
                                (P.foldl (\x c -> if c == 'a' then x + 1 else x)  0 pstr)
    test_foldr  = assertEqual (foldr (\c x -> if c == 'a' then x + 1 else x)  0 str)
                                (P.foldr (\c x -> if c == 'a' then x + 1 else x)  0 pstr)

    test_takeWhile  = do assertEqual (takeWhile (/= 'X') str)
                                       (unpack $ P.takeWhile (/= 'X') pstr)
                         assertEqual (takeWhile (/= 'X') [])
                                       (unpack $ P.takeWhile (/= 'X') P.empty)
    test_dropWhile  = do assertEqual (dropWhile (/= 'X') str)
                                       (unpack $ P.dropWhile (/= 'X') pstr)
                         assertEqual (dropWhile (/= 'X') [])
                                       (unpack $ P.dropWhile (/= 'X') P.empty)
    test_take  = do assertEqual (take 1000 str) (unpack $ P.take 1000 pstr)
                    assertEqual (take 1000 []) (unpack $ P.take 1000 P.empty)
    test_drop  = do assertEqual (drop 1000 str) (unpack $ P.drop 1000 pstr)
                    assertEqual (drop 1000 []) (unpack $ P.drop 1000 P.empty)

    test_splitAtPS = assertEqual (splitAt 1000 str)
                                (let (x,y) = P.splitAt 1000 pstr in (unpack x, unpack y))

    test_spanPS = do assertEqual (span (/= 'X') str)
                                (let (x,y) = P.span (/= 'X') pstr in (unpack x, unpack y))
                     assertEqual (span (/= 'X') [])
                                (let (x,y) = P.span (/= 'X') P.empty in (unpack x, unpack y))

    test_breakPS = do assertEqual (break (/= 'X') str)
                                (let (x,y) = P.break (/= 'X') pstr in (unpack x, unpack y))
                      assertEqual (break (/= 'X') [])
                                (let (x,y) = P.break (/= 'X') P.empty in (unpack x, unpack y))

    test_reversePS = do assertEqual (reverse str)  (unpack $ P.reverse pstr)
                        assertEqual (reverse [])  (unpack $ P.reverse P.empty)

    test_elemPS  = do assertEqual ('X' `elem` str)   ('X' `P.elem` pstr)
                      assertEqual ('X' `elem` [])   ('X' `P.elem` P.empty)

    test_concat = do
        assertEqual (concat [str,str'])       (unpack $ P.concat [pstr,qstr])
        assertEqual (concat [str,[]])         (unpack $ P.concat [pstr,P.empty])
        assertEqual (concat [[],str])         (unpack $ P.concat [P.empty, pstr])
        assertEqual (concat [str',[],str])    (unpack $ P.concat [qstr, P.empty, pstr])
        assertEqual (concat [str,[],str'])    (unpack $ P.concat [pstr, P.empty, qstr])
        assertEqual (concat [str,str',[]])    (unpack $ P.concat [pstr, qstr, P.empty])
        assertEqual (concat [[],str,str',[]]) (unpack $ P.concat [P.empty,pstr, qstr])

    test_index  = do
        assertEqual (str !! 1000)   (pstr `P.index` 1000)
        e <- Control.Exception.catch
                  (Control.Exception.evaluate $ [] !! 1000)
                  (\_ -> return (chr 0))
        f <- Control.Exception.catch
                  (Control.Exception.evaluate $ P.empty `P.index` 1000)
                  (\_ -> return (chr 0))
        assertEqual e f

    test_any   = do assertEqual (any (== 'X') str) (P.any (== 'X') pstr)
                    assertEqual (any (== '~') str) (P.any (== '~') pstr)

    test_lines    = do
        assertEqual (lines str)  (map unpack $ P.lines pstr)
        assertEqual (lines [])   (map unpack  $ P.lines P.empty)
        assertEqual (lines str') (map unpack  $ P.lines qstr)
        assertEqual (lines "a\nb\n") (map unpack  $ P.lines (pack "a\nb\n"))
        assertEqual (lines "a\nb") (map unpack  $ P.lines (pack "a\nb"))

    test_unlines  = do 
        assertEqual (unlines . lines $ str)      (unpack . P.unlines . P.lines $ pstr)
        assertEqual (unlines . lines $ str')     (unpack . P.unlines . P.lines $ qstr)
        assertEqual (unlines . lines $ [])       (unpack . P.unlines .  P.lines $ P.empty)
        assertEqual (unlines . lines $ "a\nb\n") 
                    (unpack . P.unlines . P.lines $ pack "a\nb\n")
        assertEqual (unlines . lines $ "a\nb")
                    (unpack . P.unlines . P.lines $ pack "a\nb")

    test_words    = do
        assertEqual (words "a b ") 
                    (map unpack $ P.words $ pack "a b ")
        assertEqual (words "a b") 
                    (map unpack $ P.words $ pack "a b")
        assertEqual (words str') (map unpack $ P.words qstr)

    test_unwords  = do
        assertEqual (unwords . words $ "a b ") 
                    (unpack . P.unwords . P.words $ pack "a b ")
        assertEqual (unwords $ words str) (unpack .  P.unwords . P.words $ pstr)

    test_join = do
        assertEqual (concat $ intersperse "XYX" (lines str))
                    (unpack $ P.join (pack "XYX") (P.lines pstr))

    test_elemIndex = do
        assertEqual (elemIndex 'X' str) (P.elemIndex 'X' pstr)

    test_findIndex = do
        assertEqual (findIndex (=='X') str)
                    (P.findIndex (=='X') pstr)
        assertEqual (findIndex (=='X') [])
                    (P.findIndex (=='X') P.empty)

    test_sort = assertEqual (sort str) (unpack . P.sort $ pstr)

    test_intersperse = assertEqual (intersperse 'X' str) (unpack (P.intersperse 'X' pstr))

    test_maximum = assertEqual (maximum str) (P.maximum pstr)
    test_minimum = assertEqual (minimum str) (P.minimum pstr)

    test_dropSpace = assertEqual (dropWhile isSpace str) (unpack (P.dropSpace pstr))
    test_count     = assertEqual (length (P.elemIndices 'X' pstr)) (P.count 'X' pstr)

 |])
