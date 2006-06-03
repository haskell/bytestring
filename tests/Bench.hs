{-# OPTIONS -fglasgow-exts #-}
-- ^ unboxed strings
--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--
import BenchUtils

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import Data.List
import Data.Char
import Data.Word
import Data.Int

import System.IO
import Control.Monad
import Text.Printf

main :: IO ()
main = do
    -- initialise
    -- force (fps,fps')
    force (lps,lps')

    printf "# Size of test data: %dk\n" ((floor $ (fromIntegral (B.length fps)) / 1024) :: Int)
    printf "#Byte\t Lazy\n"

    -- now get to it
    sequence_ $ zipWith doit [1..] tests

tests =
    [
      ("++",    [F ({-# SCC  "append" #-}B.append fps fps')
                ,F ({-# SCC "lazy append" #-}L.append lps lps')])

    , ("concat",[F ({-# SCC "concat"  #-}B.concat [fps,fps'])
                ,F ({-# SCC "lazy concat" #-}L.concat [lps,lps'])])

    , ("length",[F ({-# SCC "length"  #-}B.length fps)
                ,F ({-# SCC "lazy length" #-}L.length lps)])

{-
    , ("pack",  [F ({-# SCC "pack"      #-}B.pack list)])
--              ,F (SPS.pack list)
--              ,F (PS.packString list)
--              ,F ()])

    , ("unpack",[F ({-# SCC "unpack"    #-}B.unpack fps)])
--              ,F (SPS.unpack sps)
--              ,F (PS.unpackPS ps) ,F ()])
-}

    , ("compare",[F ({-# SCC "compare"  #-}compare fps fps')
                 ,F ({-# SCC "lazy compare" #-}compare lps lps')])

    , ("index", [F ({-# SCC "index" #-}B.index fps 10000000)
                 ,F ({-# SCC "lazy index" #-}L.index lps 10000000)])

    , ("map", [F ({-# SCC "map" #-}B.map (+1) fps)
              ,F ({-# SCC "lazy map" #-}L.map (+1) lps)])

    , ("filter", [F ({-# SCC "filter" #-}B.filter (/=101) fps)
                 ,F ({-# SCC "lazy filter" #-}L.filter (/=101) lps)])

    , ("map'", [F ({-# SCC "map" #-}B.map (*2) fps)
              ,F (B.map' (*1) fps)])

    , ("filter'", [F ({-# SCC "filter" #-}B.filter (/=121) fps)
                 ,F ({-# SCC "filter'" #-}B.filter' (/=121) fps)])

    , ("filterNotByte", [F ({-# SCC "filterNotByte" #-}B.filterNotByte 101 fps)
                 ,F ({-# SCC "lazy filterNotByte" #-}L.filterNotByte 101 lps)])

    , ("filterByte", [F ({-# SCC "filterByte"    #-}B.filterByte 103 fps)
                 ,F ({-# SCC "lazy filterByte" #-}L.filterByte 103 lps)])

    , ("findIndexOrEnd",[F ({-# SCC "findIndexOrEnd" #-}B.findIndexOrEnd (==126) fps)])

    , ("findIndex",[F ({-# SCC "findIndex" #-}B.findIndex (==126) fps)
                   ,F ({-# SCC "lazy findIndex" #-}L.findIndex (==126) lps)])

    , ("find",     [F ({-# SCC "find"      #-}B.find (==126) fps)
                   ,F ({-# SCC "lazy find" #-}L.find (==126) lps)])

    , ("foldl", [F ({-# SCC "fold" #-} B.foldl (\a w -> a+1::Int) 0 fps)
                ,F ({-# SCC "lazy fold" #-} L.foldl (\a w -> a+1::Int) 0 lps)])

    , ("foldl'", [F ({-# SCC "fold" #-} B.foldl' (\a w -> a+1::Int) 0 fps)
                ,F ({-# SCC "lazy fold" #-} L.foldl' (\a w -> a+1::Int) 0 lps)])

    , ("take",[F ({-# SCC "take"      #-}B.take 100000 fps)
                ,F ({-# SCC "lazy take" #-} L.take 100000 lps)])

    , ("drop",[F ({-# SCC "drop"      #-}B.drop 100000 fps)
                ,F ({-# SCC "lazy drop" #-} L.drop 100000 lps)])


    , ("takeWhile",[F ({-# SCC "takeWhile" #-}B.takeWhile (/=122) fps)
                   ,F ({-# SCC "lazy takeWhile" #-}L.takeWhile (==122) lps)])

    , ("dropWhile",[F ({-# SCC "dropWhile" #-}B.dropWhile (/=122) fps)
                 ,F ({-# SCC "lazy dropWhile" #-}L.dropWhile (/=122) lps)])


    , ("span",[F ({-# SCC "span"      #-}B.span (/=122) fps)
                 ,F ({-# SCC "lazy span" #-}L.span (/=122) lps)])

    , ("break",[F ({-# SCC "break"     #-}B.break (==122) fps)
               ,F ({-# SCC "lazy break" #-}L.break (==122) lps)])

    , ("split",[F ({-# SCC "split"     #-}B.split 0x0a fps)
               ,F ({-# SCC "lazy split" #-}L.split 0x0a lps)])

    , ("breakByte",[F ({-# SCC "breakChar" #-} B.breakByte 122 fps)
               ,F ({-# SCC "lazy breakChar" #-}L.breakByte 122 lps)])

    , ("spanByte",[F ({-# SCC "spanChar" #-} B.spanByte 122 fps)
               ,F ({-# SCC "lazy spanChar" #-}L.spanByte 122 lps)])

    , ("reverse",[F ({-# SCC "reverse"   #-}B.reverse fps)
              ,F ({-# SCC "lazy reverse" #-}L.reverse lps)])

    , ("cons",[F ({-# SCC "cons"      #-}B.cons 120 fps)
              ,F ({-# SCC "lazy cons" #-}L.cons 120 lps)])

    , ("snoc",[F ({-# SCC "snoc"      #-}B.snoc fps 120)
              ,F ({-# SCC "lazy snoc" #-}L.snoc lps 120)])

    , ("empty",[F ({-# SCC "empty"     #-}B.empty)
              ,F ({-# SCC "lazy empty" #-}L.empty)])

    , ("head",[F ({-# SCC "head"      #-}B.head fps)
              ,F ({-# SCC "lazy head" #-}L.head lps)])


    , ("tail",[F ({-# SCC "tail"      #-}B.tail fps)
              ,F ({-# SCC "lazy tail" #-}L.tail lps)])


    , ("last",[F ({-# SCC "last"      #-}B.last fps)
              ,F ({-# SCC "lazy last" #-}L.last lps)])

    , ("init",[F ({-# SCC "init"      #-}B.init fps)
              ,F ({-# SCC "lazy init" #-}L.init lps)])

    , ("count",  [F ({-# SCC "count" #-} B.count 10 fps)
              ,F ({-# SCC "lazy count" #-}L.count 10 lps)])

    , ("isPrefixOf",  [F ({-# SCC "isPrefixOf" #-}
                            B.isPrefixOf (C.pack "The Project Gutenberg eBook") fps)
                      ,F ({-# SCC "lazy isPrefixOf" #-}L.isPrefixOf 
                                          (L.pack [84,104,101,32,80,114,111,106,101
                                               ,99,116,32,71,117,116,101,110,98
                                               ,101,114,103,32,101,66,111,111,107]) lps)])

    , ("join",[F ({-# SCC "join" #-}B.join (B.pack [1,2,3]) [fps,fps'])
              ,F ({-# SCC "lazy join" #-}L.join (L.pack [1,2,3]) [lps,lps'])])

    , ("joinWithByte",[F ({-# SCC "joinWithByte" #-} B.joinWithByte 32 fps fps')
              ,F ({-# SCC "lazy joinWithByte" #-}L.joinWithByte 32 lps lps')])

    , ("any",[F ({-# SCC "any"       #-}B.any (==120) fps)
                 ,F ({-# SCC "lazy any" #-}L.any (==120) lps)])

    , ("all",[F ({-# SCC "all"       #-}B.all (==120) fps)
                 ,F ({-# SCC "lazy all" #-}L.all (==120) lps)])

    , ("maximum",[F ({-# SCC "maximum"   #-}B.maximum fps)
                 ,F ({-# SCC "lazy maximum" #-}L.maximum lps)])

    , ("minimum",[F ({-# SCC "minimum"   #-}B.minimum fps)
                 ,F ({-# SCC "lazy minimum" #-}L.minimum lps)])

    , ("elem",[F ({-# SCC "elem"      #-}B.elem 122 fps)
                 ,F ({-# SCC "lazy elem" #-}L.elem 122 lps)])

    , ("notElem",[F ({-# SCC "notElem"      #-}B.notElem 122 fps)
                 ,F ({-# SCC "lazy notElem" #-}L.notElem 122 lps)])

    , ("elemIndex",[F ({-# SCC "elemIndex" #-}B.elemIndex 122 fps)
                 ,F ({-# SCC "lazy elemIndex" #-}L.elemIndex 122 lps)])

    , ("findIndices",[F ({-# SCC "findIndicies" #-} B.findIndices (==122) fps)
                 ,F ({-# SCC "lazy findIndices" #-}L.findIndices (==122) lps)])

    , ("elemIndices",[F ({-# SCC "elemIndicies" #-} B.elemIndices 122 fps)
                 ,F ({-# SCC "lazy elemIndices" #-}L.elemIndices 122 lps)])

    , ("splitAt",[F ({-# SCC "splitAt" #-} B.splitAt 10000 fps)
                 ,F ({-# SCC "lazy splitAt" #-}L.splitAt 10000 lps)])

    , ("splitWith",[F ({-# SCC "splitWith" #-} B.splitWith (==122) fps)
                   ,F ({-# SCC "lazy splitWith" #-}L.splitWith (==122) lps)])

    , ("replicate",[F ({-# SCC "replicate" #-}B.replicate 10000000 120)
                ,F ({-# SCC "lazy replicate" #-}L.replicate 10000000 120)])

    , ("group",[F ({-# SCC "group" #-} B.group fps)
               ,F ({-# SCC "lazy group" #-}L.group lps)])

    , ("groupBy",[F ({-# SCC "groupBy" #-} B.groupBy (==) fps)
                 ,F ({-# SCC "lazy groupBy" #-}L.groupBy (==) lps)])

    , ("inits",[F ({-# SCC "inits"     #-}B.inits fps)])

    , ("tails",[F ({-# SCC "tails"     #-}B.tails fps)])

--  , ("transpose",[F ({-# SCC "transpose" #-}B.transpose [fps,fps'])])

------------------------------------------------------------------------
--
-- Char8 or ByteString only

    , ("intersperse",[F ({-# SCC "intersperse" #-}B.intersperse 120 fps)])


    , ("sort",[F ({-# SCC "sort"      #-}B.sort fps)])

    , ("lineIndices",[F ({-# SCC "lineIndicies" #-} C.lineIndices fps)])

    , ("elemIndexEnd",[F ({-# SCC "elemIndexEnd" #-}B.elemIndexEnd 122 fps)])

    , ("breakSpace",[F ({-# SCC "breakSpace" #-} C.breakSpace fps)])

    , ("dropSpace",[F ({-# SCC "dropSpace" #-} C.dropSpace fps)])
    , ("dropSpaceEnd",[F ({-# SCC "dropSpaceEnd" #-} C.dropSpaceEnd fps)])

--  , ("zip",[F ({-# SCC "zip" #-} B.zip fps fps)])
--  , ("zipWith",[F ({-# SCC "zipWith" #-} B.zipWith (+) fps fps)])

    , ("isSubstringOf",  [F ({-# SCC "isSubstringOf" #-} B.isSubstringOf (C.pack "email news") fps)])

    , ("isSuffixOf",  [F ({-# SCC "isSuffixOf" #-}
                            B.isSuffixOf (C.pack "new eBooks") fps)])
  --                  ,F (L.isSuffixOf (L.pack [110,101,119,32,101,66,111,111,107,115]) lps )])

    , ("spanEnd",[F ({-# SCC "spanEnd"      #-}B.spanEnd (/=122) fps)])
    , ("lines",[F ({-# SCC "lines"     #-}C.lines fps)])
    , ("unlines",[F ({-# SCC "unlines"   #-}C.unlines [fps,fps'])])
    , ("words",[F ({-# SCC "words"     #-}C.words fps)])
    , ("unwords",[F ({-# SCC "unwords"   #-}C.unwords [fps,fps'])])

    , ("addr1",  [F ({-# SCC "packAddress" #-}let s = "my\nstring\nhaskell"# in B.length (C.packAddress s) == 17)])
    , ("addr2",  [F ({-# SCC "unsafePackAddress" #-}let s = "my\nstring\nhaskell"# in C.unsafePackAddress 17 s == C.packAddress s)])

    ]
