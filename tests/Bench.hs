{-# OPTIONS -fglasgow-exts #-}

--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--

import qualified Data.ByteString.Char8 as P
import Data.ByteString (ByteString,mmapFile)

import qualified Data.ByteString as B

-- import qualified Data.PackedString     as PS
-- import qualified SimonPackedString     as SPS
import Data.List
import Data.Char
import Data.Word

import System.Mem
import Control.Concurrent

import System.IO
import System.CPUTime
import System.IO.Unsafe
import Control.Monad
import Control.Exception
import Text.Printf

main :: IO ()
main = do
    -- initialise
    printf "Initialising test data...\n"
    force (fps,fps') -- >> force (list)

    printf "Size of test data: %dk\n" ((floor $ (fromIntegral (B.length fps)) / 1024) :: Int)
    printf "                Char\tByte\n"

    -- now get to it
    sequence_ . map doit $ tests

doit (s,ls) = do
    printf "%-16s" s
    mapM_ (\s -> time s >> performGC >> threadDelay 100) ls

--  (\s -> time s >> performGC) (head ls) -- only test fps now, 
                                          -- we don't care about beating the other code.
    putChar '\n'
    hFlush stdout

time :: F -> IO ()
time (F a) = do
    start <- getCPUTime
    v     <- force a
    case v of
        B -> printf "--\t"
        _ -> do
            end   <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^12)
            printf "%0.3f\t" (diff :: Double)
    hFlush stdout

-- compare the 3 implementations
tests  :: [(String,[F])]

tests =
    [
      ("++",    [F ({-# SCC "append" #-}P.append fps fps')])
--              ,F () -- SPS.append sps sps)
--              ,F () -- PS.appendPS ps ps)
--              ,F (list ++ list)])

    , ("length",[F ({-# SCC "length"    #-}P.length fps)])

--              ,F (SPS.length sps)
--              ,F (PS.lengthPS ps)
--              ,F (length list)])

{-
    , ("pack",  [F ({-# SCC "pack"      #-}P.pack list)])
--              ,F (SPS.pack list)
--              ,F (PS.packString list)
--              ,F ()])

    , ("unpack",[F ({-# SCC "unpack"    #-}P.unpack fps)])
--              ,F (SPS.unpack sps)
--              ,F (PS.unpackPS ps) ,F ()])
-}

    , ("compare",[F ({-# SCC "compare"   #-}compare fps fps')])

--               ,F (compare sps sps')
--               ,F (compare ps ps')
--               ,F (compare list list')])

    , ("index", [F ({-# SCC "index" #-}P.index fps 10000)
                ,F (B.index fps 10000)])

--              ,F (SPS.index sps 10000)
--              ,F (PS.indexPS ps 10000)
--              ,F (list !! 10000)])

    , ("map", [F ({-# SCC "map"       #-}P.map id fps)
                ,F (B.map id fps)])
--            ,F (SPS.map toUpper sps)
--            ,F (PS.mapPS toUpper ps)
--            ,F (map toUpper list)])

    , ("filter", [F ({-# SCC "filter"    #-}P.filter (=='f') fps)
                 ,F (B.filter (==102) fps)])
--               ,F (SPS.filter (=='f') sps)
--               ,F (PS.filterPS (=='f') ps)
--               ,F (filter (=='f') list)])

    , ("filterChar", [F ({-# SCC "filterChar"    #-}P.filterChar 'f' fps)
                 ,F (B.filterByte 102 fps)])

    , ("filterNotChar", [F ({-# SCC "filterNotChar"    #-}P.filterNotChar 'f' fps)
                 ,F (B.filterNotByte 102 fps)])

    , ("take",[F ({-# SCC "take"      #-}P.take 100000 fps)])

--           ,F (SPS.take 100000 sps)
--           ,F (PS.takePS 100000 ps)
--           ,F (take 100000 list)])

    , ("drop",[F ({-# SCC "drop"      #-}P.drop 100000 fps)])

--           ,F (SPS.drop 100000 sps)
--           ,F (PS.dropPS 100000 ps)
--           ,F (drop 100000 list)])

    , ("takeWhile",[F ({-# SCC "takeWhile" #-}P.takeWhile (/='z') fps)
                 ,F (B.takeWhile (==122) fps)])

--                 ,F (SPS.takeWhile (/='z') sps)
--                 ,F (PS.takeWhilePS (/='z') ps)
--                 ,F (takeWhile (/='z') list)])

    , ("dropWhile",[F ({-# SCC "dropWhile" #-}P.dropWhile (/='z') fps)
                 ,F (B.dropWhile (==122) fps)])

--                 ,F (SPS.dropWhile (/='z') sps)
--                 ,F (PS.dropWhilePS (/='z') ps)
--                 ,F (dropWhile (/='z') list)])

    , ("span",[F ({-# SCC "span"      #-}P.span (/='z') fps)
                 ,F (B.span (/=122) fps)])

--                 ,F (SPS.span (/='z') sps)
--                 ,F (PS.spanPS (/='z') ps)
--                 ,F (span (/='z') list)])

    , ("break",[F ({-# SCC "break"     #-}P.break (=='z') fps)
                 ,F (B.break (==122) fps)])

--             ,F (SPS.break (=='z') sps)
--             ,F (PS.breakPS (=='z') ps)
--             ,F (break (=='z') list)])

    , ("lines",[F ({-# SCC "lines"     #-}P.lines fps)])

--             ,F (SPS.lines sps)
--             ,F (PS.linesPS ps)
--             ,F (lines list)])
    , ("split",[F ({-# SCC "split"     #-}P.split '\n' fps)
                 ,F (B.split 0x0a fps)])

    , ("unlines",[F ({-# SCC "unlines"   #-}P.unlines [fps,fps',fps])])
--                 ,F () {-F (SPS.unlines [sps,sps',sps])-}
--                 ,F () -- (PS.unlinesPS [ps,ps',ps])
--                 ,F (unlines [list,list',list])])

    , ("words",[F ({-# SCC "words"     #-}P.words fps)])
--                 ,F (SPS.words sps)
--                 ,F (PS.wordsPS ps)
--                 ,F (words list)])

    , ("unwords",[F ({-# SCC "unwords"   #-}P.unwords [fps,fps',fps])])
--                 ,F () {-(SPS.unwords [sps,sps',sps])-}
--                 ,F () -- PS.unwordsPS [ps,ps',ps])
--                 ,F (unwords [list,list',list])])

    , ("reverse",[F ({-# SCC "reverse"   #-}P.reverse fps)])
--                 ,F (SPS.reverse sps)
--                 ,F (PS.reversePS ps)
--                 ,F (reverse list)])

    , ("concat",[F ({-# SCC "concat"    #-}P.concat [fps,fps'])])
--                 ,F (SPS.concat [sps,sps'])
--                 ,F (PS.concatPS [ps,ps'])
--                 ,F (concat [list,list'])])

    , ("cons",[F ({-# SCC "cons"      #-}P.cons 'x' fps)
                 ,F (B.cons 120 fps)])
--            ,F (SPS.cons 'x' sps)
--            ,F (PS.consPS 'x' ps)
--            ,F ('x' : list)])

    , ("snoc",[F ({-# SCC "snoc"      #-}P.snoc fps 'x')
                 ,F (B.snoc fps 120)])

    , ("empty",[F ({-# SCC "empty"     #-}P.empty)])
--            ,F (SPS.nil)
--            ,F (PS.nilPS)
--            ,F []])

    , ("head",[F ({-# SCC "head"      #-}P.head fps)
                 ,F (B.head fps)])
--            ,F (SPS.head sps)
--            ,F (PS.headPS ps)
--            ,F (head list)])

    , ("tail",[F ({-# SCC "tail"      #-}P.tail fps)])
--            ,F (SPS.tail sps)
--            ,F (PS.tailPS ps)
--            ,F (tail list)])

    , ("last",[F ({-# SCC "last"      #-}P.last fps)
                 ,F (B.last fps)])
--            ,F (last list)])

    , ("init",[F ({-# SCC "init"      #-}P.init fps)])
--            ,F (init list)])

    , ("inits",[F ({-# SCC "inits"     #-}P.inits fps)])

    , ("tails",[F ({-# SCC "tails"     #-}P.tails fps)])
--            ,F (tails list)])

    , ("intersperse",[F ({-# SCC "intersperse" #-}P.intersperse 'x' fps)
                 ,F (B.intersperse 120 fps)])
--            ,F (intersperse 'x' list)])

--  , ("transpose",[F ({-# SCC "transpose" #-}P.transpose [fps,fps'])])

    , ("join",[F ({-# SCC "join" #-}P.join (P.pack "xxx") [fps,fps'])])

    , ("concatMap",[{-# SCC "concatMap" #-}F ()])
--                ,F (concatMap (\c -> [c]) list)])

    , ("any",[F ({-# SCC "any"       #-}P.any (=='z') fps)
                 ,F (B.any (==120) fps)])
--            ,F (any (=='z') list)])

    , ("all",[F ({-# SCC "all"       #-}P.all (=='z') fps)
                 ,F (B.all (==120) fps)])
--            ,F (all (=='z') list)])

    , ("sort",[F ({-# SCC "sort"      #-}P.sort fps)])

    , ("maximum",[F ({-# SCC "maximum"   #-}P.maximum fps)
                 ,F (B.maximum fps)])
--            ,F (maximum list)])

    , ("minimum",[F ({-# SCC "minimum"   #-}P.minimum fps)
                 ,F (B.minimum fps)])
--            ,F (minimum list)])

    , ("replicate",[F ({-# SCC "replicate" #-}P.replicate 10000000 'x')
                 ,F (B.replicate 10000000 120)])
--            ,F (replicate 1000000 'x')])

    , ("elem",[F ({-# SCC "elem"      #-}P.elem 'z' fps)
                 ,F (B.elem 122 fps)])
--            ,F (SPS.elem 'z' sps)
--            ,F (PS.elemPS 'z' ps)
 --           ,F (elem 'z' list)])
    , ("notElem",[F ({-# SCC "notElem"      #-}P.notElem 'z' fps)
                 ,F (B.notElem 122 fps)])

    , ("find",[F ({-# SCC "find"      #-}P.find (=='z') fps)
                 ,F (B.find (==122) fps)])
--            ,F (find (=='z') list)])

    , ("elemIndex",[F ({-# SCC "elemIndex" #-}P.elemIndex 'z' fps)
                 ,F (B.elemIndex 122 fps)])
--                 ,F (elemIndex 'z' list)])

    , ("elemIndexLast",[F ({-# SCC "elemIndexLast" #-}P.elemIndexLast 'z' fps)
                 ,F (B.elemIndexLast 122 fps)])

    , ("findIndex",[F ({-# SCC "findIndex" #-}P.findIndex (=='z') fps)
                 ,F (B.findIndex (==122) fps)])

    , ("elemIndices",[F ({-# SCC "elemIndicies" #-} P.elemIndices 'z' fps)
                 ,F (B.elemIndices 122 fps)])
--                    ,F (elemIndices 'z' list)])

    , ("findIndices",[F ({-# SCC "findIndicies" #-} P.findIndices (=='z') fps)
                 ,F (B.findIndices (==122) fps)])

    , ("splitAt",[F ({-# SCC "splitAt" #-} P.splitAt 10000 fps)])

------------------------------------------------------------------------

    , ("lineIndices",[F ({-# SCC "lineIndicies" #-} P.lineIndices fps)])

    , ("breakChar",[F ({-# SCC "breakOn" #-} P.breakChar 'z' fps)
                 ,F (B.breakByte 122 fps)])

    , ("breakSpace",[F ({-# SCC "breakSpace" #-} P.breakSpace fps)])

    , ("splitWith",[F ({-# SCC "splitWith" #-} P.splitWith (=='z') fps)
                 ,F (B.splitWith (==122) fps)])

    , ("dropSpace",[F ({-# SCC "dropSpace" #-} P.dropSpace fps)])
    , ("dropSpaceEnd",[F ({-# SCC "dropSpaceEnd" #-} P.dropSpaceEnd fps)])

    , ("joinWithChar",[F ({-# SCC "joinWithChar" #-} P.joinWithChar ' ' fps fps')
                 ,F (B.joinWithByte 32 fps fps')])

    , ("join",[F ({-# SCC "join" #-} P.join (P.packChar ' ') [fps,fps'])
                 ,F (B.join (B.packByte 120) [fps,fps'])])

    , ("zip",[F ({-# SCC "zip" #-} P.zip fps fps)])
    , ("zipWith",[F ({-# SCC "zipWith" #-} P.zipWith ((. ord) . (+) . ord) fps fps)])

    , ("isSubstringOf",  [F ({-# SCC "isSubstringOf" #-} P.isSubstringOf (P.pack "email news") fps)])

    , ("isPrefixOf",  [F ({-# SCC "isPrefixOf" #-} P.isPrefixOf (P.pack "The Project Gutenberg eBook") fps)])
    , ("isSuffixOf",  [F ({-# SCC "isSuffixOf" #-} P.isSuffixOf (P.pack "new eBooks") fps)])

    ]

------------------------------------------------------------------------
-- 
-- an existential list
--
data F = forall a . Forceable a => F a

data Result = T | B

--
-- a bit deepSeqish
--
class Forceable a where
    force :: a -> IO Result
    force v = v `seq` return T

instance Forceable P.ByteString where
    force v = P.length v `seq` return T

-- instance Forceable SPS.PackedString where
--     force v = SPS.length v `seq` return T

-- instance Forceable PS.PackedString where
--     force v = PS.lengthPS v `seq` return T

instance Forceable a => Forceable (Maybe a) where
    force Nothing  = return T
    force (Just v) = force v `seq` return T

instance Forceable [a] where
    force v = length v `seq` return T

instance (Forceable a, Forceable b) => Forceable (a,b) where
    force (a,b) = force a >> force b

instance Forceable Int
instance Forceable Bool
instance Forceable Char
instance Forceable Word8
instance Forceable Ordering

-- used to signal undefinedness
instance Forceable () where force () = return B

------------------------------------------------------------------------
--
-- some large strings to play with
--

fps :: P.ByteString
fps = unsafePerformIO $ mmapFile dict
{-# NOINLINE fps #-}

fps' :: P.ByteString
fps' = unsafePerformIO $ mmapFile dict'
{-# NOINLINE fps' #-}

{-
sps :: SPS.PackedString
sps = unsafePerformIO $ do
    h  <- openFile dict ReadMode
    l  <- hFileSize h
    ps <- SPS.hGet h (fromIntegral l)
    hClose h
    return ps
{-# NOINLINE sps #-}

sps' :: SPS.PackedString
sps' = unsafePerformIO $ do
    h  <- openFile dict' ReadMode
    l  <- hFileSize h
    ps <- SPS.hGet h (fromIntegral l)
    hClose h
    return ps
{-# NOINLINE sps' #-}

ps :: PS.PackedString
ps = unsafePerformIO $ do
    h  <- openFile dict ReadMode
    s  <- hGetContents h
    length s `seq` return ()
    hClose h
    return $! PS.packString s
{-# NOINLINE ps #-}

ps' :: PS.PackedString
ps' = unsafePerformIO $ do
    h  <- openFile dict' ReadMode
    l  <- hFileSize h
    s  <- hGetContents h
    length s `seq` return ()
    hClose h
    return $! PS.packString s
{-# NOINLINE ps' #-}
-}

{-
list :: [Char]
list = unsafePerformIO $ do
    h  <- openFile dict ReadMode
    s  <- hGetContents h
    length s `seq` return ()
    hClose h
    return s
{-# NOINLINE list #-}

list' :: [Char]
list' = unsafePerformIO $ do
    h  <- openFile dict' ReadMode
    s  <- hGetContents h
    length s `seq` return ()
    hClose h
    return s
{-# NOINLINE list' #-}
-}

dict = "bigdata"
dict' = "data"

------------------------------------------------------------------------
