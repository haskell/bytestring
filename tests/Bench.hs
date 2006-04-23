{-# OPTIONS -fglasgow-exts #-}

--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--

import qualified Data.ByteString as FPS
-- import qualified Data.PackedString     as PS
-- import qualified SimonPackedString     as SPS
import Data.List
import Data.Char

import System.Mem

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

    printf "Size of test data: %dk\n" ((floor $ (fromIntegral (FPS.length fps)) / 1024) :: Int)
    printf "                FPS\n"

    -- now get to it
    sequence_ . map doit $ tests

doit (s,ls) = do
    printf "%-16s" s
--  mapM_ (\s -> time s >> performGC) ls
    (\s -> time s >> performGC) (head ls) -- only test fps now, 
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
      ("++",    [F ({-# SCC "append" #-}FPS.append fps fps)])
--              ,F () -- SPS.append sps sps)
--              ,F () -- PS.appendPS ps ps)
--              ,F (list ++ list)])

    , ("length",[F ({-# SCC "length"    #-}FPS.length fps)])
--              ,F (SPS.length sps)
--              ,F (PS.lengthPS ps)
--              ,F (length list)])

{-
    , ("pack",  [F ({-# SCC "pack"      #-}FPS.pack list)])
--              ,F (SPS.pack list)
--              ,F (PS.packString list)
--              ,F ()])

    , ("unpack",[F ({-# SCC "unpack"    #-}FPS.unpack fps)])
--              ,F (SPS.unpack sps)
--              ,F (PS.unpackPS ps) ,F ()])
-}

    , ("compare",[F ({-# SCC "compare"   #-}compare fps fps')])
--               ,F (compare sps sps')
--               ,F (compare ps ps')
--               ,F (compare list list')])

    , ("index", [F ({-# SCC "index" #-}FPS.index fps 10000)])
--              ,F (SPS.index sps 10000)
--              ,F (PS.indexPS ps 10000)
--              ,F (list !! 10000)])

    , ("map", [F ({-# SCC "map"       #-}FPS.map toUpper fps)])
--            ,F (SPS.map toUpper sps)
--            ,F (PS.mapPS toUpper ps)
--            ,F (map toUpper list)])

    , ("filter", [F ({-# SCC "filter"    #-}FPS.filter (=='f') fps)])
--               ,F (SPS.filter (=='f') sps)
--               ,F (PS.filterPS (=='f') ps)
--               ,F (filter (=='f') list)])

    , ("filterChar", [F ({-# SCC "filterChar"    #-}FPS.filterChar 'f' fps)])

    , ("take",[F ({-# SCC "take"      #-}FPS.take 100000 fps)])
--           ,F (SPS.take 100000 sps)
--           ,F (PS.takePS 100000 ps)
--           ,F (take 100000 list)])

    , ("drop",[F ({-# SCC "drop"      #-}FPS.drop 100000 fps)])
--           ,F (SPS.drop 100000 sps)
--           ,F (PS.dropPS 100000 ps)
--           ,F (drop 100000 list)])

    , ("takeWhile",[F ({-# SCC "takeWhile" #-}FPS.takeWhile (/='z') fps)])
--                 ,F (SPS.takeWhile (/='z') sps)
--                 ,F (PS.takeWhilePS (/='z') ps)
--                 ,F (takeWhile (/='z') list)])

    , ("dropWhile",[F ({-# SCC "dropWhile" #-}FPS.dropWhile (/='z') fps)])
--                 ,F (SPS.dropWhile (/='z') sps)
--                 ,F (PS.dropWhilePS (/='z') ps)
--                 ,F (dropWhile (/='z') list)])

    , ("span",[F ({-# SCC "span"      #-}FPS.span (/='z') fps)])
--                 ,F (SPS.span (/='z') sps)
--                 ,F (PS.spanPS (/='z') ps)
--                 ,F (span (/='z') list)])

    , ("break",[F ({-# SCC "break"     #-}FPS.break (=='z') fps)])
--             ,F (SPS.break (=='z') sps)
--             ,F (PS.breakPS (=='z') ps)
--             ,F (break (=='z') list)])

    , ("lines",[F ({-# SCC "lines"     #-}FPS.lines fps)])
--             ,F (SPS.lines sps)
--             ,F (PS.linesPS ps)
--             ,F (lines list)])
    , ("split",[F ({-# SCC "split"     #-}FPS.split '\n' fps)])

    , ("unlines",[F ({-# SCC "unlines"   #-}FPS.unlines [fps,fps',fps])])
--                 ,F () {-F (SPS.unlines [sps,sps',sps])-}
--                 ,F () -- (PS.unlinesPS [ps,ps',ps])
--                 ,F (unlines [list,list',list])])

    , ("words",[F ({-# SCC "words"     #-}FPS.words fps)])
--                 ,F (SPS.words sps)
--                 ,F (PS.wordsPS ps)
--                 ,F (words list)])

    , ("unwords",[F ({-# SCC "unwords"   #-}FPS.unwords [fps,fps',fps])])
--                 ,F () {-(SPS.unwords [sps,sps',sps])-}
--                 ,F () -- PS.unwordsPS [ps,ps',ps])
--                 ,F (unwords [list,list',list])])

    , ("reverse",[F ({-# SCC "reverse"   #-}FPS.reverse fps)])
--                 ,F (SPS.reverse sps)
--                 ,F (PS.reversePS ps)
--                 ,F (reverse list)])

    , ("concat",[F ({-# SCC "concat"    #-}FPS.concat [fps,fps'])])
--                 ,F (SPS.concat [sps,sps'])
--                 ,F (PS.concatPS [ps,ps'])
--                 ,F (concat [list,list'])])

    , ("cons",[F ({-# SCC "cons"      #-}FPS.cons 'x' fps)])
--            ,F (SPS.cons 'x' sps)
--            ,F (PS.consPS 'x' ps)
--            ,F ('x' : list)])

    , ("snoc",[F ({-# SCC "snoc"      #-}FPS.snoc fps 'x')])

    , ("empty",[F ({-# SCC "empty"     #-}FPS.empty)])
--            ,F (SPS.nil)
--            ,F (PS.nilPS)
--            ,F []])

    , ("head",[F ({-# SCC "head"      #-}FPS.head fps)])
--            ,F (SPS.head sps)
--            ,F (PS.headPS ps)
--            ,F (head list)])

    , ("tail",[F ({-# SCC "tail"      #-}FPS.tail fps)])
--            ,F (SPS.tail sps)
--            ,F (PS.tailPS ps)
--            ,F (tail list)])

    , ("last",[F ({-# SCC "last"      #-}FPS.last fps)])
--            ,F (last list)])

    , ("init",[F ({-# SCC "init"      #-}FPS.init fps)])
--            ,F (init list)])

    , ("inits",[F ({-# SCC "inits"     #-}FPS.inits fps)])

    , ("tails",[F ({-# SCC "tails"     #-}FPS.tails fps)])
--            ,F (tails list)])

    , ("intersperse",[F ({-# SCC "intersperse" #-}FPS.intersperse 'x' fps)])
--            ,F (intersperse 'x' list)])

--  , ("transpose",[F ({-# SCC "transpose" #-}FPS.transpose [fps,fps'])])

    , ("join",[F ({-# SCC "join" #-}FPS.join (FPS.pack "xxx") [fps,fps'])])

    , ("concatMap",[{-# SCC "concatMap" #-}F ()])
--                ,F (concatMap (\c -> [c]) list)])

    , ("any",[F ({-# SCC "any"       #-}FPS.any (=='z') fps)])
--            ,F (any (=='z') list)])

    , ("all",[F ({-# SCC "all"       #-}FPS.all (=='z') fps)])
--            ,F (all (=='z') list)])

    , ("sort",[F ({-# SCC "sort"      #-}FPS.sort fps)])

    , ("maximum",[F ({-# SCC "maximum"   #-}FPS.maximum fps)])
--            ,F (maximum list)])

    , ("minimum",[F ({-# SCC "minimum"   #-}FPS.minimum fps)])
--            ,F (minimum list)])

    , ("replicate",[F ({-# SCC "replicate" #-}FPS.replicate 10000000 'x')])
--            ,F (replicate 1000000 'x')])

    , ("elem",[F ({-# SCC "elem"      #-}FPS.elem 'z' fps)])
--            ,F (SPS.elem 'z' sps)
--            ,F (PS.elemPS 'z' ps)
 --           ,F (elem 'z' list)])

    , ("find",[F ({-# SCC "find"      #-}FPS.find (=='z') fps)])
--            ,F (find (=='z') list)])

    , ("elemIndex",[F ({-# SCC "elemIndex" #-}FPS.elemIndex 'z' fps)])
--                 ,F (elemIndex 'z' list)])

    , ("findIndex",[F ({-# SCC "findIndex" #-}FPS.findIndex (=='z') fps)])

    , ("elemIndices",[F ({-# SCC "elemIndicies" #-} FPS.elemIndices 'z' fps)])
--                    ,F (elemIndices 'z' list)])

    , ("findIndices",[F ({-# SCC "findIndicies" #-} FPS.findIndices (=='z') fps)])

    , ("splitAt",[F ({-# SCC "splitAt" #-} FPS.splitAt 10000 fps)])

------------------------------------------------------------------------

    , ("lineIndices",[F ({-# SCC "lineIndicies" #-} FPS.lineIndices fps)])
    , ("breakOn",[F ({-# SCC "breakOn" #-} FPS.breakOn 'z' fps)])
    , ("breakSpace",[F ({-# SCC "breakSpace" #-} FPS.breakSpace fps)])
    , ("splitWith",[F ({-# SCC "splitWith" #-} FPS.splitWith (=='z') fps)])

    , ("dropSpace",[F ({-# SCC "dropSpace" #-} FPS.dropSpace fps)])
    , ("dropSpaceEnd",[F ({-# SCC "dropSpaceEnd" #-} FPS.dropSpaceEnd fps)])

    , ("join2",[F ({-# SCC "join2" #-} FPS.join2 ' ' fps fps')])
    , ("join /",[F ({-# SCC "join" #-} FPS.join (FPS.packChar ' ') [fps,fps'])])

    , ("zip",[F ({-# SCC "zip" #-} FPS.zip fps fps)])
    , ("zipWith",[F ({-# SCC "zipWith" #-} FPS.zipWith ((. ord) . (+) . ord) fps fps)])

    , ("isSubstringOf",  [F ({-# SCC "isSubstringOf" #-} FPS.isSubstringOf (FPS.pack "email news") fps)])

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

instance Forceable FPS.ByteString where
    force v = FPS.length v `seq` return T

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
instance Forceable Ordering

-- used to signal undefinedness
instance Forceable () where force () = return B

------------------------------------------------------------------------
--
-- some large strings to play with
--

fps :: FPS.ByteString
fps = unsafePerformIO $ FPS.mmapFile dict
{-# NOINLINE fps #-}

fps' :: FPS.ByteString
fps' = unsafePerformIO $ FPS.mmapFile dict'
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
