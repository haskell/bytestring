{-# OPTIONS -fglasgow-exts #-}

--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L

import Data.List
import Data.Char
import Data.Word
import Data.Int

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
    force (fps,fps') >> force (lps,lps')

    printf "# Size of test data: %dk\n" ((floor $ (fromIntegral (B.length fps)) / 1024) :: Int)
    printf "#Byte\t Lazy\n"

    -- now get to it
    sequence_ . map doit $ zip [(1::Int)..] tests

doit (n,(s,ls)) = do
    printf "%d " n
    fn ls
    printf "\t# %-16s\n" (show s)
    hFlush stdout
  where fn xs = case xs of
                    [x,y]   -> run x >> run y
                    [x]     -> run x >> printf "\t"
                    _       -> return ()
        run s = time s >> performGC >> threadDelay 100

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

      ("++",    [F ({-# SCC  "append" #-}B.append fps fps')
                ,F ({-# SCC "lappend" #-}L.append lps lps')])

    , ("concat",[F ({-# SCC "concat"  #-}B.concat [fps,fps'])
                ,F ({-# SCC "lconcat" #-}L.concat [lps,lps'])])

    , ("length",[F ({-# SCC "length"  #-}B.length fps)
                ,F ({-# SCC "llength" #-}L.length lps)])

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
                 ,F ({-# SCC "lcompare" #-}compare lps lps')])

    , ("index", [F ({-# SCC "index" #-}B.index fps 10000000)
                 ,F ({-# SCC "lindex" #-}L.index lps 10000000)])

    , ("map", [F ({-# SCC "map" #-}B.map (+1) fps)
              ,F (L.map (+1) lps)])

    , ("filter", [F ({-# SCC "filter" #-}B.filter (/=101) fps)
                 ,F (L.filter (/=101) lps)])

    , ("filterNotByte", [F ({-# SCC "filterNotChar" #-}B.filterNotByte 101 fps)
                 ,F (L.filterNotByte 101 lps)])

    , ("filterByte", [F ({-# SCC "filterChar"    #-}B.filterByte 103 fps)
                 ,F (L.filterByte 103 lps)])

    , ("findIndex",[F ({-# SCC "findIndex" #-}B.findIndex (==126) fps)
                   ,F (L.findIndex (==126) lps)])

   ,  ("find",     [F ({-# SCC "find"      #-}B.find (==126) fps)
                   ,F (L.find (==126) lps)])

    , ("foldl", [F ({-# SCC "fold" #-} B.foldl (\a w -> a+1::Int) 0 fps)
                ,F ({-# SCC "lfold" #-} L.foldl (\a w -> a+1::Int) 0 lps)])


    , ("take",[F ({-# SCC "take"      #-}B.take 100000 fps)
                ,F ({-# SCC "ltake" #-} L.take 100000 lps)])

    , ("drop",[F ({-# SCC "drop"      #-}B.drop 100000 fps)
                ,F ({-# SCC "ldrop" #-} L.drop 100000 lps)])


    , ("takeWhile",[F ({-# SCC "takeWhile" #-}B.takeWhile (/=122) fps)
                   ,F (L.takeWhile (==122) lps)])

    , ("dropWhile",[F ({-# SCC "dropWhile" #-}B.dropWhile (/=122) fps)
                 ,F (L.dropWhile (/=122) lps)])


    , ("span",[F ({-# SCC "span"      #-}B.span (/=122) fps)
                 ,F (L.span (/=122) lps)])

    , ("break",[F ({-# SCC "break"     #-}B.break (==122) fps)
               ,F (L.break (==122) lps)])

    , ("split",[F ({-# SCC "split"     #-}B.split 0x0a fps)
               ,F (L.split 0x0a lps)])

    , ("breakByte",[F ({-# SCC "breakChar" #-} B.breakByte 122 fps)
               ,F (L.breakByte 122 lps)])

    , ("spanByte",[F ({-# SCC "spanChar" #-} B.spanByte 122 fps)
               ,F (L.spanByte 122 lps)])

--  , ("group",[F ({-# SCC "group"   #-}B.group fps)])
--            ,F (L.group lps)])

--  , ("groupBy",[F ({-# SCC "groupBy"   #-}B.groupBy (==) fps)])
--            ,F (L.groupBy (==) lps)])

    , ("reverse",[F ({-# SCC "reverse"   #-}B.reverse fps)
              ,F (L.reverse lps)])

    , ("cons",[F ({-# SCC "cons"      #-}B.cons 120 fps)
              ,F (L.cons 120 lps)])

    , ("snoc",[F ({-# SCC "snoc"      #-}B.snoc fps 120)
              ,F (L.snoc lps 120)])

    , ("empty",[F ({-# SCC "empty"     #-}B.empty)
              ,F (L.empty)])

    , ("head",[F ({-# SCC "head"      #-}B.head fps)
              ,F (L.head lps)])


    , ("tail",[F ({-# SCC "tail"      #-}B.tail fps)
              ,F (L.tail lps)])


    , ("last",[F ({-# SCC "last"      #-}B.last fps)
              ,F (L.last lps)])

    , ("init",[F ({-# SCC "init"      #-}B.init fps)
              ,F (L.init lps)])

    , ("count",  [F ({-# SCC "count" #-} B.count 10 fps)
              ,F (L.count 10 lps)])

    , ("isPrefixOf",  [F ({-# SCC "isPrefixOf" #-}
                            B.isPrefixOf (C.pack "The Project Gutenberg eBook") fps)
                      ,F (L.isPrefixOf (L.pack [84,104,101,32,80,114,111,106,101
                                               ,99,116,32,71,117,116,101,110,98
                                               ,101,114,103,32,101,66,111,111,107]) lps)])

--  , ("inits",[F ({-# SCC "inits"     #-}B.inits fps)])

--  , ("tails",[F ({-# SCC "tails"     #-}B.tails fps)])

--  , ("transpose",[F ({-# SCC "transpose" #-}B.transpose [fps,fps'])])

    , ("join",[F ({-# SCC "join" #-}B.join (B.pack [1,2,3]) [fps,fps'])
              ,F (L.join (L.pack [1,2,3]) [lps,lps'])])

    , ("any",[F ({-# SCC "any"       #-}B.any (==120) fps)
                 ,F (L.any (==120) lps)])

    , ("all",[F ({-# SCC "all"       #-}B.all (==120) fps)
                 ,F (L.all (==120) lps)])

    , ("maximum",[F ({-# SCC "maximum"   #-}B.maximum fps)
                 ,F (L.maximum lps)])

    , ("minimum",[F ({-# SCC "minimum"   #-}B.minimum fps)
                 ,F (L.minimum lps)])

    , ("elem",[F ({-# SCC "elem"      #-}B.elem 122 fps)
                 ,F (L.elem 122 lps)])

    , ("notElem",[F ({-# SCC "notElem"      #-}B.notElem 122 fps)
                 ,F (L.notElem 122 lps)])

    , ("elemIndex",[F ({-# SCC "elemIndex" #-}B.elemIndex 122 fps)
                 ,F (L.elemIndex 122 lps)])

    , ("findIndices",[F ({-# SCC "findIndicies" #-} B.findIndices (==122) fps)
                 ,F (L.findIndices (==122) lps)])

    , ("elemIndices",[F ({-# SCC "elemIndicies" #-} B.elemIndices 122 fps)
                 ,F (L.elemIndices 122 lps)])

    , ("splitAt",[F ({-# SCC "splitAt" #-} B.splitAt 10000 fps)
                 ,F (L.splitAt 10000 lps)])

    , ("splitWith",[F ({-# SCC "splitWith" #-} B.splitWith (==122) fps)
                 ,F (L.splitWith (==122) lps)])

------------------------------------------------------------------------
--
-- Char8 or ByteString only

    , ("intersperse",[F ({-# SCC "intersperse" #-}B.intersperse 120 fps)])

    , ("replicate",[F ({-# SCC "replicate" #-}B.replicate 10000000 120)])

    , ("sort",[F ({-# SCC "sort"      #-}B.sort fps)])

    , ("lineIndices",[F ({-# SCC "lineIndicies" #-} C.lineIndices fps)])

    , ("elemIndexLast",[F ({-# SCC "elemIndexLast" #-}B.elemIndexLast 122 fps)])

    , ("breakSpace",[F ({-# SCC "breakSpace" #-} C.breakSpace fps)])

    , ("dropSpace",[F ({-# SCC "dropSpace" #-} C.dropSpace fps)])
    , ("dropSpaceEnd",[F ({-# SCC "dropSpaceEnd" #-} C.dropSpaceEnd fps)])

    , ("joinWithByte",[F ({-# SCC "joinWithByte" #-} B.joinWithByte 32 fps fps')])

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

    , ("addr1",  [F (let s = "my\nstring\nhaskell"# in B.length (C.packAddress s) == 17)])
    , ("addr2",  [F (let s = "my\nstring\nhaskell"# in C.unsafePackAddress 17 s == C.packAddress s)])

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

instance Forceable B.ByteString where
    force v = B.length v `seq` return T

instance Forceable L.ByteString where
    force v = L.length v `seq` return T

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
instance Forceable Int64
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

fps :: B.ByteString
fps = unsafePerformIO $ B.readFile dict
{-# NOINLINE fps #-}

fps' :: B.ByteString
fps' = unsafePerformIO $ B.readFile dict'
{-# NOINLINE fps' #-}

lps :: L.ByteString
lps = unsafePerformIO $ L.readFile dict
{-# NOINLINE lps #-}

lps' :: L.ByteString
lps' = unsafePerformIO $ L.readFile dict'
{-# NOINLINE lps' #-}

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
