{-# OPTIONS -fglasgow-exts #-}

import qualified Data.PackedString     as PS
import qualified Data.FastPackedString as FPS
import qualified SimonPackedString     as SPS
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
    force (fps,fps') >> force (sps,sps') >> force (ps,ps') >> force (list,list')

    printf "Size of test data: %dk\n" ((floor $ (fromIntegral (FPS.length fps)) / 1024) :: Int)
    printf "                FPS\tSPS\tPS\t[a]\n"

    -- now get to it
    sequence_ . map doit $ tests

doit (s,ls) = do
    printf "%-16s" s
    mapM_ (\s -> time s >> performGC) ls
    putChar '\n'

time :: F -> IO ()
time (F a) = do
    start <- getCPUTime
    v     <- force a
    case v of
        B -> printf "--\t"
        _ -> do
            end   <- getCPUTime
            let diff = (fromIntegral (end - start)) / (10^12)
            printf "%0.3f\t" (diff :: Double) >> hFlush stdout

-- compare the 3 implementations
tests  :: [(String,[F])]

tests =
    [ ("++",    [F (FPS.append fps fps)
                ,F () {-F (SPS.append sps sps)-}
                ,F () -- (PS.appendPS ps ps)
                ,F (list ++ list)])

    , ("length",[F (FPS.length fps)
                ,F (SPS.length sps)
                ,F (PS.lengthPS ps)
                ,F (length list)])

    , ("pack",  [F (FPS.pack list)
                ,F (SPS.pack list)
                ,F (PS.packString list)
                ,F ()])

    , ("unpack",[F (FPS.unpack fps)
                ,F (SPS.unpack sps)
                ,F (PS.unpackPS ps) ,F ()])

    , ("compare",[F (compare fps fps')
                 ,F (compare sps sps')
                 ,F (compare ps ps')
                 ,F (compare list list')])

    , ("index", [F (FPS.index fps 10000)
                ,F (SPS.index sps 10000)
                ,F (PS.indexPS ps 10000)
                ,F (list !! 10000)])

    , ("map", [F (FPS.map toUpper fps)
              ,F (SPS.map toUpper sps)
              ,F (PS.mapPS toUpper ps)
              ,F (map toUpper list)])

    , ("filter", [F (FPS.filter (=='f') fps)
                 ,F () {-F (SPS.filter (=='f') sps)-}
                 ,F (PS.filterPS (=='f') ps)
                 ,F (filter (=='f') list)])

    , ("take",[F (FPS.take 100000 fps)
             ,F (SPS.take 100000 sps)
             ,F (PS.takePS 100000 ps)
             ,F (take 100000 list)])

    , ("drop",[F (FPS.drop 100000 fps)
             ,F (SPS.drop 100000 sps)
             ,F (PS.dropPS 100000 ps)
             ,F (drop 100000 list)])

    , ("takeWhile",[F (FPS.takeWhile (/='z') fps)
                   ,F (SPS.takeWhile (/='z') sps)
                   ,F (PS.takeWhilePS (/='z') ps)
                   ,F (takeWhile (/='z') list)])

    , ("dropWhile",[F (FPS.dropWhile (/='z') fps)
                   ,F (SPS.dropWhile (/='z') sps)
                   ,F (PS.dropWhilePS (/='z') ps)
                   ,F (dropWhile (/='z') list)])

    , ("span",[F (FPS.span (/='z') fps)
                   ,F (SPS.span (/='z') sps)
                   ,F (PS.spanPS (/='z') ps)
                   ,F (span (/='z') list)])

    , ("break",[F (FPS.break (=='z') fps)
               ,F (SPS.break (=='z') sps)
               ,F (PS.breakPS (=='z') ps)
               ,F (break (=='z') list)])

    , ("lines",[F (FPS.lines fps)
               ,F (SPS.lines sps)
               ,F (PS.linesPS ps)
               ,F (lines list)])

    , ("unlines",[F (FPS.unlines [fps,fps',fps])
                   ,F () {-F (SPS.unlines [sps,sps',sps])-}
                   ,F (PS.unlinesPS [ps,ps',ps])
                   ,F (unlines [list,list',list])])

    , ("words",[F (FPS.words fps)
                   ,F (SPS.words sps)
                   ,F (PS.wordsPS ps)
                   ,F (words list)])

    , ("unwords",[F (FPS.unwords [fps,fps',fps])
                   ,F () {-(SPS.unwords [sps,sps',sps])-}
                   ,F (PS.unwordsPS [ps,ps',ps])
                   ,F (unwords [list,list',list])])

    , ("reverse",[F (FPS.reverse fps)
                   ,F (SPS.reverse sps)
                   ,F (PS.reversePS ps)
                   ,F (reverse list)])

    , ("concat",[F (FPS.concat [fps,fps'])
                   ,F (SPS.concat [sps,sps'])
                   ,F (PS.concatPS [ps,ps'])
                   ,F (concat [list,list'])])

    , ("cons",[F (FPS.cons 'x' fps)
              ,F (SPS.cons 'x' sps)
              ,F (PS.consPS 'x' ps)
              ,F ('x' : list)])

    , ("snoc",[F (FPS.snoc fps 'x')
              ,F ()
              ,F ()
              ,F ()])

    , ("empty",[F (FPS.empty)
              ,F (SPS.nil)
              ,F (PS.nilPS)
              ,F []])

    , ("head",[F (FPS.head fps)
              ,F (SPS.head sps)
              ,F (PS.headPS ps)
              ,F (head list)])

    , ("tail",[F (FPS.tail fps)
              ,F (SPS.tail sps)
              ,F (PS.tailPS ps)
              ,F (tail list)])

    , ("last",[F (FPS.last fps)
              ,F ()
              ,F ()
              ,F (last list)])

    , ("init",[F (FPS.init fps)
              ,F ()
              ,F ()
              ,F (init list)])

    , ("inits",[F (FPS.inits fps)
              ,F ()
              ,F ()
              ,F ()])

    , ("tails",[F (FPS.tails fps)
              ,F ()
              ,F ()
              ,F (tails list)])

    , ("intersperse",[F (FPS.intersperse 'x' fps)
              ,F ()
              ,F ()
              ,F (intersperse 'x' list)])

    , ("concatMap",[F ()
                  ,F ()
                  ,F ()
                  ,F (concatMap (\c -> [c]) list)])

    , ("any",[F (FPS.any (=='z') fps)
              ,F ()
              ,F ()
              ,F (any (=='z') list)])

    , ("all",[F (FPS.all (=='z') fps)
              ,F ()
              ,F ()
              ,F (all (=='z') list)])

    , ("sort",[F (FPS.sort fps)
              ,F ()
              ,F ()
              ,F (sort list)])

    , ("maximum",[F (FPS.maximum fps)
              ,F ()
              ,F ()
              ,F (maximum list)])

    , ("minimum",[F (FPS.minimum fps)
              ,F ()
              ,F ()
              ,F (minimum list)])

    , ("replicate",[F (FPS.replicate 1000000 'x')
              ,F ()
              ,F ()
              ,F (replicate 1000000 'x')])

    , ("elem",[F (FPS.elem 'z' fps)
              ,F (SPS.elem 'z' sps)
              ,F (PS.elemPS 'z' ps)
              ,F (elem 'z' list)])

    , ("find",[F (FPS.find (=='z') fps)
              ,F ()
              ,F ()
              ,F (find (=='z') list)])

    , ("elemIndex",[F (FPS.elemIndex 'z' fps)
                   ,F ()
                   ,F ()
                   ,F (elemIndex 'z' list)])

    , ("elemIndicies",[F (FPS.elemIndices 'z' fps)
                      ,F ()
                      ,F ()
                      ,F (elemIndices 'z' list)])

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

instance Forceable FPS.FastString where
    force v = FPS.length v `seq` return T

instance Forceable SPS.PackedString where
    force v = SPS.length v `seq` return T

instance Forceable PS.PackedString where
    force v = PS.lengthPS v `seq` return T

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

fps :: FPS.FastString
fps = unsafePerformIO $ FPS.mmapFile dict
{-# NOINLINE fps #-}

fps' :: FPS.FastString
fps' = unsafePerformIO $ FPS.mmapFile dict'
{-# NOINLINE fps' #-}

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

dict = "bigdata"
dict' = "data"

------------------------------------------------------------------------
