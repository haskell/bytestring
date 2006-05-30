{-# OPTIONS -cpp -fglasgow-exts #-}
module BenchUtils where

--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--

import Data.ByteString (ByteString)
import qualified Data.ByteString as P
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString as L

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

run tests = sequence_ $ zipWith doit [1..] tests

doit :: Int -> (String, [F]) -> IO ()
doit n (s,ls) = do
    printf "%2d " n
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

#if !defined(HEAD)
instance Forceable P.ByteString where
    force v = P.length v `seq` return T
#endif

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

fps :: P.ByteString
fps = unsafePerformIO $ P.readFile dict
{-# NOINLINE fps #-}

fps' :: P.ByteString
fps' = unsafePerformIO $ P.readFile dict'
{-# NOINLINE fps' #-}

lps :: L.ByteString
lps = unsafePerformIO $ L.readFile dict
{-# NOINLINE lps #-}

lps' :: L.ByteString
lps' = unsafePerformIO $ L.readFile dict'
{-# NOINLINE lps' #-}

dict = "bigdata"
dict' = "data"


-- Some short hand.
type X = Int
type W = Word8
type P = P.ByteString
type B = L.ByteString
