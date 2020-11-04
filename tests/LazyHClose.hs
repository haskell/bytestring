module Main (main) where

import Control.Monad (void, forM_)
import Data.ByteString.Internal (toForeignPtr)
import Foreign.C.String (withCString)
import Foreign.ForeignPtr (finalizeForeignPtr)
import System.IO (openFile, IOMode(..))
import System.Posix.Internals (c_unlink)

import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    let n = 1000
        fn = "lazyhclose-test.tmp"

    writeFile fn "x"

    ------------------------------------------------------------------------
    -- readFile tests

    putStrLn "Testing resource leaks for Strict.readFile"
    forM_ [1..n] $ const $ do
         r <- S.readFile fn
         appendFile fn "" -- will fail, if fn has not been closed yet

    putStrLn "Testing resource leaks for Lazy.readFile"
    forM_ [1..n] $ const $ do
         r <- L.readFile fn
         L.length r `seq` return ()
         appendFile fn "" -- will fail, if fn has not been closed yet

    -- manage the resources explicitly.
    putStrLn "Testing resource leaks when converting lazy to strict"
    forM_ [1..n] $ const $ do
         let release c = finalizeForeignPtr fp where (fp,_,_) = toForeignPtr c
         r <- L.readFile fn
         mapM_ release (L.toChunks r)
         appendFile fn "" -- will fail, if fn has not been closed yet

    ------------------------------------------------------------------------
    -- hGetContents tests

    putStrLn "Testing strict hGetContents"
    forM_ [1..n] $ const $ do
         h <- openFile fn ReadMode
         r <- S.hGetContents h
         S.last r `seq` return ()
         appendFile fn "" -- will fail, if fn has not been closed yet

    putStrLn "Testing lazy hGetContents"
    forM_ [1..n] $ const $ do
         h <- openFile fn ReadMode
         r <- L.hGetContents h
         L.last r `seq` return ()
         appendFile fn "" -- will fail, if fn has not been closed yet

    void $ withCString fn c_unlink
