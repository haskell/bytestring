module LazyHClose (testSuite) where

import Control.Monad (void, forM_)
import Data.ByteString.Internal (toForeignPtr)
import Foreign.C.String (withCString)
import Foreign.ForeignPtr (finalizeForeignPtr)
import System.IO (openFile, openTempFile, hClose, hPutStrLn, IOMode(..))
import System.Posix.Internals (c_unlink)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.QuickCheck (testProperty, ioProperty)

import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8

n :: Int
n = 1000

testSuite :: TestTree
testSuite = withResource
  (do (fn, h) <- openTempFile "." "lazy-hclose-test.tmp"; hPutStrLn h "x"; hClose h; pure fn)
  removeFile $ \fn' ->
    testGroup "LazyHClose"
    [ testProperty "Testing resource leaks for Strict.readFile" $ ioProperty $
      forM_ [1..n] $ const $ do
        fn <- fn'
        r <- S.readFile fn
        appendFile fn "" -- will fail, if fn has not been closed yet

    , testProperty "Testing resource leaks for Lazy.readFile" $ ioProperty $
      forM_ [1..n] $ const $ do
        fn <- fn'
        r <- L.readFile fn
        L.length r `seq` return ()
        appendFile fn "" -- will fail, if fn has not been closed yet

    , testProperty "Testing resource leaks when converting lazy to strict" $ ioProperty $
      forM_ [1..n] $ const $ do
        fn <- fn'
        let release c = finalizeForeignPtr fp where (fp,_,_) = toForeignPtr c
        r <- L.readFile fn
        mapM_ release (L.toChunks r)
        appendFile fn "" -- will fail, if fn has not been closed yet

    , testProperty "Testing strict hGetContents" $ ioProperty $
      forM_ [1..n] $ const $ do
        fn <- fn'
        h <- openFile fn ReadMode
        r <- S.hGetContents h
        S.last r `seq` return ()
        appendFile fn "" -- will fail, if fn has not been closed yet

    , testProperty "Testing Free lazy hGetContents" $ ioProperty $
      forM_ [1..n] $ const $ do
        fn <- fn'
        h <- L.openBinaryFile fn ReadMode
        r <- L.hGetContents h
        L.last r `seq` return ()
        appendFile fn "" -- will fail, if fn has not been closed yet
    , testProperty "Testing With lazy hGetContents" $ ioProperty $
      forM_ [1..n] $ const $ do
        fn <- fn'
        L.withBinaryFile fn ReadMode $
          \h -> do
            r <- L.hGetContents h
            L.last r `seq` return ()
        appendFile fn "" -- will fail, if fn has not been closed yet
    , testProperty "Testing lazy withBinaryFile seq result" $ ioProperty $
      forM_ [1..n] $ const $ do
        fn <- fn'
        r <- L.withBinaryFile fn ReadMode L.hGetContents
        L.last r `seq` return ()
        appendFile fn "" -- will fail, if fn has not been closed yet
    ]

removeFile :: String -> IO ()
removeFile fn = void $ withCString fn c_unlink
