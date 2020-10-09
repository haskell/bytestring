
{-# LANGUAGE CPP #-}
module Properties.IO
    ( io_tests
) where

import Data.List
import Data.String
import System.IO.Unsafe
import Control.Concurrent
import System.Directory
import Control.Exception

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Short      as Short

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.Char8 as D

import Test.QuickCheck
import QuickCheckUtils
#if defined(HAVE_TEST_FRAMEWORK)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
#else
import TestFramework
#endif

import Shared

io_tests = testGroup "morally sound IO tests"
    [ testProperty "readFile.writeFile" prop_read_write_file_P
    , testProperty "readFile.writeFile" prop_read_write_file_C
    , testProperty "readFile.writeFile" prop_read_write_file_L
    , testProperty "readFile.writeFile" prop_read_write_file_D

    , testProperty "appendFile        " prop_append_file_P
    , testProperty "appendFile        " prop_append_file_C
    , testProperty "appendFile        " prop_append_file_L
    , testProperty "appendFile        " prop_append_file_D

    , testProperty "packAddress       " prop_packAddress

    ]


------------------------------------------------------------------------
-- IO

prop_read_write_file_P x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do P.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- P.readFile f
                    return (x==y))

prop_read_write_file_C x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do C.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- C.readFile f
                    return (x==y))

prop_read_write_file_L x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do L.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- L.readFile f
                    return (x==y))

prop_read_write_file_D x = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do D.writeFile f x)
        (const $ do removeFile f)
        (const $ do y <- D.readFile f
                    return (x==y))

------------------------------------------------------------------------

prop_append_file_P x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do P.writeFile f x
            P.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- P.readFile f
                    return (z==(x `P.append` y)))

prop_append_file_C x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do C.writeFile f x
            C.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- C.readFile f
                    return (z==(x `C.append` y)))

prop_append_file_L x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do L.writeFile f x
            L.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- L.readFile f
                    return (z==(x `L.append` y)))

prop_append_file_D x y = unsafePerformIO $ do
    tid <- myThreadId
    let f = "qc-test-"++show tid
    bracket
        (do D.writeFile f x
            D.appendFile f y)
        (const $ do removeFile f)
        (const $ do z <- D.readFile f
                    return (z==(x `D.append` y)))

prop_packAddress = C.pack "this is a test"
            ==
                   C.pack "this is a test"