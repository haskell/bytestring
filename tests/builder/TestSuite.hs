module Main where

--import           Test.Framework (defaultMain, Test, testGroup)

import qualified Data.ByteString.Builder.Tests
import qualified Data.ByteString.Builder.Prim.Tests
import           TestFramework


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Data.ByteString.Builder"
       Data.ByteString.Builder.Tests.tests

  , testGroup "Data.ByteString.Lazy.Builder.BasicEncoding"
       Data.ByteString.Builder.Prim.Tests.tests
  ]

