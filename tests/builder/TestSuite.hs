module Main where

import qualified Data.ByteString.Builder.Tests
import qualified Data.ByteString.Builder.Prim.Tests
import           Test.Framework (defaultMain, Test, testGroup)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Data.ByteString.Builder"
       Data.ByteString.Builder.Tests.tests

  , testGroup "Data.ByteString.Builder.BasicEncoding"
       Data.ByteString.Builder.Prim.Tests.tests
  ]
