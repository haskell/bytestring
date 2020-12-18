module Main where

import qualified Data.ByteString.Builder.Tests
import qualified Data.ByteString.Builder.Prim.Tests
import           Test.Tasty (defaultMain, TestTree, testGroup)

main :: IO ()
main = defaultMain $ testGroup "All" tests

tests :: [TestTree]
tests =
  [ testGroup "Data.ByteString.Builder"
       Data.ByteString.Builder.Tests.tests

  , testGroup "Data.ByteString.Builder.BasicEncoding"
       Data.ByteString.Builder.Prim.Tests.tests
  ]
