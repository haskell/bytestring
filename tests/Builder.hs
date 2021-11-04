module Builder (testSuite) where

import qualified Data.ByteString.Builder.Tests
import qualified Data.ByteString.Builder.Prim.Tests
import           Test.Tasty (TestTree, testGroup)

testSuite :: TestTree
testSuite = testGroup "Builder"
  [ testGroup "Data.ByteString.Builder"
       Data.ByteString.Builder.Tests.tests

  , testGroup "Data.ByteString.Builder.BasicEncoding"
       Data.ByteString.Builder.Prim.Tests.tests
  ]
