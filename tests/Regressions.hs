{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (SomeException, handle)
import qualified Data.ByteString as B
import Test.Tasty
import Test.Tasty.HUnit

-- Try to generate arguments to concat that are big enough to cause an
-- Int to overflow.
concat_overflow :: IO ()
concat_overflow =
    handle (\(_::SomeException) -> return ()) $
    B.concat (replicate lsize (B.replicate bsize 0)) `seq`
    assertFailure "T.replicate should crash"
  where
    (lsize, bsize) | maxBound == (2147483647::Int) = (2^14, 2^18)
                   | otherwise                     = (2^34, 2^29)

tests :: TestTree
tests = testCase "concat_overflow" concat_overflow

main = defaultMain tests
