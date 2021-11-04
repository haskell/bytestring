module Main (main) where

import Test.Tasty

import qualified Builder
import qualified IsValidUtf8
import qualified LazyHClose
import qualified Lift
import qualified Properties

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Builder.testSuite
  , IsValidUtf8.testSuite
  , LazyHClose.testSuite
  , Lift.testSuite
  , Properties.testSuite
  ]
