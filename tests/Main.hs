{-# LANGUAGE CPP #-}

module Main (main) where

import Test.Tasty

import qualified Builder
import qualified IsValidUtf8
import qualified LazyHClose
import qualified Lift
import qualified Properties
#if BYTESTRING_PLUGIN_TESTS
import qualified PluginTests
#endif

main :: IO ()
main = defaultMain $ testGroup "All"
  [ Builder.testSuite
  , IsValidUtf8.testSuite
  , LazyHClose.testSuite
  , Lift.testSuite
  , Properties.testSuite
#if BYTESTRING_PLUGIN_TESTS
  , PluginTests.testSuite
#endif
  ]
