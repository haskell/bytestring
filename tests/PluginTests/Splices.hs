{-# LANGUAGE TemplateHaskellQuotes #-}

module PluginTests.Splices where

import Test.Tasty
import Test.Tasty.Inspection
import Language.Haskell.TH

import GHC.Base
  ( unpackCString#
  , unpackAppendCString#
  , unpackCStringUtf8#
  , unpackAppendCStringUtf8#
  , unpackNBytes#
  , unpackFoldrCString#
  , unpackFoldrCStringUtf8#
  )
import Language.Haskell.TH (Name)

unpackCString_functions_without_foldr :: [Name]
unpackCString_functions_without_foldr =
  [ 'unpackCString#
  , 'unpackAppendCString#
  , 'unpackCStringUtf8#
  , 'unpackAppendCStringUtf8#
  , 'unpackNBytes#
  ]

unpackCString_functions_all :: [Name]
unpackCString_functions_all =
  [ 'unpackFoldrCString#
  , 'unpackFoldrCStringUtf8#
  ] ++ unpackCString_functions_without_foldr

hasNoStringyStuff :: Name -> Q Exp
hasNoStringyStuff n = flip inspectObligations n
  [ (`hasNoTypes` [''Char, ''[]])
  , (`doesNotUseAnyOf` unpackCString_functions_all)
  ]

hasNoStringyStuffExceptFolds :: Name -> Q Exp
hasNoStringyStuffExceptFolds n = flip inspectObligations n
  [ (`hasNoTypes` [''Char, ''[]])
  , (`doesNotUseAnyOf` unpackCString_functions_without_foldr)
  ]
