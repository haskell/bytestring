{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module PluginTests (testSuite) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.ByteString.Builder
import Data.Word

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.Inspection

import PluginTests.Splices

testSuite :: TestTree
testSuite = testGroup "Inspection plugin tests"
  [ testGroup "Literals" $
    [ testGroup "StrictByteString"
      [ $(hasNoStringyStuff 'pack_strict_foo)
      , $(inspectTest $ 'len_pack_strict_foo === 'literal_three)
      , $(hasNoStringyStuff 'pack_strict_literal)
      , $(inspectTest $ 'len_pack_strict_literal === 'literal_thirtyOne)
      , expectFail $ $(hasNoStringyStuff 'pack_strict_nonAscii)
      , expectFail $ $(hasNoStringyStuff 'pack_strict_literal_nonAscii)
      ]

    , testGroup "Builder"
      [ $(hasNoStringyStuff 'builder_string8_foo)
      , $(hasNoStringyStuff 'builder_string8_literal)
      , $(hasNoStringyStuff 'builder_stringUtf8_foo)
      , $(hasNoStringyStuff 'builder_stringUtf8_literal)
      , $(hasNoStringyStuff 'builder_stringUtf8_nonAscii)
      , $(hasNoStringyStuff 'builder_stringUtf8_literal_nonAscii)
      ]
    ]

  , $(inspectTest $ 'append_pack_replicate_unboxing `hasNoType` ''S.ByteString)
  ]

foo_string_literal :: [Char]
foo_string_literal = "foo"

unicode_string_literal :: [Char]
unicode_string_literal = "\0example\0  ... \xff \x1f530"

pack_strict_foo :: S.ByteString
pack_strict_foo = S8.pack foo_string_literal

len_pack_strict_foo :: Int
len_pack_strict_foo = S.length pack_strict_foo

pack_strict_literal :: S.ByteString
pack_strict_literal = S8.pack "some ascii literal of length 31"

len_pack_strict_literal :: Int
len_pack_strict_literal = S.length pack_strict_literal

pack_strict_nonAscii :: S.ByteString
pack_strict_nonAscii = S8.pack unicode_string_literal

pack_strict_literal_nonAscii :: S.ByteString
pack_strict_literal_nonAscii
  = S8.pack "this\0literal contains\x80\xf0\xff non-ascii characters"

literal_three :: Int
literal_three = 3

literal_thirtyOne :: Int
literal_thirtyOne = 31

builder_string8_foo :: Builder
builder_string8_foo = string8 foo_string_literal

builder_string8_literal :: Builder
builder_string8_literal = string8 "some ascii string literal"

builder_stringUtf8_foo :: Builder
builder_stringUtf8_foo = stringUtf8 foo_string_literal

builder_stringUtf8_literal :: Builder
builder_stringUtf8_literal = stringUtf8 "some other ascii string literal"

builder_stringUtf8_nonAscii :: Builder
builder_stringUtf8_nonAscii = stringUtf8 unicode_string_literal

builder_stringUtf8_literal_nonAscii :: Builder
builder_stringUtf8_literal_nonAscii
  = stringUtf8 "inline literal string containing \0special\0 and non-ASCII characters like \x2139"

append_pack_replicate_unboxing :: Int -> Word8 -> Int
append_pack_replicate_unboxing n c
  = S.count c $ S.append (S.pack [0..c]) (S.replicate n c)

