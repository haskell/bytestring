{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@=?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Language.Haskell.TH.Syntax as TH

main :: IO ()
main = defaultMain $ testGroup "bytestring-th"
  [ testGroup "strict"
    [ testCase "normal" $ do
        let bs :: BS.ByteString
            bs = "foobar"

        bs @=? $(TH.lift $ BS.pack [102,111,111,98,97,114])

    , testCase "binary" $ do
        let bs :: BS.ByteString
            bs = "\0\1\2\3\0\1\2\3"

        bs @=? $(TH.lift $ BS.pack [0,1,2,3,0,1,2,3])

#if MIN_VERSION_template_haskell(2,16,0)
    , testCase "typed" $ do
        let bs :: BS.ByteString
            bs = "\0\1\2\3\0\1\2\3"

        bs @=? $$(TH.liftTyped $ BS.pack [0,1,2,3,0,1,2,3])
#endif
    ]

  , testGroup "lazy"
    [ testCase "normal" $ do
        let bs :: LBS.ByteString
            bs = "foobar"

        bs @=? $(TH.lift $ LBS.pack [102,111,111,98,97,114])

    , testCase "binary" $ do
        let bs :: LBS.ByteString
            bs = "\0\1\2\3\0\1\2\3"

        -- print $ LBS.unpack bs
        -- print $ LBS.unpack $(TH.lift $ LBS.pack [0,1,2,3,0,1,2,3])

        bs @=? $(TH.lift $ LBS.pack [0,1,2,3,0,1,2,3])

#if MIN_VERSION_template_haskell(2,16,0)
    , testCase "typed" $ do
        let bs :: LBS.ByteString
            bs = "\0\1\2\3\0\1\2\3"

        bs @=? $$(TH.liftTyped $ LBS.pack [0,1,2,3,0,1,2,3])
#endif
    ]

  , testGroup "short"
    [ testCase "normal" $ do
        let bs :: SBS.ShortByteString
            bs = "foobar"

        bs @=? $(TH.lift $ SBS.pack [102,111,111,98,97,114])

    , testCase "binary" $ do
        let bs :: SBS.ShortByteString
            bs = "\0\1\2\3\0\1\2\3"

        bs @=? $(TH.lift $ SBS.pack [0,1,2,3,0,1,2,3])

#if MIN_VERSION_template_haskell(2,16,0)
    , testCase "typed" $ do
        let bs :: SBS.ShortByteString
            bs = "\0\1\2\3\0\1\2\3"

        bs @=? $$(TH.liftTyped $ SBS.pack [0,1,2,3,0,1,2,3])
#endif
    ]
  ]
