{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lift (testSuite) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty, (===))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Language.Haskell.TH.Syntax as TH

testSuite :: TestTree
#ifdef wasm32_HOST_ARCH
testSuite = testGroup "Skipped, requires -fexternal-interpreter" []
#else
testSuite = testGroup "Lift"
  [ testGroup "strict"
    [ testProperty "normal" $
        let bs = "foobar" :: BS.ByteString in
        bs === $(TH.lift $ BS.pack [102,111,111,98,97,114])

    , testProperty "binary" $
        let bs = "\0\1\2\3\0\1\2\3" :: BS.ByteString in
        bs === $(TH.lift $ BS.pack [0,1,2,3,0,1,2,3])

#if MIN_VERSION_template_haskell(2,16,0)
    , testProperty "typed" $
        let bs = "\0\1\2\3\0\1\2\3" :: BS.ByteString in
        bs === $$(TH.liftTyped $ BS.pack [0,1,2,3,0,1,2,3])
#endif

    , testProperty "literalFromOctetString" $
        let bs = "EHLO" :: BS.ByteString in
        bs === $$(BS.literalFromOctetString "EHLO")

    , testProperty "literalFromHex" $
        let bs = "EHLO" :: BS.ByteString in
        bs === $$(BS.literalFromHex "45484c4F")
    ]

  , testGroup "lazy"
    [ testProperty "normal" $
        let bs = "foobar" :: LBS.ByteString in
        bs === $(TH.lift $ LBS.pack [102,111,111,98,97,114])

    , testProperty "binary" $
        let bs = "\0\1\2\3\0\1\2\3" :: LBS.ByteString in
        bs === $(TH.lift $ LBS.pack [0,1,2,3,0,1,2,3])

#if MIN_VERSION_template_haskell(2,16,0)
    , testProperty "typed" $
        let bs = "\0\1\2\3\0\1\2\3" :: LBS.ByteString in
        bs === $$(TH.liftTyped $ LBS.pack [0,1,2,3,0,1,2,3])
#endif
    ]

  , testGroup "short"
    [ testProperty "normal" $
        let bs = "foobar" :: SBS.ShortByteString in
        bs === $(TH.lift $ SBS.pack [102,111,111,98,97,114])

    , testProperty "binary" $
        let bs = "\0\1\2\3\0\1\2\3" :: SBS.ShortByteString in
        bs === $(TH.lift $ SBS.pack [0,1,2,3,0,1,2,3])

#if MIN_VERSION_template_haskell(2,16,0)
    , testProperty "typed" $
        let bs = "\0\1\2\3\0\1\2\3" :: SBS.ShortByteString in
        bs === $$(TH.liftTyped $ SBS.pack [0,1,2,3,0,1,2,3])
#endif
    ]
  ]
#endif
