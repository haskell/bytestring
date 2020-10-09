{-# LANGUAGE CPP #-}
module Properties.Misc
    ( misc_tests -- :: Test
) where

import Control.Monad ( forM_ )
import Data.Word (Word8)

import GHC.Ptr (Ptr(..))

import System.IO.Unsafe (unsafePerformIO)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal (free, copyArray, mallocArray0)
import Foreign.Storable (Storable(pokeElemOff))
import Foreign (Storable(peekElemOff), castPtr)

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..))

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
import qualified Data.ByteString.Char8      as C

import qualified Data.ByteString.Lazy.Char8 as LC

import qualified Data.ByteString.Lazy.Internal as L

import Test.QuickCheck
import QuickCheckUtils
#if defined(HAVE_TEST_FRAMEWORK)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
#else
import TestFramework
#endif 

misc_tests = testGroup "Miscellaneous Tests"
    [ testProperty "packunpack (bytes)"     prop_packunpack_s
    , testProperty "unpackpack (bytes)"     prop_unpackpack_s
    , testProperty "packunpack (chars)"     prop_packunpack_c
    , testProperty "unpackpack (chars)"     prop_unpackpack_c
    , testProperty "packunpack (lazy bytes)" prop_packunpack_l
    , testProperty "unpackpack (lazy bytes)" prop_unpackpack_l
    , testProperty "packunpack (lazy chars)" prop_packunpack_lc
    , testProperty "unpackpack (lazy chars)" prop_unpackpack_lc
    , testProperty "unpack (bytes)"         prop_unpack_s
    , testProperty "unpack (chars)"         prop_unpack_c
    , testProperty "unpack (lazy bytes)"    prop_unpack_l
    , testProperty "unpack (lazy chars)"    prop_unpack_lc
    , testProperty "packUptoLenBytes"       prop_packUptoLenBytes
    , testProperty "packUptoLenChars"       prop_packUptoLenChars
    , testProperty "unpackBytes"            prop_unpackBytes
    , testProperty "unpackChars"            prop_unpackChars
    , testProperty "unpackBytes"            prop_unpackBytes_l
    , testProperty "unpackChars"            prop_unpackChars_l
    , testProperty "unpackAppendBytesLazy"  prop_unpackAppendBytesLazy
    , testProperty "unpackAppendCharsLazy"  prop_unpackAppendCharsLazy
    , testProperty "unpackAppendBytesStrict"prop_unpackAppendBytesStrict
    , testProperty "unpackAppendCharsStrict"prop_unpackAppendCharsStrict
    , testProperty "toFromChunks"           prop_toFromChunks
    , testProperty "fromToChunks"           prop_fromToChunks
    , testProperty "toFromStrict"           prop_toFromStrict
    , testProperty "fromToStrict"           prop_fromToStrict

    , testProperty "invariant"              prop_invariant
    , testProperty "unsafe pack address"    prop_unsafePackAddress
    , testProperty "unsafe pack address len"prop_unsafePackAddressLen
    , testProperty "unsafeUseAsCString"     prop_unsafeUseAsCString
    , testProperty "unsafeUseAsCStringLen"  prop_unsafeUseAsCStringLen
    , testProperty "useAsCString"           prop_useAsCString
    , testProperty "packCString"            prop_packCString
    , testProperty "packCString safe"       prop_packCString_safe
    , testProperty "packCStringLen"         prop_packCStringLen
    , testProperty "packCStringLen safe"    prop_packCStringLen_safe
    , testProperty "packCStringFinaliser"   prop_packCStringFinaliser
    , testProperty "packMallocString"       prop_packMallocCString
    , testProperty "unsafeFinalise"         prop_unsafeFinalize
    , testProperty "invariant"              prop_internal_invariant
    , testProperty "fromForeignPtr"         prop_fromForeignPtr
    , testProperty "show 1"                 prop_showP1
    , testProperty "show 2"                 prop_showL1
    , testProperty "read 1"                 prop_readP1
    , testProperty "read 2"                 prop_readP2
    , testProperty "read 3"                 prop_readL1
    , testProperty "read 4"                 prop_readL2
    ]

prop_packunpack_s x = (P.unpack . P.pack) x == x
prop_unpackpack_s x = (P.pack . P.unpack) x == x

prop_packunpack_c (String8 x) = (C.unpack . C.pack) x == x
prop_unpackpack_c          x  = (C.pack . C.unpack) x == x

prop_packunpack_l x = (L.unpack . L.pack) x == x
prop_unpackpack_l x = (L.pack . L.unpack) x == x

prop_packunpack_lc (String8 x) = (LC.unpack . LC.pack) x == x
prop_unpackpack_lc          x  = (LC.pack . LC.unpack) x == x

prop_unpack_s cs =
    forAll (choose (0, length cs)) $ \n ->
      P.unpack (P.drop n $ P.pack cs) == drop n cs
prop_unpack_c (String8 cs) =
    forAll (choose (0, length cs)) $ \n ->
      C.unpack (C.drop n $ C.pack cs) == drop n cs

prop_unpack_l  cs =
    forAll (choose (0, length cs)) $ \n ->
      L.unpack (L.drop (fromIntegral n) $ L.pack cs) == drop n cs
prop_unpack_lc (String8 cs) =
    forAll (choose (0, length cs)) $ \n ->
      LC.unpack (L.drop (fromIntegral n) $ LC.pack cs) == drop n cs

prop_packUptoLenBytes cs =
    forAll (choose (0, length cs + 1)) $ \n ->
      let (bs, cs') = P.packUptoLenBytes n cs
       in P.length bs == min n (length cs)
       && take n cs == P.unpack bs
       && P.pack (take n cs) == bs
       && drop n cs == cs'

prop_packUptoLenChars (String8 cs) =
    forAll (choose (0, length cs + 1)) $ \n ->
      let (bs, cs') = P.packUptoLenChars n cs
       in P.length bs == min n (length cs)
       && take n cs == C.unpack bs
       && C.pack (take n cs) == bs
       && drop n cs == cs'

prop_unpackBytes cs =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackBytes (P.drop n $ P.pack cs) == drop n cs
prop_unpackChars (String8 cs) =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackChars (P.drop n $ C.pack cs) == drop n cs

prop_unpackBytes_l =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \cs ->
    forAll (choose (0, length cs)) $ \n ->
      L.unpackBytes (L.drop (fromIntegral n) $ L.pack cs) == drop n cs
prop_unpackChars_l =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \(String8 cs) ->
    forAll (choose (0, length cs)) $ \n ->
      L.unpackChars (L.drop (fromIntegral n) $ LC.pack cs) == drop n cs

prop_unpackAppendBytesLazy cs' =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \cs ->
    forAll (choose (0, 2)) $ \n ->
      P.unpackAppendBytesLazy (P.drop n $ P.pack cs) cs' == drop n cs ++ cs'
prop_unpackAppendCharsLazy (String8 cs') =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \(String8 cs) ->
    forAll (choose (0, 2)) $ \n ->
      P.unpackAppendCharsLazy (P.drop n $ C.pack cs) cs' == drop n cs ++ cs'

prop_unpackAppendBytesStrict cs cs' =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackAppendBytesStrict (P.drop n $ P.pack cs) cs' == drop n cs ++ cs'

prop_unpackAppendCharsStrict (String8 cs) (String8 cs') =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackAppendCharsStrict (P.drop n $ C.pack cs) cs' == drop n cs ++ cs'

prop_toFromChunks x = (L.fromChunks . L.toChunks) x == x
prop_fromToChunks x = (L.toChunks . L.fromChunks) x == filter (not . P.null) x

prop_toFromStrict x = (L.fromStrict . L.toStrict) x == x
prop_fromToStrict x = (L.toStrict . L.fromStrict) x == x


invariant :: L.ByteString -> Bool
invariant Empty       = True
invariant (Chunk c cs) = not (P.null c) && invariant cs

prop_invariant = invariant


------------------------------------------------------------------------
-- Unsafe functions

-- Test unsafePackAddress
prop_unsafePackAddress (CByteString x) = unsafePerformIO $ do
        let (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddress addr
        return (y == x)

-- Test unsafePackAddressLen
prop_unsafePackAddressLen x = unsafePerformIO $ do
        let i = P.length x
            (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddressLen i addr
        return (y == x)

prop_unsafeUseAsCString x = unsafePerformIO $ do
        let n = P.length x
        y <- P.unsafeUseAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_unsafeUseAsCStringLen x = unsafePerformIO $ do
        let n = P.length x
        y <- P.unsafeUseAsCStringLen x $ \(cstr,_) ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_useAsCString x = unsafePerformIO $ do
        let n = P.length x
        y <- P.useAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_packCString (CByteString x) = unsafePerformIO $ do
        y <- P.useAsCString x $ P.unsafePackCString
        return (y == x)

prop_packCString_safe (CByteString x) = unsafePerformIO $ do
        y <- P.useAsCString x $ P.packCString
        return (y == x)

prop_packCStringLen x = unsafePerformIO $ do
        y <- P.useAsCStringLen x $ P.unsafePackCStringLen
        return (y == x && P.length y == P.length x)

prop_packCStringLen_safe x = unsafePerformIO $ do
        y <- P.useAsCStringLen x $ P.packCStringLen
        return (y == x && P.length y == P.length x)

prop_packCStringFinaliser x = unsafePerformIO $ do
        y <- P.useAsCString x $ \cstr -> P.unsafePackCStringFinalizer (castPtr cstr) (P.length x) (return ())
        return (y == x)

prop_packMallocCString (CByteString x) = unsafePerformIO $ do
         let (fp,_,_) = P.toForeignPtr x
         ptr <- mallocArray0 (P.length x) :: IO (Ptr Word8)
         forM_ [0 .. P.length x] $ \n -> pokeElemOff ptr n 0
         withForeignPtr fp $ \qtr -> copyArray ptr qtr (P.length x)
         y   <- P.unsafePackMallocCString (castPtr ptr)

         let !z = y == x
         free ptr `seq` return z

prop_unsafeFinalize    x =
    P.length x > 0 ==>
      unsafePerformIO $ do
        x <- P.unsafeFinalize x
        return (x == ())

prop_internal_invariant x = L.invariant x

prop_fromForeignPtr x = (let (a,b,c) = (P.toForeignPtr x)
                                in P.fromForeignPtr a b c) == x

------------------------------------------------------------------------
-- More functions

prop_showP1 x = show x == show (C.unpack x)
prop_showL1 x = show x == show (LC.unpack x)

prop_readP1 x = read (show x) == (x :: P.ByteString)
prop_readP2 x = read (show x) == C.pack (x :: String)

prop_readL1 x = read (show x) == (x :: L.ByteString)
prop_readL2 x = read (show x) == LC.pack (x :: String)
