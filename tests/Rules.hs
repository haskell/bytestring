module Rules where
--
-- Tests to ensure rules are firing.
--

import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString             as P
import qualified Data.ByteString.Lazy        as L
import qualified Data.ByteString.Lazy.Char8  as D
import Data.List
import Data.Char
import Data.Word

import QuickCheckUtils

import Test.Framework.Providers.QuickCheck2

prop_break_C :: Word8 -> C.ByteString -> Bool
prop_break_C w = C.break ((==) x) `eq1` break ((==) x)
  where
    -- Make sure we're not testing non-octet character values, for which
    -- C.break is not isomorphic to breaking the corresponding string,
    -- Instead start with a byte, and make a character out of that.
    x = chr $ fromIntegral w

prop_break_P :: Word8 -> C.ByteString -> Bool
prop_break_P x = P.break ((==) x) `eq1` break ((==) x)
prop_intercalate_P c = (\s1 s2 -> P.intercalate (P.singleton c) (s1 : s2 : []))
                        `eq2`
                       (\s1 s2 -> intercalate [c] (s1 : s2 : []))

prop_break_isSpace_C = C.break isSpace `eq1` break isSpace
prop_dropWhile_isSpace_C = C.dropWhile isSpace `eq1` dropWhile isSpace

rules =
    [ testProperty "break (==)"        prop_break_C
    , testProperty "break (==)"        prop_break_P
    , testProperty "break isSpace"     prop_break_isSpace_C
    , testProperty "dropWhile isSpace" prop_dropWhile_isSpace_C
    , testProperty "intercalate"       prop_intercalate_P
    ]
