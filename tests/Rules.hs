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
import QuickCheckUtils


prop_break_C x = C.break ((==) x) `eq1` break ((==) x)
prop_break_P x = P.break ((==) x) `eq1` break ((==) x)
prop_intercalate_P c = (\s1 s2 -> P.intercalate (P.singleton c) (s1 : s2 : []))
                        `eq2`
                       (\s1 s2 -> intercalate [c] (s1 : s2 : []))

prop_break_isSpace_C = C.break isSpace `eq1` break isSpace
prop_dropWhile_isSpace_C = C.dropWhile isSpace `eq1` dropWhile isSpace

rules =
    [("break (==)" ,         mytest prop_break_C)
    ,("break (==)" ,         mytest prop_break_P)
    ,("break isSpace" ,      mytest prop_break_isSpace_C)

    ,("dropWhile isSpace" ,  mytest prop_dropWhile_isSpace_C)

    ,("intercalate      " ,  mytest prop_intercalate_P)
    ]
