import qualified Data.ByteString.Char8 as B
main = print . length . B.lines =<< B.readFile "bigdata" -- B.getContents

-- import qualified Data.ByteString.Lazy as L
-- main = print . L.count 10 =<< L.getContents

--
-- rule should rewrite this to:
--
-- main = print . B.count 10 =<< B.getContents
