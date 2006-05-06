import qualified Data.ByteString.Char8 as B

main = print . length . B.lines =<< B.getContents

--
-- rule should rewrite this to:
--
-- main = print . B.count 10 =<< B.getContents
