import qualified Data.ByteString.Lazy as L
import System.IO
import System.Environment

main = do
    n <- getArgs >>= readIO . head
    L.hGetContentsN (n*1024) stdin >>= L.hPut stdout .  L.filterNotByte 101

-- main = L.hGet stdin 10000000000 >>= print . L.length
