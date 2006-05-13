import qualified Data.ByteString.Lazy as L
import System.IO

main = L.hGetContentsN (64*1024) stdin >>= L.hPut stdout .  L.filterNotByte 101

-- main = L.hGet stdin 10000000000 >>= print . L.length
