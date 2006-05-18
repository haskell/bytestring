import qualified Data.ByteString as B
import System.IO

main = print . length =<< B.hGetLines stdin
