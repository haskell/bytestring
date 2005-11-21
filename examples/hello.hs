import qualified Data.FastPackedString as P

import System.IO (stdout)

main = P.hPut stdout (P.pack "Hello, World!\n")
