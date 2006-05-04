--
-- Test array fusion
--

import Char
import qualified Data.ByteString as B

main = do ps <- B.getContents
          let f = B.map (*4) . B.map (+2) . B.map (subtract 3) . B.map (+1) . B.map (*7)
          let g = B.filter (/=104) . B.filter (/=102) . B.filter (/=103) . B.filter (/=104)

          print $ B.length (f ps)
          print $ B.length (g ps)
