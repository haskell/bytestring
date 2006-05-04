--
-- Test array fusion
--

import Char
import qualified Data.ByteString as B

main = do ps <- B.getContents
          let f = B.map (*4) . B.map (+2) . B.map (subtract 3) . B.map (+1) . B.map (*7)
          print (f ps)
