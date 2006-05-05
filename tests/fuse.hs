--
-- Test array fusion
--

import Char
import qualified Data.ByteString as B

main = do ps <- B.getContents
          let f = B.map (*4) . B.map (+2) . B.map (subtract 3) . B.map (+1) . B.map (*7)
          print $ B.length (f ps)

          let g = B.filter (/=104) . B.filter (/=102) . B.filter (/=103) . B.filter (/=104)
          print $ B.length (g ps)

          let h = B.filter (/=20) . B.map (+2) . B.filter (/=107) . B.map (+8)
          print $ B.length (h ps)   -- should fuse

          let i = B.map (+2) . B.filter (/=20) . B.map (+8). B.filter (/=107)
          print $ B.length (i ps)   -- should fuse

          print $ B.length $ (B.filter (/=7) . B.map (+8)) ps -- should fuse

          print $ B.length $ B.map (+7) ps -- shouldn't fuse

          print $ B.foldl (\a _ -> a+1::Int) 0 $ B.map (+7) ps -- shouldn't fuse
