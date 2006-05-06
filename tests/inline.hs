import Data.ByteString (packByte)
import qualified Data.ByteString as B

main = do
{-
    let x = packByte 2
        y = packByte 1

    print (B.length x)
    print (B.length y)

    let (x1,s1,l1) = B.toForeignPtr x
        (x2,s2,l2) = B.toForeignPtr y

    print (x1,s1,l1)
    print (x2,s2,l2)
-}


    print "Expect: GT"
    print (packByte 255 `compare` packByte 127)
