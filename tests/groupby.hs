import qualified Data.ByteString.Lazy as P
import qualified Data.List            as L

main = do
    f [97,99,103,103]

f s = do
    print "Should have the same structure:"
    print $ L.groupBy (/=) $        s
    print $ P.groupBy (/=) $ P.pack s
{-# NOINLINE f #-}
