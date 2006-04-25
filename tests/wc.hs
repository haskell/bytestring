import Data.PackedString.Latin1 hiding (length)
import Prelude hiding (getContents,lines)
import System.IO (stdin)

main = print . length . lines =<< getContents

{-

-- import Foreign
-- import Foreign.ForeignPtr
-- import qualified Data.ByteString as P hiding (length)

main = P.hGetContents stdin >>= \(P.PS x _ l) -> withForeignPtr x $ \p -> go p l 0 0

    where go :: Ptr Word8 -> Int -> Int -> Int -> IO ()
          go p l n i | p `seq` l `seq` n `seq` i `seq` False = undefined
                     | n >= l    = print i
                     | otherwise = do w <- peekElemOff p n
                                      go p l (n+1) $ if w == 0x0a then (i+1) else i
-}


