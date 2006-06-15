import qualified Data.ByteString as P
import qualified Data.ByteString.Char8 as C

main = do
    let x = C.pack "haskellzbc"
        y = C.pack "qwertyf"
    P.putStrLn (P.pack (P.zipWith (const) x y)) -- should specialise
