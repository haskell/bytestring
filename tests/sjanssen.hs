import Data.Char (isAlpha, toLower)
import Data.List (sortBy)
import qualified Data.ByteString.Char8 as P

main =
    mapM (\(a, b) -> putStrLn ([a] ++ ": " ++ show b))
    . sortBy (\a b -> snd b `compare` snd a)
    . map (\x -> (P.unsafeHead x, P.length x))
    . P.group
    . P.sort
    . P.map toLower
    . P.filter isAlpha
    =<< P.getContents
