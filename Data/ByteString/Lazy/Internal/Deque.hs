{- |
 A Deque used for accumulating `S.StrictByteString`s in `Data.ByteString.Lazy.dropEnd`.
-}
module Data.ByteString.Lazy.Internal.Deque (
    Deque (..),
    empty,
    null,
    cons,
    snoc,
    popFront,
    popRear,
) where

import qualified Data.ByteString as S
import Data.Int (Int64)
import Prelude hiding (head, tail, length, null)

-- A `S.StrictByteString` Deque used as an accumulator for lazy
-- Bytestring operations
data Deque = Deque
    { front :: [S.StrictByteString]
    , rear :: [S.StrictByteString]
    , -- | Total length in bytes
      byteLength :: !Int64
    }

-- An empty Deque
empty :: Deque
empty = Deque [] [] 0

-- Is the `Deque` empty?
-- O(1)
null :: Deque -> Bool
null deque = byteLength deque == 0

-- Add a `S.StrictByteString` to the front of the `Deque`
-- O(1)
cons :: S.StrictByteString -> Deque -> Deque
cons x (Deque fs rs acc) = Deque (x : fs) rs (acc + len x)

-- Add a `S.StrictByteString` to the rear of the `Deque`
-- O(1)
snoc :: S.StrictByteString -> Deque -> Deque
snoc x (Deque fs rs acc) = Deque fs (x : rs) (acc + len x)

len :: S.StrictByteString -> Int64
len x = fromIntegral $ S.length x

-- Pop a `S.StrictByteString` from the front of the `Deque`
-- Returns the bytestring and the updated Deque, or Nothing if the Deque is empty
-- O(1) , occasionally O(n)
popFront :: Deque -> Maybe (S.StrictByteString, Deque)
popFront (Deque [] rs acc) = case reverse rs of
    [] -> Nothing
    x : xs -> Just (x, Deque xs [] (acc - len x))
popFront (Deque (x : xs) rs acc) = Just (x, Deque xs rs (acc - len x))

-- Pop a `S.StrictByteString` from the rear of the `Deque`
-- Returns the bytestring and the updated Deque, or Nothing if the Deque is empty
-- O(1) , occasionally O(n)
popRear :: Deque -> Maybe (Deque, S.StrictByteString)
popRear (Deque fs [] acc) = case reverse fs of
    [] -> Nothing
    x : xs -> Just (Deque [] xs (acc - len x), x)
popRear (Deque fs (x : xs) acc) = Just (Deque fs xs (acc - len x), x)
