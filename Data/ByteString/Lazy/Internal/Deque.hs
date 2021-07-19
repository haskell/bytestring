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
import Prelude hiding (head, length, null)

-- A `S.ByteString` Deque used as an accumulator for lazy
-- Bytestring operations
data Deque = Deque
    { front :: [S.ByteString]
    , rear :: [S.ByteString]
    , -- | Accumulated length of deque's elements
      elemLength :: Int64
    }

-- An empty Deque
empty :: Deque
empty = Deque [] [] 0

-- Is the `Deque` empty?
-- O(1)
null :: Deque -> Bool
null deque = elemLength deque == 0

-- Add a `S.ByteString` to the front of the `Deque`
-- O(1)
cons :: S.ByteString -> Deque -> Deque
cons x (Deque fs rs acc) = Deque (x : fs) rs (acc + len x)

-- Add a `S.ByteString` to the rear of the `Deque`
-- O(1)
snoc :: S.ByteString -> Deque -> Deque
snoc x (Deque fs rs acc) = Deque fs (x : rs) (acc + len x)

len :: S.ByteString -> Int64
len x = fromIntegral $ S.length x

-- Pop a `S.ByteString` from the front of the `Deque`
-- Returns the bytestring, or Nothing if the Deque is empty, and the updated Deque
-- O(1) , occasionally O(n)
popFront :: Deque -> (Maybe S.ByteString, Deque)
popFront (Deque [] [] _) = (Nothing, empty)
popFront (Deque [] rs acc) = popFront (Deque (reverse rs) [] acc)
popFront (Deque (x : xs) rs acc) = (Just x, Deque xs rs (acc - len x))

-- Pop a `S.ByteString` from the rear of the `Deque`
-- Returns the bytestring, or Nothing if the Deque is empty, and the updated Deque
-- O(1) , occasionally O(n)
popRear :: Deque -> (Maybe S.ByteString, Deque)
popRear (Deque [] [] _) = (Nothing, empty)
popRear (Deque fs [] acc) = popRear (Deque [] (reverse fs) acc)
popRear (Deque fs (x : xs) acc) = (Just x, Deque fs xs (acc - len x))
