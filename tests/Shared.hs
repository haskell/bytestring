module Shared where

import Data.Int (Int64)
import Data.List (findIndices, stripPrefix)
import Control.Applicative ((<$>))

import QuickCheckUtils


toInt64 :: Int -> Int64
toInt64 = fromIntegral

findIndexEnd :: (a -> Bool) -> [a] -> Maybe Int
findIndexEnd p = go . findIndices p
  where
    go [] = Nothing
    go (k:[]) = Just k
    go (k:ks) = go ks

elemIndexEnd :: Eq a => a -> [a] -> Maybe Int
elemIndexEnd = findIndexEnd . (==)

stripSuffix :: [W] -> [W] -> Maybe [W]
stripSuffix xs ys = reverse <$> stripPrefix (reverse xs) (reverse ys)
