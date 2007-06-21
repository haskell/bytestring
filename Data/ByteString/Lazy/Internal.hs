{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-- |
-- Module      : Data.ByteString.Lazy.Internal
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au, duncan@haskell.org
-- Stability   : experimental
-- Portability : portable
-- 
-- A module containing semi-public 'ByteString' internals. This exposes
-- the 'ByteString' representation and low level construction functions.
-- Modules which extend the 'ByteString' system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.ByteString.Lazy.Internal (

        -- * The lazy @ByteString@ type and representation
        ByteString(..)      -- instances: Eq, Ord, Show, Read, Data, Typeable
  ) where

import qualified Data.ByteString as S

#if defined(__GLASGOW_HASKELL__)
import Data.Generics            (Data(..), Typeable(..))
#endif

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
newtype ByteString = LPS { unLPS :: [S.ByteString] } -- LPS for lazy packed string
    deriving (Show, Read
#if defined(__GLASGOW_HASKELL__)
                        ,Data, Typeable
#endif
             )
--
-- hmm, what about getting the PS constructor unpacked into the cons cell?
--
-- data List = Nil | Cons {-# UNPACK #-} !S.ByteString List
--
-- Would avoid one indirection per chunk.
--

instance Eq  ByteString
    where (==)    = eq

instance Ord ByteString
    where compare = compareBytes

eq :: ByteString -> ByteString -> Bool
eq (LPS xs) (LPS ys) = eq' xs ys
  where eq' [] [] = True
        eq' [] _  = False
        eq' _  [] = False
        eq' (a:as) (b:bs) =
          case compare (S.length a) (S.length b) of
            LT -> a == (S.take (S.length a) b) && eq' as (S.drop (S.length a) b : bs)
            EQ -> a == b                       && eq' as bs
            GT -> (S.take (S.length b) a) == b && eq' (S.drop (S.length b) a : as) bs

compareBytes :: ByteString -> ByteString -> Ordering
compareBytes (LPS xs) (LPS ys) = cmp xs ys
  where cmp [] [] = EQ
        cmp [] _  = LT
        cmp _  [] = GT
        cmp (a:as) (b:bs) =
          case compare (S.length a) (S.length b) of
            LT -> case compare a (S.take (S.length a) b) of
                    EQ     -> cmp as (S.drop (S.length a) b : bs)
                    result -> result
            EQ -> case compare a b of
                    EQ     -> cmp as bs
                    result -> result
            GT -> case compare (S.take (S.length b) a) b of
                    EQ     -> cmp (S.drop (S.length b) a : as) bs
                    result -> result
