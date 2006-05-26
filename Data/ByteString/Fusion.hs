{-# OPTIONS_GHC -cpp -fglasgow-exts -fno-warn-orphans #-}
--
-- Module      : Data.ByteString.Fusion
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable, requires ffi and cpp
-- Tested with : GHC 6.4.1 and Hugs March 2005
-- 

--
-- | Functional array fusion for ByteStrings. 
--
-- From the Data Parallel Haskell project, 
-- >    http://www.cse.unsw.edu.au/~chak/project/dph/
--
--
module Data.ByteString.Fusion (

        -- * Fusion utilities
        noAL, NoAL, loopArr, loopAcc, loopSndAcc, StrictPair(StrictPair),
        loopU, mapEFL, filterEFL, foldEFL, foldEFL', fuseEFL, scanEFL,
        mapAccumEFL, mapIndexEFL,

  ) where

import Data.ByteString.Base

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

import Data.Word                (Word8)

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

-- |Data type for accumulators which can be ignored. The rewrite rules rely on
-- the fact that no bottoms of this type are ever constructed; hence, we can
-- assume @(_ :: NoAL) `seq` x = x@.
--
data NoAL = NoAL

-- | Special forms of loop arguments
--
-- * These are common special cases for the three function arguments of gen
--   and loop; we give them special names to make it easier to trigger RULES
--   applying in the special cases represented by these arguments.  The
--   "INLINE [1]" makes sure that these functions are only inlined in the last
--   two simplifier phases.
--
-- * In the case where the accumulator is not needed, it is better to always
--   explicitly return a value `()', rather than just copy the input to the
--   output, as the former gives GHC better local information.
-- 

-- | Element function expressing a mapping only
mapEFL :: (Word8 -> Word8) -> (NoAL -> Word8 -> (NoAL, Maybe Word8))
mapEFL f = \_ e -> (noAL, (Just $ f e))
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapEFL #-}
#endif

-- | Element function implementing a filter function only
filterEFL :: (Word8 -> Bool) -> (NoAL -> Word8 -> (NoAL, Maybe Word8))
filterEFL p = \_ e -> if p e then (noAL, Just e) else (noAL, Nothing)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] filterEFL #-}
#endif

-- |Element function expressing a reduction only
foldEFL :: (acc -> Word8 -> acc) -> (acc -> Word8 -> (acc, Maybe Word8))
foldEFL f = \a e -> (f a e, Nothing)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL #-}
#endif

-- | A strict foldEFL.
foldEFL' :: (acc -> Word8 -> acc) -> (acc -> Word8 -> (acc, Maybe Word8))
foldEFL' f = \a e -> let a' = f a e in a' `seq` (a', Nothing)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL' #-}
#endif

-- | Element function expressing a prefix reduction only
--
scanEFL :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> (Word8, Maybe Word8)
scanEFL f = \a e -> (f a e, Just a)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] scanEFL #-}
#endif

-- | Element function implementing a map and fold
--
mapAccumEFL :: (acc -> Word8 -> (acc, Word8)) -> acc -> Word8 -> (acc, Maybe Word8)
mapAccumEFL f = \a e -> case f a e of (a', e') -> (a', Just e')
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapAccumEFL #-}
#endif

-- | Element function implementing a map with index
--
mapIndexEFL :: (Int -> Word8 -> Word8) -> Int -> Word8 -> (Int, Maybe Word8)
mapIndexEFL f = \i e -> let i' = i+1 in i' `seq` (i', Just $ f i e)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapIndexEFL #-}
#endif

-- | No accumulator
noAL :: NoAL
noAL = NoAL
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] noAL #-}
#endif

-- | Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
loopArr :: (acc, byteString) -> byteString
loopArr (_, arr) = arr
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopArr #-}
#endif

loopAcc :: (acc, byteString) -> acc
loopAcc (acc, _) = acc
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopAcc #-}
#endif

loopSndAcc :: (StrictPair acc1 acc2, byteString) -> (acc2, byteString)
loopSndAcc (StrictPair _ acc, arr) = (acc, arr)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopSndAcc #-}
#endif

------------------------------------------------------------------------

--
-- size, and then percentage.
--

-- | Iteration over over ByteStrings
loopU :: (acc -> Word8 -> (acc, Maybe Word8))  -- ^ mapping & folding, once per elem
      -> acc                                   -- ^ initial acc value
      -> ByteString                            -- ^ input ByteString
      -> (acc, ByteString)

loopU f start (PS z s i) = inlinePerformIO $ withForeignPtr z $ \a -> do
    fp          <- mallocByteString i
    (ptr,n,acc) <- withForeignPtr fp $ \p -> do
        (acc, i') <- go (a `plusPtr` s) p start
        if i' == i
            then return (fp,i',acc)                 -- no realloc for map
            else do fp_ <- mallocByteString i'      -- realloc
                    withForeignPtr fp_ $ \p' -> memcpy p' p (fromIntegral i')
                    return (fp_,i',acc)

    return (acc, PS ptr 0 n)
  where
    go p ma = trans 0 0
        where
            STRICT3(trans)
            trans a_off ma_off acc
                | a_off >= i = return (acc, ma_off)
                | otherwise  = do
                    x <- peekByteOff p a_off
                    let (acc', oe) = f acc x
                    ma_off' <- case oe of
                        Nothing  -> return ma_off
                        Just e   -> do pokeByteOff ma ma_off e
                                       return $ ma_off + 1
                    trans (a_off+1) ma_off' acc'

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopU #-}
#endif

data StrictPair a b = StrictPair !a !b

infixr 9 `fuseEFL`

-- |Fuse to flat loop functions
fuseEFL :: (a1 -> Word8  -> (a1, Maybe Word8))
        -> (a2 -> Word8  -> (a2, Maybe Word8))
        -> StrictPair a1 a2
        -> Word8
        -> (StrictPair a1 a2, Maybe Word8)
fuseEFL f g (StrictPair acc1 acc2) e1 =
    case f acc1 e1 of
        (acc1', Nothing) -> (StrictPair acc1' acc2, Nothing)
        (acc1', Just e2) ->
            case g acc2 e2 of
                (acc2', res) -> (StrictPair acc1' acc2', res)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] fuseEFL #-}
#endif

{-# RULES

"loop/loop fusion!" forall em1 em2 start1 start2 arr.
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) =
    loopSndAcc (loopU (em1 `fuseEFL` em2) (StrictPair start1 start2) arr)

"loopArr/loopSndAcc" forall x.
  loopArr (loopSndAcc x) = loopArr x

"seq/NoAL" forall (u::NoAL) e.
  u `seq` e = e

  #-}


{-

Alternate experimental formulation of loopU which partitions it into
an allocating wrapper and an imperitive array-mutating loop.

The point in doing this split is that we might be able to fuse multiple
loops into a single wrapper. This would save reallocating another buffer.
It should also give better cache locality by reusing the buffer.

The RULE is:

"loop/loop wrapper elimination" forall loop1 loop2 arr.
  loopWrapper loop2 (loopArr (loopWrapper loop1 arr)) =
    loopWrapper (combineLoops loop1 loop2) arr

though of course we only want to do this if we can't do ordinary fusion.

loopU :: (acc -> Word8 -> (acc, Maybe Word8))
      -> acc
      -> ByteString
      -> (acc, ByteString)
loopU f a arr = loopWrapper (doUpLoop f a) arr
{-# INLINE loopU #-}
-- we always inline loopU now to expose the loopWrapper for wrapper elimination

type ImperativeLoop acc = Ptr Word8 -> Ptr Word8 -> Int -> IO (acc, Int)

loopWrapper :: ImperativeLoop acc
            -> ByteString
            -> (acc, ByteString)
loopWrapper body (PS z s i) = inlinePerformIO $ withForeignPtr z $ \a -> do
    fp          <- mallocByteString i
    (ptr,n,acc) <- withForeignPtr fp $ \p -> do
        (acc, i') <- body (a `plusPtr` s) p i
        if i' == i
            then return (fp,i',acc)                 -- no realloc for map
            else do fp_ <- mallocByteString i'      -- realloc
                    withForeignPtr fp_ $ \p' -> memcpy p' p (fromIntegral i')
                    return (fp_,i',acc)

    return (acc, PS ptr 0 n)
{-# INLINE [1] loopWrapper #-}
-- but loopWrapper like our previous loopU must not be inlined too early
-- or the RULES will never match.

doUpLoop :: (acc -> Word8 -> (acc, Maybe Word8))
         -> acc
         -> ImperativeLoop acc
doUpLoop f start ma p i = trans 0 0 start
  where STRICT3(trans)
        trans a_off ma_off acc
            | a_off >= i = return (acc, ma_off)
            | otherwise  = do
                x <- peekByteOff p a_off
                let (acc', oe) = f acc x
                ma_off' <- case oe of
                    Nothing  -> return ma_off
                    Just e   -> do pokeByteOff ma ma_off e
                                   return $ ma_off + 1
                trans (a_off+1) ma_off' acc'
{-# INLINE [1] doUpLoop #-}
-- not sure if this phase 1 control is strictly necessary

combineLoops :: ImperativeLoop acc -> ImperativeLoop acc' -> ImperativeLoop acc'
combineLoops loop1 loop2 p1 p2 i = do
  (_, i') <- loop1 p1 p2 i
  loop2 p1 p2 i'
-}
