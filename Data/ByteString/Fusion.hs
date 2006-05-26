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
    loopU, fuseEFL,
    noAL, NoAL, loopArr, loopAcc, loopSndAcc, unSP,
    mapEFL, filterEFL, foldEFL, foldEFL', scanEFL, mapAccumEFL, mapIndexEFL,

    -- * Strict pairs and sums
    PairS(..), MaybeS(..)

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

infixl 2 :*:

-- |Strict pair
data PairS a b = !a :*: !b deriving (Eq,Ord,Show)

-- |Strict Maybe
data MaybeS a = NothingS | JustS !a

-- |Data type for accumulators which can be ignored. The rewrite rules rely on
-- the fact that no bottoms of this type are ever constructed; hence, we can
-- assume @(_ :: NoAL) `seq` x = x@.
--
data NoAL = NoAL

-- | No accumulator
noAL :: NoAL
noAL = NoAL
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] noAL #-}
#endif

-- |Type of loop functions
type EFL acc e1 e2 = acc -> e1 -> (PairS acc (MaybeS e2))

type W = Word8

infixr 9 `fuseEFL`

-- |Fuse to flat loop functions
fuseEFL :: EFL acc1 W W -> EFL acc2 W W -> EFL (PairS acc1 acc2) W W
fuseEFL f g (acc1 :*: acc2) e1 =
    case f acc1 e1 of
        acc1' :*: NothingS -> (acc1' :*: acc2) :*: NothingS
        acc1' :*: JustS e2 ->
            case g acc2 e2 of
                acc2' :*: res -> (acc1' :*: acc2') :*: res
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] fuseEFL #-}
#endif

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
mapEFL :: (W -> W) -> EFL NoAL W W
mapEFL f = \_ e -> (noAL :*: (JustS $ f e))
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapEFL #-}
#endif

-- | Element function implementing a filter function only
filterEFL :: (W -> Bool) -> EFL NoAL W W
filterEFL p = \_ e -> if p e then (noAL :*: JustS e) else (noAL :*: NothingS)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] filterEFL #-}
#endif

-- |Element function expressing a reduction only
foldEFL :: (acc -> W -> acc) -> EFL acc W W
foldEFL f = \a e -> (f a e :*: NothingS)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL #-}
#endif

-- | A strict foldEFL.
foldEFL' :: (acc -> W -> acc) -> EFL acc W W
foldEFL' f = \a e -> let a' = f a e in a' `seq` (a' :*: NothingS)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] foldEFL' #-}
#endif

-- | Element function expressing a prefix reduction only
--
scanEFL :: (W -> W -> W) -> EFL W W W
scanEFL f = \a e -> (f a e :*: JustS a)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] scanEFL #-}
#endif

-- | Element function implementing a map and fold
--
mapAccumEFL :: (acc -> W -> (acc, W)) -> EFL acc W W
mapAccumEFL f = \a e -> case f a e of (a', e') -> (a' :*: JustS e')
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapAccumEFL #-}
#endif

-- | Element function implementing a map with index
--
mapIndexEFL :: (Int -> W -> W) -> EFL Int W W
mapIndexEFL f = \i e -> let i' = i+1 in i' `seq` (i' :*: JustS (f i e))
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] mapIndexEFL #-}
#endif

-- | Projection functions that are fusion friendly (as in, we determine when
-- they are inlined)
loopArr :: (PairS acc arr) -> arr
loopArr (_ :*: arr) = arr
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopArr #-}
#endif

loopAcc :: (PairS acc arr) -> acc
loopAcc (acc :*: _) = acc
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopAcc #-}
#endif

loopSndAcc :: (PairS (PairS acc1 acc2) arr) -> (PairS acc2 arr)
loopSndAcc ((_ :*: acc) :*: arr) = (acc :*: arr)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopSndAcc #-}
#endif

unSP :: (PairS acc arr) -> (acc, arr)
unSP (acc :*: arr) = (acc, arr)
#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] unSP #-}
#endif

------------------------------------------------------------------------
--
-- Loop combinator and fusion rules for flat arrays
-- |Iteration over over ByteStrings

-- | Iteration over over ByteStrings
loopU :: EFL acc W W                -- ^ mapping & folding, once per elem
      -> acc                        -- ^ initial acc value
      -> ByteString                 -- ^ input ByteString
      -> (PairS acc ByteString)

loopU f start (PS z s i) = inlinePerformIO $ withForeignPtr z $ \a -> do
    fp          <- mallocByteString i
    (ptr,n,acc) <- withForeignPtr fp $ \p -> do
        (acc :*: i') <- go (a `plusPtr` s) p start
        if i' == i
            then return (fp,i',acc)                 -- no realloc for map
            else do fp_ <- mallocByteString i'      -- realloc
                    withForeignPtr fp_ $ \p' -> memcpy p' p (fromIntegral i')
                    return (fp_,i',acc)

    return (acc :*: PS ptr 0 n)
  where
    go p ma = trans 0 0
        where
            STRICT3(trans)
            trans a_off ma_off acc
                | a_off >= i = return (acc :*: ma_off)
                | otherwise  = do
                    x <- peekByteOff p a_off
                    let (acc' :*: oe) = f acc x
                    ma_off' <- case oe of
                        NothingS -> return ma_off
                        JustS e  -> do pokeByteOff ma ma_off e
                                       return $ ma_off + 1
                    trans (a_off+1) ma_off' acc'

#if defined(__GLASGOW_HASKELL__)
{-# INLINE [1] loopU #-}
#endif

{-# RULES

"loop/loop fusion!" forall em1 em2 start1 start2 arr.
  loopU em2 start2 (loopArr (loopU em1 start1 arr)) =
    loopSndAcc (loopU (em1 `fuseEFL` em2) (start1 :*: start2) arr)

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
