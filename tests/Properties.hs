{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}

-- We need @AllowAmbiguousTypes@ in order to be able to use @TypeApplications@
-- to disambiguate the desired instance of class methods whose instance cannot
-- be inferred from the caller's context.  We would otherwise have to use
-- proxy arguments.  Here the 'RdInt' class methods used to generate tests for
-- all the various 'readInt' types require explicit type applications.

module Properties (testSuite) where

import Prelude hiding (head, tail)
import Foreign.C.String (withCString)
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import GHC.Ptr
import Test.Tasty.QuickCheck
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Posix.Internals (c_unlink)

import qualified Data.List as List
import Data.Char
import Data.Data (toConstr, showConstr, Data)
import Data.Word
import Data.Maybe
import Data.Either (isLeft)
import Data.Bits (finiteBitSize, bit)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Generics.Text (gread, gshow)
import Data.Semigroup
import GHC.Exts (Int(..), newPinnedByteArray#, unsafeFreezeByteArray#)
import GHC.ST (ST(..), runST)

import Text.Printf
import Data.String

import System.Environment
import System.IO

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (ByteString(..))

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Short      as Short

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.Char8 as D

import qualified Data.ByteString.Lazy.Internal as L

import QuickCheckUtils
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Properties.ShortByteString as PropSBS
import qualified Properties.ByteString as PropBS
import qualified Properties.ByteStringChar8 as PropBS8
import qualified Properties.ByteStringLazy as PropBL
import qualified Properties.ByteStringLazyChar8 as PropBL8
import qualified Data.ByteString.Char8 as Char8

prop_unsafeIndexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_bijectionBB  (Char8 c) = (P.w2c . P.c2w) c == id c
prop_bijectionBB'        w  = (P.c2w . P.w2c) w == id w

prop_unsafeHead xs = not (P.null xs) ==> P.head xs === P.unsafeHead xs
prop_unsafeTail xs = not (P.null xs) ==> P.tail xs === P.unsafeTail xs
prop_unsafeLast xs = not (P.null xs) ==> P.last xs === P.unsafeLast xs
prop_unsafeInit xs = not (P.null xs) ==> P.init xs === P.unsafeInit xs

prop_lines_empty_invariant =
     True === case LC.lines (LC.pack "\nfoo\n") of
        Empty : _ -> True
        _         -> False

prop_lines_lazy =
    take 2 (LC.lines (LC.append (LC.pack "a\nb\n") undefined)) === [LC.pack "a", LC.pack "b"]

prop_lines_lazy2 =
     c === case LC.lines (Chunk c undefined) of
        Chunk c _ : _ -> c
        _             -> P.empty
  where
    c = C.pack "etc..."

prop_lines_lazy3 =
     c === case LC.lines d of
        Chunk c _ : _ -> c
        _             -> P.empty
  where
    c = C.pack "etc..."
    d = Chunk c d

prop_strip x = C.strip x == (C.dropSpace . C.reverse . C.dropSpace . C.reverse) x

prop_toConstr :: P.ByteString -> Property
prop_toConstr bs = True ==> "pack" == ((showConstr  . toConstr) bs)

prop_gshow_empty :: P.ByteString -> Property
prop_gshow_empty b = (not . null . Char8.unpack) b ==> (not . null . gshow) b

prop_gshow_equal :: P.ByteString -> Property
prop_gshow_equal b = True ==> read_bs b == read_string b
    where
        read_bs :: P.ByteString -> [(P.ByteString, String)]
        read_bs = gread . gshow
        read_string :: P.ByteString -> [(P.ByteString, String)]
        read_string = gread . Char8.unpack

prop_gshow_string :: P.ByteString -> Property
prop_gshow_string b = True ==> (gshow . Char8.pack) "A" == "(pack ((:) (65) ([])))"

class (Bounded a, Integral a, Show a) => RdInt a where
    rdIntC :: C.ByteString -> Maybe (a, C.ByteString)
    rdIntD :: D.ByteString -> Maybe (a, D.ByteString)

instance RdInt Int    where { rdIntC = C.readInt;    rdIntD = D.readInt }
instance RdInt Int8   where { rdIntC = C.readInt8;   rdIntD = D.readInt8 }
instance RdInt Int16  where { rdIntC = C.readInt16;  rdIntD = D.readInt16 }
instance RdInt Int32  where { rdIntC = C.readInt32;  rdIntD = D.readInt32 }
instance RdInt Int64  where { rdIntC = C.readInt64;  rdIntD = D.readInt64 }
--
instance RdInt Word   where { rdIntC = C.readWord;   rdIntD = D.readWord }
instance RdInt Word8  where { rdIntC = C.readWord8;  rdIntD = D.readWord8 }
instance RdInt Word16 where { rdIntC = C.readWord16; rdIntD = D.readWord16 }
instance RdInt Word32 where { rdIntC = C.readWord32; rdIntD = D.readWord32 }
instance RdInt Word64 where { rdIntC = C.readWord64; rdIntD = D.readWord64 }

smax :: forall a. (Bounded a, Show a) => String
smax = show $ maxBound @a
smax1 :: forall a. (Bounded a, Integral a) => String
smax1 = show $ fromIntegral @a @Integer maxBound + 1
smax10 :: forall a. (Bounded a, Integral a) => String
smax10 = show $ fromIntegral @a @Integer maxBound + 10

smin :: forall a. (Bounded a, Show a) => String
smin = show (minBound @a)
smin1 :: forall a. (Bounded a, Integral a) => String
smin1 = show $ fromIntegral @a @Integer minBound - 1
smin10 :: forall a. (Bounded a, Integral a) => String
smin10 = show $ fromIntegral @a @Integer minBound - 10

-- Ensure that readWord64 and readInteger over lazy ByteStrings are not
-- excessively strict.
prop_readWordSafe        = (fst . fromJust . D.readWord64) (Chunk (C.pack "1z") Empty)      == 1
prop_readWordUnsafe      = (fst . fromJust . D.readWord64) (Chunk (C.pack "2z") undefined)  == 2
prop_readIntegerSafe     = (fst . fromJust . D.readInteger) (Chunk (C.pack "1z") Empty)     == 1
prop_readIntegerUnsafe   = (fst . fromJust . D.readInteger) (Chunk (C.pack "2z") undefined) == 2
prop_readNaturalSafe     = (fst . fromJust . D.readNatural) (Chunk (C.pack "1z") Empty)     == 1
prop_readNaturalUnsafe   = (fst . fromJust . D.readNatural) (Chunk (C.pack "2z") undefined) == 2
prop_readIntBoundsCC     =     rdWordBounds @Word
                            && rdWordBounds @Word8
                            && rdWordBounds @Word16
                            && rdWordBounds @Word32
                            && rdWordBounds @Word64
                            && rdIntBounds  @Int
                            && rdIntBounds  @Int8
                            && rdIntBounds  @Int16
                            && rdIntBounds  @Int32
                            && rdIntBounds  @Int64
  where
    tailStr      = " tail"
    zeroStr      = "000000000000000000000000000"
    spack s      = C.pack $ s ++ tailStr
    spackPlus s  = C.pack $ '+' : (s ++ tailStr)
    spackMinus s = C.pack $ '-' : (s ++ tailStr)
    spackLong s  = C.pack $ s ++ zeroStr ++ tailStr
    spackZeros s = case s of
                    '+':num -> C.pack $ '+' : zeroStr ++ num ++ tailStr
                    '-':num -> C.pack $ '-' : zeroStr ++ num ++ tailStr
                    num     -> C.pack $ zeroStr ++ num ++ tailStr
    good i       = Just (i, C.pack tailStr)
    --
    rdWordBounds :: forall a. RdInt a => Bool
    rdWordBounds =
        -- Upper bound
        rdIntC @a (spack (smax @a)) == good maxBound
        -- With leading zeros
        && rdIntC @a (spackZeros (smax @a)) == good maxBound
        -- Overflow in last digit
        && rdIntC @a (spack (smax1 @a)) == Nothing
        -- Overflow in 2nd-last digit
        && rdIntC @a (spack (smax10 @a)) == Nothing
        -- Trailing zeros
        && rdIntC @a (spackLong (smax @a)) == Nothing
    --
    rdIntBounds :: forall a. RdInt a => Bool
    rdIntBounds =
        rdWordBounds @a
        -- Lower bound
        && rdIntC @a (spack (smin @a)) == good minBound
        -- With leading signs
        && rdIntC @a (spackPlus (smax @a)) == good maxBound
        && rdIntC @a (spackMinus (smax @a)) == good (negate maxBound)
        -- With leading zeros
        && rdIntC @a (spackZeros (smax @a)) == good maxBound
        -- Underflow in last digit
        && rdIntC @a (spack (smin1 @a)) == Nothing
        -- Underflow in 2nd-last digit
        && rdIntC @a (spack (smin10 @a)) == Nothing
        -- Trailing zeros
        && rdIntC @a (spackLong (smin @a)) == Nothing

prop_readIntBoundsLC     =     rdWordBounds @Word
                            && rdWordBounds @Word8
                            && rdWordBounds @Word16
                            && rdWordBounds @Word32
                            && rdWordBounds @Word64
                            && rdIntBounds  @Int
                            && rdIntBounds  @Int8
                            && rdIntBounds  @Int16
                            && rdIntBounds  @Int32
                            && rdIntBounds  @Int64
  where
    tailStr      = " tail"
    zeroStr      = "000000000000000000000000000"
    spack s      = LC.pack $ s ++ tailStr
    spackPlus s  = LC.singleton '+' `D.append` LC.pack s `D.append` LC.pack tailStr
    spackMinus s = LC.singleton '-' `D.append` LC.pack s `D.append` LC.pack tailStr
    spackLong1 s = LC.pack s `D.append` LC.pack zeroStr `D.append` LC.pack tailStr
    spackLong2 s = LC.pack (s ++ zeroStr) `D.append` LC.pack tailStr
    spackZeros s = case s of
                    '+':num -> LC.pack ('+' : zeroStr) `D.append` LC.pack (num ++ tailStr)
                    '-':num -> LC.pack ('-' : zeroStr) `D.append` LC.pack (num ++ tailStr)
                    num     -> LC.pack $ zeroStr ++ num ++ tailStr
    good i       = Just (i, LC.pack tailStr)
    --
    rdWordBounds :: forall a. RdInt a => Bool
    rdWordBounds =
        -- Upper bound
        rdIntD @a (spack (smax @a)) == good maxBound
        -- With leading zeros
        && rdIntD @a (spackZeros (smax @a)) == good maxBound
        -- Overflow in last digit
        && rdIntD @a (spack (smax1 @a)) == Nothing
        -- Overflow in 2nd-last digit
        && rdIntD @a (spack (smax10 @a)) == Nothing
        -- Overflow across chunk boundary
        && rdIntD @a (spackLong1 (smax @a)) == Nothing
        -- Overflow within chunk
        && rdIntD @a (spackLong2 (smax @a)) == Nothing
        -- Sign with no digits
        && rdIntD @a (LC.pack "+ foo") == Nothing
        && rdIntD @a (LC.pack "-bar") == Nothing
    --
    rdIntBounds :: forall a. RdInt a => Bool
    rdIntBounds =
        rdWordBounds @a
        -- Lower bound
        && rdIntD @a (spack (smin @a)) == good minBound
        -- With leading signs
        && rdIntD @a (spackPlus (smax @a)) == good maxBound
        && rdIntD @a (spackMinus (smax @a)) == good (negate maxBound)
        -- With leading zeros
        && rdIntD @a (spackZeros (smin @a)) == good minBound
        -- Overflow in last digit
        && rdIntD @a (spack (smin1 @a)) == Nothing
        -- Overflow in 2nd-last digit
        && rdIntD @a (spack (smin10 @a)) == Nothing
        -- Overflow across chunk boundary
        && rdIntD @a (spackLong1 (smin @a)) == Nothing
        -- Overflow within chunk
        && rdIntD @a (spackLong2 (smin @a)) == Nothing

------------------------------------------------------------------------

expectSizeOverflow :: a -> Property
expectSizeOverflow val = ioProperty $ do
  isLeft <$> try @P.SizeOverflowException (evaluate val)

prop_checkedAdd = forAll (vectorOf 2 nonNeg) $ \[x, y] -> if oflo x y
  then expectSizeOverflow (P.checkedAdd "" x y)
  else property $ P.checkedAdd "" x y == x + y
  where nonNeg = choose (0, (maxBound @Int))
        oflo x y = toInteger x + toInteger y /= toInteger @Int (x + y)

multCompl :: Int -> Gen Int
multCompl x = choose (0, fromInteger @Int maxc)
  -- This choice creates products with magnitude roughly in the range
  -- [0..5*(maxBound @Int)], which results in a roughly even split
  -- between positive and negative overflowed Int results, while still
  -- producing a fair number of non-overflowing products.
  where maxc = toInteger (maxBound @Int) * 5 `quot` max 5 (abs $ toInteger x)

prop_checkedMultiply = forAll genScale $ \scale ->
  forAll (genVal scale) $ \x ->
    forAll (multCompl x) $ \y -> if oflo x y
      then expectSizeOverflow (P.checkedMultiply "" x y)
      else property $ P.checkedMultiply "" x y == x * y
  where genScale = choose (0, finiteBitSize @Int 0 - 1)
        genVal scale = choose (0, bit scale - 1)
        oflo x y = toInteger x * toInteger y /= toInteger @Int (x * y)

prop_stimesOverflowBasic bs = forAll (multCompl len) $ \n ->
  toInteger n * toInteger len > maxInt ==> expectSizeOverflow (stimes n bs)
  where
    maxInt = toInteger @Int (maxBound @Int)
    len = P.length bs

prop_stimesOverflowScary bs =
  -- "Scary" because this test will cause heap corruption
  -- (not just memory exhaustion) with the old stimes implementation.
  n > 1 ==> expectSizeOverflow (stimes reps bs)
  where
    n = P.length bs
    reps = maxBound @Word `quot` fromIntegral @Int @Word n + 1

prop_stimesOverflowEmpty = forAll (choose (0, maxBound @Word)) $ \n ->
  stimes n mempty === mempty @P.ByteString

concat32bitOverflow :: (Int -> a) -> ([a] -> a) -> Property
concat32bitOverflow replicateLike concatLike = let
  intBits = finiteBitSize @Int 0
  largeBS = concatLike $ replicate (bit 14) $ replicateLike (bit 17)
  in if intBits /= 32
     then label "skipped due to non-32-bit Int" True
     else expectSizeOverflow largeBS

prop_32bitOverflow_Strict_mconcat :: Property
prop_32bitOverflow_Strict_mconcat =
  concat32bitOverflow (`P.replicate` 0) mconcat

prop_32bitOverflow_Lazy_toStrict :: Property
prop_32bitOverflow_Lazy_toStrict =
  concat32bitOverflow (`P.replicate` 0) (L.toStrict . L.fromChunks)

prop_32bitOverflow_Short_mconcat :: Property
prop_32bitOverflow_Short_mconcat =
  concat32bitOverflow makeShort mconcat
  where makeShort n = Short.toShort $ P.replicate n 0


------------------------------------------------------------------------

prop_packUptoLenBytes cs =
    forAll (choose (0, length cs + 1)) $ \n ->
      let (bs, cs') = P.packUptoLenBytes n cs
       in P.length bs == min n (length cs)
       && take n cs == P.unpack bs
       && P.pack (take n cs) == bs
       && drop n cs == cs'

prop_packUptoLenChars (String8 cs) =
    forAll (choose (0, length cs + 1)) $ \n ->
      let (bs, cs') = P.packUptoLenChars n cs
       in P.length bs == min n (length cs)
       && take n cs == C.unpack bs
       && C.pack (take n cs) == bs
       && drop n cs == cs'

prop_unpackAppendBytesLazy cs' =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \cs ->
    forAll (choose (0, 2)) $ \n ->
      P.unpackAppendBytesLazy (P.drop n $ P.pack cs) cs' == drop n cs ++ cs'
prop_unpackAppendCharsLazy (String8 cs') =
    forAll (sized $ \n -> resize (n * 10) arbitrary) $ \(String8 cs) ->
    forAll (choose (0, 2)) $ \n ->
      P.unpackAppendCharsLazy (P.drop n $ C.pack cs) cs' == drop n cs ++ cs'

prop_unpackAppendBytesStrict cs cs' =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackAppendBytesStrict (P.drop n $ P.pack cs) cs' == drop n cs ++ cs'

prop_unpackAppendCharsStrict (String8 cs) (String8 cs') =
    forAll (choose (0, length cs)) $ \n ->
      P.unpackAppendCharsStrict (P.drop n $ C.pack cs) cs' == drop n cs ++ cs'

------------------------------------------------------------------------
-- Unsafe functions

-- Test unsafePackAddress
prop_unsafePackAddress (CByteString x) = ioProperty $ do
        let (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddress addr
        return (y == x)

-- Test unsafePackAddressLen
prop_unsafePackAddressLen x = ioProperty $ do
        let i = P.length x
            (p,_,_) = P.toForeignPtr (x `P.snoc` 0)
        y <- withForeignPtr p $ \(Ptr addr) ->
            P.unsafePackAddressLen i addr
        return (y == x)

prop_unsafeUseAsCString x = ioProperty $ do
        let n = P.length x
        y <- P.unsafeUseAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_unsafeUseAsCStringLen x = ioProperty $ do
        let n = P.length x
        y <- P.unsafeUseAsCStringLen x $ \(cstr,_) ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_useAsCString x = ioProperty $ do
        let n = P.length x
        y <- P.useAsCString x $ \cstr ->
                    sequence [ do a <- peekElemOff cstr i
                                  let b = x `P.index` i
                                  return (a == fromIntegral b)
                             | i <- [0.. n-1]     ]
        return (and y)

prop_packCString (CByteString x) = ioProperty $ do
        y <- P.useAsCString x $ P.unsafePackCString
        return (y == x)

prop_packCString_safe (CByteString x) = ioProperty $ do
        y <- P.useAsCString x $ P.packCString
        return (y == x)

prop_packCStringLen x = ioProperty $ do
        y <- P.useAsCStringLen x $ P.unsafePackCStringLen
        return (y == x && P.length y == P.length x)

prop_packCStringLen_safe x = ioProperty $ do
        y <- P.useAsCStringLen x $ P.packCStringLen
        return (y == x && P.length y == P.length x)

prop_packMallocCString (CByteString x) = ioProperty $ do

         let (fp,_,_) = P.toForeignPtr x
         ptr <- mallocArray0 (P.length x) :: IO (Ptr Word8)
         forM_ [0 .. P.length x] $ \n -> pokeElemOff ptr n 0
         withForeignPtr fp $ \qtr -> copyArray ptr qtr (P.length x)
         y   <- P.unsafePackMallocCString (castPtr ptr)

         let !z = y == x
         free ptr `seq` return z

prop_unsafeFinalize    x =
    P.length x > 0 ==>
      ioProperty $ do
        x <- P.unsafeFinalize x
        return (x == ())

prop_packCStringFinaliser x = ioProperty $ do
        y <- P.useAsCString x $ \cstr -> P.unsafePackCStringFinalizer (castPtr cstr) (P.length x) (return ())
        return (y == x)

prop_fromForeignPtr x = (let (a,b,c) = (P.toForeignPtr x)
                                in P.fromForeignPtr a b c) == x

------------------------------------------------------------------------
-- IO

prop_read_write_file_P x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    P.writeFile fn x
    y <- P.readFile fn
    removeFile fn
    return (x == y)

prop_read_write_file_C x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    C.writeFile fn x
    y <- C.readFile fn
    removeFile fn
    return (x == y)

prop_read_write_file_L x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    L.writeFile fn x
    y <- L.readFile fn
    L.length y `seq` removeFile fn
    return (x == y)

prop_read_write_file_D x = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    D.writeFile fn x
    y <- D.readFile fn
    D.length y `seq` removeFile fn
    return (x == y)

------------------------------------------------------------------------

prop_append_file_P x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    P.writeFile fn x
    P.appendFile fn y
    z <- P.readFile fn
    removeFile fn
    return (z == x `P.append` y)

prop_append_file_C x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    C.writeFile fn x
    C.appendFile fn y
    z <- C.readFile fn
    removeFile fn
    return (z == x `C.append` y)

prop_append_file_L x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    L.writeFile fn x
    L.appendFile fn y
    z <- L.readFile fn
    L.length y `seq` removeFile fn
    return (z == x `L.append` y)

prop_append_file_D x y = ioProperty $ do
    (fn, h) <- openTempFile "." "prop-compiled.tmp"
    hClose h
    D.writeFile fn x
    D.appendFile fn y
    z <- D.readFile fn
    D.length y `seq` removeFile fn
    return (z == x `D.append` y)

prop_packAddress = C.pack "this is a test"
            ==
                   C.pack "this is a test"

prop_isSpaceWord8 w = isSpace c == P.isSpaceChar8 c
   where c = chr (fromIntegral (w :: Word8))


------------------------------------------------------------------------
-- ByteString.Short
--

prop_short_pack_unpack xs =
    (Short.unpack . Short.pack) xs == xs
prop_short_toShort_fromShort bs =
    (Short.fromShort . Short.toShort) bs == bs

prop_short_toShort_unpack bs =
    (Short.unpack . Short.toShort) bs == P.unpack bs
prop_short_pack_fromShort xs =
    (Short.fromShort . Short.pack) xs == P.pack xs

prop_short_empty =
    Short.empty == Short.toShort P.empty
 && Short.empty == Short.pack []
 && Short.null (Short.toShort P.empty)
 && Short.null (Short.pack [])
 && Short.null Short.empty

prop_short_null_toShort bs =
    P.null bs == Short.null (Short.toShort bs)
prop_short_null_pack xs =
    null xs == Short.null (Short.pack xs)

prop_short_length_toShort bs =
    P.length bs == Short.length (Short.toShort bs)
prop_short_length_pack xs =
    length xs == Short.length (Short.pack xs)

prop_short_index_pack xs =
    all (\i -> Short.pack xs `Short.index` i == xs !! i)
        [0 .. length xs - 1]
prop_short_index_toShort bs =
    all (\i -> Short.toShort bs `Short.index` i == bs `P.index` i)
        [0 .. P.length bs - 1]

prop_short_eq xs ys =
    (xs == ys) == (Short.pack xs == Short.pack ys)
prop_short_ord xs ys =
    (xs `compare` ys) == (Short.pack xs `compare` Short.pack ys)

prop_short_mappend_empty_empty =
    Short.empty `mappend` Short.empty  == Short.empty
prop_short_mappend_empty xs =
    Short.empty `mappend` Short.pack xs == Short.pack xs
 && Short.pack xs `mappend` Short.empty == Short.pack xs
prop_short_mappend xs ys =
    (xs `mappend` ys) == Short.unpack (Short.pack xs `mappend` Short.pack ys)
prop_short_mconcat xss =
    mconcat xss == Short.unpack (mconcat (map Short.pack xss))

prop_short_fromString s =
    fromString s == Short.fromShort (fromString s)

prop_short_show xs =
    show (Short.pack xs) == show (map P.w2c xs)
prop_short_show' xs =
    show (Short.pack xs) == show (P.pack xs)

prop_short_read xs =
    read (show (Short.pack xs)) == Short.pack xs

prop_short_pinned :: NonNegative Int -> Property
prop_short_pinned (NonNegative (I# len#)) = runST $ ST $ \s ->
  case newPinnedByteArray# len# s of
    (# s', mba# #) -> case unsafeFreezeByteArray# mba# s' of
      (# s'', ba# #) -> let sbs = Short.SBS ba# in
        (# s'', sbs === Short.toShort (Short.fromShort sbs) #)

short_tests =
    [ testProperty "pack/unpack"              prop_short_pack_unpack
    , testProperty "toShort/fromShort"        prop_short_toShort_fromShort
    , testProperty "toShort/unpack"           prop_short_toShort_unpack
    , testProperty "pack/fromShort"           prop_short_pack_fromShort
    , testProperty "empty"                    prop_short_empty
    , testProperty "null/toShort"             prop_short_null_toShort
    , testProperty "null/pack"                prop_short_null_pack
    , testProperty "length/toShort"           prop_short_length_toShort
    , testProperty "length/pack"              prop_short_length_pack
    , testProperty "index/pack"               prop_short_index_pack
    , testProperty "index/toShort"            prop_short_index_toShort
    , testProperty "Eq"                       prop_short_eq
    , testProperty "Ord"                      prop_short_ord
    , testProperty "mappend/empty/empty"      prop_short_mappend_empty_empty
    , testProperty "mappend/empty"            prop_short_mappend_empty
    , testProperty "mappend"                  prop_short_mappend
    , testProperty "mconcat"                  prop_short_mconcat
    , testProperty "fromString"               prop_short_fromString
    , testProperty "show"                     prop_short_show
    , testProperty "show'"                    prop_short_show'
    , testProperty "read"                     prop_short_read
    , testProperty "pinned"                   prop_short_pinned
    ]

------------------------------------------------------------------------
-- Strictness checks.

explosiveTail :: L.ByteString -> L.ByteString
explosiveTail = (`L.append` error "Tail of this byte string is undefined!")

------------------------------------------------------------------------
-- The entry point

testSuite :: TestTree
testSuite = testGroup "Properties"
  [ testGroup "ShortByteString" PropSBS.tests
  , testGroup "StrictWord8"     PropBS.tests
  , testGroup "StrictChar8"     PropBS8.tests
  , testGroup "LazyWord8"       PropBL.tests
  , testGroup "LazyChar8"       PropBL8.tests
  , testGroup "Overflow"        overflow_tests
  , testGroup "Misc"            misc_tests
  , testGroup "IO"              io_tests
  , testGroup "Short"           short_tests
  , testGroup "Strictness"      strictness_checks
  ]

io_tests =
    [ testProperty "readFile.writeFile" prop_read_write_file_P
    , testProperty "readFile.writeFile" prop_read_write_file_C
    , testProperty "readFile.writeFile" prop_read_write_file_L
    , testProperty "readFile.writeFile" prop_read_write_file_D

    , testProperty "appendFile        " prop_append_file_P
    , testProperty "appendFile        " prop_append_file_C
    , testProperty "appendFile        " prop_append_file_L
    , testProperty "appendFile        " prop_append_file_D

    , testProperty "packAddress       " prop_packAddress
    ]

overflow_tests =
    [ testProperty "checkedAdd" prop_checkedAdd
    , testProperty "checkedMultiply" prop_checkedMultiply
    , testProperty "StrictByteString stimes (basic)" prop_stimesOverflowBasic
    , testProperty "StrictByteString stimes (scary)" prop_stimesOverflowScary
    , testProperty "StrictByteString stimes (empty)" prop_stimesOverflowEmpty
    , testProperty "StrictByteString mconcat" prop_32bitOverflow_Strict_mconcat
    , testProperty "LazyByteString toStrict"  prop_32bitOverflow_Lazy_toStrict
    , testProperty "ShortByteString mconcat"  prop_32bitOverflow_Short_mconcat
    ]

misc_tests =
    [ testProperty "packUptoLenBytes"       prop_packUptoLenBytes
    , testProperty "packUptoLenChars"       prop_packUptoLenChars
    , testProperty "unpackAppendBytesLazy"  prop_unpackAppendBytesLazy
    , testProperty "unpackAppendCharsLazy"  prop_unpackAppendCharsLazy
    , testProperty "unpackAppendBytesStrict"prop_unpackAppendBytesStrict
    , testProperty "unpackAppendCharsStrict"prop_unpackAppendCharsStrict

    , testProperty "unsafe pack address"    prop_unsafePackAddress
    , testProperty "unsafe pack address len"prop_unsafePackAddressLen
    , testProperty "unsafeUseAsCString"     prop_unsafeUseAsCString
    , testProperty "unsafeUseAsCStringLen"  prop_unsafeUseAsCStringLen
    , testProperty "useAsCString"           prop_useAsCString
    , testProperty "packCString"            prop_packCString
    , testProperty "packCString safe"       prop_packCString_safe
    , testProperty "packCStringLen"         prop_packCStringLen
    , testProperty "packCStringLen safe"    prop_packCStringLen_safe
    , testProperty "packCStringFinaliser"   prop_packCStringFinaliser
    , testProperty "packMallocString"       prop_packMallocCString
    , testProperty "unsafeFinalise"         prop_unsafeFinalize
    , testProperty "fromForeignPtr"         prop_fromForeignPtr

    , testProperty "w2c . c2w"      prop_bijectionBB
    , testProperty "c2w . w2c"      prop_bijectionBB'

    , testProperty "unsafeHead"     prop_unsafeHead
    , testProperty "unsafeTail"     prop_unsafeTail
    , testProperty "unsafeLast"     prop_unsafeLast
    , testProperty "unsafeInit"     prop_unsafeInit
    , testProperty "unsafeIndex"    prop_unsafeIndexBB

    , testProperty "lines_lazy"     prop_lines_lazy
    , testProperty "lines_lazy2"    prop_lines_lazy2
    , testProperty "lines_lazy3"    prop_lines_lazy3
    , testProperty "lines_invar"    prop_lines_empty_invariant
    , testProperty "strip"          prop_strip
    , testProperty "isSpace"        prop_isSpaceWord8

    , testProperty "readWordSafe"      prop_readWordSafe
    , testProperty "readWordUnsafe"    prop_readWordUnsafe
    , testProperty "readIntBoundsCC"   prop_readIntBoundsCC
    , testProperty "readIntBoundsLC"   prop_readIntBoundsLC
    , testProperty "readIntegerSafe"   prop_readIntegerSafe
    , testProperty "readIntegerUnsafe" prop_readIntegerUnsafe
    , testProperty "readNaturalSafe"   prop_readNaturalSafe
    , testProperty "readNaturalUnsafe" prop_readNaturalUnsafe
    , testProperty "instance Data toConstr" prop_toConstr
    , testProperty "gshow empty" prop_gshow_empty
    , testProperty "gshow equal" prop_gshow_equal
    , testProperty "gshow string" prop_gshow_string
    ]

strictness_checks =
  [ testGroup "Lazy Word8"
    [ testProperty "foldr is lazy" $ \ xs ->
        List.genericTake (L.length xs) (L.foldr (:) [ ] (explosiveTail xs)) === L.unpack xs
    , testProperty "foldr' is strict" $ expectFailure $ \ xs ys ->
        List.genericTake (L.length xs) (L.foldr' (:) [ ] (explosiveTail (xs <> ys))) === L.unpack xs
    , testProperty "foldr1 is lazy" $ \ xs -> L.length xs > 0 ==>
        L.foldr1 const (explosiveTail (xs <> L.singleton 1)) === L.head xs
    , testProperty "foldr1' is strict" $ expectFailure $ \ xs ys -> L.length xs > 0 ==>
        L.foldr1' const (explosiveTail (xs <> L.singleton 1 <> ys)) === L.head xs
    , testProperty "scanl is lazy" $ \ xs ->
        L.take (L.length xs + 1) (L.scanl (+) 0 (explosiveTail (xs <> L.singleton 1))) === (L.pack . fmap (L.foldr (+) 0) . L.inits) xs
    , testProperty "scanl1 is lazy" $ \ xs -> L.length xs > 0 ==>
        L.take (L.length xs) (L.scanl1 (+) (explosiveTail (xs <> L.singleton 1))) === (L.pack . fmap (L.foldr1 (+)) . drop 1 . L.inits) xs
    ]
  , testGroup "Lazy Char"
    [ testProperty "foldr is lazy" $ \ xs ->
        List.genericTake (D.length xs) (D.foldr (:) [ ] (explosiveTail xs)) === D.unpack xs
    , testProperty "foldr' is strict" $ expectFailure $ \ xs ys ->
        List.genericTake (D.length xs) (D.foldr' (:) [ ] (explosiveTail (xs <> ys))) === D.unpack xs
    , testProperty "foldr1 is lazy" $ \ xs -> D.length xs > 0 ==>
        D.foldr1 const (explosiveTail (xs <> D.singleton 'x')) === D.head xs
    , testProperty "foldr1' is strict" $ expectFailure $ \ xs ys -> D.length xs > 0 ==>
        D.foldr1' const (explosiveTail (xs <> D.singleton 'x' <> ys)) === D.head xs
    , testProperty "scanl is lazy" $ \ xs -> let char1 +. char2 = toEnum (fromEnum char1 + fromEnum char2) in
        D.take (D.length xs + 1) (D.scanl (+.) '\NUL' (explosiveTail (xs <> D.singleton '\SOH'))) === (D.pack . fmap (D.foldr (+.) '\NUL') . D.inits) xs
    , testProperty "scanl1 is lazy" $ \ xs -> D.length xs > 0 ==> let char1 +. char2 = toEnum (fromEnum char1 + fromEnum char2) in
        D.take (D.length xs) (D.scanl1 (+.) (explosiveTail (xs <> D.singleton '\SOH'))) === (D.pack . fmap (D.foldr1 (+.)) . drop 1 . D.inits) xs
    , testProperty "unlines is lazy" $ \ xs -> D.take (D.length xs + 1) (D.unlines (xs : error "Tail of this list is undefined!")) === xs `D.snoc` '\n'
    ]
  ]

removeFile :: String -> IO ()
removeFile fn = void $ withCString fn c_unlink
