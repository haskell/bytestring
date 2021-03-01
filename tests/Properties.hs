{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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
import Data.Word
import Data.Maybe
import Data.Int (Int64)
import Data.Monoid
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
import Prelude hiding (abs)

import QuickCheckUtils
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Properties.ByteString as PropBS
import qualified Properties.ByteStringChar8 as PropBS8
import qualified Properties.ByteStringLazy as PropBL
import qualified Properties.ByteStringLazyChar8 as PropBL8

prop_unsafeIndexBB xs =
  not (null xs) ==>
    forAll indices $ \i -> (xs !! i) == P.pack xs `P.unsafeIndex` i
  where indices = choose (0, length xs -1)

prop_bijectionBB  (Char8 c) = (P.w2c . P.c2w) c == id c
prop_bijectionBB'        w  = (P.c2w . P.w2c) w == id w

prop_head2BB xs    = (not (null xs)) ==> head xs   == (P.unsafeHead . P.pack) xs

prop_tail1BB xs    = (not (null xs)) ==> tail xs    == (P.unpack . P.unsafeTail. P.pack) xs

prop_last1BB xs    = (not (null xs)) ==> last xs    == (P.unsafeLast . P.pack) xs

prop_init1BB xs     =
    (not (null xs)) ==>
    init xs    == (P.unpack . P.unsafeInit . P.pack) xs

prop_lines_lazy1 =
    head (LC.lines (LC.append (LC.pack "a\nb\n") undefined)) == LC.pack "a"
prop_lines_lazy2 =
    head (tail (LC.lines (LC.append (LC.pack "a\nb\n") undefined))) == LC.pack "b"

prop_strip x = C.strip x == (C.dropSpace . C.reverse . C.dropSpace . C.reverse) x

-- Ensure that readInt and readInteger over lazy ByteStrings are not
-- excessively strict.
prop_readIntSafe         = (fst . fromJust . D.readInt) (Chunk (C.pack "1z") Empty)         == 1
prop_readIntUnsafe       = (fst . fromJust . D.readInt) (Chunk (C.pack "2z") undefined)     == 2
prop_readIntegerSafe     = (fst . fromJust . D.readInteger) (Chunk (C.pack "1z") Empty)     == 1
prop_readIntegerUnsafe   = (fst . fromJust . D.readInteger) (Chunk (C.pack "2z") undefined) == 2
prop_readIntBoundsCC     = let !smax   = show (maxBound :: Int)
                               !smin   = show (minBound :: Int)
                               !smax1  = show (fromIntegral (maxBound :: Int) + 1 :: Integer)
                               !smin1  = show (fromIntegral (minBound :: Int) - 1 :: Integer)
                               !smax10 = show (fromIntegral (maxBound :: Int) + 10 :: Integer)
                               !smin10 = show (fromIntegral (minBound :: Int) - 10 :: Integer)
                            --
                            in C.readInt (spack smax) == good maxBound
                            && C.readInt (spack smin) == good minBound
                            --
                            && C.readInt (spackPlus smax) == good maxBound
                            && C.readInt (spackMinus smax) == good (negate maxBound)
                            --
                            && C.readInt (spackZeros smax) == good maxBound
                            && C.readInt (spackZeros smin) == good minBound
                            --
                            && C.readInt (spack smax1 ) == Nothing
                            && C.readInt (spack smin1 ) == Nothing
                            --
                            && C.readInt (spack smax10) == Nothing
                            && C.readInt (spack smin10) == Nothing
                            --
                            && C.readInt (spackLong smax) == Nothing
                            && C.readInt (spackLong smin) == Nothing
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
prop_readIntBoundsLC     = let !smax   = show (maxBound :: Int)
                               !smin   = show (minBound :: Int)
                               !smax1  = show (fromIntegral (maxBound :: Int) + 1 :: Integer)
                               !smin1  = show (fromIntegral (minBound :: Int) - 1 :: Integer)
                               !smax10 = show (fromIntegral (maxBound :: Int) + 10 :: Integer)
                               !smin10 = show (fromIntegral (minBound :: Int) - 10 :: Integer)
                            -- Plain min/maxBound
                            in LC.readInt (spack smax) == good maxBound
                            && LC.readInt (spack smin) == good minBound
                            -- With explicit [+-] sign for maxBound
                            && LC.readInt (spackPlus smax) == good maxBound
                            && LC.readInt (spackMinus smax) == good (negate maxBound)
                            -- With leading zeros
                            && LC.readInt (spackZeros smax) == good maxBound
                            && LC.readInt (spackZeros smin) == good minBound
                            -- Overflow in last digit
                            && LC.readInt (spack smax1 ) == Nothing
                            && LC.readInt (spack smin1 ) == Nothing
                            -- Overflow in 2nd-last digit
                            && LC.readInt (spack smax10) == Nothing
                            && LC.readInt (spack smin10) == Nothing
                            -- Overflow across chunk boundary
                            && LC.readInt (spackLong1 smax) == Nothing
                            && LC.readInt (spackLong1 smin) == Nothing
                            -- Overflow within chunk
                            && LC.readInt (spackLong2 smax) == Nothing
                            && LC.readInt (spackLong2 smin) == Nothing
                            -- Sign with no digits
                            && LC.readInt (LC.pack "+ foo") == Nothing
                            && LC.readInt (LC.pack "-bar") == Nothing
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

explosiveTailD :: D.ByteString -> D.ByteString
explosiveTailD = (`L.append` error "Tail of this byte string is undefined!")

------------------------------------------------------------------------
-- The entry point

main :: IO ()
main = defaultMain $ testGroup "All"
  [ testGroup "StrictWord8" PropBS.tests
  , testGroup "StrictChar8" PropBS8.tests
  , testGroup "LazyWord8"   PropBL.tests
  , testGroup "LazyChar8"   PropBL8.tests
  , testGroup "Misc"        misc_tests
  , testGroup "IO"          io_tests
  , testGroup "Short"       short_tests
  , testGroup "Strictness"  strictness_checks
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

    , testProperty "unsafeHead"     prop_head2BB
    , testProperty "unsafeTail"     prop_tail1BB
    , testProperty "unsafeLast"     prop_last1BB
    , testProperty "unsafeInit"     prop_init1BB
    , testProperty "unsafeIndex"    prop_unsafeIndexBB

    , testProperty "lines_lazy1"    prop_lines_lazy1
    , testProperty "lines_lazy2"    prop_lines_lazy2
    , testProperty "strip"          prop_strip
    , testProperty "isSpace"        prop_isSpaceWord8

    , testProperty "readIntSafe"       prop_readIntSafe
    , testProperty "readIntUnsafe"     prop_readIntUnsafe
    , testProperty "readIntBoundsCC"   prop_readIntBoundsCC
    , testProperty "readIntBoundsLC"   prop_readIntBoundsLC
    , testProperty "readIntegerSafe"   prop_readIntegerSafe
    , testProperty "readIntegerUnsafe" prop_readIntegerUnsafe
    ]

strictness_checks =
  [ testGroup "Lazy Word8"
    [ testProperty "foldr is lazy" $ \ xs ->
        List.genericTake (L.length xs) (L.foldr (:) [ ] (explosiveTail xs)) == L.unpack xs
    , testProperty "foldr' is strict" $ expectFailure $ \ xs ys ->
        List.genericTake (L.length xs) (L.foldr' (:) [ ] (explosiveTail (xs `L.append` ys))) == L.unpack xs
    , testProperty "foldr1 is lazy" $ \ xs -> L.length xs > 0 ==>
      L.foldr1 const (explosiveTail (xs `L.append` L.singleton 1)) == L.head xs
    , testProperty "foldr1' is strict" $ expectFailure $ \ xs ys -> L.length xs > 0 ==>
      L.foldr1' const (explosiveTail (xs `L.append` L.singleton 1 `L.append` ys)) == L.head xs
    ]
  , testGroup "Lazy Char"
    [ testProperty "foldr is lazy" $ \ xs ->
        List.genericTake (D.length xs) (D.foldr (:) [ ] (explosiveTailD xs)) == D.unpack xs
    , testProperty "foldr' is strict" $ expectFailure $ \ xs ys ->
        List.genericTake (D.length xs) (D.foldr' (:) [ ] (explosiveTailD (xs `D.append` ys))) == D.unpack xs
    , testProperty "foldr1 is lazy" $ \ xs -> D.length xs > 0 ==>
        D.foldr1 const (explosiveTailD (xs `D.append` D.singleton 'x')) == D.head xs
    , testProperty "foldr1' is strict" $ expectFailure $ \ xs ys -> D.length xs > 0 ==>
        D.foldr1' const (explosiveTailD (xs `D.append` D.singleton 'x' `D.append` ys)) == D.head xs
    ]
  ]

removeFile :: String -> IO ()
removeFile fn = void $ withCString fn c_unlink
