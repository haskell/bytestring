{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE CPP              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright   : (c) 2011 Simon Meier
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- Testing composition of 'Builders'.

module Data.ByteString.Builder.Tests (tests) where

import           Prelude hiding (writeFile)

import           Control.Applicative
import           Control.Monad (unless, void)
import           Control.Monad.Trans.State (StateT, evalStateT, evalState, put, get)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, tell)

import           Foreign (minusPtr)

import           Data.Char (chr)
import           Data.Bits ((.|.), shiftL)
import           Data.Foldable
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif
import           Data.Word

import qualified Data.ByteString          as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy     as L
import qualified Data.ByteString.Short    as Sh

import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra
import           Data.ByteString.Builder.Internal (Put, putBuilder, fromPut)
import qualified Data.ByteString.Builder.Internal   as BI
import qualified Data.ByteString.Builder.Prim       as BP
import           Data.ByteString.Builder.Prim.TestUtils

import           Control.Exception (evaluate)
import           System.IO (openTempFile, hPutStr, hClose, hSetBinaryMode, hSetEncoding, utf8, hSetNewlineMode, noNewlineTranslation)
import           Foreign (ForeignPtr, withForeignPtr, castPtr)
import           Foreign.C.String (withCString)
import           Numeric (showFFloat)
import           System.Posix.Internals (c_unlink)

import           Test.Tasty (TestTree, TestName, testGroup)
import           Test.Tasty.QuickCheck
                   ( Arbitrary(..), oneof, choose, listOf, elements, forAll
                   , counterexample, ioProperty, UnicodeString(..), Property, testProperty
                   , (===), (.&&.), conjoin )


tests :: [TestTree]
tests =
  [ testBuilderRecipe
  , testHandlePutBuilder
  , testHandlePutBuilderChar8
  , testPut
  , testRunBuilder
  , testWriteFile
  ] ++
  testsEncodingToBuilder ++
  testsBinary ++
  testsASCII ++
  testsFloating ++
  testsChar8 ++
  testsUtf8


------------------------------------------------------------------------------
-- Testing 'Builder' execution
------------------------------------------------------------------------------

testBuilderRecipe :: TestTree
testBuilderRecipe =
    testProperty "toLazyByteStringWith" $ testRecipe <$> arbitrary
  where
    testRecipe r =
        counterexample msg $ x1 == x2
      where
        x1 = renderRecipe r
        x2 = buildRecipe r
        toString = map (chr . fromIntegral)
        msg = unlines
          [ "recipe: " ++ show r
          , "render: " ++ toString x1
          , "build : " ++ toString x2
          , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
          ]

testHandlePutBuilder :: TestTree
testHandlePutBuilder =
    testProperty "hPutBuilder" testRecipe
  where
    testRecipe :: (UnicodeString, UnicodeString, UnicodeString, Recipe) -> Property
    testRecipe args =
      ioProperty $ do
        let { ( UnicodeString before
              , UnicodeString between
              , UnicodeString after
              , recipe) = args }
        (tempFile, tempH) <- openTempFile "." "test-builder.tmp"
        -- switch to UTF-8 encoding
        hSetEncoding tempH utf8
        hSetNewlineMode tempH noNewlineTranslation
        -- output recipe with intermediate direct writing to handle
        let b = fst $ recipeComponents recipe
        hPutStr tempH before
        hPutBuilder tempH b
        hPutStr tempH between
        hPutBuilder tempH b
        hPutStr tempH after
        hClose tempH
        -- read file
        lbs <- L.readFile tempFile
        _ <- evaluate (L.length $ lbs)
        removeFile tempFile
        -- compare to pure builder implementation
        let lbsRef = toLazyByteString $ fold
              [stringUtf8 before, b, stringUtf8 between, b, stringUtf8 after]
        -- report
        let msg = unlines
              [ "task:     " ++ show args
              , "via file: " ++ show lbs
              , "direct :  " ++ show lbsRef
              -- , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
              ]
            success = lbs == lbsRef
        unless success (error msg)
        return success

testHandlePutBuilderChar8 :: TestTree
testHandlePutBuilderChar8 =
    testProperty "char8 hPutBuilder" testRecipe
  where
    testRecipe :: (String, String, String, Recipe) -> Property
    testRecipe args@(before, between, after, recipe) = ioProperty $ do
        (tempFile, tempH) <- openTempFile "." "TestBuilder"
        -- switch to binary / latin1 encoding
        hSetBinaryMode tempH True
        -- output recipe with intermediate direct writing to handle
        let b = fst $ recipeComponents recipe
        hPutStr tempH before
        hPutBuilder tempH b
        hPutStr tempH between
        hPutBuilder tempH b
        hPutStr tempH after
        hClose tempH
        -- read file
        lbs <- L.readFile tempFile
        _ <- evaluate (L.length $ lbs)
        removeFile tempFile
        -- compare to pure builder implementation
        let lbsRef = toLazyByteString $ fold
              [string8 before, b, string8 between, b, string8 after]
        -- report
        let msg = unlines
              [ "task:     " ++ show args
              , "via file: " ++ show lbs
              , "direct :  " ++ show lbsRef
              -- , "diff  : " ++ show (dropWhile (uncurry (==)) $ zip x1 x2)
              ]
            success = lbs == lbsRef
        unless success (error msg)
        return success

testWriteFile :: TestTree
testWriteFile =
    testProperty "writeFile" testRecipe
  where
    testRecipe :: Recipe -> Property
    testRecipe recipe =
        ioProperty $ do
            (tempFile, tempH) <- openTempFile "." "test-builder-writeFile.tmp"
            hClose tempH
            let b = fst $ recipeComponents recipe
            writeFile tempFile b
            lbs <- L.readFile tempFile
            _ <- evaluate (L.length $ lbs)
            removeFile tempFile
            let lbsRef = toLazyByteString b
            -- report
            let msg =
                    unlines
                        [ "recipe:   " ++ show recipe
                        , "via file: " ++ show lbs
                        , "direct :  " ++ show lbsRef
                        ]
                success = lbs == lbsRef
            unless success (error msg)
            return success

removeFile :: String -> IO ()
removeFile fn = void $ withCString fn c_unlink

-- Recipes with which to test the builder functions
---------------------------------------------------

data Mode =
       Threshold Int
     | Insert
     | Copy
     | Smart
     | Hex
     deriving( Eq, Ord, Show )

data Action =
       SBS Mode S.ByteString
     | LBS Mode L.ByteString
     | ShBS Sh.ShortByteString
     | W8  Word8
     | W8S [Word8]
     | String String
     | FDec Float
     | DDec Double
     | Flush
     | EnsureFree Word
     | ModState Int
     deriving( Eq, Ord, Show )

data Strategy = Safe | Untrimmed
     deriving( Eq, Ord, Show )

data Recipe = Recipe Strategy Int Int L.ByteString [Action]
     deriving( Eq, Ord, Show )

newtype DList a = DList ([a] -> [a])

instance Semigroup (DList a) where
  DList f <> DList g = DList (f . g)

instance Monoid (DList a) where
  mempty = DList id
  mappend = (<>)

fromDList :: DList a -> [a]
fromDList (DList f) = f []

toDList :: [a] -> DList a
toDList xs = DList (xs <>)

renderRecipe :: Recipe -> [Word8]
renderRecipe (Recipe _ firstSize _ cont as) =
  fromDList $ evalState (execWriterT (traverse_ renderAction as)) firstSize <> renderLBS cont
  where
    renderAction :: Monad m => Action -> WriterT (DList Word8) (StateT Int m) ()
    renderAction (SBS Hex bs)   = tell $ foldMap hexWord8 $ S.unpack bs
    renderAction (SBS _ bs)     = tell $ toDList $ S.unpack bs
    renderAction (LBS Hex lbs)  = tell $ foldMap hexWord8 $ L.unpack lbs
    renderAction (LBS _ lbs)    = tell $ renderLBS lbs
    renderAction (ShBS sbs)     = tell $ toDList $ Sh.unpack sbs
    renderAction (W8 w)         = tell $ toDList [w]
    renderAction (W8S ws)       = tell $ toDList ws
    renderAction (String cs)    = tell $ foldMap (toDList . charUtf8_list) cs
    renderAction Flush          = tell $ mempty
    renderAction (EnsureFree _) = tell $ mempty
    renderAction (FDec f)       = tell $ toDList $ encodeASCII $ show f
    renderAction (DDec d)       = tell $ toDList $ encodeASCII $ show d
    renderAction (ModState i)   = do
        s <- lift get
        tell (toDList $ encodeASCII $ show s)
        lift $ put (s - i)

    renderLBS = toDList . L.unpack
    hexWord8  = toDList . wordHexFixed_list

buildAction :: Action -> StateT Int Put ()
buildAction (SBS Hex bs)            = lift $ putBuilder $ byteStringHex bs
buildAction (SBS Smart bs)          = lift $ putBuilder $ byteString bs
buildAction (SBS Copy bs)           = lift $ putBuilder $ byteStringCopy bs
buildAction (SBS Insert bs)         = lift $ putBuilder $ byteStringInsert bs
buildAction (SBS (Threshold i) bs)  = lift $ putBuilder $ byteStringThreshold i bs
buildAction (LBS Hex lbs)           = lift $ putBuilder $ lazyByteStringHex lbs
buildAction (LBS Smart lbs)         = lift $ putBuilder $ lazyByteString lbs
buildAction (LBS Copy lbs)          = lift $ putBuilder $ lazyByteStringCopy lbs
buildAction (LBS Insert lbs)        = lift $ putBuilder $ lazyByteStringInsert lbs
buildAction (LBS (Threshold i) lbs) = lift $ putBuilder $ lazyByteStringThreshold i lbs
buildAction (ShBS sbs)              = lift $ putBuilder $ shortByteString sbs
buildAction (W8 w)                  = lift $ putBuilder $ word8 w
buildAction (W8S ws)                = lift $ putBuilder $ BP.primMapListFixed BP.word8 ws
buildAction (String cs)             = lift $ putBuilder $ stringUtf8 cs
buildAction (FDec f)                = lift $ putBuilder $ floatDec f
buildAction (DDec d)                = lift $ putBuilder $ doubleDec d
buildAction Flush                   = lift $ putBuilder $ flush
buildAction (EnsureFree minFree)    = lift $ putBuilder $ ensureFree $ fromIntegral minFree
buildAction (ModState i)            = do
    s <- get
    lift $ putBuilder $ intDec s
    put (s - i)

buildRecipe :: Recipe -> [Word8]
buildRecipe recipe =
    L.unpack $ toLBS b
  where
    (b, toLBS) = recipeComponents recipe


recipeComponents :: Recipe -> (Builder, Builder -> L.ByteString)
recipeComponents (Recipe how firstSize otherSize cont as) =
    (b, toLBS)
  where
    toLBS = toLazyByteStringWith (strategy how firstSize otherSize) cont
      where
        strategy Safe      = safeStrategy
        strategy Untrimmed = untrimmedStrategy

    b = fromPut $ evalStateT (traverse_ buildAction as) firstSize


-- 'Arbitary' instances
-----------------------

instance Arbitrary L.ByteString where
    arbitrary = L.fromChunks <$> listOf arbitrary
    shrink lbs
      | L.null lbs = []
      | otherwise = pure $ L.take (L.length lbs `div` 2) lbs

instance Arbitrary S.ByteString where
    arbitrary =
        trim S.drop =<< trim S.take =<< S.pack <$> listOf arbitrary
      where
        trim f bs = oneof [pure bs, f <$> choose (0, S.length bs) <*> pure bs]

    shrink bs
      | S.null bs = []
      | otherwise = pure $ S.take (S.length bs `div` 2) bs

instance Arbitrary Mode where
    arbitrary = oneof
        [Threshold <$> arbitrary, pure Smart, pure Insert, pure Copy, pure Hex]

    shrink (Threshold i) = Threshold <$> shrink i
    shrink _             = []

instance Arbitrary Action where
    arbitrary = oneof
      [ SBS <$> arbitrary <*> arbitrary
      , LBS <$> arbitrary <*> arbitrary
      , ShBS . Sh.toShort <$> arbitrary
      , W8  <$> arbitrary
      , W8S <$> listOf arbitrary
        -- ensure that larger character codes are also tested
      , String . getUnicodeString <$> arbitrary
      , pure Flush
        -- never request more than 64kb free space
      , (EnsureFree . (`mod` 0xffff)) <$> arbitrary
      , FDec <$> arbitrary
      , DDec <$> arbitrary
      , ModState <$> arbitrary
      ]
      where

    shrink (SBS m bs) =
      (SBS <$> shrink m <*> pure bs) <|>
      (SBS <$> pure m   <*> shrink bs)
    shrink (LBS m lbs) =
      (LBS <$> shrink m <*> pure lbs) <|>
      (LBS <$> pure m   <*> shrink lbs)
    shrink (ShBS sbs) =
      ShBS . Sh.toShort <$> shrink (Sh.fromShort sbs)
    shrink (W8 w)         = W8 <$> shrink w
    shrink (W8S ws)       = W8S <$> shrink ws
    shrink (String cs)    = String <$> shrink cs
    shrink Flush          = []
    shrink (EnsureFree i) = EnsureFree <$> shrink i
    shrink (FDec f)       = FDec <$> shrink f
    shrink (DDec d)       = DDec <$> shrink d
    shrink (ModState i)   = ModState <$> shrink i

instance Arbitrary Strategy where
    arbitrary = elements [Safe, Untrimmed]
    shrink _  = []

instance Arbitrary Recipe where
    arbitrary =
        Recipe <$> arbitrary
               <*> ((`mod` 33333) <$> arbitrary)  -- bound max chunk-sizes
               <*> ((`mod` 33337) <$> arbitrary)
               <*> arbitrary
               <*> listOf arbitrary

    -- shrinking the actions first is desirable
    shrink (Recipe a b c d e) = asum
      [ (\x -> Recipe a b c d x) <$> shrink e
      , (\x -> Recipe a b c x e) <$> shrink d
      , (\x -> Recipe a b x d e) <$> shrink c
      , (\x -> Recipe a x c d e) <$> shrink b
      , (\x -> Recipe x b c d e) <$> shrink a
      ]


------------------------------------------------------------------------------
-- Creating Builders from basic encodings
------------------------------------------------------------------------------

testsEncodingToBuilder :: [TestTree]
testsEncodingToBuilder =
  [ test_encodeUnfoldrF
  , test_encodeUnfoldrB
  ]


-- Unfoldr fused with encoding
------------------------------

test_encodeUnfoldrF :: TestTree
test_encodeUnfoldrF =
    compareImpls "encodeUnfoldrF word8" id encode
  where
    toLBS = toLazyByteStringWith (safeStrategy 23 101) L.empty
    encode =
        L.unpack . toLBS . BP.primUnfoldrFixed BP.word8 go
      where
        go []     = Nothing
        go (w:ws) = Just (w, ws)


test_encodeUnfoldrB :: TestTree
test_encodeUnfoldrB =
    compareImpls "encodeUnfoldrB charUtf8" (foldMap charUtf8_list) encode
  where
    toLBS = toLazyByteStringWith (safeStrategy 23 101) L.empty
    encode =
        L.unpack . toLBS . BP.primUnfoldrBounded BP.charUtf8 go
      where
        go []     = Nothing
        go (c:cs) = Just (c, cs)


------------------------------------------------------------------------------
-- Testing the Put monad
------------------------------------------------------------------------------

testPut :: TestTree
testPut = testGroup "Put monad"
  [ testLaw "identity" (\v -> (pure id <*> putInt v) `eqPut` (putInt v))

  , testLaw "composition" $ \(u, v, w) ->
        (pure (.) <*> minusInt u <*> minusInt v <*> putInt w) `eqPut`
        (minusInt u <*> (minusInt v <*> putInt w))

  , testLaw "homomorphism" $ \(f, x) ->
        (pure (f -) <*> pure x) `eqPut` (pure (f - x))

  , testLaw "interchange" $ \(u, y) ->
        (minusInt u <*> pure y) `eqPut` (pure ($ y) <*> minusInt u)

  , testLaw "ignore left value" $ \(u, v) ->
        (putInt u *> putInt v) `eqPut` (pure (const id) <*> putInt u <*> putInt v)

  , testLaw "ignore right value" $ \(u, v) ->
        (putInt u <* putInt v) `eqPut` (pure const <*> putInt u <*> putInt v)

  , testLaw "functor" $ \(f, x) ->
        (fmap (f -) (putInt x)) `eqPut` (pure (f -) <*> putInt x)

  ]
  where
    putInt i    = putBuilder (integerDec i) >> return i
    minusInt i  = (-) <$> putInt i
    run p       = toLazyByteString $ fromPut (do i <- p; _ <- putInt i; return ())
    eqPut p1 p2 = (run p1, run p2)

    testLaw name f = compareImpls name (fst . f) (snd . f)


------------------------------------------------------------------------------
-- Testing the Driver <-> Builder protocol
------------------------------------------------------------------------------

-- | Ensure that there are at least 'n' free bytes for the following 'Builder'.
{-# INLINE ensureFree #-}
ensureFree :: Int -> Builder
ensureFree minFree =
    BI.builder step
  where
    step k br@(BI.BufferRange op ope)
      | ope `minusPtr` op < minFree = return $ BI.bufferFull minFree op next
      | otherwise                   = k br
      where
        next br'@(BI.BufferRange op' ope')
          |  freeSpace < minFree =
              error $ "ensureFree: requested " ++ show minFree ++ " bytes, " ++
                      "but got only " ++ show freeSpace ++ " bytes"
          | otherwise = k br'
          where
            freeSpace = ope' `minusPtr` op'


------------------------------------------------------------------------------
-- Testing the Builder runner
------------------------------------------------------------------------------

testRunBuilder :: TestTree
testRunBuilder =
    testProperty "runBuilder" prop
  where
    prop actions =
        ioProperty $ do
          let (builder, _) = recipeComponents recipe
              expected     = renderRecipe recipe
          actual <- bufferWriterOutput (runBuilder builder)
          return (S.unpack actual == expected)
      where
        recipe = Recipe Safe 0 0 L.empty actions

bufferWriterOutput :: BufferWriter -> IO S.ByteString
bufferWriterOutput bwrite0 = do
    let len0 = 8
    buf <- S.mallocByteString len0
    bss <- go [] buf len0 bwrite0
    return (S.concat (reverse bss))
  where
    go :: [S.ByteString] -> ForeignPtr Word8 -> Int -> BufferWriter -> IO [S.ByteString]
    go bss !buf !len bwrite = do
      (wc, next) <- withForeignPtr buf $ \ptr -> bwrite ptr len
      bs <- getBuffer buf wc
      case next of
        Done                        -> return (bs:bss)
        More  m bwrite' | m <= len  -> go (bs:bss)   buf len bwrite'
                        | otherwise -> do let len' = m
                                          buf' <- S.mallocByteString len'
                                          go (bs:bss) buf' len' bwrite'
        Chunk c bwrite'             -> go (c:bs:bss) buf len bwrite'

    getBuffer :: ForeignPtr Word8 -> Int -> IO S.ByteString
    getBuffer buf len = withForeignPtr buf $ \ptr ->
                          S.packCStringLen (castPtr ptr, len)


------------------------------------------------------------------------------
-- Testing the pre-defined builders
------------------------------------------------------------------------------

testBuilderConstr :: (Arbitrary a, Show a)
                  => TestName -> (a -> [Word8]) -> (a -> Builder) -> TestTree
testBuilderConstr name ref mkBuilder =
    testProperty name check
  where
    check x = forAll (choose (0, maxPaddingAmount)) $ \paddingAmount -> let
      -- use padding to make sure we test at unaligned positions
      ws = ref x
      b1 = mkBuilder x
      b2 = byteStringCopy (S.take paddingAmount padBuf) <> b1 <> b1
      in (replicate paddingAmount (S.c2w ' ') ++ ws ++ ws) ===
         (L.unpack $ toLazyByteString b2)

    maxPaddingAmount = 15
    padBuf = S.replicate maxPaddingAmount (S.c2w ' ')


testsBinary :: [TestTree]
testsBinary =
  [ testBuilderConstr "word8"     bigEndian_list    word8
  , testBuilderConstr "int8"      bigEndian_list    int8

  --  big-endian
  , testBuilderConstr "int16BE"   bigEndian_list    int16BE
  , testBuilderConstr "int32BE"   bigEndian_list    int32BE
  , testBuilderConstr "int64BE"   bigEndian_list    int64BE

  , testBuilderConstr "word16BE"  bigEndian_list    word16BE
  , testBuilderConstr "word32BE"  bigEndian_list    word32BE
  , testBuilderConstr "word64BE"  bigEndian_list    word64BE

  , testBuilderConstr "floatLE"     (float_list  littleEndian_list) floatLE
  , testBuilderConstr "doubleLE"    (double_list littleEndian_list) doubleLE

  --  little-endian
  , testBuilderConstr "int16LE"   littleEndian_list int16LE
  , testBuilderConstr "int32LE"   littleEndian_list int32LE
  , testBuilderConstr "int64LE"   littleEndian_list int64LE

  , testBuilderConstr "word16LE"  littleEndian_list word16LE
  , testBuilderConstr "word32LE"  littleEndian_list word32LE
  , testBuilderConstr "word64LE"  littleEndian_list word64LE

  , testBuilderConstr "floatBE"     (float_list  bigEndian_list)   floatBE
  , testBuilderConstr "doubleBE"    (double_list bigEndian_list)   doubleBE

  --  host dependent
  , testBuilderConstr "int16Host"   hostEndian_list  int16Host
  , testBuilderConstr "int32Host"   hostEndian_list  int32Host
  , testBuilderConstr "int64Host"   hostEndian_list  int64Host
  , testBuilderConstr "intHost"     hostEndian_list  intHost

  , testBuilderConstr "word16Host"  hostEndian_list  word16Host
  , testBuilderConstr "word32Host"  hostEndian_list  word32Host
  , testBuilderConstr "word64Host"  hostEndian_list  word64Host
  , testBuilderConstr "wordHost"    hostEndian_list  wordHost

  , testBuilderConstr "floatHost"   (float_list  hostEndian_list)   floatHost
  , testBuilderConstr "doubleHost"  (double_list hostEndian_list)   doubleHost
  ]

testsASCII :: [TestTree]
testsASCII =
  [ testBuilderConstr "char7" char7_list char7
  , testBuilderConstr "string7" (foldMap char7_list) string7

  , testBuilderConstr "int8Dec"   dec_list int8Dec
  , testBuilderConstr "int16Dec"  dec_list int16Dec
  , testBuilderConstr "int32Dec"  dec_list int32Dec
  , testBuilderConstr "int64Dec"  dec_list int64Dec
  , testBuilderConstr "intDec"    dec_list intDec

  , testBuilderConstr "word8Dec"  dec_list word8Dec
  , testBuilderConstr "word16Dec" dec_list word16Dec
  , testBuilderConstr "word32Dec" dec_list word32Dec
  , testBuilderConstr "word64Dec" dec_list word64Dec
  , testBuilderConstr "wordDec"   dec_list wordDec

  , testBuilderConstr "integerDec" (dec_list . enlarge) (integerDec . enlarge)
  , testBuilderConstr "floatDec"   dec_list floatDec
  , testBuilderConstr "doubleDec"  dec_list doubleDec

  , testBuilderConstr "word8Hex"  hex_list word8Hex
  , testBuilderConstr "word16Hex" hex_list word16Hex
  , testBuilderConstr "word32Hex" hex_list word32Hex
  , testBuilderConstr "word64Hex" hex_list word64Hex
  , testBuilderConstr "wordHex"   hex_list wordHex

  , testBuilderConstr "word8HexFixed"  wordHexFixed_list word8HexFixed
  , testBuilderConstr "word16HexFixed" wordHexFixed_list word16HexFixed
  , testBuilderConstr "word32HexFixed" wordHexFixed_list word32HexFixed
  , testBuilderConstr "word64HexFixed" wordHexFixed_list word64HexFixed

  , testBuilderConstr "int8HexFixed"  int8HexFixed_list  int8HexFixed
  , testBuilderConstr "int16HexFixed" int16HexFixed_list int16HexFixed
  , testBuilderConstr "int32HexFixed" int32HexFixed_list int32HexFixed
  , testBuilderConstr "int64HexFixed" int64HexFixed_list int64HexFixed

  , testBuilderConstr "floatHexFixed"  floatHexFixed_list  floatHexFixed
  , testBuilderConstr "doubleHexFixed" doubleHexFixed_list doubleHexFixed
  ]
  where
    enlarge (n, e) = n ^ (abs (e `mod` (50 :: Integer)))

testsFloating :: [TestTree]
testsFloating =
  [ testMatches "f2sBasic" floatDec show
        [ ( 0.0    , "0.0" )
        , ( (-0.0) , "-0.0" )
        , ( 1.0    , "1.0" )
        , ( (-1.0) , "-1.0" )
        , ( (0/0)  , "NaN" )
        , ( (1/0)  , "Infinity" )
        , ( (-1/0) , "-Infinity" )
        ]
  , testMatches "f2sSubnormal" floatDec show
        [ ( 1.1754944e-38 , "1.1754944e-38" )
        ]
  , testMatches "f2sMinAndMax" floatDec show
        [ ( coerceWord32ToFloat 0x7f7fffff , "3.4028235e38" )
        , ( coerceWord32ToFloat 0x00000001 , "1.0e-45" )
        ]
  , testMatches "f2sBoundaryRound" floatDec show
        [ ( 3.355445e7   , "3.3554448e7" )
        , ( 8.999999e9   , "8.999999e9" )
        , ( 3.4366717e10 , "3.4366718e10" )
        ]
  , testMatches "f2sExactValueRound" floatDec show
        [ ( 3.0540412e5 , "305404.13" )
        , ( 8.0990312e3 , "8099.0313" )
        ]
  , testMatches "f2sTrailingZeros" floatDec show
        -- Pattern for the first test: 00111001100000000000000000000000
        [ ( 2.4414062e-4 , "2.4414063e-4" )
        , ( 2.4414062e-3 , "2.4414063e-3" )
        , ( 4.3945312e-3 , "4.3945313e-3" )
        , ( 6.3476562e-3 , "6.3476563e-3" )
        ]
  , testMatches "f2sRegression" floatDec show
        [ ( 4.7223665e21   , "4.7223665e21" )
        , ( 8388608.0      , "8388608.0" )
        , ( 1.6777216e7    , "1.6777216e7" )
        , ( 3.3554436e7    , "3.3554436e7" )
        , ( 6.7131496e7    , "6.7131496e7" )
        , ( 1.9310392e-38  , "1.9310392e-38" )
        , ( (-2.47e-43)    , "-2.47e-43" )
        , ( 1.993244e-38   , "1.993244e-38" )
        , ( 4103.9003      , "4103.9004" )
        , ( 5.3399997e9    , "5.3399997e9" )
        , ( 6.0898e-39     , "6.0898e-39" )
        , ( 0.0010310042   , "1.0310042e-3" )
        , ( 2.8823261e17   , "2.882326e17" )
        , ( 7.0385309e-26  , "7.038531e-26" )
        , ( 9.2234038e17   , "9.223404e17" )
        , ( 6.7108872e7    , "6.710887e7" )
        , ( 1.0e-44        , "1.0e-44" )
        , ( 2.816025e14    , "2.816025e14" )
        , ( 9.223372e18    , "9.223372e18" )
        , ( 1.5846085e29   , "1.5846086e29" )
        , ( 1.1811161e19   , "1.1811161e19" )
        , ( 5.368709e18    , "5.368709e18" )
        , ( 4.6143165e18   , "4.6143166e18" )
        , ( 0.007812537    , "7.812537e-3" )
        , ( 1.4e-45        , "1.0e-45" )
        , ( 1.18697724e20  , "1.18697725e20" )
        , ( 1.00014165e-36 , "1.00014165e-36" )
        , ( 200.0          , "200.0" )
        , ( 3.3554432e7    , "3.3554432e7" )
        , ( 2.0019531      , "2.0019531" )
        , ( 2.001953       , "2.001953" )
        ]
  , testExpected "f2sScientific" (formatFloat scientific)
        [ ( 0.0            , "0.0e0"         )
        , ( 8388608.0      , "8.388608e6"    )
        , ( 1.6777216e7    , "1.6777216e7"   )
        , ( 3.3554436e7    , "3.3554436e7"   )
        , ( 6.7131496e7    , "6.7131496e7"   )
        , ( 1.9310392e-38  , "1.9310392e-38" )
        , ( (-2.47e-43)    , "-2.47e-43"     )
        , ( 1.993244e-38   , "1.993244e-38"  )
        , ( 4103.9003      , "4.1039004e3"   )
        , ( 0.0010310042   , "1.0310042e-3"  )
        , ( 0.007812537    , "7.812537e-3"   )
        , ( 200.0          , "2.0e2"         )
        , ( 2.0019531      , "2.0019531e0"   )
        , ( 2.001953       , "2.001953e0"    )
        ]
  , testMatches "f2sLooksLikePowerOf5" floatDec show
        [ ( coerceWord32ToFloat 0x5D1502F9 , "6.7108864e17" )
        , ( coerceWord32ToFloat 0x5D9502F9 , "1.3421773e18" )
        , ( coerceWord32ToFloat 0x5e1502F9 , "2.6843546e18" )
        ]
  , testMatches "f2sOutputLength" floatDec show
        [ ( 1.0            , "1.0" )
        , ( 1.2            , "1.2" )
        , ( 1.23           , "1.23" )
        , ( 1.234          , "1.234" )
        , ( 1.2345         , "1.2345" )
        , ( 1.23456        , "1.23456" )
        , ( 1.234567       , "1.234567" )
        , ( 1.2345678      , "1.2345678" )
        , ( 1.23456735e-36 , "1.23456735e-36" )
        ]
  , testMatches "d2sBasic" doubleDec show
        [ ( 0.0    , "0.0" )
        , ( (-0.0) , "-0.0" )
        , ( 1.0    , "1.0" )
        , ( (-1.0) , "-1.0" )
        , ( (0/0)  , "NaN" )
        , ( (1/0)  , "Infinity" )
        , ( (-1/0) , "-Infinity" )
        ]
  , testMatches "d2sSubnormal" doubleDec show
        [ ( 2.2250738585072014e-308 , "2.2250738585072014e-308" )
        ]
  , testMatches "d2sMinAndMax" doubleDec show
        [ ( (coerceWord64ToDouble 0x7fefffffffffffff) , "1.7976931348623157e308" )
        , ( (coerceWord64ToDouble 0x0000000000000001) , "5.0e-324" )
        ]
  , testMatches "d2sTrailingZeros" doubleDec show
        [ ( 2.98023223876953125e-8 , "2.9802322387695313e-8" )
        ]
  , testMatches "d2sRegression" doubleDec show
        [ ( (-2.109808898695963e16) , "-2.1098088986959632e16" )
        , ( 4.940656e-318           , "4.940656e-318" )
        , ( 1.18575755e-316         , "1.18575755e-316" )
        , ( 2.989102097996e-312     , "2.989102097996e-312" )
        , ( 9.0608011534336e15      , "9.0608011534336e15" )
        , ( 4.708356024711512e18    , "4.708356024711512e18" )
        , ( 9.409340012568248e18    , "9.409340012568248e18" )
        , ( 1.2345678               , "1.2345678" )
        , ( 1.9430376160308388e16   , "1.9430376160308388e16" )
        , ( (-6.9741824662760956e19), "-6.9741824662760956e19" )
        , ( 4.3816050601147837e18   , "4.3816050601147837e18" )
        ]
  , testExpected "d2sScientific" (formatDouble scientific)
        [ ( 0.0         , "0.0e0"         )
        , ( 1.2345678   , "1.2345678e0"   )
        , ( 4.294967294 , "4.294967294e0" )
        , ( 4.294967295 , "4.294967295e0" )
        ]
  , testProperty "d2sStandard" $ conjoin
        [ singleMatches (formatDouble (standard 2)) (flip (showFFloat (Just 2)) []) ( 12.345 , "12.34"    )
        , singleMatches (formatDouble (standard 2)) (flip (showFFloat (Just 2)) []) ( 0.0050 , "0.00"     )
        , singleMatches (formatDouble (standard 2)) (flip (showFFloat (Just 2)) []) ( 0.0051 , "0.01"     )
        , singleMatches (formatDouble (standard 5)) (flip (showFFloat (Just 5)) []) ( 12.345 , "12.34500" )
        ]
  , testMatches "d2sLooksLikePowerOf5" doubleDec show
        [ ( (coerceWord64ToDouble 0x4830F0CF064DD592) , "5.764607523034235e39" )
        , ( (coerceWord64ToDouble 0x4840F0CF064DD592) , "1.152921504606847e40" )
        , ( (coerceWord64ToDouble 0x4850F0CF064DD592) , "2.305843009213694e40" )
        , ( (coerceWord64ToDouble 0x4400000000000004) , "3.689348814741914e19" )

        -- here v- is a power of 5 but since we don't accept bounds there is no
        -- interesting trailing behavior
        , ( (coerceWord64ToDouble 0x440000000000301d) , "3.6893488147520004e19" )
        ]
  , testMatches "d2sOutputLength" doubleDec show
        [ ( 1                  , "1.0" )
        , ( 1.2                , "1.2" )
        , ( 1.23               , "1.23" )
        , ( 1.234              , "1.234" )
        , ( 1.2345             , "1.2345" )
        , ( 1.23456            , "1.23456" )
        , ( 1.234567           , "1.234567" )
        , ( 1.2345678          , "1.2345678" )
        , ( 1.23456789         , "1.23456789" )
        , ( 1.234567895        , "1.234567895" )
        , ( 1.2345678901       , "1.2345678901" )
        , ( 1.23456789012      , "1.23456789012" )
        , ( 1.234567890123     , "1.234567890123" )
        , ( 1.2345678901234    , "1.2345678901234" )
        , ( 1.23456789012345   , "1.23456789012345" )
        , ( 1.234567890123456  , "1.234567890123456" )
        , ( 1.2345678901234567 , "1.2345678901234567" )

        -- Test 32-bit chunking
        , ( 4.294967294 , "4.294967294" )
        , ( 4.294967295 , "4.294967295" )
        , ( 4.294967296 , "4.294967296" )
        , ( 4.294967297 , "4.294967297" )
        , ( 4.294967298 , "4.294967298" )
        ]
  , testMatches "d2sMinMaxShift" doubleDec show
        [ ( (ieeeParts2Double False 4 0) , "1.7800590868057611e-307" )
        -- 32-bit opt-size=0:  49 <= dist <= 49
        -- 32-bit opt-size=1:  28 <= dist <= 49
        -- 64-bit opt-size=0:  50 <= dist <= 50
        -- 64-bit opt-size=1:  28 <= dist <= 50
        , ( (ieeeParts2Double False 6 maxMantissa) , "2.8480945388892175e-306" )
        -- 32-bit opt-size=0:  52 <= dist <= 53
        -- 32-bit opt-size=1:   2 <= dist <= 53
        -- 64-bit opt-size=0:  53 <= dist <= 53
        -- 64-bit opt-size=1:   2 <= dist <= 53
        , ( (ieeeParts2Double False 41 0) , "2.446494580089078e-296" )
        -- 32-bit opt-size=0:  52 <= dist <= 52
        -- 32-bit opt-size=1:   2 <= dist <= 52
        -- 64-bit opt-size=0:  53 <= dist <= 53
        -- 64-bit opt-size=1:   2 <= dist <= 53
        , ( (ieeeParts2Double False 40 maxMantissa) , "4.8929891601781557e-296" )
        -- 32-bit opt-size=0:  57 <= dist <= 58
        -- 32-bit opt-size=1:  57 <= dist <= 58
        -- 64-bit opt-size=0:  58 <= dist <= 58
        -- 64-bit opt-size=1:  58 <= dist <= 58
        , ( (ieeeParts2Double False 1077 0) , "1.8014398509481984e16" )
        -- 32-bit opt-size=0:  57 <= dist <= 57
        -- 32-bit opt-size=1:  57 <= dist <= 57
        -- 64-bit opt-size=0:  58 <= dist <= 58
        -- 64-bit opt-size=1:  58 <= dist <= 58
        , ( (ieeeParts2Double False 1076 maxMantissa) , "3.6028797018963964e16" )
        -- 32-bit opt-size=0:  51 <= dist <= 52
        -- 32-bit opt-size=1:  51 <= dist <= 59
        -- 64-bit opt-size=0:  52 <= dist <= 52
        -- 64-bit opt-size=1:  52 <= dist <= 59
        , ( (ieeeParts2Double False 307 0) , "2.900835519859558e-216" )
        -- 32-bit opt-size=0:  51 <= dist <= 51
        -- 32-bit opt-size=1:  51 <= dist <= 59
        -- 64-bit opt-size=0:  52 <= dist <= 52
        -- 64-bit opt-size=1:  52 <= dist <= 59
        , ( (ieeeParts2Double False 306 maxMantissa) , "5.801671039719115e-216" )
        -- 32-bit opt-size=0:  49 <= dist <= 49
        -- 32-bit opt-size=1:  44 <= dist <= 49
        -- 64-bit opt-size=0:  50 <= dist <= 50
        -- 64-bit opt-size=1:  44 <= dist <= 50
        , ( (ieeeParts2Double False 934 0x000FA7161A4D6e0C) , "3.196104012172126e-27" )
        ]
  , testMatches "d2sSmallIntegers" doubleDec show
        [ ( 9007199254740991.0 , "9.007199254740991e15" )
        , ( 9007199254740992.0 , "9.007199254740992e15" )

        , ( 1.0e+0                , "1.0" )
        , ( 1.2e+1                , "12.0" )
        , ( 1.23e+2               , "123.0" )
        , ( 1.234e+3              , "1234.0" )
        , ( 1.2345e+4             , "12345.0" )
        , ( 1.23456e+5            , "123456.0" )
        , ( 1.234567e+6           , "1234567.0" )
        , ( 1.2345678e+7          , "1.2345678e7" )
        , ( 1.23456789e+8         , "1.23456789e8" )
        , ( 1.23456789e+9         , "1.23456789e9" )
        , ( 1.234567895e+9        , "1.234567895e9" )
        , ( 1.2345678901e+10      , "1.2345678901e10" )
        , ( 1.23456789012e+11     , "1.23456789012e11" )
        , ( 1.234567890123e+12    , "1.234567890123e12" )
        , ( 1.2345678901234e+13   , "1.2345678901234e13" )
        , ( 1.23456789012345e+14  , "1.23456789012345e14" )
        , ( 1.234567890123456e+15 , "1.234567890123456e15" )

        -- 10^i
        , ( 1.0e+0  , "1.0" )
        , ( 1.0e+1  , "10.0" )
        , ( 1.0e+2  , "100.0" )
        , ( 1.0e+3  , "1000.0" )
        , ( 1.0e+4  , "10000.0" )
        , ( 1.0e+5  , "100000.0" )
        , ( 1.0e+6  , "1000000.0" )
        , ( 1.0e+7  , "1.0e7" )
        , ( 1.0e+8  , "1.0e8" )
        , ( 1.0e+9  , "1.0e9" )
        , ( 1.0e+10 , "1.0e10" )
        , ( 1.0e+11 , "1.0e11" )
        , ( 1.0e+12 , "1.0e12" )
        , ( 1.0e+13 , "1.0e13" )
        , ( 1.0e+14 , "1.0e14" )
        , ( 1.0e+15 , "1.0e15" )

        -- 10^15 + 10^i
        , ( (1.0e+15 + 1.0e+0)  , "1.000000000000001e15" )
        , ( (1.0e+15 + 1.0e+1)  , "1.00000000000001e15" )
        , ( (1.0e+15 + 1.0e+2)  , "1.0000000000001e15" )
        , ( (1.0e+15 + 1.0e+3)  , "1.000000000001e15" )
        , ( (1.0e+15 + 1.0e+4)  , "1.00000000001e15" )
        , ( (1.0e+15 + 1.0e+5)  , "1.0000000001e15" )
        , ( (1.0e+15 + 1.0e+6)  , "1.000000001e15" )
        , ( (1.0e+15 + 1.0e+7)  , "1.00000001e15" )
        , ( (1.0e+15 + 1.0e+8)  , "1.0000001e15" )
        , ( (1.0e+15 + 1.0e+9)  , "1.000001e15" )
        , ( (1.0e+15 + 1.0e+10) , "1.00001e15" )
        , ( (1.0e+15 + 1.0e+11) , "1.0001e15" )
        , ( (1.0e+15 + 1.0e+12) , "1.001e15" )
        , ( (1.0e+15 + 1.0e+13) , "1.01e15" )
        , ( (1.0e+15 + 1.0e+14) , "1.1e15" )

        -- Largest power of 2 <= 10^(i+1)
        , ( 8.0                , "8.0" )
        , ( 64.0               , "64.0" )
        , ( 512.0              , "512.0" )
        , ( 8192.0             , "8192.0" )
        , ( 65536.0            , "65536.0" )
        , ( 524288.0           , "524288.0" )
        , ( 8388608.0          , "8388608.0" )
        , ( 67108864.0         , "6.7108864e7" )
        , ( 536870912.0        , "5.36870912e8" )
        , ( 8589934592.0       , "8.589934592e9" )
        , ( 68719476736.0      , "6.8719476736e10" )
        , ( 549755813888.0     , "5.49755813888e11" )
        , ( 8796093022208.0    , "8.796093022208e12" )
        , ( 70368744177664.0   , "7.0368744177664e13" )
        , ( 562949953421312.0  , "5.62949953421312e14" )
        , ( 9007199254740992.0 , "9.007199254740992e15" )

        -- 1000 * (Largest power of 2 <= 10^(i+1))
        , ( 8.0e+3             , "8000.0" )
        , ( 64.0e+3            , "64000.0" )
        , ( 512.0e+3           , "512000.0" )
        , ( 8192.0e+3          , "8192000.0" )
        , ( 65536.0e+3         , "6.5536e7" )
        , ( 524288.0e+3        , "5.24288e8" )
        , ( 8388608.0e+3       , "8.388608e9" )
        , ( 67108864.0e+3      , "6.7108864e10" )
        , ( 536870912.0e+3     , "5.36870912e11" )
        , ( 8589934592.0e+3    , "8.589934592e12" )
        , ( 68719476736.0e+3   , "6.8719476736e13" )
        , ( 549755813888.0e+3  , "5.49755813888e14" )
        , ( 8796093022208.0e+3 , "8.796093022208e15" )
        ]
  , testMatches "f2sPowersOf10" floatDec show $
        fmap asShowRef [read ("1.0e" ++ show x) :: Float | x <- [-46..39 :: Int]]
  , testMatches "d2sPowersOf10" doubleDec show $
        fmap asShowRef [read ("1.0e" ++ show x) :: Double | x <- [-324..309 :: Int]]
  ]
  where
    testExpected :: TestName -> (a -> Builder) -> [(a, String)] -> TestTree
    testExpected name dec lst = testProperty name . conjoin $
      fmap (\(x, ref) -> L.unpack (toLazyByteString (dec x)) === encodeASCII ref) lst

    singleMatches :: (a -> Builder) -> (a -> String) -> (a, String) -> Property
    singleMatches dec refdec (x, ref) = L.unpack (toLazyByteString (dec x)) === encodeASCII (refdec x) .&&. refdec x === ref

    testMatches :: TestName -> (a -> Builder) -> (a -> String) -> [(a, String)] -> TestTree
    testMatches name dec refdec lst = testProperty name . conjoin $ fmap (singleMatches dec refdec) lst

    maxMantissa = (1 `shiftL` 53) - 1 :: Word64

    ieeeParts2Double :: Bool -> Int -> Word64 -> Double
    ieeeParts2Double sign expo mantissa =
        coerceWord64ToDouble $ (fromIntegral (fromEnum sign) `shiftL` 63) .|. (fromIntegral expo `shiftL` 52) .|. mantissa

    asShowRef x = (x, show x)

testsChar8 :: [TestTree]
testsChar8 =
  [ testBuilderConstr "charChar8" char8_list char8
  , testBuilderConstr "stringChar8" (foldMap char8_list) string8
  ]

testsUtf8 :: [TestTree]
testsUtf8 =
  [ testBuilderConstr "charUtf8" charUtf8_list charUtf8
  , testBuilderConstr "stringUtf8" (foldMap charUtf8_list) stringUtf8
  ]
