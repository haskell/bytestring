{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Bits (shiftR, (.&.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (chr, ord)
import Data.Word (Word8)
import Test.QuickCheck (Property, forAll, (===))
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Test.QuickCheck.Gen (oneof, Gen, choose, vectorOf, listOf1, sized, resize,
                            elements)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

main :: IO ()
main = defaultMain . testGroup "UTF-8 validation" $ [
  testProperty "Valid UTF-8" goValid,
  testProperty "Invalid UTF-8" goInvalid
  ]
  where
    goValid :: Property
    goValid = forAll arbitrary $
      \(ValidUtf8 ss) -> (B.isValidUtf8 . foldMap sequenceToBS $ ss) === True
    goInvalid :: Property
    goInvalid = forAll arbitrary $ 
      \inv -> (B.isValidUtf8 . toByteString $ inv) === False

-- Helpers

data Utf8Sequence = 
  One Word8 |
  Two Word8 Word8 |
  Three Word8 Word8 Word8 |
  Four Word8 Word8 Word8 Word8
  deriving (Eq)

instance Arbitrary Utf8Sequence where
  arbitrary = oneof [
    One <$> elements [0x00 .. 0x7F],
    Two <$> elements [0xC2 .. 0xDF] <*> elements [0x80 .. 0xBF],
    genThree,
    genFour
    ]
    where
      genThree :: Gen Utf8Sequence
      genThree = do
        w1 <- elements [0xE0 .. 0xED]
        w2 <- elements $ case w1 of 
          0xE0 -> [0xA0 .. 0xBF]
          0xED -> [0x80 .. 0x9F]
          _ -> [0x80 .. 0xBF]
        w3 <- elements [0x80 .. 0xBF]
        pure . Three w1 w2 $ w3
      genFour :: Gen Utf8Sequence
      genFour = do
        w1 <- elements [0xF0 .. 0xF4]
        w2 <- elements $ case w1 of 
          0xF0 -> [0x90 .. 0xBF]
          0xF4 -> [0x80 .. 0x8F]
          _ -> [0x80 .. 0xBF]
        w3 <- elements [0x80 .. 0xBF]
        w4 <- elements [0x80 .. 0xBF]
        pure . Four w1 w2 w3 $ w4
  shrink = \case
    One w1 -> One <$> case w1 of 
      0x00 -> []
      _ -> [0x00 .. (w1 - 1)]
    Two w1 w2 -> case (w1, w2) of 
      (0xC2, 0x80) -> allOnes
      _ -> (Two <$> [0xC2 .. (w1 - 1)] <*> [0x80 .. (w2 - 1)]) ++ allOnes
    Three w1 w2 w3 -> case (w1, w2, w3) of 
      (0xE0, 0xA0, 0x80) -> allTwos ++ allOnes
      (0xE0, 0xA0, _) -> (Three 0xE0 0xA0 <$> [0x80 .. (w3 - 1)]) ++ allTwos ++ allOnes
      (0xE0, _, _) -> 
        (Three 0xE0 <$> [0xA0 .. (w2 - 1)] <*> [0x80 .. (w3 - 1)]) ++ allTwos ++ allOnes
      _ -> do
        w1' <- [0xE0 .. (w1 - 1)]
        case w1' of 
          0xE0 -> (Three 0xE0 <$> [0xA0 .. 0xBF] <*> [0x80 .. 0xBF]) ++ 
                  allTwos ++ 
                  allOnes
          _ -> (Three w1' <$> [0x80 .. 0xBF] <*> [0x80 .. 0xBF]) ++ 
               allTwos ++ 
               allOnes
    Four w1 w2 w3 w4 -> case (w1, w2, w3, w4) of 
      (0xF0, 0x90, 0x80, 0x80) -> allThrees ++ allTwos ++ allOnes
      (0xF0, 0x90, 0x80, _) -> 
        (Four 0xF0 0x90 0x80 <$> [0x80 .. (w4 - 1)]) ++ 
        allThrees ++
        allTwos ++
        allOnes
      (0xF0, 0x90, _, _) -> 
        (Four 0xF0 0x90 <$> [0x80 .. (w3 - 1)] <*> [0x80 .. (w4 - 1)]) ++
        allThrees ++
        allTwos ++
        allOnes
      (0xF0, _, _, _) -> 
        (Four 0xF0 <$> [0x90 .. (w2 - 1)] <*> [0x80 .. (w3 - 1)] <*> [0x80 .. (w4 - 1)]) ++
        allThrees ++
        allTwos ++
        allOnes
      _ -> do
        w1' <- [0xF0 .. (w1 - 1)]
        case w1' of 
          0xF0 -> (Four 0xF0 <$> [0x90 .. 0xBF] <*> [0x80 .. 0xBF] <*> [0x80 .. 0xBF]) ++
                  allThrees ++
                  allTwos ++
                  allOnes
          _ -> (Four w1' <$> [0x80 .. 0xBF] <*> [0x80 .. 0xBF] <*> [0x80 .. 0xBF]) ++
               allThrees ++
               allTwos ++
               allOnes

allOnes :: [Utf8Sequence]
allOnes = One <$> [0x00 .. 0x7F]

allTwos :: [Utf8Sequence]
allTwos = Two <$> [0xC2 .. 0xDF] <*> [0x80 .. 0xBF]

allThrees :: [Utf8Sequence]
allThrees = (Three 0xE0 <$> [0xA0 .. 0xBF] <*> [0x80 .. 0xBF]) ++ 
            (Three 0xED <$> [0x80 .. 0x9F] <*> [0x80 .. 0xBF]) ++
            (Three <$> [0xE1 .. 0xEC] <*> [0x80 .. 0xBF] <*> [0x80 .. 0xBF]) ++
            (Three <$> [0xEE .. 0xEF] <*> [0x80 .. 0xBF] <*> [0x80 .. 0xBF])

sequenceToBS :: Utf8Sequence -> ByteString
sequenceToBS = B.pack . \case
  One w1 -> [w1]
  Two w1 w2 -> [w1, w2]
  Three w1 w2 w3 -> [w1, w2, w3]
  Four w1 w2 w3 w4 -> [w1, w2, w3, w4]

newtype ValidUtf8 = ValidUtf8 [Utf8Sequence]
  deriving (Eq)

instance Show ValidUtf8 where
  show (ValidUtf8 ss) = show . foldMap sequenceToBS $ ss

instance Arbitrary ValidUtf8 where
  arbitrary = ValidUtf8 <$> arbitrary
  shrink (ValidUtf8 ss) = ValidUtf8 <$> shrink ss

data InvalidUtf8 = InvalidUtf8 {
  prefix :: ByteString,
  invalid :: ByteString,
  suffix :: ByteString
  }
  deriving (Eq)

instance Show InvalidUtf8 where
  show i = "InvalidUtf8 {prefix = " ++ show (prefix i)
                  ++ ", invalid = " ++ show (invalid i)
                  ++ ", suffix = " ++ show (suffix i)
                  ++ ", asBS = "   ++ show (toByteString i)
                  ++ ", length = " ++ show (B.length . toByteString $ i)
                  ++ "}"

instance Arbitrary InvalidUtf8 where
  arbitrary = oneof
    [ InvalidUtf8 mempty <$> genInvalidUtf8 <*> pure mempty
    , InvalidUtf8 mempty <$> genInvalidUtf8 <*> genBS
    , InvalidUtf8 <$> genValidUtf8 <*> genInvalidUtf8 <*> pure mempty
    , InvalidUtf8 <$> genValidUtf8 <*> genInvalidUtf8 <*> genBS
    ]
  shrink (InvalidUtf8 p i s) = 
    (InvalidUtf8 p i <$> shrinkBS s) ++
    ((\p' -> InvalidUtf8 p' i s) <$> shrinkBS p)

toByteString :: InvalidUtf8 -> ByteString
toByteString (InvalidUtf8 p i s) = p `B.append` i `B.append` s

genInvalidUtf8 :: Gen ByteString
genInvalidUtf8 = B.pack <$> oneof [
    -- invalid leading byte of a 2-byte sequence
    (:) <$> choose (0xC0, 0xC1) <*> upTo 1 contByte
    -- invalid leading byte of a 4-byte sequence
  , (:) <$> choose (0xF5, 0xFF) <*> upTo 3 contByte
    -- 4-byte sequence greater than U+10FFF
  , do k <- choose (0x11, 0x13)
       let w0 = 0xF0 + (k `shiftR` 2)
       let w1 = 0x80 + ((k .&. 3) `shiftL` 4)
       ([w0, w1] ++) <$> vectorOf 2 contByte
    -- continuation bytes without a start byte
  , listOf1 contByte
    -- short 2-byte sequence
  , (:[]) <$> choose (0xC2, 0xDF)
    -- short 3-byte sequence
  , (:) <$> choose (0xE0, 0xEF) <*> upTo 1 contByte
    -- short 4-byte sequence
  , (:) <$> choose (0xF0, 0xF4) <*> upTo 2 contByte
    -- overlong encoding
  , do k <- choose (0, 0xFFFF)
       let c = chr k
       case k of 
        _ | k < 0x80    -> oneof [ let (w, x)       = ord2 c in pure [w, x]
                                 , let (w, x, y)    = ord3 c in pure [w, x, y]
                                 , let (w, x, y, z) = ord4 c in pure [w, x, y, z] ]
          | k < 0x7FF   -> oneof [ let (w, x, y)    = ord3 c in pure [w, x, y]
                                 , let (w, x, y, z) = ord4 c in pure [w, x, y, z] ]
          | otherwise   -> oneof [ let (w, x, y, z) = ord4 c in pure [w, x, y, z] ]
  ]
  where
    contByte :: Gen Word8
    contByte = (0x80 +) <$> choose (0, 0x3F)
    upTo :: Int -> Gen a -> Gen [a]
    upTo n gen = do
      k <- choose (0, n)
      vectorOf k gen

genValidUtf8 :: Gen ByteString
genValidUtf8 = sized $ \size -> 
  if size <= 0
  then pure mempty
  else oneof [
    B.append <$> genAscii <*> resize (size `div` 2) genValidUtf8,
    B.append <$> gen2Byte <*> resize (size `div` 2) genValidUtf8,
    B.append <$> gen3Byte <*> resize (size `div` 2) genValidUtf8,
    B.append <$> gen4Byte <*> resize (size `div` 2) genValidUtf8
    ]
  where
    genAscii :: Gen ByteString
    genAscii = B.pack . (:[]) <$> elements [0x00 .. 0x7F]
    gen2Byte :: Gen ByteString
    gen2Byte = do
      b1 <- elements [0xC2 .. 0xDF]
      b2 <- elements [0x80 .. 0xBF]
      pure . B.pack $ [b1, b2]
    gen3Byte :: Gen ByteString
    gen3Byte = do
      b1 <- elements [0xE0 .. 0xED]
      b2 <- elements $ case b1 of 
        0xE0 -> [0xA0 .. 0xBF]
        0xED -> [0x80 .. 0x9F]
        _ -> [0x80 .. 0xBF]
      b3 <- elements [0x80 .. 0xBF]
      pure . B.pack $ [b1, b2, b3]
    gen4Byte :: Gen ByteString
    gen4Byte = do
      b1 <- elements [0xF0 .. 0xF4]
      b2 <- elements $ case b1 of 
        0xF0 -> [0x90 .. 0xBF]
        0xF4 -> [0x80 .. 0x8F]
        _ -> [0x80 .. 0xBF]
      b3 <- elements [0x80 .. 0xBF]
      b4 <- elements [0x80 .. 0xBF]
      pure . B.pack $ [b1, b2, b3, b4]

genBS :: Gen ByteString
genBS = B.pack <$> arbitrary

shrinkBS :: ByteString -> [ByteString]
shrinkBS bs = B.pack <$> (shrink . B.unpack $ bs)

ord2 :: Char -> (Word8, Word8)
ord2 c = (x, y)
  where
    n :: Int
    n = ord c
    x :: Word8
    x = fromIntegral $ (n `shiftR` 6) + 0xC0
    y :: Word8
    y = fromIntegral $ (n .&. 0x3F) + 0x80

ord3 :: Char -> (Word8, Word8, Word8)
ord3 c = (x, y, z)
  where
    n :: Int
    n = ord c
    x :: Word8
    x = fromIntegral $ (n `shiftR` 12) + 0xE0
    y :: Word8
    y = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    z :: Word8
    z = fromIntegral $ (n .&. 0x3F) + 0x80

ord4 :: Char -> (Word8, Word8, Word8, Word8)
ord4 c = (x, y, z, a)
  where
    n :: Int
    n = ord c
    x :: Word8
    x = fromIntegral $ (n `shiftR` 18) + 0xF0
    y :: Word8
    y = fromIntegral $ ((n `shiftR` 12) .&. 0x3F) + 0x80
    z :: Word8
    z = fromIntegral $ ((n `shiftR` 6) .&. 0x3F) + 0x80
    a :: Word8
    a = fromIntegral $ (n .&. 0x3F) + 0x80
