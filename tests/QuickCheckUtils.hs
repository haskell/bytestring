{-# OPTIONS_GHC -fglasgow-exts -fallow-overlapping-instances #-}
module QuickCheckUtils where

import Test.QuickCheck.Batch
import Test.QuickCheck
import Text.Show.Functions

import Control.Monad.Reader ({-instances Functor (-> c)-})
import Control.Monad        ( liftM2 )
import Data.Char
import Data.List
import Data.Word
import Data.Int
import System.Random
import System.IO

import Data.ByteString.Fusion
import qualified Data.ByteString      as P
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy (ByteString(..))

-- Enable this to get verbose test output. Including the actual tests.
debug = False

mytest :: Testable a => a -> Int -> IO ()
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO ()
mycheck config a =
  do rnd <- newStdGen
     mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
mytests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = do done "OK," ntest stamps
  | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
  | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps =
  do putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
 where
  table = display
        . map entry
        . reverse
        . sort
        . map pairLength
        . group
        . sort
        . filter (not . null)
        $ stamps

  display []  = ".\n"
  display [x] = " (" ++ x ++ ").\n"
  display xs  = ".\n" ++ unlines (map (++ ".") xs)

  pairLength xss@(xs:_) = (length xss, xs)
  entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

  percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------

instance Arbitrary Char where
    arbitrary     = choose ('a', 'i')
    coarbitrary c = variant (ord c `rem` 4)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PairS a b) where
  arbitrary             = liftM2 (:*:) arbitrary arbitrary
  coarbitrary (a :*: b) = coarbitrary a . coarbitrary b

instance Arbitrary Word8 where
    arbitrary = choose (97, 105)
    coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 4))

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary           = do a <- arbitrary ; elements [Nothing, Just a]
  coarbitrary Nothing = variant 0
  coarbitrary _       = variant 1 -- ok?

instance Arbitrary a => Arbitrary (MaybeS a) where
  arbitrary            = do a <- arbitrary ; elements [NothingS, JustS a]
  coarbitrary NothingS = variant 0
  coarbitrary _        = variant 1 -- ok?

{-
instance Arbitrary Char where
  arbitrary = choose ('\0', '\255') -- since we have to test words, unlines too
  coarbitrary c = variant (ord c `rem` 16)

instance Arbitrary Word8 where
  arbitrary = choose (minBound, maxBound)
  coarbitrary c = variant (fromIntegral ((fromIntegral c) `rem` 16))
-}

instance Random Word8 where
  randomR (a,b) g = case randomR (fromIntegral a :: Integer
                                 ,fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x :: Word8, g)
  random g        = randomR (minBound,maxBound) g

instance Arbitrary L.ByteString where
    arbitrary     = arbitrary >>= return . L.LPS . filter (not. P.null) -- maintain the invariant.
    coarbitrary s = coarbitrary (L.unpack s)

instance Arbitrary P.ByteString where
  arbitrary = P.pack `fmap` arbitrary
  coarbitrary s = coarbitrary (P.unpack s)

------------------------------------------------------------------------
--
-- We're doing two forms of testing here. Firstly, model based testing.
-- For our Lazy and strict bytestring types, we have model types:
--
--  i.e.    Lazy    ==   Byte
--              \\      //
--                 List 
--
-- That is, the Lazy type can be modeled by functions in both the Byte
-- and List type. For each of the 3 models, we have a set of tests that
-- check those types match.
--
-- The Model class connects a type and its model type, via a conversion
-- function. 
--
class Model a b where
  model :: a -> b  -- get the abstract vale from a concrete value

--
-- Connecting our Lazy and Strict types to their models. We also check
-- the data invariant on Lazy types.
--
-- These instances represent the arrows in the above diagram
--
instance Model P [W] where model = P.unpack
instance Model B [W] where model = L.unpack . checkInvariant
instance Model B P   where model = abstr . checkInvariant

-- Types are trivially modeled by themselves
instance Model Bool  Bool         where model = id
instance Model Int   Int          where model = id
instance Model Int64 Int64        where model = id
instance Model Word8 Word8        where model = id
instance Model Ordering Ordering  where model = id

-- More structured types are modeled recursively, using the NatTrans class from Gofer.
class (Functor f, Functor g) => NatTrans f g where
    eta :: f a -> g a

-- The transformation of the same type is identity
instance NatTrans [] []             where eta = id
instance NatTrans Maybe Maybe       where eta = id
instance NatTrans ((->) X) ((->) X) where eta = id
instance NatTrans ((->) W) ((->) W) where eta = id

-- Missing from < ghc 6.5 compilers
instance Functor ((,)   a) where fmap f (x,y) = (x, f y)

-- We have a transformation of pairs, if the pairs are in Model
instance Model f g => NatTrans ((,) f) ((,) g) where eta (f,a) = (model f, a)

-- And finally, we can take any (m a) to (n b), if we can Model m n, and a b
instance (NatTrans m n, Model a b) => Model (m a) (n b) where model x = fmap model (eta x)

------------------------------------------------------------------------

-- In a form more useful for QC testing (and it's lazy)
checkInvariant :: L.ByteString -> L.ByteString
checkInvariant (LPS lps) = LPS (check lps)
  where check []     = []
        check (x:xs) | P.null x  = error ("invariant violation: " ++ show lps)
                     | otherwise = x : check xs

abstr :: L.ByteString -> P.ByteString
abstr (LPS []) = P.empty
abstr (LPS xs) = P.concat xs

-- Some short hand.
type X = Int
type W = Word8
type P = P.ByteString
type B = L.ByteString

------------------------------------------------------------------------
--
-- These comparison functions handle wrapping and equality.
--

compare1 :: (Model a1 b1, Model a b, Eq b)
         => (a1 -> a) -> (b1 -> b) -> a1 -> Bool
compare1 f f' a = model (f a) == f' (model a)

compare2 :: (Model a2 b2, Model a1 b1, Model a b, Eq b)
         => (a1 -> a2 -> a) -> (b1 -> b2 -> b) -> a1 -> a2 -> Bool
compare2 f f' a b = model (f a b) == f' (model a) (model b)

compare3 :: (Model a3 b3, Model a2 b2, Model a1 b1, Model a b, Eq b)
         => (a1 -> a2 -> a3 -> a) -> (b1 -> b2 -> b3 -> b) -> a1 -> a2 -> a3 -> Bool
compare3 f f' a b c = model (f a b c) == f' (model a) (model b) (model c)

--
-- And for functions that take non-null input
--
notLNull1 :: (Testable a) =>
             (ByteString -> a) -> ByteString -> Property
notLNull1 f = \x -> (not . L.null $ x) ==> f x

notPNull1 :: (Testable a) =>
             (P -> a) -> P -> Property
notPNull1 f = \x -> (not . P.null $ x) ==> f x

notLNull2 :: (Testable a) =>
             (t -> ByteString -> a) -> t -> ByteString -> Property
notLNull2 f = \x y -> (not . L.null $ y) ==> f x y

notLNull3 :: (Testable a)
          => (t -> t1 -> ByteString -> a) -> t -> t1 -> ByteString -> Property
notLNull3 f = \x y z -> (not . L.null $ z) ==> f x y z

notPNull2 :: (Testable a)
          => (t -> P -> a) -> t -> P -> Property
notPNull2 f = \x y -> (not . P.null $ y) ==> f x y

notPNull3 :: (Testable a)
          => (t -> t1 -> P -> a) -> t -> t1 -> P -> Property
notPNull3 f = \x y z -> (not . P.null $ z) ==> f x y z
