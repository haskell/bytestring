{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns #-}
--
-- Must have rules off, otherwise the rewrite rules will replace the rhs
-- with the lhs, and we only end up testing lhs == lhs
--

--
-- -fhpc interferes with rewrite rules firing.
--

import Test.QuickCheck
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Exception
import System.Directory

import Data.List
import Data.Char
import Data.Word
import Data.Maybe
import Data.Int (Int64)
import Data.Monoid

import Data.String

import System.IO
import System.IO.Unsafe

import Data.ByteString.Lazy (ByteString(..), pack , unpack)
import qualified Data.ByteString.Lazy as L

import qualified Data.ByteString            as P
import qualified Data.ByteString.Internal   as P
import qualified Data.ByteString.Unsafe     as P
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Short      as Short

import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.Char8 as D

import Prelude hiding (abs)

import QuickCheckUtils
#if defined(HAVE_TEST_FRAMEWORK)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
#else
import TestFramework
#endif

import Properties.Misc (misc_tests)
import Properties.BSLazyModelList (bl_tests)
import Properties.BSLazyChar8ModelBSChar8 (cc_tests)
import Properties.BSLazyModelBS (bp_tests)
import Properties.BSModelList (pl_tests)
import Properties.BSExtra (bb_tests)
import Properties.MiscLL (ll_tests)
import Properties.IO (io_tests)
import Properties.BSShort (short_tests)
import Properties.Rules (rules)

------------------------------------------------------------------------
-- The entry point

main :: IO ()
main = defaultMainWithArgs tests ["-o 3"] -- timeout if a test runs for >3 secs

--
-- And now a list of all the properties to test.
--

tests =
     [ misc_tests
     , bl_tests
     , cc_tests
     , bp_tests
     , pl_tests
     , bb_tests
     , ll_tests
     , io_tests
     , short_tests
     , rules
     ]