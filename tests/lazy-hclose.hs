import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8

import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8

import Control.Monad
import System.Directory
import System.Mem
import System.IO

import Data.ByteString.Internal
import Foreign.ForeignPtr

-- works fine for strict bytestrings.
main_strict = forever $ do
     r <- S.readFile "a"
     S.writeFile "b" (S8.pack "abc")
     renameFile "b" "a"

-- lazy bytestrings
main_too_lazy = forever $ do
     r <- L.readFile "a"
     L.length r `seq` return ()      -- force the input, and done with 'r' now.
     L.writeFile "b" (L8.pack "abc") -- but we still need the finalizers to run
     renameFile "b" "a"
{-
    $ time ./A            
    A: b: openBinaryFile: resource exhausted (Too many open files)
-}

-- manage the resources explicitly.
main_broken = forever $ do
     r <- L.readFile "a"
     L.length r `seq` return ()
     mapM_ release (L.toChunks r) -- should be enough
     performGC
     L.writeFile "b" (L8.pack "abc")
     renameFile "b" "a"
  where
    release c = finalizeForeignPtr fp where (fp,_,_) = toForeignPtr c

{-
        however, looking in Data.ByteString.Lazy, we're 
        not putting handles into closed states in lazyRead.
-}

{-
-- we can close explicitly...
main = forever $ do
     h <- openFile "a" ReadMode
     r <- L.hGetContents h
     L.length r `seq` return ()
     hClose h

     L.writeFile "b" (L8.pack "abc")
     renameFile "b" "a"
  where
    release c = finalizeForeignPtr fp where (fp,_,_) = toForeignPtr c
-}

-- works now
main = forever $ do
     r <- L.readFile "a" ; L.last r `seq` return ()
     L.writeFile "b" (L8.pack "abc")
     renameFile "b" "a"
