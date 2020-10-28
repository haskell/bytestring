{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
import qualified Data.ByteString.Char8 as S8


import Test.HUnit (assertEqual, assertBool)
import qualified Test.Framework as F
import qualified Test.Framework.Providers.HUnit as F

import Control.Monad
import System.IO


testfile_eof :: FilePath
testfile_eof = "line-endings_eof.txt"

testfile_lf_eof :: FilePath
testfile_lf_eof = "line-endings_lf_eof.txt"

testfile_cr_eof :: FilePath
testfile_cr_eof = "line-endings_cr_eof.txt"

testfile_crlf_eof :: FilePath
testfile_crlf_eof = "line-endings_crlf_eof.txt"


testString :: String
testString = concat [
      "This file\r\n"
    , "tests how hGetLine\r\r\n"
    , " handles\n"
    , " newlines.\r\n"
    , "It is intentionally\n\r"
    , "inconsistent\r\n\r\n \r\n"
    , "in how it\r"
    , "handles newlines.\r\n\n"
    , "If it was in the git repo\n\n"
    , "it would need to be marked in\r\r"
    , "in .gitattributes as a binary file\n"
    , "to stop\n\r\r\n"
    , "git from changing its endings"
    ]

writeTestFiles :: IO ()
writeTestFiles = do
  writeFile testfile_eof $ testString
  writeFile testfile_lf_eof $ testString <> "\n"
  writeFile testfile_cr_eof $ testString <> "\r"
  writeFile testfile_crlf_eof $ testString <> "\r\n"
    

readByLinesBS :: Handle -> IO [S8.ByteString]
readByLinesBS h_ = go []
  where 
    go lines = do
      isEnd <- hIsEOF h_
      if isEnd
        then return $ reverse lines
        else do
          !nextLine <- S8.hGetLine h_
          go (nextLine : lines)

readByLinesS :: Handle -> IO [String]
readByLinesS h_ = go []
  where 
    go lines = do
      isEnd <- hIsEOF h_
      if isEnd
        then return $ reverse lines
        else do
          !nextLine <- hGetLine h_
          go (nextLine : lines)

hgetline_like_s8_hgetline :: IO ()
hgetline_like_s8_hgetline = 
  mapM_ (uncurry hgetline_like_s8_hgetline') $ do
      file <- [testfile_eof, testfile_lf_eof, testfile_cr_eof, testfile_crlf_eof]
      linemode <- [NewlineMode LF LF, NewlineMode CRLF CRLF, NewlineMode LF CRLF, NewlineMode CRLF LF]
      return (file, linemode)


hgetline_like_s8_hgetline' :: FilePath -> NewlineMode -> IO ()
hgetline_like_s8_hgetline' file newlineMode = do
    bsLines <- withFile file ReadMode (\h -> do
        hSetNewlineMode h newlineMode
        readByLinesBS h
      )
    sLines  <- withFile file ReadMode (\h -> do
        hSetNewlineMode h newlineMode
        readByLinesS h
      )
    assertEqual ("unpacking S8.hGetLines should equal hGetLines for newlineMode " <> show newlineMode <> " for file " <> file) 
        (map S8.unpack bsLines)
        sLines
    assertBool "The test file for hGetLine sshould not be empty" $ bsLines /= []


tests :: [F.Test]
tests = [
    F.testCase "hgetline_like_s8_hgetline" hgetline_like_s8_hgetline
  ]

main :: IO ()
main = do
  writeTestFiles
  F.defaultMain tests
