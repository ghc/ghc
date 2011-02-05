-- !!! Testing hSeek
module Main(main) where

import System.Directory
import System.IO

main :: IO ()
main = do
  h <- openFile "tst-seek" WriteMode
  hSetEncoding h utf8 -- hSeek/hTell work with Unicode streams
  hPutStr h "test string1"
   -- seek to EOF should be cool..
  hSeek h SeekFromEnd 0
  hPutStr h "test string2"
   -- seek past EOF should now also be cool..
  hSeek h SeekFromEnd 3
  hPutStr h "test string3"
  hSeek h AbsoluteSeek 13
  hPutStr h "test string4"
  x <- hTell h
  print x
  hSeek h AbsoluteSeek 30
  x1 <- hTell h
  hPutStr h "人間虫" -- we should be able to output Unicode too
  x2 <- hTell h
  print (x2 - x1)
  hPutStr h "filler"
  hClose h
  h <- openFile "tst-seek" ReadMode
  hSetEncoding h utf8
  str <- hGetContents h
  putStrLn str
  removeFile "tst-seek"
