-- !!! Testing hSeek
module Main(main) where

import IO
import System.IO
import Directory

main :: IO ()
main = do
  h <- openBinaryFile "tst-seek" WriteMode
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
  hPutStr h "filler"
  hClose h
  ls <- readFile "tst-seek"
  putStrLn ls
  removeFile "tst-seek"
