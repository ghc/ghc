-- !!! Testing hSeek
module Main(main) where

import IO
import IOExts
import Directory

main :: IO ()
main = do
  h <- openFile "tst-seek" WriteMode
  hPutStrLn h "test string1"
   -- seek to EOF should be cool..
  hSeek h SeekFromEnd 0
  hPutStr h "test string2"
   -- seek past EOF should now be cool..
  hSeek h SeekFromEnd 3
  hPutStr h "test string3"
  hSeek h AbsoluteSeek 13
  hPutStr h "test string4"
  x <- hTell h
  print x
  hClose h
  ls <- readFile "tst-seek"
  putStrLn ls
  removeFile "tst-seek"
