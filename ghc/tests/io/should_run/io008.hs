-- !!! Test file positioning

module Main(main) where

import IO
import Monad

import Directory (removeFile, doesFileExist)

main = do
  hIn <- openFile "io008.in" ReadMode
  f <- doesFileExist "io008.out"
  when f (removeFile "io008.out")
  hOut <- openFile "io008.out" ReadWriteMode
  bof <- hGetPosn hIn
  copy hIn hOut
  hSetPosn bof
  copy hIn hOut
  hSeek hOut AbsoluteSeek 0
  stuff <- hGetContents hOut
  putStr stuff

copy :: Handle -> Handle -> IO ()
copy hIn hOut =
    try (hGetChar hIn) >>=
    either (\ err -> if isEOFError err then return () else error "copy")
	   ( \ x -> hPutChar hOut x >> copy hIn hOut)
