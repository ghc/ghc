module Main(main) where

import IO -- 1.3
--import IOBase -- tryIO 1.3
--import GHCio

import Directory (removeFile)

main = do
  hIn   <- openFile "io008.in" ReadMode
  hOut  <- openFile "io008.out" ReadWriteMode
  removeFile "io008.out"
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

