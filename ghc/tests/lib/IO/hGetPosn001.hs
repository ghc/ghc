-- !!! Test file positioning

module Main(main) where

import IO
import Monad
import Directory (removeFile, doesFileExist)
#if defined(__MINGW32__)
import PrelHandle(hSetBinaryMode)
#endif

main = do
  hIn <- openFile "hGetPosn001.in" ReadMode
# if defined(__MINGW32__)
  hSetBinaryMode hIn True
# endif
  f <- doesFileExist "hGetPosn001.out"
  when f (removeFile "hGetPosn001.out")
  hOut <- openFile "hGetPosn001.out" ReadWriteMode
# if defined(__MINGW32__)
  hSetBinaryMode hOut True
# endif
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
