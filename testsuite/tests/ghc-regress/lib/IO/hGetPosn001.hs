-- !!! Test file positioning

module Main(main) where

import IO
import Monad
import Directory (removeFile, doesFileExist)
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  hIn <- openFile "hGetPosn001.in" ReadMode
#ifdef i386_unknown_mingw32
  hSetBinaryMode hIn True
#endif
  f <- doesFileExist "hGetPosn001.out"
  when f (removeFile "hGetPosn001.out")
  hOut <- openFile "hGetPosn001.out" ReadWriteMode
  bof <- hGetPosn hIn
  putStrLn (show bof)  -- you can show HandlePosns
  copy hIn hOut
  hSetPosn bof
  copy hIn hOut
#ifdef i386_unknown_mingw32
  hSetBinaryMode hOut True
#endif
  hSeek hOut AbsoluteSeek 0
#ifdef i386_unknown_mingw32
  hSetBinaryMode hOut False
#endif
  stuff <- hGetContents hOut
  putStr stuff

copy :: Handle -> Handle -> IO ()
copy hIn hOut =
    try (hGetChar hIn) >>=
    either (\ err -> if isEOFError err then return () else error "copy")
	   ( \ x -> hPutChar hOut x >> copy hIn hOut)
