-- !!! Test file positioning

module Main(main) where

import Control.Monad
import System.Directory (removeFile, doesFileExist)
import System.IO
import System.IO.Error

main = do
  hIn <- openFile "hGetPosn001.in" ReadMode
  f <- doesFileExist "hGetPosn001.out"
  when f (removeFile "hGetPosn001.out")
  hOut <- openFile "hGetPosn001.out" ReadWriteMode
  bof <- hGetPosn hIn
  putStrLn (show bof)  -- you can show HandlePosns
  copy hIn hOut
  hSetPosn bof
  copy hIn hOut
  hSeek hOut AbsoluteSeek 0
  stuff <- hGetContents hOut
  putStr stuff

copy :: Handle -> Handle -> IO ()
copy hIn hOut =
    tryIOError (hGetChar hIn) >>=
    either (\ err -> if isEOFError err then return () else error "copy")
	   ( \ x -> hPutChar hOut x >> copy hIn hOut)
