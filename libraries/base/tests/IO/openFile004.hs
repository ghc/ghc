-- !!! Open a non-existent file for writing

import Control.Monad
import Data.Char
import System.Directory
import System.IO

file = "openFile004.out"

main = do
  b <- doesFileExist file
  when b (removeFile file)

  h <- openFile file WriteMode
  hPutStr h "hello world\n"
  hClose h

  h <- openFile file ReadMode
  let loop = do
	b <- hIsEOF h 
	if b then return () 
	     else do c <- hGetChar h; putChar c; loop
  loop
