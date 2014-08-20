import System.IO

import Data.Char          (toUpper)
import System.Directory   (removeFile, doesFileExist)
import System.Environment (getArgs)

main   =  do
  [f1,f2] <- getArgs
  h1 <- openFile f1 ReadMode
  f <- doesFileExist f2
  if f then removeFile f2 else return ()
  h2 <- openFile f2 WriteMode
  copyFile h1 h2
  hClose h1
  hClose h2

copyFile h1 h2 = do
  eof <- hIsEOF h1
  if eof 
	then return ()
    	else do
  c <- hGetChar h1
  c <- hPutChar h2 (toUpper c)
  copyFile h1 h2
