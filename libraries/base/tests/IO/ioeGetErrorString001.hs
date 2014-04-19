-- !!! test ioeGetErrorString

import System.IO
import System.IO.Error
import Data.Maybe

main = do
  h <- openFile "ioeGetErrorString001.hs" ReadMode
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catchIOError`
	\e -> if isEOFError e
		then print (ioeGetErrorString e)
		else putStrLn "failed."
