-- !!! test ioeGetFileName

import System.IO
import System.IO.Error

main = do
  h <- openFile "ioeGetFileName001.hs" ReadMode
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catchIOError`
	\e -> if isEOFError e 
		then print (ioeGetFileName e)
		else putStrLn "failed."
