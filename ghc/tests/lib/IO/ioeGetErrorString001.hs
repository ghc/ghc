-- !!! test ioeGetErrorString

import IO
import Maybe

main = do
  h <- openFile "ioeGetErrorString001.hs" ReadMode
  hSetBinaryMode h True
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e
		then print (ioeGetErrorString e)
		else putStrLn "failed."
