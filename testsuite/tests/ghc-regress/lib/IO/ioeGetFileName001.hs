-- !!! test ioeGetFileName

import IO
#ifdef mingw32_HOST_OS
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  h <- openFile "ioeGetFileName001.hs" ReadMode
#ifdef mingw32_HOST_OS
  hSetBinaryMode h True
#endif
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e 
		then print (ioeGetFileName e)
		else putStrLn "failed."
