-- !!! test ioeGetFileName

import IO
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  h <- openFile "ioeGetFileName001.hs" ReadMode
#ifdef i386_unknown_mingw32
  hSetBinaryMode h True
#endif
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e 
		then print (ioeGetFileName e)
		else putStrLn "failed."
