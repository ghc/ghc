-- !!! test ioeGetErrorString

import IO
import Maybe
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  h <- openFile "ioeGetErrorString001.hs" ReadMode
#ifdef i386_unknown_mingw32
  hSetBinaryMode h True
#endif
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e
		then print (ioeGetErrorString e)
		else putStrLn "failed."
