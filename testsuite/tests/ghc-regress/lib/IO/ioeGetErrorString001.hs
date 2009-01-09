-- !!! test ioeGetErrorString

import IO
import Maybe
#ifdef mingw32_HOST_OS
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  h <- openFile "ioeGetErrorString001.hs" ReadMode
#ifdef mingw32_HOST_OS
  hSetBinaryMode h True
#endif
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e
		then print (ioeGetErrorString e)
		else putStrLn "failed."
