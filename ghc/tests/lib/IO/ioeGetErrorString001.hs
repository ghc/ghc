-- !!! test ioeGetErrorString

import IO
import Maybe
#if defined(__MINGW32__)
import PrelHandle(hSetBinaryMode)
#endif

main = do
  h <- openFile "ioeGetErrorString001.hs" ReadMode
# if defined(__MINGW32__)
  hSetBinaryMode h True
# endif
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e
		then print (ioeGetErrorString e)
		else putStrLn "failed."
