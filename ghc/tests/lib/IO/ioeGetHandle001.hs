-- !!! test ioeGetHandle

import IO
import Maybe
#if defined(__MINGW32__)
import PrelHandle(hSetBinaryMode)
#endif

main = do
  h <- openFile "ioeGetHandle001.hs" ReadMode
# if defined(__MINGW32__)
  hSetBinaryMode h True
# endif
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e && fromJust (ioeGetHandle e) == h
		then putStrLn "ok."
		else putStrLn "failed."
