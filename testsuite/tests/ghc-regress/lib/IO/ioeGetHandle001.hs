-- !!! test ioeGetHandle

import IO
import Maybe
#ifdef i386_unknown_mingw32
import GHC.Handle(hSetBinaryMode)
#endif

main = do
  h <- openFile "ioeGetHandle001.hs" ReadMode
#ifdef i386_unknown_mingw32
  hSetBinaryMode h True
#endif
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e && fromJust (ioeGetHandle e) == h
		then putStrLn "ok."
		else putStrLn "failed."
