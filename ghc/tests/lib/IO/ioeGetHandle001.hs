-- !!! test ioeGetHandle

import IO
import Maybe

main = do
  h <- openFile "ioeGetHandle001.hs" ReadMode
  hSetBinaryMode h True
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catch`
	\e -> if isEOFError e && fromJust (ioeGetHandle e) == h
		then putStrLn "ok."
		else putStrLn "failed."
