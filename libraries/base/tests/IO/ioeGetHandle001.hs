-- !!! test ioeGetHandle

import System.IO
import System.IO.Error
import Data.Maybe

main = do
  h <- openFile "ioeGetHandle001.hs" ReadMode
  hSeek h SeekFromEnd 0
  (hGetChar h >> return ()) `catchIOError`
	\e -> if isEOFError e && fromJust (ioeGetHandle e) == h
		then putStrLn "ok."
		else putStrLn "failed."
