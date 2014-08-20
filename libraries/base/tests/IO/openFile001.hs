-- !!! test that a file opened in ReadMode can't be written to

import System.IO
import System.IO.Error

main = do
  hIn <- openFile "openFile001.hs" ReadMode
  hPutStr hIn "test" `catchIOError` \ err ->
      if isIllegalOperation err 
	then putStrLn "ok."
	else error "Oh dear\n"
