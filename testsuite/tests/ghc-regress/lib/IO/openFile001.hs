-- !!! test that a file opened in ReadMode can't be written to

import IO

main = do
  hIn <- openFile "openFile001.hs" ReadMode
  hPutStr hIn "test" `catch` \ err ->
      if isIllegalOperation err 
	then putStrLn "ok."
	else error "Oh dear\n"
