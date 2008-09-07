import IO
import System.IO
import Foreign
import CForeign

main = do
  h <- openBinaryFile "hGetBuf002.hs" ReadMode

  let sz = 42
      loop = do
  	 b <- allocaBytes sz $ \ptr -> do
                r <- hGetBuf h ptr sz
         	if (r == 0)
		   then return True
		   else do s <- peekCStringLen (ptr,r)
			   putStr s
			   return False
	 if b then return () else loop -- tail call

  loop

