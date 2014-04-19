import System.IO

main = f stdin
  where f h = do p <- hIsEOF h
		 if p then putStrLn "done" 
		      else do l <- hGetLine h
			      putStrLn l
			      f h

