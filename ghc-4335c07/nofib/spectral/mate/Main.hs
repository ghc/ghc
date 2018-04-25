module Main(main) where
import Board
import Solution
import Problem
import System.Environment( getArgs )

main = do { files <- getArgs ;
	    mapM mateInN files }

mateInN file
  = do { putStrLn "" ;
	 putStrLn ("File: " ++ file) ;
	 input <- readFile file ;
	 let {	(bd, (c,n)) = readProblem input ;
		result      = showBoard bd ++ 
				"\n" ++
				show c ++ " to move and mate in " ++ show n ++ "\n" ++
				"\n" ++
				solve bd c n
		
	 } ;
	 putStr result  }
        
