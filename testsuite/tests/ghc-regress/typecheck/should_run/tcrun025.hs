{-# LANGUAGE ImplicitParams #-}

-- Like tcrun024, but cross module

module Main where
	import TcRun025_B

	just = [Just "fred",Just "bill"]

	main = do { putStrLn (let ?p = "ok1" in fc1);  
		    putStrLn (let ?p = "ok2" in fc2);  
		    putStrLn (show (fd1 just)) ;
		    putStrLn (show (fd2 just)) }


