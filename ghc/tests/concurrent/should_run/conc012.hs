module Main where

import Concurrent
import Exception

data Result = Died Exception | Finished

-- Test stack overflow catching.  Should print "Died: stack overflow".

main = do
  let x = sum [1..100000]  -- relies on sum being implemented badly :-)
  result <- newEmptyMVar 
  forkIO (catchAllIO (x `seq` putMVar result Finished) 
		     (\e -> putMVar result (Died e)))
  res <- takeMVar result
  case res of
	Died e -> putStr ("Died: " ++ show e ++ "\n")
	Finished -> putStr "Ok.\n"
