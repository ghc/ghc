module Main where

import Control.Concurrent

-- This test hopefully exercises the black hole code.  The main thread
-- forks off another thread and starts on a large computation.  
-- The child thread attempts to get the result of the same large
-- computation (and should get blocked doing so, because the parent
-- won't have evaluated it yet).  When the result is available, the
-- child passes it back to the parent who prints it out.

test = sum [1..10000]

main = do
  x <- newEmptyMVar
  forkIO (if test > 0 
		then putMVar x test
		else error "proc"
         )
  if test > 0	-- evaluate test
	then do result <- takeMVar x
		print result
	else error "main"
