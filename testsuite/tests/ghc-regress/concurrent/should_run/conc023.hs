{-# LANGUAGE ScopedTypeVariables #-}
-- !!! test threadDelay, Random, and QSemN.

-- start a large number (n) of threads each of which will wait for a
-- random delay between 0 and m seconds.  We use a semaphore to wait
-- for all the threads to finish.

import System.Random
import Control.Concurrent
import Control.Exception

n = 5000  -- no. of threads
m = 3000  -- maximum delay

main = do
   s <- newQSemN n
   (is :: [Int]) <- sequence (take n (repeat (getStdRandom (randomR (1,m)))))
   mapM (fork_sleep s) is
   waitQSemN s n
   where
	fork_sleep s i = forkIO (do waitQSemN s 1
			  	    threadDelay (i*1000)
				    signalQSemN s 1)
