-- !!! test threadDelay, Random, and QSemN.

-- Variation of conc023, testing STM timeouts instead of IO

import Random
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM

n = 5000  -- no. of threads
m = 3000  -- maximum delay

main = do
   s <- newQSemN n
   (is :: [Int]) <- sequence (take n (repeat (getStdRandom (randomR (1,m)))))
   mapM (fork_sleep s) is
   waitQSemN s n
   where
	fork_sleep s i = forkIO (do waitQSemN s 1
			  	    t <- registerDelay (i*1000)
				    atomically $ (readTVar t >>= check)
				    signalQSemN s 1)
