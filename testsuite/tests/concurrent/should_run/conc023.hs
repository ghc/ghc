{-# LANGUAGE ScopedTypeVariables #-}
-- !!! test threadDelay, Random, and QSemN.

-- start a large number (n) of threads each of which will wait for a
-- random delay between 0 and m seconds.  We use a semaphore to wait
-- for all the threads to finish.

import System.Random
import Control.Concurrent
import Control.Exception
import Control.Monad

n = 5000  -- no. of threads
m = 3000  -- maximum delay

main = do
   v <- newEmptyMVar
   is <- replicateM n $ getStdRandom (randomR (1,m))
   mapM_ (fork_sleep v) is
   replicateM_ n (takeMVar v)
   where
     fork_sleep v i = forkIO $ do threadDelay (i*1000); putMVar v ()
