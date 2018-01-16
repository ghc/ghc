-- !!! test threadDelay, Random, and QSemN.

-- Variation of conc023, testing STM timeouts instead of IO

import System.Random
import Control.Concurrent
import Control.Exception
import Control.Concurrent.STM
import Control.Monad

n = 5000  -- no. of threads
m = 3000  -- maximum delay

main = do
   s <- newTVarIO 0
   is <- replicateM n (getStdRandom (randomR (1,m)))
   mapM (fork_sleep s) is
   atomically $ do i <- readTVar s; check (i == n)
   where
        fork_sleep :: TVar Int -> Int -> IO ThreadId
        fork_sleep s i = forkIO $ do
             t <- registerDelay (i*1000)
             atomically $ do readTVar t >>= check; modifyTVar s (+1)
