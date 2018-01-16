-- benchmarks communication on TChan
-- 
-- This is a synthetic benchmark that is sensitive to scheduler
-- behaviour.  In GHC 6.12 and earlier we triggered a context switch
-- shortly after waking up a thread, whereas in 6.14 and later we
-- stopped doing that.  This benchmark performs worse with 6.14
-- becauuse not doing the context switch allows a lot of data to build
-- up in the Chan, making GC expensive.

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import System.Environment
import Control.Monad

main = do
  [n] <- fmap (fmap read) getArgs
  c <- newTChanIO
  m <- newEmptyMVar
  a <- forkIO $ forM_ [1..n] $ \i -> atomically $ writeTChan c i
  b <- forkIO $ do forM_ [1..n] $ \i -> atomically $ readTChan c; putMVar m ()
  takeMVar m
