-- test for blocking putMVar

import Control.Concurrent

main = do
  m <- newMVar ()
  forkIO (threadDelay 100000 >> takeMVar m)
  putMVar m ()  
