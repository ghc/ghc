-- test for blocking putMVar

import Concurrent

main = do
  m <- newMVar ()
  forkIO (threadDelay 100000 >> takeMVar m)
  putMVar m ()  
