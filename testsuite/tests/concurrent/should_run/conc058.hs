import Control.Concurrent
import Control.Exception

-- variation on conc020 that tests for threadDelay being interruptible.
-- On Windows, with the threaded RTS, in 6.6 and earlier, threadDelay is 
-- not interruptible.
main = do
  m <- newEmptyMVar
  t <- forkIO (block $ threadDelay 1000000)
  threadDelay 100000
  throwTo t (ErrorCall "I'm Interruptible")
  threadDelay 100000
  putMVar m ()  -- to avoid t being garbage collected
