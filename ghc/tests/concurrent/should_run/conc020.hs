import Concurrent
import Exception

main = do
  m <- newEmptyMVar
  t <- forkIO (blockAsyncExceptions $ takeMVar m)
  threadDelay 100000
  raiseInThread t (ErrorCall "I'm Interruptible")
  threadDelay 100000
  putMVar m ()  -- to avoid t being garbage collected
