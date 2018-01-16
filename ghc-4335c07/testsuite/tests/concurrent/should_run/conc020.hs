import Control.Concurrent
import Control.Exception

main = do
  m <- newEmptyMVar
  t <- forkIO (mask_ $ takeMVar m)
  threadDelay 100000
  throwTo t (ErrorCall "I'm Interruptible")
  threadDelay 100000
  putMVar m ()  -- to avoid t being garbage collected
