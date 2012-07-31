import Control.Concurrent

main = do
  t <- myThreadId
  forkIO (threadDelay 100000 >> killThread t)
  threadDelay 9223372036840001
    -- caused an overflow in 7.4.2, non-threaded RTS
