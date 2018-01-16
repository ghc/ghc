import Control.Concurrent.STM

main = do
  x <- atomically $ do
         r <- newTVar []
         writeTVar r [2]
         writeTVar r [] `orElse` return ()
         readTVar r
  print x
