import Control.Concurrent.STM
main = do
  x <- atomically $ do
         t <- newTVar 1
         writeTVar t 2
         ((readTVar t >> retry) `orElse` return ()) `orElse` return ()
         readTVar t
  print x
