-- test tryPutMVar

import Control.Concurrent

main = do
  m <- newMVar ()
  r <- tryPutMVar m ()
  print r
  takeMVar m
  r <- tryPutMVar m ()
  print r
