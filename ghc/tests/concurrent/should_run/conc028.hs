-- test tryPutMVar

import Concurrent

main = do
  m <- newMVar ()
  r <- tryPutMVar m ()
  print r
