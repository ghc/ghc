import Control.Concurrent
import Control.Exception

-- !!! test that deadlock is raised as an exception properly
main = do
  r <- Exception.try $ do
          m <- newEmptyMVar
          takeMVar m
	  return ()
  print r
