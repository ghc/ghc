module Main where

import Exception
import Concurrent
import Prelude hiding (catch)

-- illustrates the BlockOnDeadMVar exception

main = do
  id <- myThreadId
  forkIO (catch (do m <- newEmptyMVar; takeMVar m)
		(\e -> throwTo id e))
  catch (print (sum [1..1000000]))
	(\e -> print e)
