module Main where

import Exception
import Concurrent
import Prelude hiding (catch)

-- illustrates the BlockOnDeadMVar exception

main = do
  id <- myThreadId
  forkIO (catchAllIO (do m <- newEmptyMVar; takeMVar m)
		(\e -> raiseInThread id e))
  catchAllIO (print (sum [1..1000000]))
	(\e -> print e)
