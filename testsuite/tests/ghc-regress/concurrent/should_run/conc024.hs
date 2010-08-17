module Main where

import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)

-- illustrates the BlockOnDeadMVar exception

main = do
  id <- myThreadId
  forkIO (catch (do m <- newEmptyMVar; takeMVar m)
		(\e -> throwTo id (e::SomeException)))
  catch (threadDelay 1000000)
	(\e -> print (e::SomeException))
