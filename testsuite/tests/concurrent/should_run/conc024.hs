{-# LANGUAGE CPP #-}
module Main where

import Control.Exception
import Control.Concurrent
#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif
import System.Mem

-- illustrates the BlockOnDeadMVar exception

main = do
  id <- myThreadId
  forkIO (catch (do m <- newEmptyMVar; takeMVar m)
		(\e -> throwTo id (e::SomeException)))
  catch (do yield; performGC; threadDelay 1000000)
	(\e -> print (e::SomeException))
