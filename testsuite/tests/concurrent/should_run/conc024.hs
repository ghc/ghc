{-# LANGUAGE CPP #-}
module Main where

import Control.Exception
import Control.Concurrent
import System.Mem

-- illustrates the BlockOnDeadMVar exception

main = do
  id <- myThreadId
  forkIO (catch (do m <- newEmptyMVar; takeMVar m)
                (\e -> throwTo id (e::SomeExceptionWithBacktrace)))
  catch (do yield; performGC; threadDelay 1000000)
        (\e -> print (e::SomeExceptionWithBacktrace))
