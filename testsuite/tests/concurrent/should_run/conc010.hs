
module Main where

import Control.Concurrent
import Control.Exception

-- Raise an exception in another thread.  We need a lot of synchronisation here:

--   - an MVar for the second thread to block on which it waits for the
--     signal (block)

--   - an MVar to signal the main thread that the second thread is ready to
--     accept the signal (ready)

--   - an MVar to signal the main thread that the second thread has received
--     the signal (ready2).  If we don't have this MVar, then the main
--     thread could exit before the second thread has time to print
--     the result.

main = do
  block  <- newEmptyMVar
  ready  <- newEmptyMVar
  ready2 <- newEmptyMVar
  id <- forkIO (Control.Exception.catch (putMVar ready () >> takeMVar block)
                (\e -> putStr (show (e::SomeExceptionWithLocation)) >> putMVar ready2 ()))
  takeMVar ready
  throwTo id (ErrorCall "hello")
  takeMVar ready2
