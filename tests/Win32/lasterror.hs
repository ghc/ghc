{-# LANGUAGE ForeignFunctionInterface #-}
-- Test that the Win32 error code from getLastError is thread-local.

import System.Win32
import Control.Monad
import Control.Concurrent

main = do
  setLastError 42
  r <- getLastError
  when (r /= 42) $ fail ("wrong: " ++ show r)
  m <- newEmptyMVar
  forkIO $ do setLastError 43; putMVar m ()
  takeMVar m
  r <- getLastError
  when (r /= 42) $ fail ("wrong: " ++ show r)
