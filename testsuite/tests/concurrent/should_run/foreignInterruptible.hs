{-# LANGUAGE CPP,ForeignFunctionInterface,InterruptibleFFI #-}
module Main where

import Control.Concurrent
import Control.Exception
#if __GLASGOW_HASKELL__ < 705
import Prelude hiding (catch)
#endif
import Foreign
import System.IO

#ifdef mingw32_HOST_OS
sleep n = sleepBlock (n*1000)
foreign import stdcall interruptible "Sleep" sleepBlock :: Int -> IO ()
#else
sleep n = sleepBlock n
foreign import ccall interruptible "sleep" sleepBlock :: Int -> IO ()
#endif

main :: IO ()
main = do
  newStablePtr stdout -- prevent stdout being finalized
  th <- newEmptyMVar

  tid <- forkIO $ do
     putStrLn "newThread started"
     (sleep 2 >> putStrLn "fail") `catch` (\ThreadKilled -> putStrLn "pass")
     putMVar th "child"

  -- if the killThread below gets blocked for more than a second, then
  -- this thread will kill the main thread and the test will fail.
  main <- myThreadId
  forkIO $ do threadDelay 1000000; throwTo main (ErrorCall "still waiting")

  yield
  threadDelay 500000
  killThread tid
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
