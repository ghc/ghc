{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -cpp #-}
module Main where

import Control.Concurrent
import Control.Exception
import Prelude hiding (catch)
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
  yield
  threadDelay 500000
  killThread tid
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
