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
foreign import stdcall unsafe "Sleep" sleepBlock :: Int -> IO ()
#else
sleep n = sleepBlock n
foreign import ccall unsafe "sleep" sleepBlock :: Int -> IO ()
#endif

main :: IO ()
main = do
  newStablePtr stdout -- prevent stdout being finalized, sigh
  th <- newEmptyMVar
  forkIO $ do
     putStrLn "newThread started"
     sleep 1
     putMVar th "child"
  threadDelay 500000
  yield -- another hack, just in case child yields right after "sleep 1"
  putMVar th "main" `catch` (\BlockedIndefinitelyOnMVar -> return ())
	-- tests that the other thread doing an unsafe call to 
	-- sleep(3) has blocked this thread.  Not sure if this
	-- is a useful test.
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
