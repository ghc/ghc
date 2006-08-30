{-# OPTIONS_GHC -cpp #-}
module Main where

import Control.Concurrent

#ifdef mingw32_HOST_OS
foreign import stdcall safe "Sleep" _sleepBlock :: Int -> IO ()
sleepBlock n = _sleepBlock (n*1000)
#else
foreign import ccall safe "sleep" sleepBlock :: Int -> IO ()
#endif

main :: IO ()
main = do
  th <- newEmptyMVar
  forkIO $ do
     putStrLn "newThread started"
     sleepBlock 1
     putStrLn "newThread back again"
     putMVar th "1 sec later"
  yield -- make sure the newly created thread is run.
  putStrLn "mainThread"
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
  
