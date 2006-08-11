module Main where

import Control.Concurrent

foreign import ccall safe "sleep" sleepBlock :: Int -> IO ()

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
  
