module Main where

import Control.Concurrent

foreign import "sleep" unsafe sleepBlock :: Int -> IO ()

main :: IO ()
main = do
  th <- newEmptyMVar
  forkIO $ do
     putStrLn "newThread started"
     sleepBlock 5
     putStrLn "newThread back again"
     putMVar th "5 secs later"
  yield -- make sure the newly created thread is run.
  putStrLn "mainThread"
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
  
