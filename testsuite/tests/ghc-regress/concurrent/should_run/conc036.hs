module Main where

import Control.Concurrent

foreign import "sleep" unsafe sleepBlock :: Int -> IO ()

main :: IO ()
main = do
  th <- newEmptyMVar
  forkIO $ do
     putStrLn "newThread started"
     sleepBlock 3
     putStrLn "newThread back again"
     putMVar th "5 secs later"
  threadDelay 1000000 >> putStrLn "mainThread"
	-- tests that the other thread doing an unsafe call to 
	-- sleep(3) has blocked this thread.  Not sure if this
	-- is a useful test.
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
