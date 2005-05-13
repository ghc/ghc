module Main where

import Control.Concurrent
import Control.Exception
import Prelude hiding (catch)
import Foreign
import System.IO

foreign import "sleep" unsafe sleepBlock :: Int -> IO ()

main :: IO ()
main = do
  newStablePtr stdout -- prevent stdout being finalized, sigh
  th <- newEmptyMVar
  forkIO $ do
     putStrLn "newThread started"
     sleepBlock 1
     putMVar th "child"
  threadDelay 500000 >> putMVar th "main" `catch` \_ -> return ()
	-- tests that the other thread doing an unsafe call to 
	-- sleep(3) has blocked this thread.  Not sure if this
	-- is a useful test.
  x <- takeMVar th
  putStrLn x
  putStrLn "\nshutting down"
