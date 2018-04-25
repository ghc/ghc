module Main where

-- Test thread creation.
-- (from: Einar Wolfgang Karlsen <ewk@Informatik.Uni-Bremen.DE>)

-- This test is essentially single-threaded, there is no parallelism
-- available.  It just tests how quickly we can create a new thread
-- and context switch to it, many times.

import Control.Concurrent
import System.Environment

main :: IO ()
main = do
   [n] <- getArgs

   mvar <- newEmptyMVar

   let 
   	spawner :: (IO () -> IO ThreadId) -> Int -> IO ()
   	spawner c 0 = putMVar mvar ()
   	spawner c n = do { c (spawner c (n-1)); return ()}

   spawner forkIO (read n :: Int)
   takeMVar mvar
   putStr "done"
