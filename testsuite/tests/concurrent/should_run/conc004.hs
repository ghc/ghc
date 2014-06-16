module Main where

-- Test thread creation.
-- (from: Einar Wolfgang Karlsen <ewk@Informatik.Uni-Bremen.DE>)

import Control.Concurrent

main :: IO ()
main = do
   mvar <- newEmptyMVar

   let 
   	spawner :: (IO () -> IO ThreadId) -> Int -> IO ()
   	spawner c 0 = putMVar mvar ()
   	spawner c n = do { c (spawner c (n-1)); return ()}

   spawner forkIO 100000
   takeMVar mvar
   putStr "done"
