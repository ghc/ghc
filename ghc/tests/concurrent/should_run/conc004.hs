module Main where

-- Test thread creation.
-- (from: Einar Wolfgang Karlsen <ewk@Informatik.Uni-Bremen.DE>)

import Concurrent

main :: IO ()
main = spawner forkIO 1000000

spawner :: (IO () -> IO ()) -> Int -> IO ()
spawner c 0 = print "done"
spawner c n = do { c (spawner c (n-1)); return ()}
