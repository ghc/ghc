-- Test for bug #418

module Main where

import Control.Concurrent
import System.IO.Unsafe (unsafeInterleaveIO)

main = do
    v <- newEmptyMVar
    a <- unsafeInterleaveIO (readMVar v)
    t <- forkIO (print a)
    threadDelay (100*1000)
    killThread t
    forkIO $ print a
    putMVar v ()

