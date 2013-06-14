module Main where

import GHC.MVar
import Control.Concurrent

main = do
    m <- newEmptyMVar
    sync <- newEmptyMVar
    let f = atomicReadMVar m
    t1 <- forkIO (f >> error "FAILURE")
    t2 <- forkIO (f >> putMVar sync ())
    killThread t1
    putMVar m (0 :: Int)
    atomicReadMVar sync
