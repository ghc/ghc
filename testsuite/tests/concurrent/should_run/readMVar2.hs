module Main where

import Control.Concurrent

main = do
    m <- newEmptyMVar
    sync <- newEmptyMVar
    let f = readMVar m
    t1 <- forkIO (f >> error "FAILURE")
    t2 <- forkIO (f >> putMVar sync ())
    killThread t1
    putMVar m (0 :: Int)
    readMVar sync
