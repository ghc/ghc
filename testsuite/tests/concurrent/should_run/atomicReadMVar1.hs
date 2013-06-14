module Main where

import GHC.MVar
import Control.Concurrent

main = do
    let i = 1000000
    m <- newMVar (0 :: Int)
    let readloop 0 = return ()
        readloop i = do
            atomicReadMVar m
            readloop (i-1)
        writeloop 0 = return ()
        writeloop i = do
            readMVar m
            writeloop (i-1)
    forkIO $ readloop i
    writeloop i
