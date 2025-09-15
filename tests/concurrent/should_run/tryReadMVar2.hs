module Main where

import Control.Concurrent
import Control.Monad

main = do
    m <- newEmptyMVar
    done <- newEmptyMVar
    let q = 200000
    forkIO (do mapM (\n -> putMVar m n) [1..q]; putMVar done ())
    forkIO (do replicateM_ q $ readMVar m; putMVar done ())
    forkIO (do replicateM_ q $ tryReadMVar m; putMVar done ())
    forkIO (do replicateM_ q $ takeMVar m; putMVar done ())
    replicateM_ 4 $ takeMVar done

