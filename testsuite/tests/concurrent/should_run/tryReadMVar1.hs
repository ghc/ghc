module Main where

import Control.Concurrent

main = do
    m <- newMVar (0 :: Int)
    Just 0 <- tryReadMVar m
    takeMVar m
    Nothing <- tryReadMVar m
    return ()
