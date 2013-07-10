module Main where

import GHC.MVar
import Control.Concurrent

main = do
    m <- newMVar (0 :: Int)
    Just 0 <- tryAtomicReadMVar m
    takeMVar m
    Nothing <- tryAtomicReadMVar m
    return ()
