module Main where

import Control.Concurrent

-- example from
-- http://www.haskell.org/pipermail/glasgow-haskell-users/2008-November/015878.html

main = do
    m <- newMVar (0 :: Int)
    forkIO $ putMVar m 1
    yield
    r1 <- readMVar m
    r2 <- takeMVar m
    r3 <- takeMVar m
    return ()
