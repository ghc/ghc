module Main (main) where

import System.Environment
import Control.Concurrent
import Control.Monad

-----------------------------------------------------------------------------
-- test MVar throughput between the main thread and a child thread

-- This test runs quite slowly on the threaded/SMP RTS vs. the normal RTS,
-- because the main thread and child thread are run by different OS threads,
-- so each MVar communication requires real OS thread switching.
--
-- Figures I get are about a factor of 10 difference in speed, at GHC 6.5.

main = chanTest 300000

chanTest :: Int -> IO ()
chanTest n = do
  chan <- newEmptyMVar
  forkIO (writer chan n)
  reader chan n

reader chan 0 = return ()
reader chan n = do
  takeMVar chan
  reader chan (n-1)

writer chan 0 = return ()
writer chan n = do
  putMVar chan ()
  writer chan (n-1)
