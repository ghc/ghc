module Main where

import GHC.Conc.Sync

-- Regression test for the JS backend's ListThreadsOp, which used to omit the
-- running thread. Whatever other threads the RTS has is irrelevant here.
main :: IO ()
main = do
  tid <- myThreadId
  ts <- listThreads
  print (tid `elem` ts)
