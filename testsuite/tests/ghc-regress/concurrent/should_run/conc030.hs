module Main where

import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)

-- the ThreadKilled exception doesn't cause any output by default

main = do
  m <- newEmptyMVar
  id <- forkIO (takeMVar m)
  yield
  killThread id
  putMVar m ()
  print (sum [1..50000])
