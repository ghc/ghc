module Main where

import Control.Exception
import Control.Concurrent
import Prelude hiding (catch)

-- the BlockOnDeadMVar exception doesn't cause any output by default

main = do
  forkIO (do m <- newEmptyMVar; takeMVar m)
  print (sum [1..10000])
