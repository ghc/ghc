module Main where

import System.Posix.Process
import Control.Concurrent

main = do
  pid1 <- forkProcess $ do threadDelay 100000
  pid2 <- forkProcess $ do threadDelay 100000
  print ()

