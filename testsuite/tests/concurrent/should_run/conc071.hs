module Main where

import Control.Concurrent

main = do
  t <- forkIO (return ())
  threadCapability t >>= print
  t <- forkOn 0 (return ())
  threadCapability t >>= print
  t <- forkOn 1 (return ())
  threadCapability t >>= print
