module Main where

-- !!! test Eq and Ord instances over thread Ids.

import Control.Concurrent

main = do
  tso1 <- forkIO (return ())
  tso2 <- forkIO (return ())
  print [compare tso1 tso2, compare tso1 tso1, compare tso2 tso1]
