module Main where

import Control.Concurrent
import Control.Exception

main = do
  -- stack will grow as we evaluate the expression, and then shrink again
  evaluate $ foldr (+) 0 [1..200000]
  threadDelay 10 -- allow stack to shrink back to its smallest size
