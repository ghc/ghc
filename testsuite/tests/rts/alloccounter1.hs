module Main where

import Control.Exception
import Control.Monad
import Data.List
import System.Mem

main = do
  let
    testAlloc n = do
      let start = 999999
      setAllocationCounter start
      evaluate (last [1..n])
      c <- getAllocationCounter
      -- print (start - c)
      return (start - c)
  results <- forM [1..1000] testAlloc
  print (sort results == results)
    -- results better be in ascending order
