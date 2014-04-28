module Main (main) where

import GHC.Conc
import Control.Concurrent
import Control.Exception
import System.Exit

main = do
  m <- newEmptyMVar
  let action =  do setAllocationCounter (10*1024)
                   enableAllocationLimit
                   print (length [1..])
  forkFinally action (putMVar m)
  r <- takeMVar m
  case r of
    Left e | Just AllocationLimitExceeded <- fromException e -> return ()
    _ -> print r >> exitFailure
