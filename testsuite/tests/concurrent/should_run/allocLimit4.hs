module Main (main) where

import GHC.Conc
import Control.Concurrent
import Control.Exception
import System.Exit
import Control.Monad

-- check that +RTS -xq is doing the right thing: the test requires
-- +RTS -xq300k

main = do
  m <- newEmptyMVar
  let action = do
         e <- try $ do
          setAllocationCounter (10*1024)
          enableAllocationLimit
          print (length [1..])
         case e of
           Left AllocationLimitExceeded{} -> do
             c <- getAllocationCounter
             when (c < 250*1024 || c > 350*1024) $ fail "wrong limit grace"
             print (length [2..])
           Right _ ->
             fail "didn't catch AllocationLimitExceeded"

  forkFinally action (putMVar m)
  r <- takeMVar m
  case r of
    Left e | Just AllocationLimitExceeded <- fromException e -> return ()
    _ -> print r >> exitFailure
