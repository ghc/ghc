{-# OPTIONS_GHC -finfo-table-map #-}

-- | Check that a @BlockedIndefinitelyOnMVar@ deadlock exception carries a
-- backtrace mentioning the blocking site in this module.
import Control.Concurrent.MVar
import GHC.Exception.Backtrace.Experimental

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  mv <- newEmptyMVar :: IO (MVar ())
  x <- takeMVar mv
  print x
