{-# OPTIONS_GHC -finfo-table-map #-}

-- | Check that a @BlockedIndefinitelyOnSTM@ deadlock exception carries a
-- backtrace mentioning the blocking site in this module.
import GHC.Conc (atomically, retry)
import GHC.Exception.Backtrace.Experimental

main :: IO ()
main = do
  setBacktraceMechanismState IPEBacktrace True
  x <- atomically retry :: IO ()
  print x
