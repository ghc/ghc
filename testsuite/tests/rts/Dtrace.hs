{-# language NumericUnderscores #-}

import Debug.Trace
import Control.Concurrent

main :: IO ()
main = do
  -- Pause for 500ms so we don't finish before bpftrace attaches
  threadDelay 500_000
  traceEventIO "dtrace works"
