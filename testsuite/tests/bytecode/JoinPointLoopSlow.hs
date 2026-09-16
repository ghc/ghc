-- The loop's safepoint is the only way out of its defining BCO (see
-- LoopSlow.hs), so a rise in the GC count while it runs proves the
-- safepoint was taken, sized on allocation rather than on wall time.
-- JoinPointLoopSlowOff
-- runs the same program with -fno-bc-join-points-as-labels as the control,
-- without the GC-count assertion.
module Main (main) where

import Control.Monad (when)
import GHC.Stats (getRTSStats, gcs)
import System.Environment (getArgs)

import LoopSlow (grow)

main :: IO ()
main = do
  args <- getArgs
  before <- gcs <$> getRTSStats
  print (grow 1000000)
  after <- gcs <$> getRTSStats
  when ("check-gcs" `elem` args) (print (after - before >= 8))
