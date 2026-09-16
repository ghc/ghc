-- JoinPointLoopSlow.hs run with a compacting collector (+RTS -c): the frame
-- the loop's safepoint leaves behind is walked by Compact.c as well, and
-- compaction relocates every pointer it finds, so a slot of that frame
-- described with the wrong polarity shows up here even without a debug RTS.
-- The GC-count assertion is the one JoinPointLoopSlow makes: the loop's only
-- way out of its BCO is the safepoint.
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
