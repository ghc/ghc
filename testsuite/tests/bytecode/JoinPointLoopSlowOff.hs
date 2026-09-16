-- The control for JoinPointLoopSlow.hs: same program with
-- -fno-bc-join-points-as-labels, no loop is emitted, and no GC-count
-- assertion is made (its GC count is not what this test checks).
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
