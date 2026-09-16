-- The control for JoinPointLoopSlowCompact.hs: the same program under +RTS -c
-- with -fno-bc-join-points-as-labels, where no loop and so no resume frame is
-- emitted, and no GC-count assertion is made.
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
