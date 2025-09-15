-- | Check that large objects are properly accounted for by GHC.Stats
module Main (main) where

import Control.Monad
import Control.Exception
import Control.Concurrent
import System.Mem
import System.Exit
import GHC.Stats
import GHC.Compact
import Data.List (replicate)

import qualified Data.ByteString.Char8 as BS

doGC :: IO ()
doGC = do
  performMajorGC
  threadDelay 1000 -- small delay to allow GC to run when using concurrent gc

main :: IO ()
main = do
  let size = 4096*2
  largeString <- evaluate $ BS.replicate size 'A'
  compactString <- compact $ replicate size 'A'
  doGC
  doGC -- run GC twice to make sure the objects end up in the oldest gen
  stats <- getRTSStats
  let large_obj_bytes = gcdetails_large_objects_bytes $ gc stats
  let compact_obj_bytes = gcdetails_compact_bytes $ gc stats
  -- assert that large_obj_bytes is at least as big as size
  -- this indicates that `largeString` is being accounted for by the stats department
  when (large_obj_bytes < fromIntegral size) $ do
    putStrLn $ "large_obj_bytes is: " <> show large_obj_bytes <> " but expected at least: " <> show size
    exitFailure
  when (compact_obj_bytes < fromIntegral size) $ do
    putStrLn $ "compact_obj_bytes is: " <> show large_obj_bytes <> " but expected at least: " <> show size
    exitFailure
  -- keep them alive
  print $ BS.length largeString
  print $ length $ getCompact compactString
