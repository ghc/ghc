{-# LANGUAGE NumericUnderscores #-}
module Main where

import Data.List (transpose, foldl')
import GHC.Stats
import System.Exit

thingy :: [[[Int]]]
thingy = [ [[1],[2]], [[1..10^7], [3]]]

thingy2 :: [[[Int]]]
thingy2 = [ [[1],[2]], [[3], [2..10^7+1]]]

main = do
  htr : ttr <- pure $ transpose thingy
  print $ even $ foldl' (+) 0 . head . tail $ htr

  htr2 : ttr2 <- pure $ transpose thingy2
  print $ even $ foldl' (+) 0 . head . tail . head $ ttr2

  maxLiveBytes <- max_live_bytes <$> getRTSStats
  if (maxLiveBytes) < 200_000
  then putStrLn "Test is running in the expected residency limit"
  else do
    putStrLn $ "Test is running with " <> show maxLiveBytes <> " bytes of residency!"
    exitFailure

