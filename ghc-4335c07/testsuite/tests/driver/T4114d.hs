-- Test the flage `force-no-intermediates` (issue #4114)
module Main (main) where

import T4114dSub

keep, nokeep :: [FilePath]
keep   = ["T4114dSub.myhi", "T4114d.myhi"]
nokeep = ["T4114dSub.myo", "T4114d.myo"]


main :: IO ()
main = do
  mapM_ assertNoKeep nokeep
  mapM_ assertKeep keep
