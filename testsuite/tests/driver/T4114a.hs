-- Test the flage `force-no-intermediates` (issue #4114)
module Main (main) where

import T4114aSub

keep, nokeep :: [FilePath]
keep   = ["T4114aSub.hi", "T4114aSub.o", "T4114a.hi", "T4114a.o"]
nokeep = [ ]


main :: IO ()
main = do
  mapM_ assertNoKeep nokeep
  mapM_ assertKeep keep
