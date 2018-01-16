-- Test the flage `force-no-intermediates` (issue #4114)
module Main (main) where

import T4114cSub

keep, nokeep :: [FilePath]
keep   = ["T4114cSub.hi", "T4114c.hi"]
nokeep = ["T4114cSub.o", "T4114c.o"]


main :: IO ()
main = do
  mapM_ assertNoKeep nokeep
  mapM_ assertKeep keep
