-- Test the flage `force-no-intermediates` (issue #4114)
module Main (main) where

import T4114bSub

keep, nokeep :: [FilePath]
keep   = ["T4114bSub.o", "T4114b.o"]
nokeep = ["T4114bSub.hi", "T4114b.hi"]

main :: IO ()
main = do
  mapM_ assertNoKeep nokeep
  mapM_ assertKeep keep
