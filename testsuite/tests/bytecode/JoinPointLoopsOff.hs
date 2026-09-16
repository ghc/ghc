-- Same as JoinPointLoops.hs, but with -fno-bc-join-points-as-labels: the
-- control that shows the same output is produced when no loop is emitted at
-- all.
module Main (main) where

import LoopShapes (count, alloc, wide, mixed)

main :: IO ()
main = do
  print (count 200000)
  print (alloc 200000)
  print (wide 1 2 3 4 5 100000)
  print (mixed 100000 1.5 3)
