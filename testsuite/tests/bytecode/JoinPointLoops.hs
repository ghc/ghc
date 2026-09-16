-- Run the four loop shapes from LoopShapes and check their output against
-- native compilation, with -fbc-join-points-as-labels on (see
-- JoinPointLoopsOff.hs for the control with the flag off). Note [Join points
-- as loops] in StgToByteCode.hs; loops only appear at -O with breakpoints
-- off, which is why this test passes -fno-unoptimized-core-for-interpreter
-- -fno-break-points.
module Main (main) where

import LoopShapes (count, alloc, wide, mixed)

main :: IO ()
main = do
  print (count 200000)
  print (alloc 200000)
  print (wide 1 2 3 4 5 100000)
  print (mixed 100000 1.5 3)
