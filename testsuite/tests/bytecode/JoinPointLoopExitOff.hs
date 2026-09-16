-- The control for JoinPointLoopExit with -fno-bc-join-points-as-labels: no
-- loop and no label exist, the jump out is a closure entry, and the program
-- must still print what native code prints.
module Main (main) where

import LoopExit (run)

main :: IO ()
main = print (run 1000000 1)
