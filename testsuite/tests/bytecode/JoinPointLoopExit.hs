-- The loop of LoopExit.hs jumps out of itself to a join point defined
-- outside it, carrying an Int# and a list that are read after the jump. The
-- answer is the one native code gives; JoinPointLoopExitOff is the control
-- with the flag off, where no loop and no label exist and the jump is a
-- closure entry.
module Main (main) where

import LoopExit (run)

main :: IO ()
main = print (run 1000000 1)
