-- The loop of LoopYield.hs has an Int# parameter and a boxed one live across
-- its safepoint, so the bitmap of the resume frame the safepoint leaves
-- behind has to get both kinds of slot right. The answer is the one native
-- code gives; JoinPointLoopYieldOff is the control with the flag off, where
-- no loop and so no resume frame exists.
module Main (main) where

import LoopYield (walk)

main :: IO ()
main = print (walk 1000000)
