-- The control for JoinPointLoopYield.hs: the same program compiled with
-- -fno-bc-join-points-as-labels, where no loop is emitted, so its answer is
-- the one the resume frame must not change.
module Main (main) where

import LoopYield (walk)

main :: IO ()
main = print (walk 1000000)
