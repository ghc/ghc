module Main where

-- 		GHC 4.04
-- I've been having problems getting GHC to compile some code I'm working
-- on with optimisation (-O) turned on.  Compilation is fine without -O
-- specified.  Through a process of elimination I've managed to reproduce
-- the problemin the following (much simpler) piece of code: 

import Data.List

test es = 
  concat (groupBy eq (zip [0..(length es) - 1] es))
  where
  eq a b = (fst a) == (fst b)

main = putStr (show (test [1,2,3,4]))


