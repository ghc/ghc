module Main where

import BuggyOpt() -- bug inducer!

import Prelude hiding (lookup)
import Good
import M

mkLin :: Array Int -> Map (Array Int) Int
mkLin mseqs =
   (isJust (lookup mseqs empty)) `seq` (insert mseqs 1 empty)

main :: IO ()
main = print $ isGood $ mkLin (array (1,1) [ (1,array (3,3) [(3, 42)]) ]!!!1)
