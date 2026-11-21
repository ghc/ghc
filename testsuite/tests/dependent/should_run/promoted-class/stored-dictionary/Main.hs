{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes #-}
module Main where

import FoldCell (At(..), foldCell)
import StoredEvidence (Cell(Stored), Pick(..), pack)

instance Pick Bool where
    pick = not

readCell :: Cell [Bool] -> Bool
readCell c = case foldCell c of At b -> b

main :: IO ()
main = print (readCell (pack True), readCell (Stored True))
