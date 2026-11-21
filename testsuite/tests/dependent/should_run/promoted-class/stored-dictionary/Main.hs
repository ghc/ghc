{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes #-}
module Main where

import FoldCell (At(..), foldCell, foldDirect, foldValue)
import StoredEvidence (Cell(Stored), Pick(..), pack)

instance Pick Bool where
    pick = not

readCell :: Cell [Bool] -> Bool
readCell c = case foldCell c of At b -> b

readDirect :: Cell [Bool] -> Bool
readDirect c = case foldDirect c of At b -> b

readValue :: Cell [Bool] -> Bool
readValue c = case foldValue c of At b -> b

main :: IO ()
main = print ((readCell (pack True), readCell (Stored True)),
              (readDirect (pack True), readDirect (Stored True)),
              (readValue (pack True), readValue (Stored True)))
