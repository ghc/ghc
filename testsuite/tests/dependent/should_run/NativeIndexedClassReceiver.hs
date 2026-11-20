{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module Main (main) where

import Control.Monad (unless)
import IndexedMap (Batch(..), familyInput, getCells)
import IndexedMapClient (mappedCells)
import Type.Reflection (TypeRep, typeRep)
import Type.Reflection.Unsafe (mkTrApp)

direct, empty, populated :: [Maybe Bool]
direct = getCells familyInput
empty = getCells (Batch [])
populated = getCells (Batch [Just True])

arrow :: TypeRep (Int -> Bool)
arrow = typeRep `mkTrApp` typeRep @Int `mkTrApp` typeRep @Bool

main :: IO ()
main = do
    unless (direct == [Just False, Nothing] && null empty && populated == [Just True]) $
        fail "unexpected receiver contents"
    unless (arrow == typeRep @(Int -> Bool)) $
        fail "unexpected arrow representation"
    print mappedCells
