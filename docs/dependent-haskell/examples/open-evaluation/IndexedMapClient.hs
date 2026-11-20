{-# LANGUAGE NoExperimentalDependentTypes #-}
module IndexedMapClient (mappedCells) where

import IndexedMap (familyInput, getCells, mapIndexed, matchableInput)

mappedCells :: ([Maybe Bool], [Maybe Bool])
mappedCells = (familyCells, maybeCells)
  where
    familyCells = getCells (mapIndexed (fmap not) familyInput)
    maybeCells = getCells (mapIndexed (fmap not) matchableInput)
