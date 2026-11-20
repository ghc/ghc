{-# LANGUAGE NoExperimentalDependentTypes #-}
module IndexedMapClient (mappedCells) where

import IndexedMap (familyInput, getCells, mapIndexed, matchableInput)

mappedCells :: ([Maybe Bool], [Maybe Bool])
mappedCells =
    ( getCells (mapIndexed (fmap not) familyInput)
    , getCells (mapIndexed (fmap not) matchableInput)
    )
