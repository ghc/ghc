{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RoleAnnotations #-}
module RetainedStoredDictionary (Result(..), inspectStored, ignoreStored) where

import StoredDictionary (Cell(Stored), Pick(pick))

type role Tagged nominal
data Tagged (b :: Bool) = Tagged Bool

tag :: foreach (b :: Bool) -> Tagged b
tag b = Tagged @b b

-- | Return the result at the index of the matched cell.
type role Result nominal
data Result (cell :: Cell [Bool]) = Result Bool

-- | Match the cell and use its dictionary in a retained call.
inspectStored :: foreach (cell :: Cell [Bool]) -> Result cell
inspectStored cell = Result @cell (case cell of
    Stored x -> case tag (pick x) of Tagged b -> b)

-- | Keep the cell's index without evaluating the cell.
ignoreStored :: foreach (cell :: Cell [Bool]) -> Result cell
ignoreStored cell = Result @cell True
