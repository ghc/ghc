module GHC.Unit.State where

import GHC.Prelude
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, UnitId)

data UnitState
data UnitDatabase unit

emptyUnitState :: UnitState
displayUnitId :: UnitState -> UnitId -> Maybe String
updateIndefUnitId :: UnitState -> IndefUnitId -> IndefUnitId
