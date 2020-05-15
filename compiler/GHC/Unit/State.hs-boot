module GHC.Unit.State where

import GHC.Prelude
import GHC.Data.FastString
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, UnitId)

data UnitState
data UnitDatabase unit

emptyUnitState :: UnitState
mkIndefUnitId :: UnitState -> FastString -> IndefUnitId
displayUnitId :: UnitState -> UnitId -> Maybe String
updateIndefUnitId :: UnitState -> IndefUnitId -> IndefUnitId
