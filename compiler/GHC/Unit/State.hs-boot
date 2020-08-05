module GHC.Unit.State where

import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, UnitId)

data UnitState
data UnitDatabase unit

emptyUnitState :: UnitState
mkIndefUnitId :: UnitState -> UnitId -> IndefUnitId
pprUnitIdForUser :: UnitState -> UnitId -> SDoc
updateIndefUnitId :: UnitState -> IndefUnitId -> IndefUnitId
