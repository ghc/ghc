module GHC.Unit.State where

import GHC.Data.FastString
import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, UnitId)

data UnitState
data UnitDatabase unit

emptyUnitState :: UnitState
mkIndefUnitId :: UnitState -> FastString -> IndefUnitId
pprUnitIdForUser :: UnitState -> UnitId -> SDoc
updateIndefUnitId :: UnitState -> IndefUnitId -> IndefUnitId
