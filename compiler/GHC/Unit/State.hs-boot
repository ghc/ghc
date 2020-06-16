module GHC.Unit.State where

import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.Types (UnitId,Unit)

data UnitState
data UnitDatabase unit

emptyUnitState :: UnitState
pprUnitIdForUser :: UnitState -> UnitId -> SDoc
pprWithUnitState :: UnitState -> SDoc -> SDoc
unwireUnit :: UnitState -> Unit-> Unit
