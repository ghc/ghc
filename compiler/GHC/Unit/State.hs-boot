module GHC.Unit.State where

import GHC.Prelude
import GHC.Data.FastString
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, UnitId)

data PackageState
data UnitDatabase unit

emptyPackageState :: PackageState
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
displayUnitId :: PackageState -> UnitId -> Maybe String
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
