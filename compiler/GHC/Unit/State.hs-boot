module GHC.Unit.State where
import GHC.Prelude
import GHC.Data.FastString
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, Unit, UnitId)
data PackageState
data UnitInfoMap
data PackageDatabase unit
emptyPackageState :: PackageState
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
displayUnitId :: PackageState -> UnitId -> Maybe String
improveUnit :: UnitInfoMap -> Unit -> Unit
unitInfoMap :: PackageState -> UnitInfoMap
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
