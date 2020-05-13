module GHC.Unit.State where
import GHC.Prelude
import GHC.Data.FastString
import {-# SOURCE #-} GHC.Unit.Types (IndefUnitId, Unit, UnitId)
data PackageState
data ClosureUnitInfoMap
data UnitDatabase unit
emptyPackageState :: PackageState
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
displayUnitId :: PackageState -> UnitId -> Maybe String
improveUnit :: ClosureUnitInfoMap -> Unit -> Unit
unitInfoMap :: PackageState -> ClosureUnitInfoMap
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
