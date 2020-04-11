module GHC.Driver.Packages where
import GhcPrelude
import FastString
import {-# SOURCE #-} GHC.Driver.Session (DynFlags)
import {-# SOURCE #-} GHC.Types.Module(IndefUnitId, Unit)
import {-# SOURCE #-} GHC.Types.Unit.Id(UnitId)
data PackageState
data UnitInfoMap
data PackageDatabase unit
emptyPackageState :: PackageState
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
displayUnitId :: PackageState -> UnitId -> Maybe String
improveUnit :: UnitInfoMap -> Unit -> Unit
getUnitInfoMap :: DynFlags -> UnitInfoMap
unitInfoMap :: PackageState -> UnitInfoMap
getPackageState :: DynFlags -> PackageState
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
