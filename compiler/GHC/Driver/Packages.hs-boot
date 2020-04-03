module GHC.Driver.Packages where
import GhcPrelude
import FastString
import {-# SOURCE #-} GHC.Driver.Session (DynFlags)
import {-# SOURCE #-} GHC.Types.Module(IndefUnitId, UnitId, InstalledUnitId)
data PackageState
data UnitInfoMap
data PackageDatabase
emptyPackageState :: PackageState
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
displayInstalledUnitId :: PackageState -> InstalledUnitId -> Maybe String
improveUnitId :: UnitInfoMap -> UnitId -> UnitId
getUnitInfoMap :: DynFlags -> UnitInfoMap
unitInfoMap :: PackageState -> UnitInfoMap
getPackageState :: DynFlags -> PackageState
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
