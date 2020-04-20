module GHC.Driver.Packages where
import GHC.Prelude
import GHC.Data.FastString
import {-# SOURCE #-} GHC.Driver.Session (DynFlags)
import {-# SOURCE #-} GHC.Types.Module(ComponentId, UnitId, InstalledUnitId)
data PackageState
data UnitInfoMap
data PackageDatabase
emptyPackageState :: PackageState
componentIdString :: ComponentId -> String
mkComponentId :: PackageState -> FastString -> ComponentId
displayInstalledUnitId :: PackageState -> InstalledUnitId -> Maybe String
improveUnitId :: UnitInfoMap -> UnitId -> UnitId
getUnitInfoMap :: DynFlags -> UnitInfoMap
getPackageState :: DynFlags -> PackageState
