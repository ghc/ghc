module GHC.Driver.Packages where
import GhcPrelude
import {-# SOURCE #-} GHC.Driver.Session (DynFlags)
import {-# SOURCE #-} Module(ComponentId, UnitId, InstalledUnitId)
data PackageState
data UnitInfoMap
data PackageDatabase
emptyPackageState :: PackageState
componentIdString :: DynFlags -> ComponentId -> Maybe String
displayInstalledUnitId :: DynFlags -> InstalledUnitId -> Maybe String
improveUnitId :: UnitInfoMap -> UnitId -> UnitId
getUnitInfoMap :: DynFlags -> UnitInfoMap
