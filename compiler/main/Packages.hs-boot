module Packages where
import GhcPrelude
import {-# SOURCE #-} DynFlags(DynFlags)
import {-# SOURCE #-} Module(ComponentId, UnitId, InstalledUnitId)
data PackageState
data PackageConfigMap
emptyPackageState :: PackageState
componentIdString :: DynFlags -> ComponentId -> Maybe String
displayInstalledUnitId :: DynFlags -> InstalledUnitId -> Maybe String
improveUnitId :: PackageConfigMap -> UnitId -> UnitId
getPackageConfigMap :: DynFlags -> PackageConfigMap
