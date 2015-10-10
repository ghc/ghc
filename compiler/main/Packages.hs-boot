module Packages where
import {-# SOURCE #-} DynFlags(DynFlags)
import {-# SOURCE #-} Module(ComponentId, UnitId)
data PackageState
data PackageConfigMap
emptyPackageState :: PackageState
componentIdString :: DynFlags -> ComponentId -> Maybe String
improveUnitId :: PackageConfigMap -> UnitId -> UnitId
getPackageConfigMap :: DynFlags -> PackageConfigMap
