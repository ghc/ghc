module Packages where

import GhcPrelude
import {-# SOURCE #-} Module(ComponentId, UnitId, InstalledUnitId)

data PackageState
data PackageConfigMap

class HasPackageState c where
  getPackageState :: c -> PackageState

emptyPackageState :: PackageState

componentIdString
  :: HasPackageState r
  => r
  -> ComponentId
  -> Maybe String

displayInstalledUnitId
  :: HasPackageState r
  => r
  -> InstalledUnitId
  -> Maybe String

improveUnitId :: PackageConfigMap -> UnitId -> UnitId

getPackageConfigMap :: HasPackageState r => r -> PackageConfigMap
