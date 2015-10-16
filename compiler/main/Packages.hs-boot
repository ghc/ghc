module Packages where
-- Well, this is kind of stupid...
import {-# SOURCE #-} Module (UnitId)
import {-# SOURCE #-} DynFlags (DynFlags)
data PackageState
unitIdPackageIdString :: DynFlags -> UnitId -> Maybe String
emptyPackageState :: PackageState
