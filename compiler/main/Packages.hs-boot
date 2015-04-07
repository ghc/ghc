module Packages where
-- Well, this is kind of stupid...
import {-# SOURCE #-} Module (PackageKey)
import {-# SOURCE #-} DynFlags (DynFlags)
data PackageState
packageKeyPackageIdString :: DynFlags -> PackageKey -> Maybe String
