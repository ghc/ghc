module GHC.Driver.Session where

import GhcPrelude
import GHC.Platform
import {-# SOURCE #-} Outputable
import {-# SOURCE #-} GHC.Unit.State

data DynFlags

targetPlatform           :: DynFlags -> Platform
pprUserLength            :: DynFlags -> Int
pprCols                  :: DynFlags -> Int
pkgState                 :: DynFlags -> PackageState
unsafeGlobalDynFlags     :: DynFlags
hasPprDebug              :: DynFlags -> Bool
hasNoDebugOutput         :: DynFlags -> Bool
initSDocContext          :: DynFlags -> PprStyle -> SDocContext
