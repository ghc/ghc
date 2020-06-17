module GHC.Driver.Session where

import GHC.Prelude
import GHC.Platform
import {-# SOURCE #-} GHC.Utils.Outputable
import {-# SOURCE #-} GHC.Unit.State

data DynFlags

targetPlatform           :: DynFlags -> Platform
unitState                :: DynFlags -> UnitState
unsafeGlobalDynFlags     :: DynFlags
hasPprDebug              :: DynFlags -> Bool
hasNoDebugOutput         :: DynFlags -> Bool
initSDocContext          :: DynFlags -> PprStyle -> SDocContext
