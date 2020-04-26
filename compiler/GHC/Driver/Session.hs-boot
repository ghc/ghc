module GHC.Driver.Session where

import GHC.Prelude
import GHC.Platform
import {-# SOURCE #-} GHC.Utils.Outputable

data DynFlags

targetPlatform           :: DynFlags -> Platform
pprUserLength            :: DynFlags -> Int
unsafeGlobalDynFlags     :: DynFlags
hasPprDebug              :: DynFlags -> Bool
hasNoDebugOutput         :: DynFlags -> Bool
initSDocContext          :: DynFlags -> PprStyle -> SDocContext
