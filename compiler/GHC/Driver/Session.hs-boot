module GHC.Driver.Session where

import GhcPrelude
import GHC.Platform
import {-# SOURCE #-} Outputable

data DynFlags

targetPlatform           :: DynFlags -> Platform
pprUserLength            :: DynFlags -> Int
unsafeGlobalDynFlags     :: DynFlags
hasPprDebug              :: DynFlags -> Bool
hasNoDebugOutput         :: DynFlags -> Bool
initSDocContext          :: DynFlags -> PprStyle -> SDocContext
