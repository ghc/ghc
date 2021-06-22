module GHC.Driver.Session where

import GHC.Prelude
import GHC.Platform
import GHC.Utils.Misc (OverridingBool)
import {-# SOURCE #-} GHC.Utils.Outputable

data DynFlags

targetPlatform           :: DynFlags -> Platform
hasPprDebug              :: DynFlags -> Bool
hasNoDebugOutput         :: DynFlags -> Bool
initSDocContext          :: DynFlags -> PprStyle -> SDocContext
useColor                 :: DynFlags -> OverridingBool
