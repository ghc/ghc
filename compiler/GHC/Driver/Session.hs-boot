module GHC.Driver.Session
where

import GHC.Platform ( Platform )
import GHC.Utils.Outputable ( PprStyle, SDocContext )

data DynFlags
data CompilerInfo
clang :: CompilerInfo
targetPlatform :: DynFlags -> Platform


initSDocContext :: DynFlags -> PprStyle -> SDocContext
