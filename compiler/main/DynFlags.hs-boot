module DynFlags where

import GhcPrelude

import OutputOptions
import GHC.Platform
import NameSuppress
import TypeSuppress
import {-# SOURCE #-} Outputable (HasPprConfig)
import {-# SOURCE #-} Packages (HasPackageState)

data DynFlags
data DumpFlag
data GeneralFlag

targetPlatform           :: DynFlags -> Platform
pprCols                  :: DynFlags -> Int
unsafeGlobalDynFlags     :: DynFlags
useUnicode               :: DynFlags -> Bool
useUnicodeSyntax         :: DynFlags -> Bool
useStarIsType            :: DynFlags -> Bool
shouldUseColor           :: DynFlags -> Bool
shouldUseHexWordLiterals :: DynFlags -> Bool

instance HasOutputOptions DynFlags
instance HasPprConfig DynFlags
instance HasNameSuppress DynFlags
instance HasTypeSuppress DynFlags
instance HasPackageState DynFlags
