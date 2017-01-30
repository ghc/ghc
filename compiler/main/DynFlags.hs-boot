
module DynFlags where

import Platform

data DynFlags
data OverridingBool
data DumpFlag

targetPlatform       :: DynFlags -> Platform
pprUserLength        :: DynFlags -> Int
pprCols              :: DynFlags -> Int
unsafeGlobalDynFlags :: DynFlags
useUnicode           :: DynFlags -> Bool
useUnicodeSyntax     :: DynFlags -> Bool
useColor             :: DynFlags -> OverridingBool
canUseColor          :: DynFlags -> Bool
overrideWith         :: Bool -> OverridingBool -> Bool
