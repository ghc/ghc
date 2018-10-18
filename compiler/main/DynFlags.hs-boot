module DynFlags where

import GhcPrelude
import Platform

data DynFlags
data DumpFlag
data GeneralFlag

targetPlatform           :: DynFlags -> Platform
pprUserLength            :: DynFlags -> Int
pprCols                  :: DynFlags -> Int
unsafeGlobalDynFlags     :: DynFlags
useUnicode               :: DynFlags -> Bool
useUnicodeSyntax         :: DynFlags -> Bool
useStarIsType            :: DynFlags -> Bool
shouldUseColor           :: DynFlags -> Bool
shouldUseHexWordLiterals :: DynFlags -> Bool
hasPprDebug              :: DynFlags -> Bool
hasNoDebugOutput         :: DynFlags -> Bool
