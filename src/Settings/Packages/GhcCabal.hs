module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Base
import Expression
import GHC (ghcCabal)
import Predicates (builderGhc, package, stage0)
import Settings

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = package ghcCabal ? mconcat
    [ builderGhc ?
      mconcat [ ghcCabalBootArgs
              , remove ["-no-auto-link-packages"] ] ]

-- Boostrapping ghcCabal
-- TODO: do we need -DCABAL_VERSION=$(CABAL_VERSION)?
ghcCabalBootArgs :: Args
ghcCabalBootArgs = stage0 ? do
    path <- getTargetPath
    let cabalMacros     = path -/- "build/autogen/cabal_macros.h"
        cabalMacrosBoot = pkgPath ghcCabal -/- "cabal_macros_boot.h"
    mconcat
        [ remove ["-hide-all-packages"]
        , removePair "-optP-include" $ "-optP" ++ cabalMacros
        , arg "--make"
        , arg "-DBOOTSTRAPPING"
        , arg "-DMIN_VERSION_binary_0_8_0"
        , arg "-DGENERICS"
        , arg "-optP-include"
        , arg $ "-optP" ++ cabalMacrosBoot
        , arg "-ilibraries/Cabal/Cabal"
        , arg "-ilibraries/binary/src"
        , arg "-ilibraries/filepath"
        , arg "-ilibraries/hpc" ]
