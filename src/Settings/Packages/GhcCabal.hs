module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Hadrian.Haskell.Cabal

import Base
import Expression
import GHC
import Utilities

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = stage0 ? package ghcCabal ? builder Ghc ? do
    cabalDeps <- expr $ pkgDependencies cabal
    (_, cabalVersion) <- expr $ cabalNameVersion (pkgCabalFile cabal)
    mconcat
        [ pure [ "-package " ++ pkgNameString pkg | pkg <- cabalDeps ]
        , arg "--make"
        , arg "-j"
        , arg ("-DCABAL_VERSION=" ++ replace "." "," cabalVersion)
        , arg "-DBOOTSTRAPPING"
        , arg "-DMIN_VERSION_binary_0_8_0"
        , arg "-DGENERICS"
        , arg "-optP-include"
        , arg $ "-optP" ++ pkgPath ghcCabal -/- "cabal_macros_boot.h"
        , arg "-ilibraries/Cabal/Cabal"
        , arg "-ilibraries/binary/src"
        , arg "-ilibraries/filepath"
        , arg "-ilibraries/hpc" ]
