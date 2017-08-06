module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (packageDescription)
import Distribution.PackageDescription.Parse
import qualified Distribution.PackageDescription as DP
import Distribution.Text (display)
import Distribution.Verbosity (silent)

import Base
import Expression
import GHC
import Oracles.Dependencies (pkgDependencies)

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = stage0 ? package ghcCabal ? builder Ghc ? do
    cabalDeps <- expr $ pkgDependencies cabal
    expr $ need [pkgCabalFile cabal]
    pd <- exprIO . readGenericPackageDescription silent $ pkgCabalFile cabal
    let identifier   = DP.package . packageDescription $ pd
        cabalVersion = display . pkgVersion $ identifier

    mconcat
        [ append [ "-package " ++ pkgNameString pkg | pkg <- cabalDeps ]
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
