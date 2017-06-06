module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Distribution.PackageDescription.Parse

import Base
import GHC
import Oracles.Config.Setting
import Oracles.Dependencies (pkgDependencies)
import Predicate
import Package (pkgCabalFile)
import Distribution.Verbosity (silent)
import Distribution.Text (display)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (packageDescription)
import qualified Distribution.PackageDescription as DP

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = stage0 ? package ghcCabal ? builder Ghc ? do
    win <- lift windowsHost
    cabalDeps <- lift $ pkgDependencies cabal
    lift $ need [pkgCabalFile cabal]
    pd <- liftIO . readGenericPackageDescription silent $ pkgCabalFile cabal
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
