module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Distribution.PackageDescription.Parse

import Base
import GHC
import Oracles.Config.Setting
import Predicate
import Package (pkgCabalFile)
import Distribution.Verbosity (silent)
import Distribution.Text (display)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (packageDescription)
import qualified Distribution.PackageDescription as DP

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = stage0 ? package ghcCabal ? builder Ghc ? do
    -- Note: We could compute 'cabalDeps' instead of hard-coding it but this
    -- seems unnecessary since we plan to drop @ghc-cabal@ altogether, #18.
    win <- lift windowsHost
    let cabalDeps = [ array, base, bytestring, containers, deepseq, directory
                    , pretty, process, time, if win then win32 else unix ]

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
