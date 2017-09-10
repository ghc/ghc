module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Hadrian.Haskell.Cabal

import Base
import Expression
import Utilities

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = stage0 ? package ghcCabal ? builder Ghc ? do
    cabalDeps    <- expr $ stage1Dependencies cabal
    cabalVersion <- expr $ pkgVersion (unsafePkgCabalFile cabal) -- TODO: improve
    mconcat
        [ pure [ "-package " ++ pkgName pkg | pkg <- cabalDeps, pkg /= parsec ]
        , arg "--make"
        , arg "-j"
        , arg ("-DCABAL_VERSION=" ++ replace "." "," cabalVersion)
        , arg "-DBOOTSTRAPPING"
        , arg "-DMIN_VERSION_binary_0_8_0"
        , arg "-ilibraries/Cabal/Cabal"
        , arg "-ilibraries/binary/src"
        , arg "-ilibraries/filepath"
        , arg "-ilibraries/hpc" ]
