module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Hadrian.Haskell.Cabal

import Base
import Expression
import Utilities

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = stage0 ? package ghcCabal ? builder Ghc ? do
    cabalDeps <- expr $ stage1Dependencies cabal
    let bootDeps = cabalDeps \\ [integerGmp, integerSimple, mtl, parsec, text]
    cabalVersion <- expr $ pkgVersion (unsafePkgCabalFile cabal) -- TODO: improve
    mconcat
        [ pure [ "-package " ++ pkgName pkg | pkg <- bootDeps ]
        , arg "--make"
        , arg "-j"
        , pure ["-Wall", "-fno-warn-unused-imports", "-fno-warn-warnings-deprecations"]
        , arg ("-DCABAL_VERSION=" ++ replace "." "," cabalVersion)
        , arg "-DCABAL_PARSEC"
        , arg "-DBOOTSTRAPPING"
        , arg "-DMIN_VERSION_binary_0_8_0"
        , arg "libraries/text/cbits/cbits.c"
        , arg "-ilibraries/Cabal/Cabal"
        , arg "-ilibraries/binary/src"
        , arg "-ilibraries/filepath"
        , arg "-ilibraries/hpc"
        , arg "-ilibraries/mtl"
        , arg "-ilibraries/text"
        , arg "-Ilibraries/text/include"
        , arg "-ilibraries/parsec" ]

