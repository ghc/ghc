module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Base
import GHC
import Oracles.Config.Setting
import Predicate
import Settings.Path

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = package ghcCabal ?
    builder Ghc ? mconcat [ ghcCabalBootArgs
                          , remove ["-no-auto-link-packages"] ]

-- Boostrapping ghcCabal
-- TODO: do we need -DCABAL_VERSION=$(CABAL_VERSION)?
ghcCabalBootArgs :: Args
ghcCabalBootArgs = stage0 ? do
    -- Note: We could have computed 'cabalDeps' instead of hard-coding it
    -- but this doesn't worth the effort, since we plan to drop ghc-cabal
    -- altogether at some point. See #18.
    cabalDeps <- fromDiffExpr $ mconcat
        [ append [ array, base, bytestring, containers, deepseq, directory
                 , pretty, process, time ]
        , notM windowsHost ? append [unix]
        , windowsHost ? append [win32] ]
    context <- getContext
    mconcat
        [ append [ "-package " ++ pkgNameString pkg | pkg <- cabalDeps ]
        , arg "--make"
        , arg "-j"
        , arg "-DBOOTSTRAPPING"
        , arg "-DMIN_VERSION_binary_0_8_0"
        , arg "-DGENERICS"
        , removePair "-optP-include" $ "-optP" ++ autogenPath context -/- "cabal_macros.h"
        , arg "-optP-include"
        , arg $ "-optP" ++ pkgPath ghcCabal -/- "cabal_macros_boot.h"
        , arg "-ilibraries/Cabal/Cabal"
        , arg "-ilibraries/binary/src"
        , arg "-ilibraries/filepath"
        , arg "-ilibraries/hpc" ]
