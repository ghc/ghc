module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Base
import GHC
import Oracles.Config.Setting
import Predicates
import Settings

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = package ghcCabal ? mconcat
    [ builder Ghc ?
      mconcat [ ghcCabalBootArgs
              , remove ["-no-auto-link-packages"] ] ]

-- Boostrapping ghcCabal
-- TODO: do we need -DCABAL_VERSION=$(CABAL_VERSION)?
ghcCabalBootArgs :: Args
ghcCabalBootArgs = stage0 ? do
    path <- getBuildPath
    let cabalMacros     = path -/- "autogen/cabal_macros.h"
        cabalMacrosBoot = pkgPath ghcCabal -/- "cabal_macros_boot.h"
    -- Note: We could have computed 'cabalDeps' instead of hard-coding it
    -- but this doesn't worth the effort, since we plan to drop ghc-cabal
    -- altogether at some point. See #18.
    cabalDeps <- fromDiffExpr $ mconcat
        [ append [ array, base, bytestring, containers, deepseq, directory
                 , pretty, process, time ]
        , notM windowsHost ? append [unix]
        , windowsHost ? append [win32] ]
    mconcat
        [ append [ "-package " ++ pkgNameString pkg | pkg <- cabalDeps ]
        , removePair "-optP-include" $ "-optP" ++ cabalMacros
        , arg "--make"
        , arg "-j"
        , arg "-DBOOTSTRAPPING"
        , arg "-DMIN_VERSION_binary_0_8_0"
        , arg "-DGENERICS"
        , arg "-optP-include"
        , arg $ "-optP" ++ cabalMacrosBoot
        , arg "-ilibraries/Cabal/Cabal"
        , arg "-ilibraries/binary/src"
        , arg "-ilibraries/filepath"
        , arg "-ilibraries/hpc" ]
