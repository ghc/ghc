module Settings.Packages.GhcCabal (ghcCabalPackageArgs) where

import Base
import GHC
import Oracles.Config.Setting
import Predicate

ghcCabalPackageArgs :: Args
ghcCabalPackageArgs = stage0 ? package ghcCabal ? builder Ghc ? do
    -- Note: We could compute 'cabalDeps' instead of hard-coding it but this
    -- seems unnecessary since we plan to drop @ghc-cabal@ altogether, #18.
    win <- lift windowsHost
    let cabalDeps = [ array, base, bytestring, containers, deepseq, directory
                    , pretty, process, time, if win then win32 else unix ]
    mconcat
        [ append [ "-package " ++ pkgNameString pkg | pkg <- cabalDeps ]
        , arg "--make"
        , arg "-j"
        , arg "-DBOOTSTRAPPING"
        , arg "-DMIN_VERSION_binary_0_8_0"
        , arg "-DGENERICS"
        , arg "-optP-include"
        , arg $ "-optP" ++ pkgPath ghcCabal -/- "cabal_macros_boot.h"
        , arg "-ilibraries/Cabal/Cabal"
        , arg "-ilibraries/binary/src"
        , arg "-ilibraries/filepath"
        , arg "-ilibraries/hpc" ]
