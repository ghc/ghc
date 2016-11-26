module Settings.Packages.Touchy (touchyPackageArgs) where

import Base
import GHC
import Predicate
import Settings

touchyPackageArgs :: Args
touchyPackageArgs = package touchy ? do
    path <- getBuildPath
    let cabalMacros = path -/- "build/autogen/cabal_macros.h"
    builder Ghc ? mconcat [ arg "-no-hs-main"
                          , remove ["-hide-all-packages"]
                          , removePair "-optP-include" $ "-optP" ++ cabalMacros ]
