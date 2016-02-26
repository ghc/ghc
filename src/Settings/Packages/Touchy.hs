module Settings.Packages.Touchy (touchyPackageArgs) where

import Base
import Expression
import GHC (touchy)
import Predicates (builderGhc, package)
import Settings

touchyPackageArgs :: Args
touchyPackageArgs = package touchy ? do
    path <- getBuildPath
    let cabalMacros = path -/- "autogen/cabal_macros.h"
    mconcat [ builderGhc ?
              mconcat [ arg "-no-hs-main"
                      , remove ["-hide-all-packages"]
                      , removePair "-optP-include" $ "-optP" ++ cabalMacros ] ]
