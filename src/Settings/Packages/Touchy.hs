module Settings.Packages.Touchy (touchyPackageArgs) where

import Base
import Expression
import GHC
import Predicates
import Settings

touchyPackageArgs :: Args
touchyPackageArgs = package touchy ? do
    path <- getBuildPath
    let cabalMacros = path -/- "autogen/cabal_macros.h"
    mconcat [ builder Ghc ?
              mconcat [ arg "-no-hs-main"
                      , remove ["-hide-all-packages"]
                      , removePair "-optP-include" $ "-optP" ++ cabalMacros ] ]
