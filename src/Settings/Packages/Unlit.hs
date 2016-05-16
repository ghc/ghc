module Settings.Packages.Unlit (unlitPackageArgs) where

import Base
import GHC
import Predicates
import Settings

unlitPackageArgs :: Args
unlitPackageArgs = package unlit ? do
    path <- getBuildPath
    let cabalMacros = path -/- "autogen/cabal_macros.h"
    mconcat [ builder Ghc ?
              mconcat [ arg "-no-hs-main"
                      , remove ["-hide-all-packages"]
                      , removePair "-optP-include" $ "-optP" ++ cabalMacros ] ]
