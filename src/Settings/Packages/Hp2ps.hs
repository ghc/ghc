module Settings.Packages.Hp2ps (hp2psPackageArgs) where

import Base
import Expression
import GHC (hp2ps)
import Predicates (builder, package)
import Settings

hp2psPackageArgs :: Args
hp2psPackageArgs = package hp2ps ? do
    path <- getBuildPath
    let cabalMacros = path -/- "build/autogen/cabal_macros.h"
    mconcat [ builder Ghc ?
              mconcat [ arg "-no-hs-main"
                      , remove ["-hide-all-packages"]
                      , removePair "-optP-include" $ "-optP" ++ cabalMacros ] ]
