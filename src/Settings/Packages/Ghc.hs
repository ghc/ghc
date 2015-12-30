module Settings.Packages.Ghc (ghcPackageArgs) where

import Base
import Expression
import GHC (ghc, compiler)
import Predicates (builderGhc, package)

ghcPackageArgs :: Args
ghcPackageArgs = package ghc ? do
    stage <- getStage
    mconcat [ builderGhc ?
              mconcat [ arg ("-I" ++ pkgPath compiler -/- stageString stage)
                      , arg "-no-hs-main" ] ]
