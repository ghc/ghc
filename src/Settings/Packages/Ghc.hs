module Settings.Packages.Ghc (ghcPackageArgs) where

import Base
import Expression
import GHC (ghc, compiler)
import Oracles.Config.Setting
import Predicates (builder, builderGhc, package, notStage0)

ghcPackageArgs :: Args
ghcPackageArgs = package ghc ? do
    stage <- getStage
    mconcat [ builderGhc ? mconcat
              [ arg ("-I" ++ pkgPath compiler -/- stageString stage)
              , arg "-no-hs-main" ]

            , builder GhcCabal ?
              ghcWithInterpreter ? notStage0 ? arg "--flags=ghci" ]
