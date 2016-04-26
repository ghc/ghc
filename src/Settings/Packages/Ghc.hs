module Settings.Packages.Ghc (ghcPackageArgs) where

import Expression
import GHC (ghc, compiler)
import Oracles.Config.Setting
import Predicates (builder, package, notStage0)
import Settings.Paths

ghcPackageArgs :: Args
ghcPackageArgs = package ghc ? do
    stage <- getStage
    mconcat [ builder Ghc ? mconcat
              [ arg $ "-I" ++ buildPath (vanillaContext stage compiler)
              , arg "-no-hs-main" ]

            , builder GhcCabal ?
              ghcWithInterpreter ? notStage0 ? arg "--flags=ghci" ]
