module Settings.Packages.Ghc (ghcPackageArgs) where

import GHC
import Oracles.Config.Setting
import Predicate
import Settings.Paths

ghcPackageArgs :: Args
ghcPackageArgs = package ghc ? do
    stage <- getStage
    mconcat [ builder Ghc ? mconcat
              [ arg $ "-I" ++ buildPath (vanillaContext stage compiler)
              , arg "-no-hs-main" ]

            , builder GhcCabal ?
              ghcWithInterpreter ? notStage0 ? arg "--flags=ghci" ]
