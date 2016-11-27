module Settings.Packages.Ghc (ghcPackageArgs) where

import GHC
import Oracles.Config.Setting
import Predicate
import Settings.Path

ghcPackageArgs :: Args
ghcPackageArgs = package ghc ? do
    stage <- getStage
    mconcat [ builder Ghc ? arg ("-I" ++ buildPath (vanillaContext stage compiler))

            , builder GhcCabal ?
              ghcWithInterpreter ? notStage0 ? arg "--flags=ghci" ]
