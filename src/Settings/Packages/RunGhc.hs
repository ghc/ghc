module Settings.Packages.RunGhc (runGhcPackageArgs) where

import GHC
import Oracles.Config.Setting
import Predicates

runGhcPackageArgs :: Args
runGhcPackageArgs = package runGhc ? do
    version <- getSetting ProjectVersion
    mconcat [ builder Ghc ?
              input "//Main.hs" ?
              append ["-cpp", "-DVERSION=\"" ++ version ++ "\""] ]
