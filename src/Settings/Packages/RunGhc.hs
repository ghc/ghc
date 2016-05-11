module Settings.Packages.RunGhc (runGhcPackageArgs) where

import Expression
import GHC (runGhc)
import Oracles.Config.Setting
import Predicates (builder, input, package)

runGhcPackageArgs :: Args
runGhcPackageArgs = package runGhc ? do
    version <- getSetting ProjectVersion
    mconcat [ builder Ghc ?
              input "//Main.hs" ?
              append ["-cpp", "-DVERSION=\"" ++ version ++ "\""] ]
