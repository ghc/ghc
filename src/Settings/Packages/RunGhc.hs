module Settings.Packages.RunGhc (runGhcPackageArgs) where

import GHC
import Oracles.Config.Setting
import Predicate

runGhcPackageArgs :: Args
runGhcPackageArgs = package runGhc ? builder Ghc ? input "//Main.hs" ? do
    version <- getSetting ProjectVersion
    append ["-cpp", "-DVERSION=" ++ show version]
