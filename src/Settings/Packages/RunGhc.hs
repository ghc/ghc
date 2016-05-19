module Settings.Packages.RunGhc (runGhcPackageArgs) where

import GHC
import Oracles.Config.Setting
import Predicate

runGhcPackageArgs :: Args
runGhcPackageArgs = package runGhc ? do
    version <- getSetting ProjectVersion
    builder Ghc ? input "//Main.hs" ?
        append ["-cpp", "-DVERSION=" ++ show version]
