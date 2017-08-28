module Settings.Packages.RunGhc (runGhcPackageArgs) where

import Oracles.Setting
import Expression

runGhcPackageArgs :: Args
runGhcPackageArgs = package runGhc ? builder Ghc ? input "//Main.hs" ? do
    version <- getSetting ProjectVersion
    pure ["-cpp", "-DVERSION=" ++ show version]
