module Settings.Packages.RunGhc (runGhcPackageArgs) where

import Expression
import GHC (runGhc)
import Oracles.Config.Setting
import Predicates (builderGhc, file, package)

runGhcPackageArgs :: Args
runGhcPackageArgs = package runGhc ? do
    version <- getSetting ProjectVersion
    mconcat [ builderGhc ?
              file "//Main.*" ?
              append ["-cpp", "-DVERSION=\"" ++ version ++ "\""] ]
