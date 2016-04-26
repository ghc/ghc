module Settings.Packages.RunGhc (runGhcPackageArgs) where

import Expression
import GHC (runGhc)
import Oracles.Config.Setting
import Predicates (builder, file, package)

runGhcPackageArgs :: Args
runGhcPackageArgs = package runGhc ? do
    version <- getSetting ProjectVersion
    mconcat [ builder Ghc ?
              file "//Main.*" ?
              append ["-cpp", "-DVERSION=\"" ++ version ++ "\""] ]
