module Rules.Oracles (oracleRules) where

import Base
import Oracles.ArgsHash
import Oracles.Base
import Oracles.Dependencies
import Oracles.PackageData
import Oracles.PackageDeps
import Oracles.WindowsRoot
import Settings.User

oracleRules :: Rules ()
oracleRules = do
    configOracle                         -- see Oracles.Base
    packageDataOracle                    -- see Oracles.PackageData
    packageDepsOracle                    -- see Oracles.PackageDeps
    dependenciesOracle                   -- see Oracles.Dependencies
    when trackBuildSystem argsHashOracle -- see Oracles.ArgsHash
    windowsRootOracle                    -- see Oracles.WindowsRoot
