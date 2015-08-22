module Rules.Oracles (oracleRules) where

import Base
import Oracles
import Oracles.ArgsHash

oracleRules :: Rules ()
oracleRules = do
    configOracle       -- see Oracles.Config
    packageDataOracle  -- see Oracles.PackageData
    packageDepsOracle  -- see Oracles.PackageDeps
    dependenciesOracle -- see Oracles.Dependencies
    argsHashOracle     -- see Oracles.ArgsHash
    windowsRootOracle  -- see Oracles.WindowsRoot
