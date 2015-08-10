module Rules.Oracles (
    oracleRules
    ) where

import Base
import Oracles.Base
import Oracles.ArgsHash
import Oracles.PackageData
import Oracles.WindowsRoot
import Oracles.PackageDeps
import Oracles.Dependencies

oracleRules :: Rules ()
oracleRules = do
    configOracle       -- see Oracles.Base
    packageDataOracle  -- see Oracles.PackageData
    packageDepsOracle  -- see Oracles.PackageDeps
    dependenciesOracle -- see Oracles.Dependencies
    argsHashOracle     -- see Oracles.ArgsHash
    windowsRootOracle  -- see Oracles.WindowsRoot
