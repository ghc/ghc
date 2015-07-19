module Rules.Oracles (
    oracleRules
    ) where

import Oracles.Base
import Oracles.ArgsHash
import Oracles.PackageData
import Oracles.DependencyList
import Data.Monoid

oracleRules :: Rules ()
oracleRules =
    configOracle <> packageDataOracle <> dependencyListOracle <> argsHashOracle
