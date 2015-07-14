module Rules.Oracles (
    oracleRules
    ) where

import Base
import Oracles
import Oracles.ArgsHash

oracleRules :: Rules ()
oracleRules =
    configOracle <> packageDataOracle <> dependencyOracle <> argsHashOracle
