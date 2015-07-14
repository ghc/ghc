module Rules.Oracles (
    oracleRules
    ) where

import Base hiding (arg, args, Args)
import Oracles
import Oracles.ArgsHash

oracleRules :: Rules ()
oracleRules =
    configOracle <> packageDataOracle <> dependencyOracle <> argsHashOracle
