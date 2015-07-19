module Rules.Oracles (
    oracleRules
    ) where

import Oracles
import Oracles.ArgsHash
import Data.Monoid
import Development.Shake

oracleRules :: Rules ()
oracleRules =
    configOracle <> packageDataOracle <> dependencyOracle <> argsHashOracle
