module Rules.Oracles (oracleRules) where

import Base
import qualified Oracles.ArgsHash
import qualified Oracles.Config
import qualified Oracles.Dependencies
import qualified Oracles.DirectoryContents
import qualified Oracles.ModuleFiles
import qualified Oracles.PackageData
import qualified Oracles.Path

oracleRules :: Rules ()
oracleRules = do
    Oracles.ArgsHash.argsHashOracle
    Oracles.Config.configOracle
    Oracles.Dependencies.dependenciesOracles
    Oracles.DirectoryContents.directoryContentsOracle
    Oracles.ModuleFiles.moduleFilesOracle
    Oracles.PackageData.packageDataOracle
    Oracles.Path.pathOracle
