module Rules.Oracles (oracleRules) where

import Base
import qualified Oracles.Config
import qualified Oracles.Dependencies
import qualified Oracles.LookupInPath
import qualified Oracles.PackageData
import qualified Oracles.PackageDeps
import qualified Oracles.WindowsPath
import qualified Oracles.ArgsHash
import qualified Oracles.ModuleFiles
import qualified Oracles.PackageDb

oracleRules :: Rules ()
oracleRules = do
    Oracles.ArgsHash.argsHashOracle
    Oracles.Config.configOracle
    Oracles.Dependencies.dependenciesOracle
    Oracles.LookupInPath.lookupInPathOracle
    Oracles.ModuleFiles.moduleFilesOracle
    Oracles.PackageData.packageDataOracle
    Oracles.PackageDb.packageDbOracle
    Oracles.PackageDeps.packageDepsOracle
    Oracles.WindowsPath.windowsPathOracle
