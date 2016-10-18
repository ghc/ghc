module Rules.Oracles (oracleRules) where

import Base
import qualified Oracles.ArgsHash
import qualified Oracles.Config
import qualified Oracles.Dependencies
import qualified Oracles.DirectoryContent
import qualified Oracles.LookupInPath
import qualified Oracles.ModuleFiles
import qualified Oracles.PackageData
import qualified Oracles.WindowsPath

oracleRules :: Rules ()
oracleRules = do
    Oracles.ArgsHash.argsHashOracle
    Oracles.Config.configOracle
    Oracles.Dependencies.dependenciesOracles
    Oracles.DirectoryContent.directoryContentOracle
    Oracles.LookupInPath.lookupInPathOracle
    Oracles.ModuleFiles.moduleFilesOracle
    Oracles.PackageData.packageDataOracle
    Oracles.WindowsPath.windowsPathOracle
