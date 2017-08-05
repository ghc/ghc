module Rules.Oracles (oracleRules) where

import qualified Hadrian.Oracles.ArgsHash

import Base
import qualified Oracles.Config
import qualified Oracles.Dependencies
import qualified Oracles.DirectoryContents
import qualified Oracles.ModuleFiles
import qualified Oracles.PackageData
import qualified Oracles.Path
import Target
import Settings

oracleRules :: Rules ()
oracleRules = do
    Hadrian.Oracles.ArgsHash.argsHashOracle trackArgument getArgs
    Oracles.Config.configOracle
    Oracles.Dependencies.dependenciesOracles
    Oracles.DirectoryContents.directoryContentsOracle
    Oracles.ModuleFiles.moduleFilesOracle
    Oracles.PackageData.packageDataOracle
    Oracles.Path.pathOracle
