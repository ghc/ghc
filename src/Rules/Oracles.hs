module Rules.Oracles (oracleRules) where

import qualified Hadrian.Oracles.ArgsHash
import qualified Hadrian.Oracles.DirectoryContents

import Base
import qualified Oracles.Config
import qualified Oracles.Dependencies
import qualified Oracles.ModuleFiles
import qualified Oracles.PackageData
import qualified Oracles.Path
import Target
import Settings

oracleRules :: Rules ()
oracleRules = do
    Hadrian.Oracles.ArgsHash.argsHashOracle trackArgument getArgs
    Hadrian.Oracles.DirectoryContents.directoryContentsOracle
    Oracles.Config.configOracle
    Oracles.Dependencies.dependenciesOracles
    Oracles.ModuleFiles.moduleFilesOracle
    Oracles.PackageData.packageDataOracle
    Oracles.Path.pathOracle
