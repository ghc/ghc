module Rules.Oracles (oracleRules) where

import qualified Hadrian.Oracles.ArgsHash
import qualified Hadrian.Oracles.DirectoryContents
import qualified Hadrian.Oracles.Path

import Base
import qualified Oracles.Config
import qualified Oracles.Dependencies
import qualified Oracles.ModuleFiles
import qualified Oracles.PackageData
import Target
import Settings

oracleRules :: Rules ()
oracleRules = do
    Hadrian.Oracles.ArgsHash.argsHashOracle trackArgument getArgs
    Hadrian.Oracles.DirectoryContents.directoryContentsOracle
    Hadrian.Oracles.Path.pathOracle
    Oracles.Config.configOracle
    Oracles.Dependencies.dependenciesOracles
    Oracles.ModuleFiles.moduleFilesOracle
    Oracles.PackageData.packageDataOracle
