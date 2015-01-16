module Oracles (
    module Oracles.Base,
    module Oracles.Flag,
    module Oracles.Option,
    module Oracles.Builder,
    module Oracles.PackageData,
    oracleRules
    ) where

import Development.Shake.Config
import qualified Data.HashMap.Strict as M
import Base
import Util
import Config
import Oracles.Base
import Oracles.Flag
import Oracles.Option
import Oracles.Builder
import Oracles.PackageData

defaultConfig, userConfig :: FilePath
defaultConfig = cfgPath </> "default.config"
userConfig    = cfgPath </> "user.config"

-- Oracle for configuration files.
configOracle :: Rules ()
configOracle = do
    cfg <- newCache $ \() -> do
        unless (doesFileExist $ defaultConfig <.> "in") $ do
            error $ "\nDefault configuration file '"
                ++ (defaultConfig <.> "in")
                ++ "' is missing; unwilling to proceed."
            return ()
        need [defaultConfig]
        putNormal $ "Parsing " ++ toStandard defaultConfig ++ "..."
        cfgDefault <- liftIO $ readConfigFile defaultConfig
        existsUser <- doesFileExist userConfig
        cfgUser    <- if existsUser
                      then do
                          putNormal $ "Parsing "
                                    ++ toStandard userConfig ++ "..."
                          liftIO $ readConfigFile userConfig
                      else do
                          putColoured Dull Red $
                              "\nUser defined configuration file '"
                              ++ userConfig ++ "' is missing; "
                              ++ "proceeding with default configuration.\n"
                          return M.empty
        putColoured Vivid Green $ "Finished processing configuration files."
        return $ cfgUser `M.union` cfgDefault
    addOracle $ \(ConfigKey key) -> M.lookup key <$> cfg ()
    return ()

-- Oracle for 'package-data.mk' files.
packageDataOracle :: Rules ()
packageDataOracle = do
    pkgData <- newCache $ \file -> do
        need [file]
        putNormal $ "Parsing " ++ toStandard file ++ "..."
        liftIO $ readConfigFile file
    addOracle $ \(PackageDataKey (file, key)) -> M.lookup key <$> pkgData file
    return ()

oracleRules :: Rules ()
oracleRules = configOracle <> packageDataOracle
