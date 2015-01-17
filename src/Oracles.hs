module Oracles (
    module Oracles.Base,
    module Oracles.Flag,
    module Oracles.Option,
    module Oracles.Builder,
    module Oracles.PackageData,
    module Oracles.DependencyList,
    oracleRules
    ) where

import Development.Shake.Config
import Development.Shake.Util
import qualified Data.HashMap.Strict as M
import Data.Bifunctor
import Base
import Util
import Config
import Oracles.Base
import Oracles.Flag
import Oracles.Option
import Oracles.Builder
import Oracles.PackageData
import Oracles.DependencyList

defaultConfig, userConfig :: FilePath
defaultConfig = cfgPath </> "default.config"
userConfig    = cfgPath </> "user.config"

-- Oracle for configuration files
configOracle :: Rules ()
configOracle = do
    cfg <- newCache $ \() -> do
        unless (doesFileExist $ defaultConfig <.> "in") $
            redError_ $ "\nDefault configuration file '"
                      ++ (defaultConfig <.> "in")
                      ++ "' is missing; unwilling to proceed."
        need [defaultConfig]
        putOracle $ "Parsing " ++ toStandard defaultConfig ++ "..."
        cfgDefault <- liftIO $ readConfigFile defaultConfig
        existsUser <- doesFileExist userConfig
        cfgUser    <- if existsUser
                      then do
                          putOracle $ "Parsing "
                                    ++ toStandard userConfig ++ "..."
                          liftIO $ readConfigFile userConfig
                      else do
                          putColoured Red $
                              "\nUser defined configuration file '"
                              ++ userConfig ++ "' is missing; "
                              ++ "proceeding with default configuration.\n"
                          return M.empty
        putColoured Green $ "Finished processing configuration files."
        return $ cfgUser `M.union` cfgDefault
    addOracle $ \(ConfigKey key) -> M.lookup key <$> cfg ()
    return ()

-- Oracle for 'package-data.mk' files
packageDataOracle :: Rules ()
packageDataOracle = do
    pkgData <- newCache $ \file -> do
        need [file]
        putOracle $ "Parsing " ++ toStandard file ++ "..."
        liftIO $ readConfigFile file
    addOracle $ \(PackageDataKey (file, key)) -> M.lookup key <$> pkgData file
    return ()

-- Oracle for 'path/dist/*.deps' files
dependencyOracle :: Rules ()
dependencyOracle = do
    deps <- newCache $ \depFile -> do
        need [depFile]
        putOracle $ "Parsing " ++ toStandard depFile ++ "..."
        contents <- parseMakefile <$> (liftIO $ readFile depFile)
        return $ M.fromList
               $ map (bimap head concat . unzip)
               $ groupBy ((==) `on` fst)
               $ sortBy (compare `on` fst) contents
    addOracle $ \(DependencyListKey (file, obj)) -> M.lookup obj <$> deps file
    return ()

oracleRules :: Rules ()
oracleRules = configOracle <> packageDataOracle <> dependencyOracle

-- Make oracle's output more distinguishable
putOracle :: String -> Action ()
putOracle = putColoured Blue
