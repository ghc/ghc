module Oracles (
    module Oracles.Base,
    module Oracles.Flag,
    module Oracles.Option,
    module Oracles.Builder,
    module Oracles.PackageData,
    module Oracles.DependencyList,
    configOracle, packageDataOracle, dependencyOracle
    ) where

import Development.Shake.Config
import Development.Shake.Util
import qualified Data.HashMap.Strict as M
-- TODO: get rid of Bifunctor dependency
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
        putOracle $ "Reading " ++ unifyPath defaultConfig ++ "..."
        cfgDefault <- liftIO $ readConfigFile defaultConfig
        existsUser <- doesFileExist userConfig
        cfgUser    <- if existsUser
                      then do
                          putOracle $ "Reading "
                                    ++ unifyPath userConfig ++ "..."
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
        putOracle $ "Reading " ++ file ++ "..."
        liftIO $ readConfigFile file
    addOracle $ \(PackageDataKey (file, key)) ->
        M.lookup key <$> pkgData (unifyPath file)
    return ()

-- Oracle for 'path/dist/*.deps' files
dependencyOracle :: Rules ()
dependencyOracle = do
    deps <- newCache $ \file -> do
        need [file]
        putOracle $ "Reading " ++ file ++ "..."
        contents <- parseMakefile <$> (liftIO $ readFile file)
        return $ M.fromList
               $ map (bimap unifyPath (map unifyPath))
               $ map (bimap head concat . unzip)
               $ groupBy ((==) `on` fst)
               $ sortBy (compare `on` fst) contents
    addOracle $ \(DependencyListKey (file, obj)) ->
        M.lookup (unifyPath obj) <$> deps (unifyPath file)
    return ()

-- Make oracle's output more distinguishable
putOracle :: String -> Action ()
putOracle = putColoured Blue
