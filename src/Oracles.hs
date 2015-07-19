module Oracles (
    module Oracles.Base,
    configOracle, packageDataOracle, dependencyOracle
    ) where

import Util
import Config
import Oracles.Base
import Oracles.PackageData
import Oracles.DependencyList
import Data.List
import Data.Function
import qualified Data.HashMap.Strict as M
import Control.Applicative
import Control.Monad.Extra
import Development.Shake
import Development.Shake.Util
import Development.Shake.Config
import Development.Shake.FilePath

-- Oracle for configuration files
configOracle :: Rules ()
configOracle = do
    let configFile = cfgPath </> "system.config"
    cfg <- newCache $ \() -> do
        unlessM (doesFileExist $ configFile <.> "in") $
            redError_ $ "\nConfiguration file '" ++ (configFile <.> "in")
                      ++ "' is missing; unwilling to proceed."
        need [configFile]
        putOracle $ "Reading " ++ unifyPath configFile ++ "..."
        liftIO $ readConfigFile configFile
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

bimap :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimap f g (x, y) = (f x, g y)

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
