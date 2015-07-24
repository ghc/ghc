{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.Base (
    module Development.Shake,
    module Development.Shake.Util,
    module Development.Shake.Config,
    module Development.Shake.Classes,
    askConfigWithDefault, askConfig, configOracle,
    configPath,
    putOracle
    ) where

import Util
import Control.Applicative
import Control.Monad.Extra
import Development.Shake
import Development.Shake.Util
import Development.Shake.Config
import Development.Shake.Classes
import qualified Data.HashMap.Strict as Map

configPath :: FilePath
configPath = "shake" -/- "cfg"

newtype ConfigKey = ConfigKey String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askConfigWithDefault :: String -> Action String -> Action String
askConfigWithDefault key defaultAction = do
    maybeValue <- askOracle $ ConfigKey key
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

askConfig :: String -> Action String
askConfig key = askConfigWithDefault key . redError
    $ "Cannot find key '" ++ key ++ "' in configuration files."

-- Oracle for configuration files
configOracle :: Rules ()
configOracle = do
    let configFile = configPath -/- "system.config"
    cfg <- newCache $ \() -> do
        unlessM (doesFileExist $ configFile <.> "in") $
            redError_ $ "\nConfiguration file '" ++ (configFile <.> "in")
                      ++ "' is missing; unwilling to proceed."
        need [configFile]
        putOracle $ "Reading " ++ configFile ++ "..."
        liftIO $ readConfigFile configFile
    addOracle $ \(ConfigKey key) -> Map.lookup key <$> cfg ()
    return ()

-- Make oracle's output more distinguishable
putOracle :: String -> Action ()
putOracle = putColoured Blue
