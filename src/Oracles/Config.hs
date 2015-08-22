{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Oracles.Config (askConfig, askConfigWithDefault, configOracle) where

import Base
import qualified Data.HashMap.Strict as Map

newtype ConfigKey = ConfigKey String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askConfig :: String -> Action String
askConfig key = askConfigWithDefault key . putError
    $ "Cannot find key '" ++ key ++ "' in configuration files."

askConfigWithDefault :: String -> Action String -> Action String
askConfigWithDefault key defaultAction = do
    maybeValue <- askOracle $ ConfigKey key
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

-- Oracle for configuration files
configOracle :: Rules ()
configOracle = do
    let configFile = configPath -/- "system.config"
    cfg <- newCache $ \() -> do
        unlessM (doesFileExist $ configFile <.> "in") $
            putError $ "\nConfiguration file '" ++ (configFile <.> "in")
                     ++ "' is missing; unwilling to proceed."
        need [configFile]
        putOracle $ "Reading " ++ configFile ++ "..."
        liftIO $ readConfigFile configFile
    _ <- addOracle $ \(ConfigKey key) -> Map.lookup key <$> cfg ()
    return ()
