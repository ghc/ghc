{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.Config (askConfig, askConfigWithDefault, configOracle) where

import qualified Data.HashMap.Strict as Map
import Development.Shake.Config

import Base

newtype ConfigKey = ConfigKey String
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

askConfig :: String -> Action String
askConfig key = askConfigWithDefault key . error
    $ "Cannot find key " ++ quote key ++ " in configuration files."

askConfigWithDefault :: String -> Action String -> Action String
askConfigWithDefault key defaultAction = do
    maybeValue <- askOracle $ ConfigKey key
    case maybeValue of
        Just value -> return value
        Nothing    -> defaultAction

-- Oracle for configuration files
configOracle :: Rules ()
configOracle = do
    cfg <- newCache $ \() -> do
        need [configFile]
        putLoud $ "Reading " ++ configFile ++ "..."
        liftIO $ readConfigFile configFile
    _ <- addOracle $ \(ConfigKey key) -> Map.lookup key <$> cfg ()
    return ()
