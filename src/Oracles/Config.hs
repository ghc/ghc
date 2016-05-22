{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.Config (askConfig, unsafeAskConfig, configOracle) where

import qualified Data.HashMap.Strict as Map
import Development.Shake.Config

import Base

newtype ConfigKey = ConfigKey String
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

unsafeAskConfig :: String -> Action String
unsafeAskConfig key = (fromMaybe $ error msg) <$> askConfig key
  where
    msg = "Key " ++ quote key ++ " not found in configuration files."

askConfig :: String -> Action (Maybe String)
askConfig = askOracle . ConfigKey

-- Oracle for configuration files
configOracle :: Rules ()
configOracle = void $ do
    cfg <- newCache $ \() -> do
        need [configFile]
        putLoud $ "Reading " ++ configFile ++ "..."
        liftIO $ readConfigFile configFile
    addOracle $ \(ConfigKey key) -> Map.lookup key <$> cfg ()
