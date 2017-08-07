{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hadrian.Oracles.Config (askConfig, unsafeAskConfig, configOracle) where

import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config

import Hadrian.Utilities

newtype ConfigKey = ConfigKey String
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | Lookup a configuration setting raising an error if the key is not found.
unsafeAskConfig :: String -> Action String
unsafeAskConfig key = (fromMaybe $ error msg) <$> askConfig key
  where
    msg = "Key " ++ quote key ++ " not found in configuration files."

-- | Lookup a configuration setting.
askConfig :: String -> Action (Maybe String)
askConfig = askOracle . ConfigKey

-- | This oracle reads and parses a configuration file consisting of key-value
-- pairs @key = value@ and answers 'askConfig' queries tracking the results.
configOracle :: FilePath -> Rules ()
configOracle configFile = void $ do
    cfg <- newCache $ \() -> do
        need [configFile]
        putLoud $ "Reading " ++ configFile ++ "..."
        liftIO $ readConfigFile configFile
    addOracle $ \(ConfigKey key) -> Map.lookup key <$> cfg ()
