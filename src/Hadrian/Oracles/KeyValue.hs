{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hadrian.Oracles.KeyValue (
    lookupValue, lookupValueOrEmpty, lookupValueOrError, keyValueOracle
    ) where

import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Development.Shake
import Development.Shake.Classes
import Development.Shake.Config

import Hadrian.Utilities

newtype KeyValue = KeyValue (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | Lookup a value in a key-value text file, tracking the result.
lookupValue :: FilePath -> String -> Action (Maybe String)
lookupValue file key = askOracle $ KeyValue (file, key)

-- | Lookup a value in a key-value text file, tracking the result. Return the
-- empty string if the key is not found.
lookupValueOrEmpty :: FilePath -> String -> Action String
lookupValueOrEmpty file key = fromMaybe "" <$> askOracle (KeyValue (file, key))

-- | Lookup a value in a key-value text file, tracking the result. Raise an
-- error if the key is not found.
lookupValueOrError :: FilePath -> String -> Action String
lookupValueOrError file key = (fromMaybe $ error msg) <$> lookupValue file key
  where
    msg = "Key " ++ quote key ++ " not found in file " ++ quote file

-- | This oracle reads and parses text files consisting of key-value pairs
-- @key = value@ and answers 'lookupValue' queries tracking the results.
keyValueOracle :: Rules ()
keyValueOracle = void $ do
    cache <- newCache $ \file -> do
        need [file]
        putLoud $ "Reading " ++ file ++ "..."
        liftIO $ readConfigFile file
    addOracle $ \(KeyValue (file, key)) -> Map.lookup key <$> cache file
