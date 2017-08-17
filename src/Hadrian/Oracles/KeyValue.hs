{-# LANGUAGE TypeFamilies #-}
module Hadrian.Oracles.KeyValue (
    lookupValue, lookupValueOrEmpty, lookupValueOrError, lookupValues,
    lookupValuesOrEmpty, lookupValuesOrError, lookupDependencies, keyValueOracle
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
type instance RuleResult KeyValue = Maybe String

newtype KeyValues = KeyValues (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult KeyValues = Maybe [String]

-- | Lookup a value in a text file, tracking the result. Each line of the file
-- is expected to have @key = value@ format.
lookupValue :: FilePath -> String -> Action (Maybe String)
lookupValue file key = askOracle $ KeyValue (file, key)

-- | Like 'lookupValue' but returns the empty string if the key is not found.
lookupValueOrEmpty :: FilePath -> String -> Action String
lookupValueOrEmpty file key = fromMaybe "" <$> lookupValue file key

-- | Like 'lookupValue' but raises an error if the key is not found.
lookupValueOrError :: FilePath -> String -> Action String
lookupValueOrError file key = (fromMaybe $ error msg) <$> lookupValue file key
  where
    msg = "Key " ++ quote key ++ " not found in file " ++ quote file

-- | Lookup a list of values in a text file, tracking the result. Each line of
-- the file is expected to have @key value1 value2 ...@ format.
lookupValues :: FilePath -> String -> Action (Maybe [String])
lookupValues file key = askOracle $ KeyValues (file, key)

-- | Like 'lookupValues' but returns the empty list if the key is not found.
lookupValuesOrEmpty :: FilePath -> String -> Action [String]
lookupValuesOrEmpty file key = fromMaybe [] <$> lookupValues file key

-- | Like 'lookupValues' but raises an error if the key is not found.
lookupValuesOrError :: FilePath -> String -> Action [String]
lookupValuesOrError file key = (fromMaybe $ error msg) <$> lookupValues file key
  where
    msg = "Key " ++ quote key ++ " not found in file " ++ quote file

-- | The 'Action' @lookupDependencies depFile file@ looks up dependencies of a
-- @file@ in a (typically generated) dependency file @depFile@. The action
-- returns a pair @(source, files)@, such that the @file@ can be produced by
-- compiling @source@, which in turn also depends on a number of other @files@.
lookupDependencies :: FilePath -> FilePath -> Action (FilePath, [FilePath])
lookupDependencies depFile file = do
    deps <- lookupValues depFile file
    case deps of
        Nothing -> error $ "No dependencies found for file " ++ quote file
        Just [] -> error $ "No source file found for file " ++ quote file
        Just (source : files) -> return (source, files)

-- | This oracle reads and parses text files to answer 'lookupValue' and
-- 'lookupValues' queries, as well as their derivatives, tracking the results.
keyValueOracle :: Rules ()
keyValueOracle = void $ do
    kv <- newCache $ \file -> do
        need [file]
        putLoud $ "Reading " ++ file ++ "..."
        liftIO $ readConfigFile file
    kvs <- newCache $ \file -> do
        need [file]
        putLoud $ "Reading " ++ file ++ "..."
        contents <- map words <$> readFileLines file
        return $ Map.fromList [ (key, values) | (key:values) <- contents ]
    void $ addOracle $ \(KeyValue  (file, key)) -> Map.lookup key <$> kv  file
    void $ addOracle $ \(KeyValues (file, key)) -> Map.lookup key <$> kvs file
