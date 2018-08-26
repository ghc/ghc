-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.TextFile
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Read and parse text files, tracking their contents. This oracle can be used
-- to read configuration or package metadata files and cache the parsing.
-- This module exports various oracle queries, whereas the corresponing Shake
-- rules can be found in "Hadrian.Oracles.TextFile.Rules".
-----------------------------------------------------------------------------
module Hadrian.Oracles.TextFile (
    readTextFile, lookupValue, lookupValueOrEmpty, lookupValueOrError,
    lookupValues, lookupValuesOrEmpty, lookupValuesOrError, lookupDependencies,
    readCabalData, unsafeReadCabalData, readPackageData
    ) where

import Data.Maybe
import Development.Shake
import GHC.Stack

import Context.Type
import Hadrian.Haskell.Cabal.CabalData
import Hadrian.Haskell.Cabal.PackageData
import Hadrian.Oracles.TextFile.Type
import Hadrian.Utilities

-- | Read a text file, caching and tracking the result. To read and track
-- individual lines of a text file use 'lookupValue' and its derivatives.
readTextFile :: FilePath -> Action String
readTextFile = askOracle . TextFile

-- | Lookup a value in a text file, tracking the result. Each line of the file
-- is expected to have @key = value@ format.
lookupValue :: FilePath -> String -> Action (Maybe String)
lookupValue file key = askOracle $ KeyValue (file, key)

-- | Like 'lookupValue' but returns the empty string if the key is not found.
lookupValueOrEmpty :: FilePath -> String -> Action String
lookupValueOrEmpty file key = fromMaybe "" <$> lookupValue file key

-- | Like 'lookupValue' but raises an error if the key is not found.
lookupValueOrError :: FilePath -> String -> Action String
lookupValueOrError file key = fromMaybe (error msg) <$> lookupValue file key
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
lookupValuesOrError file key = fromMaybe (error msg) <$> lookupValues file key
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

-- | Read and parse a @.cabal@ file, caching and tracking the result.
readCabalData :: Context -> Action (Maybe CabalData)
readCabalData = askOracle . CabalFile

-- | Like 'readCabalData' but raises an error on a non-Cabal context.
unsafeReadCabalData :: HasCallStack => Context -> Action CabalData
unsafeReadCabalData context = fromMaybe (error msg) <$> readCabalData context
  where
    msg = "[unsafeReadCabalData] Non-Cabal context: " ++ show context

-- | Read and parse a @.cabal@ file recording the obtained 'PackageData',
-- caching and tracking the result. Note that unlike 'readCabalData' this
-- function resolves all Cabal configuration flags and associated conditionals.
readPackageData :: Context -> Action (Maybe PackageData)
readPackageData = askOracle . PackageDataFile
