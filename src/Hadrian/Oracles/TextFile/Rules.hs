-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.TextFile.Rules
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines Shake rules corresponing to the /text file oracle/; see
-- the module "Hadrian.Oracles.TextFile" for various supported queries.
-----------------------------------------------------------------------------
module Hadrian.Oracles.TextFile.Rules (textFileOracle) where

import Control.Monad
import qualified Data.HashMap.Strict as Map
import Development.Shake
import Development.Shake.Config

import Context.Type
import Hadrian.Haskell.Cabal.Parse
import Hadrian.Oracles.TextFile.Type
import Hadrian.Package
import Hadrian.Utilities
import Stage

-- | This oracle reads and parses text files to answer various queries, caching
-- and tracking the results.
textFileOracle :: Rules ()
textFileOracle = do
    text <- newCache $ \file -> do
        need [file]
        putLoud $ "| TextFile oracle: reading " ++ quote file ++ "..."
        liftIO $ readFile file
    void $ addOracle $ \(TextFile file) -> text file

    kv <- newCache $ \file -> do
        need [file]
        putLoud $ "| KeyValue oracle: reading " ++ quote file ++ "..."
        liftIO $ readConfigFile file
    void $ addOracle $ \(KeyValue (file, key)) -> Map.lookup key <$> kv file

    kvs <- newCache $ \file -> do
        need [file]
        putLoud $ "| KeyValues oracle: reading " ++ quote file ++ "..."
        contents <- map words <$> readFileLines file
        return $ Map.fromList [ (key, values) | (key:values) <- contents ]
    void $ addOracle $ \(KeyValues (file, key)) -> Map.lookup key <$> kvs file

    cabal <- newCache $ \(ctx@Context {..}) ->
        case pkgCabalFile package of
            Just file -> do
                need [file]
                putLoud $ "| CabalFile oracle: reading " ++ quote file
                       ++ " (Stage: " ++ stageString stage ++ ")..."
                Just <$> parseCabalFile ctx
            Nothing -> return Nothing
    void $ addOracle $ \(CabalFile ctx) -> cabal ctx

    confCabal <- newCache $ \(ctx@Context {..}) ->
        case pkgCabalFile package of
            Just file -> do
                need [file]
                putLoud $ "| PackageDataFile oracle: reading " ++ quote file
                       ++ " (Stage: " ++ stageString stage ++ ")..."
                Just <$> parsePackageData ctx
            Nothing -> return Nothing
    void $ addOracle $ \(PackageDataFile ctx) -> confCabal ctx
