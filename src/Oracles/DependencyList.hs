{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.DependencyList (
    dependencyList,
    dependencyListOracle
    ) where

import Base
import Util
import Data.List
import Data.Maybe
import Data.Function
import qualified Data.HashMap.Strict as Map
import Control.Applicative

newtype DependencyListKey = DependencyListKey (FilePath, FilePath)
    deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- dependencyList depFile objFile is an action that looks up dependencies of an
-- object file (objFile) in a generated dependecy file (depFile).
dependencyList :: FilePath -> FilePath -> Action [FilePath]
dependencyList depFile objFile = do
    res <- askOracle $ DependencyListKey (depFile, objFile)
    return . fromMaybe [] $ res

-- Oracle for 'path/dist/*.deps' files
dependencyListOracle :: Rules ()
dependencyListOracle = do
    deps <- newCache $ \file -> do
        need [file]
        putOracle $ "Reading dependencies from " ++ file ++ "..."
        contents <- parseMakefile <$> (liftIO $ readFile file)
        return . Map.fromList
               . map (bimap unifyPath (map unifyPath))
               . map (bimap head concat . unzip)
               . groupBy ((==) `on` fst)
               . sortBy (compare `on` fst) $ contents
    addOracle $ \(DependencyListKey (file, obj)) ->
        Map.lookup (unifyPath obj) <$> deps (unifyPath file)
    return ()
