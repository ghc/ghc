{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.DependencyList (
    DependencyList (..),
    DependencyListKey (..),
    dependencyList
    ) where

import Data.Maybe
import Development.Shake
import Development.Shake.Classes

data DependencyList = DependencyList FilePath FilePath

newtype DependencyListKey = DependencyListKey (FilePath, FilePath)
                        deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

dependencyList :: DependencyList -> Action [FilePath]
dependencyList (DependencyList file obj) = do
        res <- askOracle $ DependencyListKey (file, obj)
        return $ fromMaybe [] res
