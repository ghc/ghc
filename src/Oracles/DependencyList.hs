{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Oracles.DependencyList (
    DependencyList (..),
    DependencyListKey (..)
    ) where

import Development.Shake.Classes
import Base
import Data.Maybe

data DependencyList = DependencyList FilePath FilePath

newtype DependencyListKey = DependencyListKey (FilePath, FilePath)
                        deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

instance ShowArgs DependencyList where
    showArgs (DependencyList file obj) = do
        res <- askOracle $ DependencyListKey (file, obj)
        return $ fromMaybe [] res
