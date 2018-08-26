{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.TextFile.Type
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines the types of keys used by the /text file oracle/. See the
-- module "Hadrian.Oracles.TextFile" for various supported queries, and the
-- module "Hadrian.Oracles.TextFile.Rules" for the corresponing Shake rules.
-----------------------------------------------------------------------------
module Hadrian.Oracles.TextFile.Type where

import Development.Shake
import Development.Shake.Classes

import Context.Type
import Hadrian.Haskell.Cabal.CabalData
import Hadrian.Haskell.Cabal.PackageData

newtype TextFile = TextFile FilePath
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult TextFile = String

newtype CabalFile = CabalFile Context
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult CabalFile = Maybe CabalData

newtype PackageDataFile = PackageDataFile Context
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult PackageDataFile = Maybe PackageData

newtype KeyValue = KeyValue (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult KeyValue = Maybe String

newtype KeyValues = KeyValues (FilePath, String)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
type instance RuleResult KeyValues = Maybe [String]
