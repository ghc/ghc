-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Oracles.Cabal
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- This module defines oracles for reading and parsing Cabal files, as well as
-- for configuring Haskell packages.
-----------------------------------------------------------------------------
module Hadrian.Oracles.Cabal (
    readPackageData, readContextData, configurePackageGHC
    ) where

import Development.Shake
import Distribution.Simple (Compiler)
import Distribution.System (Platform)

import Context.Type
import Hadrian.Haskell.Cabal.Type
import Hadrian.Oracles.Cabal.Type
import Hadrian.Package
import Stage

-- | Read and parse a Cabal file, caching and tracking the result.
readPackageData :: Package -> Action PackageData
readPackageData = askOracle . PackageDataKey

-- | Read and parse a Cabal file recording the obtained 'ContextData', caching
-- and tracking the result. Note that unlike 'readPackageData' this function
-- resolves all Cabal configuration flags and associated conditionals.
readContextData :: Context -> Action ContextData
readContextData = askOracle . ContextDataKey

-- | Configure a 'Package' using the GHC corresponding to a given 'Stage',
-- caching and tracking the result.
configurePackageGHC :: Package -> Stage -> Action (Compiler, Platform)
configurePackageGHC pkg stage = do
    PackageConfiguration res <- askOracle $ PackageConfigurationKey (pkg, stage)
    return res
