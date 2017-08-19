-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Basic functionality for extracting Haskell package metadata stored in
-- @.cabal@ files.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal (pkgNameVersion, pkgDependencies) where

import Data.List
import Development.Shake

import Hadrian.Haskell.Cabal.Parse
import Hadrian.Haskell.Package
import Hadrian.Oracles.TextFile

-- | Read the @.cabal@ file of a given package and return the package name and
-- version. The @.cabal@ file is tracked.
pkgNameVersion :: Package -> Action (PackageName, String)
pkgNameVersion pkg = do
    cabal <- readCabalFile (pkgCabalFile pkg)
    return (name cabal, version cabal)

-- | Read the @.cabal@ file of a given package and return the sorted list of its
-- dependencies. The current version does not take care of Cabal conditionals
-- and therefore returns a crude overapproximation of actual dependencies. The
-- @.cabal@ file is tracked.
pkgDependencies :: Package -> Action [PackageName]
pkgDependencies pkg = do
    cabal <- readCabalFile (pkgCabalFile pkg)
    return (dependencies cabal \\ [pkgName pkg])
