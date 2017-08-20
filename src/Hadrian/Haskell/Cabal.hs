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
module Hadrian.Haskell.Cabal (
    pkgVersion, pkgIdentifier, pkgDependencies
    ) where

import Control.Monad
import Development.Shake

import Hadrian.Haskell.Cabal.Parse
import Hadrian.Haskell.Package
import Hadrian.Oracles.TextFile
import Hadrian.Utilities

-- | Read the @.cabal@ file of a given package and return the package version.
-- The @.cabal@ file is tracked.
pkgVersion :: Package -> Action String
pkgVersion pkg = do
    cabal <- readCabalFile (pkgCabalFile pkg)
    return (version cabal)

-- | Read the @.cabal@ file of a given package and return the package identifier,
-- e.g. @base-4.10.0.0@. If the @.cabal@ file does not exist return just the
-- package name, e.g. @rts@. If the @.cabal@ file exists then it is tracked, and
-- furthermore we check that the recorded package name matches the name of the
-- package passed as the parameter and raise an error otherwise.
pkgIdentifier :: Package -> Action String
pkgIdentifier pkg = do
    cabalExists <- doesFileExist (pkgCabalFile pkg)
    if not cabalExists
    then return (pkgName pkg)
    else do
        cabal <- readCabalFile (pkgCabalFile pkg)
        when (pkgName pkg /= name cabal) $
            error $ "[Hadrian.Haskell.Cabal] Inconsistent package name: expected "
                 ++ quote (pkgName pkg) ++ ", but " ++ quote (pkgCabalFile pkg)
                 ++ " specifies " ++ quote (name cabal) ++ "."
        return $ if (null $ version cabal)
            then pkgName pkg
            else pkgName pkg ++ "-" ++ version cabal

-- | Read the @.cabal@ file of a given package and return the sorted list of its
-- dependencies. The current version does not take care of Cabal conditionals
-- and therefore returns a crude overapproximation of actual dependencies. The
-- @.cabal@ file is tracked.
pkgDependencies :: Package -> Action [PackageName]
pkgDependencies pkg = do
    cabal <- readCabalFile (pkgCabalFile pkg)
    return (dependencies cabal)
