-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Package
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- A /package/ is a collection of files. We currently only support C and Haskell
-- packages and treat a package as either a library or a program. The latter is
-- a gross oversimplification as, for example, Haskell packages can be both.
-- This works for now, but should be improved in future.
-----------------------------------------------------------------------------
module Hadrian.Package (
    -- * Data types
    Package (..), PackageName, PackageLanguage, PackageType,

    -- * Construction and properties
    cLibrary, cProgram, hsLibrary, hsProgram, dummyPackage,
    isLibrary, isProgram, isCPackage, isHsPackage,

    -- * Package directory structure
    pkgCabalFile, unsafePkgCabalFile
    ) where

import Data.Maybe
import Development.Shake.FilePath
import GHC.Stack
import Hadrian.Package.Type
import Hadrian.Utilities

-- | Construct a C library package.
cLibrary :: PackageName -> FilePath -> Package
cLibrary = Package C Library

-- | Construct a C program package.
cProgram :: PackageName -> FilePath -> Package
cProgram = Package C Program

-- | Construct a Haskell library package.
hsLibrary :: PackageName -> FilePath -> Package
hsLibrary = Package Haskell Library

-- | Construct a Haskell program package.
hsProgram :: PackageName -> FilePath -> Package
hsProgram = Package Haskell Program

-- | A dummy package, which we never try to build
--   but just use as a better @undefined@ in code
--   where we need a 'Package' to set up a Context
--   but will not really operate over one.
dummyPackage :: Package
dummyPackage = hsLibrary "dummy" "dummy/path/"

-- | Is this a library package?
isLibrary :: Package -> Bool
isLibrary (Package _ Library _ _) = True
isLibrary _ = False

-- | Is this a program package?
isProgram :: Package -> Bool
isProgram (Package _ Program _ _) = True
isProgram _ = False

-- | Is this a C package?
isCPackage :: Package -> Bool
isCPackage (Package C _ _ _) = True
isCPackage _ = False

-- | Is this a Haskell package?
isHsPackage :: Package -> Bool
isHsPackage (Package Haskell _ _ _) = True
-- we consider the RTS as a haskell package because we
-- use information from its Cabal file to build it,
-- and we e.g want 'pkgCabalFile' to point us to
-- 'rts/rts.cabal' when passed the rts package as argument.
isHsPackage (Package _ _ "rts" _)   = True
isHsPackage _ = False

-- | The path to the Cabal file of a Haskell package, e.g. @ghc/ghc-bin.cabal@,
-- or @Nothing@ if the argument is not a Haskell package.
pkgCabalFile :: Package -> Maybe FilePath
pkgCabalFile p | isHsPackage p = Just $ pkgPath p -/- pkgName p <.> "cabal"
               | otherwise     = Nothing

-- | Like 'pkgCabalFile' but raises an error on a non-Haskell package.
unsafePkgCabalFile :: HasCallStack => Package -> FilePath
unsafePkgCabalFile p = fromMaybe (error msg) (pkgCabalFile p)
  where
    msg = "[unsafePkgCabalFile] Not a Haskell package: " ++ show p
