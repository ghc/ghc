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
    Package (..), PackageName, PackageType,

    -- * Construction and properties
    library, program, dummyPackage, isLibrary, isProgram,

    -- * Package directory structure
    pkgCabalFile
    ) where

import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics

import Hadrian.Utilities

-- TODO: Make PackageType more precise.
-- See https://github.com/snowleopard/hadrian/issues/12.
data PackageType = Library | Program deriving (Eq, Generic, Ord, Show)

type PackageName = String

-- TODO: Consider turning Package into a GADT indexed with language and type.
data Package = Package {
    -- | The package type. 'Library' and 'Program' packages are supported.
    pkgType :: PackageType,
    -- | The package name. We assume that all packages have different names,
    -- hence two packages with the same name are considered equal.
    pkgName :: PackageName,
    -- | The path to the package source code relative to the root of the build
    -- system. For example, @libraries/Cabal/Cabal@ and @ghc@ are paths to the
    -- @Cabal@ and @ghc-bin@ packages in GHC.
    pkgPath :: FilePath
    } deriving (Eq, Generic, Ord, Show)

-- | Construct a library package.
library :: PackageName -> FilePath -> Package
library = Package Library

-- | Construct a program package.
program :: PackageName -> FilePath -> Package
program = Package Program

-- TODO: Remove this hack.
-- | A dummy package that we never try to build but use when we need a 'Package'
-- to construct a 'Context' but do not need to access the package field.
dummyPackage :: Package
dummyPackage = library "dummy" "dummy/path/"

-- | Is this a library package?
isLibrary :: Package -> Bool
isLibrary (Package Library _ _) = True
isLibrary _ = False

-- | Is this a program package?
isProgram :: Package -> Bool
isProgram (Package Program _ _) = True
isProgram _ = False

-- | The path to the Cabal file of a Haskell package, e.g. @ghc/ghc-bin.cabal@.
pkgCabalFile :: Package -> FilePath
pkgCabalFile p = pkgPath p -/- pkgName p <.> "cabal"

instance Binary   PackageType
instance Hashable PackageType
instance NFData   PackageType

instance Binary   Package
instance Hashable Package
instance NFData   Package