-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Package
-- Copyright  : (c) Andrey Mokhov 2014-2017
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Haskell packages and operations on them.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Package (
    -- * Data type
    Package, PackageName,

    -- * Construction and properties
    library, program, pkgName, pkgPath, isLibrary, isProgram,

    -- * Package directory structure
    pkgCabalFile
    ) where

import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics
import Hadrian.Utilities

type PackageName = String

-- TODO: Make PackageType more precise, #12.
data PackageType = Library | Program deriving (Generic, Show)

-- | A Haskell package. The current implementation treats a package as either
-- a library or a program, which is a gross oversimplification as Haskell
-- packages can be both. This works for now, but in future we plan to support
-- general Haskell packages. Also note that we assume that all packages have
-- different names, hence two packages with the same name are considered equal.
data Package = Package PackageType PackageName FilePath deriving Generic

-- | The name of a Haskell package. Examples: @Cabal@, @ghc-bin@.
pkgName :: Package -> PackageName
pkgName (Package _ name _) = name

-- | The path to the package source code relative to the root of the build
-- system. For example, @libraries/Cabal/Cabal@ and @ghc@ are paths to the
-- @Cabal@ and @ghc-bin@ packages in GHC.
pkgPath :: Package -> FilePath
pkgPath (Package _ _ path) = path

instance Show Package where
    show (Package Library n p) = "library " ++ show n ++ " " ++ show p
    show (Package Program n p) = "program " ++ show n ++ " " ++ show p

instance Eq Package where
    p == q = pkgName p == pkgName q

instance Ord Package where
    compare p q = compare (pkgName p) (pkgName q)

instance Binary   Package
instance Hashable Package
instance NFData   Package

instance Binary   PackageType
instance Hashable PackageType
instance NFData   PackageType

-- | Construct a library package.
library :: PackageName -> FilePath -> Package
library = Package Library

-- | Construct a program package.
program :: PackageName -> FilePath -> Package
program = Package Program

-- | Check whether a package is a library.
isLibrary :: Package -> Bool
isLibrary (Package Library _ _) = True
isLibrary _ = False

-- | Check whether a package is a program.
isProgram :: Package -> Bool
isProgram (Package Program _ _) = True
isProgram _ = False

-- | The path to a package cabal file, e.g.: @ghc/ghc-bin.cabal@.
pkgCabalFile :: Package -> FilePath
pkgCabalFile pkg = pkgPath pkg -/- pkgName pkg <.> "cabal"
