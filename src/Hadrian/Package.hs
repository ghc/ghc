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
    cLibrary, cProgram, hsLibrary, hsProgram,
    isLibrary, isProgram, isCPackage, isHsPackage,

    -- * Package directory structure
    pkgCabalFile, unsafePkgCabalFile
    ) where

import Data.Maybe
import Development.Shake.Classes
import Development.Shake.FilePath
import GHC.Generics
import GHC.Stack
import Hadrian.Utilities

data PackageLanguage = C | Haskell deriving (Generic, Show)

-- TODO: Make PackageType more precise.
-- See https://github.com/snowleopard/hadrian/issues/12.
data PackageType = Library | Program deriving (Generic, Show)

type PackageName = String

-- TODO: Consider turning Package into a GADT indexed with language and type.
data Package = Package {
    -- | The package language. 'C' and 'Haskell' packages are supported.
    pkgLanguage :: PackageLanguage,
    -- | The package type. 'Library' and 'Program' packages are supported.
    pkgType :: PackageType,
    -- | The package name. We assume that all packages have different names,
    -- hence two packages with the same name are considered equal.
    pkgName :: PackageName,
    -- | The path to the package source code relative to the root of the build
    -- system. For example, @libraries/Cabal/Cabal@ and @ghc@ are paths to the
    -- @Cabal@ and @ghc-bin@ packages in GHC.
    pkgPath :: FilePath
    } deriving (Generic, Show)

instance Eq Package where
    p == q = pkgName p == pkgName q

instance Ord Package where
    compare p q = compare (pkgName p) (pkgName q)

instance Binary   PackageLanguage
instance Hashable PackageLanguage
instance NFData   PackageLanguage

instance Binary   PackageType
instance Hashable PackageType
instance NFData   PackageType

instance Binary   Package
instance Hashable Package
instance NFData   Package

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
