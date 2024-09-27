-----------------------------------------------------------------------------
-- |
-- Module     : Hadrian.Haskell.Cabal.Type
-- Copyright  : (c) Andrey Mokhov 2014-2018
-- License    : MIT (see the file LICENSE)
-- Maintainer : andrey.mokhov@gmail.com
-- Stability  : experimental
--
-- Data types for storing basic Haskell package metadata, such as package name,
-- version and dependencies, extracted from a Cabal file.
-----------------------------------------------------------------------------
module Hadrian.Haskell.Cabal.Type where

import Development.Shake.Classes
import Distribution.PackageDescription
import GHC.Generics

import Hadrian.Package as Hadrian

-- | Haskell package metadata extracted from a Cabal file without performing
-- the resolution of package configuration flags and associated conditionals,
-- which are build context specific. Note that 'packageDependencies' is an
-- overappoximation of actual package dependencies; for example, both @unix@ and
-- @win32@ packages may be included even if only one of them is required on the
-- target OS. See 'ContextData' for metadata obtained after resolving package
-- configuration flags and conditionals according to the current build context.
data PackageData = PackageData
    { name                      :: Hadrian.PackageName
    , version                   :: String
    , synopsis                  :: String
    , description               :: String
    , packageDependencies       :: [Package]
    , dependsOnSystemCxxStdLib  :: Bool
    , genericPackageDescription :: GenericPackageDescription
    } deriving (Eq, Generic, Show, Typeable)

-- | Haskell package metadata obtained after resolving package configuration
-- flags and associated conditionals according to the current build context.
-- See 'PackageData' for metadata that can be obtained without resolving package
-- configuration flags and conditionals.
data ContextData = ContextData
    { dependencies       :: [Hadrian.PackageName]
    , componentId        :: String
    , mainIs             :: Maybe (String, FilePath)  -- ("Main", filepath)
    , modules            :: [String]
    , otherModules       :: [String]
    , reexportModules    :: [String]
    , srcDirs            :: [String]
    , depIds             :: [String]
    , depNames           :: [String]
    , includeDirs        :: [String]
    , includes           :: [String]
    , installIncludes    :: [String]
    , extraLibs          :: [String]
    , extraLibDirs       :: [String]
    , asmSrcs            :: [String]
    , cSrcs              :: [String]
    , cxxSrcs            :: [String]
    , cmmSrcs            :: [String]
    , jsSrcs             :: [String]
    , hcOpts             :: [String]
    , asmOpts            :: [String]
    , ccOpts             :: [String]
    , cxxOpts            :: [String]
    , cmmOpts            :: [String]
    , cppOpts            :: [String]
    , ldOpts             :: [String]
    , depIncludeDirs     :: [String]
    , depCcOpts          :: [String]
    , depLdOpts          :: [String]
    , buildGhciLib       :: Bool
    , frameworks         :: [String]
    , packageDescription :: PackageDescription

    -- The location where normal library files go
    , contextLibdir    :: FilePath
    -- The location where dynamic libraries go
    , contextDynLibdir :: FilePath
    } deriving (Eq, Generic, Show, Typeable)

instance Binary   PackageData
instance Hashable PackageData where hashWithSalt salt = hashWithSalt salt . show
instance NFData   PackageData

instance Binary   ContextData
instance Hashable ContextData where hashWithSalt salt = hashWithSalt salt . show
instance NFData   ContextData
