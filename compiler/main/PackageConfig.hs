{-# LANGUAGE CPP, RecordWildCards #-}

-- |
-- Package configuration information: essentially the interface to Cabal, with
-- some utilities
--
-- (c) The University of Glasgow, 2004
--
module PackageConfig (
        -- $package_naming

        -- * PackageKey
        packageConfigId,

        -- * The PackageConfig type: information about a package
        PackageConfig,
        InstalledPackageInfo(..),
        InstalledPackageId(..),
        SourcePackageId(..),
        PackageName(..),
        Version(..),
        defaultPackageConfig,
        installedPackageIdString,
        sourcePackageIdString,
        packageNameString,
        pprPackageConfig,
    ) where

#include "HsVersions.h"

import GHC.PackageDb
import qualified Data.ByteString.Char8 as BS
import Data.Version

import Outputable
import Module

-- -----------------------------------------------------------------------------
-- Our PackageConfig type is the InstalledPackageInfo from bin-package-db,
-- which is similar to a subset of the InstalledPackageInfo type from Cabal.

type PackageConfig = InstalledPackageInfo
                       InstalledPackageId
                       SourcePackageId
                       PackageName
                       Module.PackageKey
                       Module.ModuleName

newtype InstalledPackageId = InstalledPackageId String deriving (Eq, Ord, Show)
newtype SourcePackageId    = SourcePackageId String    deriving (Eq, Ord, Show)
newtype PackageName        = PackageName String        deriving (Eq, Ord, Show)

instance BinaryStringRep InstalledPackageId where
  fromStringRep = InstalledPackageId . BS.unpack
  toStringRep   (InstalledPackageId s) = BS.pack s

instance BinaryStringRep SourcePackageId where
  fromStringRep = SourcePackageId . BS.unpack
  toStringRep   (SourcePackageId s) = BS.pack s

instance BinaryStringRep PackageName where
  fromStringRep = PackageName . BS.unpack
  toStringRep   (PackageName s) = BS.pack s

instance BinaryStringRep PackageKey where
  fromStringRep = Module.stringToPackageKey . BS.unpack
  toStringRep   = BS.pack . Module.packageKeyString

instance BinaryStringRep Module.ModuleName where
  fromStringRep = Module.mkModuleName . BS.unpack
  toStringRep   = BS.pack . Module.moduleNameString

instance Outputable InstalledPackageId where
  ppr (InstalledPackageId str) = text str

instance Outputable SourcePackageId where
  ppr (SourcePackageId str) = text str

instance Outputable PackageName where
  ppr (PackageName str) = text str

defaultPackageConfig :: PackageConfig
defaultPackageConfig = emptyInstalledPackageInfo

installedPackageIdString :: PackageConfig -> String
installedPackageIdString pkg = str
  where
    InstalledPackageId str = installedPackageId pkg

sourcePackageIdString :: PackageConfig -> String
sourcePackageIdString pkg = str
  where
    SourcePackageId str = sourcePackageId pkg

packageNameString :: PackageConfig -> String
packageNameString pkg = str
  where
    PackageName str = packageName pkg

pprPackageConfig :: PackageConfig -> SDoc
pprPackageConfig InstalledPackageInfo {..} =
    vcat [
      field "name"                 (ppr packageName),
      field "version"              (text (showVersion packageVersion)),
      field "id"                   (ppr installedPackageId),
      field "key"                  (ppr packageKey),
      field "exposed"              (ppr exposed),
      field "exposed-modules"      (fsep (map ppr exposedModules)),
      field "hidden-modules"       (fsep (map ppr hiddenModules)),
      field "reexported-modules"   (fsep (map ppr haddockHTMLs)),
      field "trusted"              (ppr trusted),
      field "import-dirs"          (fsep (map text importDirs)),
      field "library-dirs"         (fsep (map text libraryDirs)),
      field "hs-libraries"         (fsep (map text hsLibraries)), 
      field "extra-libraries"      (fsep (map text extraLibraries)),
      field "extra-ghci-libraries" (fsep (map text extraGHCiLibraries)),
      field "include-dirs"         (fsep (map text includeDirs)),
      field "includes"             (fsep (map text includes)),
      field "depends"              (fsep (map ppr  depends)),
      field "cc-options"           (fsep (map text ccOptions)),
      field "ld-options"           (fsep (map text ldOptions)),
      field "framework-dirs"       (fsep (map text frameworkDirs)),
      field "frameworks"           (fsep (map text frameworks)),
      field "haddock-interfaces"   (fsep (map text haddockInterfaces)),
      field "haddock-html"         (fsep (map text haddockHTMLs))
    ]
  where
    field name body = text name <> colon <+> nest 4 body


-- -----------------------------------------------------------------------------
-- PackageKey (package names, versions and dep hash)

-- $package_naming
-- #package_naming#
-- Mostly the compiler deals in terms of 'PackageKey's, which are md5 hashes
-- of a package ID, keys of its dependencies, and Cabal flags. You're expected
-- to pass in the package key in the @-this-package-key@ flag. However, for
-- wired-in packages like @base@ & @rts@, we don't necessarily know what the
-- version is, so these are handled specially; see #wired_in_packages#.

-- | Get the GHC 'PackageKey' right out of a Cabalish 'PackageConfig'
packageConfigId :: PackageConfig -> PackageKey
packageConfigId = packageKey

