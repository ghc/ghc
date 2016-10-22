{-# LANGUAGE CPP, RecordWildCards #-}

-- |
-- Package configuration information: essentially the interface to Cabal, with
-- some utilities
--
-- (c) The University of Glasgow, 2004
--
module PackageConfig (
        -- $package_naming

        -- * UnitId
        packageConfigId,

        -- * The PackageConfig type: information about a package
        PackageConfig,
        InstalledPackageInfo(..),
        ComponentId(..),
        SourcePackageId(..),
        PackageName(..),
        Version(..),
        defaultPackageConfig,
        sourcePackageIdString,
        packageNameString,
        pprPackageConfig,
    ) where

#include "HsVersions.h"

import GHC.PackageDb
import Data.Version

import FastString
import Outputable
import Module
import Unique

-- -----------------------------------------------------------------------------
-- Our PackageConfig type is the InstalledPackageInfo from ghc-boot,
-- which is similar to a subset of the InstalledPackageInfo type from Cabal.

type PackageConfig = InstalledPackageInfo
                       SourcePackageId
                       PackageName
                       Module.UnitId
                       Module.ModuleName

-- TODO: there's no need for these to be FastString, as we don't need the uniq
--       feature, but ghc doesn't currently have convenient support for any
--       other compact string types, e.g. plain ByteString or Text.

newtype ComponentId = ComponentId FastString deriving (Eq, Ord)
newtype SourcePackageId    = SourcePackageId    FastString deriving (Eq, Ord)
newtype PackageName        = PackageName        FastString deriving (Eq, Ord)

instance BinaryStringRep ComponentId where
  fromStringRep = ComponentId . mkFastStringByteString
  toStringRep (ComponentId s) = fastStringToByteString s

instance BinaryStringRep SourcePackageId where
  fromStringRep = SourcePackageId . mkFastStringByteString
  toStringRep (SourcePackageId s) = fastStringToByteString s

instance BinaryStringRep PackageName where
  fromStringRep = PackageName . mkFastStringByteString
  toStringRep (PackageName s) = fastStringToByteString s

instance Uniquable ComponentId where
  getUnique (ComponentId n) = getUnique n

instance Uniquable SourcePackageId where
  getUnique (SourcePackageId n) = getUnique n

instance Uniquable PackageName where
  getUnique (PackageName n) = getUnique n

instance Outputable ComponentId where
  ppr (ComponentId str) = ftext str

instance Outputable SourcePackageId where
  ppr (SourcePackageId str) = ftext str

instance Outputable PackageName where
  ppr (PackageName str) = ftext str

-- | Pretty-print an 'ExposedModule' in the same format used by the textual
-- installed package database.
pprExposedModule :: (Outputable a, Outputable b) => ExposedModule a b -> SDoc
pprExposedModule (ExposedModule exposedName exposedReexport) =
    sep [ ppr exposedName
        , case exposedReexport of
            Just m -> sep [text "from", pprOriginalModule m]
            Nothing -> empty
        ]

-- | Pretty-print an 'OriginalModule' in the same format used by the textual
-- installed package database.
pprOriginalModule :: (Outputable a, Outputable b) => OriginalModule a b -> SDoc
pprOriginalModule (OriginalModule originalPackageId originalModuleName) =
    ppr originalPackageId <> char ':' <> ppr originalModuleName

defaultPackageConfig :: PackageConfig
defaultPackageConfig = emptyInstalledPackageInfo

sourcePackageIdString :: PackageConfig -> String
sourcePackageIdString pkg = unpackFS str
  where
    SourcePackageId str = sourcePackageId pkg

packageNameString :: PackageConfig -> String
packageNameString pkg = unpackFS str
  where
    PackageName str = packageName pkg

pprPackageConfig :: PackageConfig -> SDoc
pprPackageConfig InstalledPackageInfo {..} =
    vcat [
      field "name"                 (ppr packageName),
      field "version"              (text (showVersion packageVersion)),
      field "id"                   (ppr unitId),
      field "exposed"              (ppr exposed),
      field "exposed-modules"
        (if all isExposedModule exposedModules
           then fsep (map pprExposedModule exposedModules)
           else pprWithCommas pprExposedModule exposedModules),
      field "hidden-modules"       (fsep (map ppr hiddenModules)),
      field "trusted"              (ppr trusted),
      field "import-dirs"          (fsep (map text importDirs)),
      field "library-dirs"         (fsep (map text libraryDirs)),
      field "dynamic-library-dirs" (fsep (map text libraryDynDirs)),
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
    isExposedModule (ExposedModule _ Nothing) = True
    isExposedModule _ = False


-- -----------------------------------------------------------------------------
-- UnitId (package names, versions and dep hash)

-- $package_naming
-- #package_naming#
-- Mostly the compiler deals in terms of 'UnitId's, which are md5 hashes
-- of a package ID, keys of its dependencies, and Cabal flags. You're expected
-- to pass in the unit id in the @-this-unit-id@ flag. However, for
-- wired-in packages like @base@ & @rts@, we don't necessarily know what the
-- version is, so these are handled specially; see #wired_in_packages#.

-- | Get the GHC 'UnitId' right out of a Cabalish 'PackageConfig'
packageConfigId :: PackageConfig -> UnitId
packageConfigId = unitId
