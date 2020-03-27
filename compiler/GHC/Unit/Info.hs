{-# LANGUAGE CPP, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Package configuration information: essentially the interface to Cabal, with
-- some utilities
--
-- (c) The University of Glasgow, 2004
--
module GHC.Unit.Info (
        -- $package_naming

        -- * UnitId
        packageConfigId,
        expandedUnitInfoId,
        definiteUnitInfoId,
        installedUnitInfoId,

        -- * The UnitInfo type: information about a unit
        UnitInfo,
        GenericUnitInfo(..),
        ComponentId(..),
        PackageId(..),
        PackageName(..),
        Version(..),
        defaultUnitInfo,
        unitPackageNameString,
        unitPackageIdString,
        pprUnitInfo,
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.PackageDb
import Data.Version

import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Types.Module as Module
import GHC.Types.Unique

-- -----------------------------------------------------------------------------
-- Our UnitInfo type is the GenericUnitInfo from ghc-boot,
-- which is similar to a subset of the InstalledPackageInfo type from Cabal.

type UnitInfo = GenericUnitInfo
                       ComponentId
                       PackageId
                       PackageName
                       Module.InstalledUnitId
                       Module.UnitId
                       Module.ModuleName
                       Module.Module

-- TODO: there's no need for these to be FastString, as we don't need the uniq
--       feature, but ghc doesn't currently have convenient support for any
--       other compact string types, e.g. plain ByteString or Text.

newtype PackageId   = PackageId    FastString deriving (Eq, Ord)
newtype PackageName = PackageName
   { unPackageName :: FastString
   }
   deriving (Eq, Ord)

instance BinaryStringRep PackageId where
  fromStringRep = PackageId . mkFastStringByteString
  toStringRep (PackageId s) = bytesFS s

instance BinaryStringRep PackageName where
  fromStringRep = PackageName . mkFastStringByteString
  toStringRep (PackageName s) = bytesFS s

instance Uniquable PackageId where
  getUnique (PackageId n) = getUnique n

instance Uniquable PackageName where
  getUnique (PackageName n) = getUnique n

instance Outputable PackageId where
  ppr (PackageId str) = ftext str

instance Outputable PackageName where
  ppr (PackageName str) = ftext str

defaultUnitInfo :: UnitInfo
defaultUnitInfo = emptyGenericUnitInfo

unitPackageIdString :: UnitInfo -> String
unitPackageIdString pkg = unpackFS str
  where
    PackageId str = unitPackageId pkg

unitPackageNameString :: UnitInfo -> String
unitPackageNameString pkg = unpackFS str
  where
    PackageName str = unitPackageName pkg

pprUnitInfo :: UnitInfo -> SDoc
pprUnitInfo GenericUnitInfo {..} =
    vcat [
      field "name"                 (ppr unitPackageName),
      field "version"              (text (showVersion unitPackageVersion)),
      field "id"                   (ppr unitId),
      field "exposed"              (ppr unitIsExposed),
      field "exposed-modules"      (ppr unitExposedModules),
      field "hidden-modules"       (fsep (map ppr unitHiddenModules)),
      field "trusted"              (ppr unitIsTrusted),
      field "import-dirs"          (fsep (map text unitImportDirs)),
      field "library-dirs"         (fsep (map text unitLibraryDirs)),
      field "dynamic-library-dirs" (fsep (map text unitLibraryDynDirs)),
      field "hs-libraries"         (fsep (map text unitLibraries)),
      field "extra-libraries"      (fsep (map text unitExtDepLibsSys)),
      field "extra-ghci-libraries" (fsep (map text unitExtDepLibsGhc)),
      field "include-dirs"         (fsep (map text unitIncludeDirs)),
      field "includes"             (fsep (map text unitIncludes)),
      field "depends"              (fsep (map ppr  unitDepends)),
      field "cc-options"           (fsep (map text unitCcOptions)),
      field "ld-options"           (fsep (map text unitLinkerOptions)),
      field "framework-dirs"       (fsep (map text unitExtDepFrameworkDirs)),
      field "frameworks"           (fsep (map text unitExtDepFrameworks)),
      field "haddock-interfaces"   (fsep (map text unitHaddockInterfaces)),
      field "haddock-html"         (fsep (map text unitHaddockHTMLs))
    ]
  where
    field name body = text name <> colon <+> nest 4 body

-- -----------------------------------------------------------------------------
-- UnitId (package names, versions and dep hash)

-- $package_naming
-- #package_naming#
-- Mostly the compiler deals in terms of 'UnitId's, which are md5 hashes
-- of a package ID, keys of its dependencies, and Cabal flags. You're expected
-- to pass in the unit id in the @-this-unit-id@ flag. However, for
-- wired-in packages like @base@ & @rts@, we don't necessarily know what the
-- version is, so these are handled specially; see #wired_in_packages#.

-- | Get the GHC 'UnitId' right out of a Cabalish 'UnitInfo'
installedUnitInfoId :: UnitInfo -> InstalledUnitId
installedUnitInfoId = unitId

packageConfigId :: UnitInfo -> UnitId
packageConfigId p =
    if unitIsIndefinite p
        then newUnitId (unitInstanceOf p) (unitInstantiations p)
        else DefiniteUnitId (DefUnitId (unitId p))

expandedUnitInfoId :: UnitInfo -> UnitId
expandedUnitInfoId p =
    newUnitId (unitInstanceOf p) (unitInstantiations p)

definiteUnitInfoId :: UnitInfo -> Maybe DefUnitId
definiteUnitInfoId p =
    case packageConfigId p of
        DefiniteUnitId def_uid -> Just def_uid
        _ -> Nothing
