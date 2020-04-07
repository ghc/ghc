{-# LANGUAGE CPP, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}

-- |
-- Package configuration information: essentially the interface to Cabal, with
-- some utilities
--
-- (c) The University of Glasgow, 2004
--
module UnitInfo
   ( GenericUnitInfo (..)
   , GenUnitInfo
   , UnitInfo
   , toUnitInfo

   , mkUnit
   , expandedUnitInfoId
   , definiteUnitInfoId

   , PackageId(..)
   , PackageName(..)
   , Version(..)
   , unitPackageNameString
   , unitPackageIdString
   , pprUnitInfo
   )
where

#include "HsVersions.h"

import GhcPrelude

import GHC.PackageDb
import Data.Version
import Data.Bifunctor

import FastString
import Outputable
import GHC.Types.Module as Module
import GHC.Types.Unique

-- | Information about an installed unit
--
-- We parameterize on the unit identifier:
--    * UnitKey: identifier used in the database (cf 'UnitKeyInfo')
--    * UnitId: identifier used to generate code (cf 'UnitInfo')
--
-- These two identifiers are different for wired-in packages. See Note [The
-- identifier lexicon] in GHC.Types.Module
type GenUnitInfo unit = GenericUnitInfo (Indefinite unit) PackageId PackageName unit ModuleName (GenModule (GenUnit unit))

-- | Information about an installed unit (units are identified by their internal
-- UnitId)
type UnitInfo    = GenUnitInfo UnitId

-- | Convert a DbUnitInfo (read from a package database) into `UnitKeyInfo`
toUnitInfo :: DbUnitInfo -> UnitInfo
toUnitInfo = mapGenericUnitInfo
   mkUnitId'
   mkIndefUnitId'
   mkPackageIdentifier'
   mkPackageName'
   mkModuleName'
   mkModule'
   where
     mkPackageIdentifier' = PackageId      . mkFastStringByteString
     mkPackageName'       = PackageName    . mkFastStringByteString
     mkUnitId'            = UnitId         . mkFastStringByteString
     mkModuleName'        = mkModuleNameFS . mkFastStringByteString
     mkIndefUnitId' cid   = Indefinite (UnitId (mkFastStringByteString cid)) Nothing
     mkInstUnitId' i = case i of
      DbInstUnitId cid insts -> mkInstUnit (mkIndefUnitId' cid) (fmap (bimap mkModuleName' mkModule') insts)
      DbUnitId uid           -> DefUnit (Definite (mkUnitId' uid))
     mkModule' m = case m of
       DbModule uid n -> mkModule (mkInstUnitId' uid) (mkModuleName' n)
       DbModuleVar  n -> mkHoleModule (mkModuleName' n)


-- TODO: there's no need for these to be FastString, as we don't need the uniq
--       feature, but ghc doesn't currently have convenient support for any
--       other compact string types, e.g. plain ByteString or Text.

newtype PackageId   = PackageId    FastString deriving (Eq, Ord)
newtype PackageName = PackageName
   { unPackageName :: FastString
   }
   deriving (Eq, Ord)

instance Uniquable PackageId where
  getUnique (PackageId n) = getUnique n

instance Uniquable PackageName where
  getUnique (PackageName n) = getUnique n

instance Outputable PackageId where
  ppr (PackageId str) = ftext str

instance Outputable PackageName where
  ppr (PackageName str) = ftext str

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

mkUnit :: UnitInfo -> Unit
mkUnit p =
    if unitIsIndefinite p
        then mkInstUnit (unitInstanceOf p) (unitInstantiations p)
        else DefUnit (Definite (unitId p))

expandedUnitInfoId :: UnitInfo -> Unit
expandedUnitInfoId p =
    mkInstUnit (unitInstanceOf p) (unitInstantiations p)

definiteUnitInfoId :: UnitInfo -> Maybe DefUnitId
definiteUnitInfoId p =
    case mkUnit p of
        DefUnit def_uid -> Just def_uid
        _               -> Nothing
