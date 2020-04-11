{-# LANGUAGE CPP, RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}

-- | Info about installed units (compiled libraries)
module GHC.Unit.Info
   ( GenericUnitInfo (..)
   , GenUnitInfo
   , UnitInfo
   , UnitKey (..)
   , UnitKeyInfo
   , mkUnitKeyInfo
   , mapUnitInfo
   , mkUnitPprInfo

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

import GHC.Prelude

import GHC.Unit.Database
import Data.Version
import Data.Bifunctor

import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Unit.Module as Module
import GHC.Types.Unique
import GHC.Unit.Ppr

-- | Information about an installed unit
--
-- We parameterize on the unit identifier:
--    * UnitKey: identifier used in the database (cf 'UnitKeyInfo')
--    * UnitId: identifier used to generate code (cf 'UnitInfo')
--
-- These two identifiers are different for wired-in packages. See Note [About
-- Units] in GHC.Unit
type GenUnitInfo unit = GenericUnitInfo (Indefinite unit) PackageId PackageName unit ModuleName (GenModule (GenUnit unit))

-- | A unit key in the database
newtype UnitKey = UnitKey FastString

unitKeyFS :: UnitKey -> FastString
unitKeyFS (UnitKey fs) = fs

-- | Information about an installed unit (units are identified by their database
-- UnitKey)
type UnitKeyInfo = GenUnitInfo UnitKey

-- | Information about an installed unit (units are identified by their internal
-- UnitId)
type UnitInfo    = GenUnitInfo UnitId

-- | Convert a DbUnitInfo (read from a package database) into `UnitKeyInfo`
mkUnitKeyInfo :: DbUnitInfo -> UnitKeyInfo
mkUnitKeyInfo = mapGenericUnitInfo
   mkUnitKey'
   mkIndefUnitKey'
   mkPackageIdentifier'
   mkPackageName'
   mkModuleName'
   mkModule'
   where
     mkPackageIdentifier' = PackageId      . mkFastStringByteString
     mkPackageName'       = PackageName    . mkFastStringByteString
     mkUnitKey'           = UnitKey        . mkFastStringByteString
     mkModuleName'        = mkModuleNameFS . mkFastStringByteString
     mkIndefUnitKey' cid  = Indefinite (mkUnitKey' cid) Nothing
     mkVirtUnitKey' i = case i of
      DbInstUnitId cid insts -> mkGenVirtUnit unitKeyFS (mkIndefUnitKey' cid) (fmap (bimap mkModuleName' mkModule') insts)
      DbUnitId uid           -> RealUnit (Definite (mkUnitKey' uid))
     mkModule' m = case m of
       DbModule uid n -> mkModule (mkVirtUnitKey' uid) (mkModuleName' n)
       DbModuleVar  n -> mkHoleModule (mkModuleName' n)

-- | Map over the unit parameter
mapUnitInfo :: (u -> v) -> (v -> FastString) -> GenUnitInfo u -> GenUnitInfo v
mapUnitInfo f gunitFS = mapGenericUnitInfo
   f         -- unit identifier
   (fmap f)  -- indefinite unit identifier
   id        -- package identifier
   id        -- package name
   id        -- module name
   (fmap (mapGenUnit f gunitFS)) -- instantiating modules

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

unitPackageIdString :: GenUnitInfo u -> String
unitPackageIdString pkg = unpackFS str
  where
    PackageId str = unitPackageId pkg

unitPackageNameString :: GenUnitInfo u -> String
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
        then mkVirtUnit (unitInstanceOf p) (unitInstantiations p)
        else RealUnit (Definite (unitId p))

expandedUnitInfoId :: UnitInfo -> Unit
expandedUnitInfoId p =
    mkVirtUnit (unitInstanceOf p) (unitInstantiations p)

definiteUnitInfoId :: UnitInfo -> Maybe DefUnitId
definiteUnitInfoId p =
    case mkUnit p of
        RealUnit def_uid -> Just def_uid
        _               -> Nothing

-- | Create a UnitPprInfo from a UnitInfo
mkUnitPprInfo :: GenUnitInfo u -> UnitPprInfo
mkUnitPprInfo i = UnitPprInfo
   (unitPackageNameString i)
   (unitPackageVersion i)
   ((unpackFS . unPackageName) <$> unitComponentName i)
