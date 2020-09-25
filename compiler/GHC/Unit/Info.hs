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
import qualified GHC.Data.ShortText as ST
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
-- Units] in "GHC.Unit"
type GenUnitInfo unit = GenericUnitInfo (Indefinite unit) PackageId PackageName unit ModuleName (GenModule (GenUnit unit))

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
     mkIndefUnitKey' cid  = Indefinite (mkUnitKey' cid)
     mkVirtUnitKey' i = case i of
      DbInstUnitId cid insts -> mkVirtUnit (mkIndefUnitKey' cid) (fmap (bimap mkModuleName' mkModule') insts)
      DbUnitId uid           -> RealUnit (Definite (mkUnitKey' uid))
     mkModule' m = case m of
       DbModule uid n -> mkModule (mkVirtUnitKey' uid) (mkModuleName' n)
       DbModuleVar  n -> mkHoleModule (mkModuleName' n)

-- | Map over the unit parameter
mapUnitInfo :: IsUnitId v => (u -> v) -> GenUnitInfo u -> GenUnitInfo v
mapUnitInfo f = mapGenericUnitInfo
   f         -- unit identifier
   (fmap f)  -- indefinite unit identifier
   id        -- package identifier
   id        -- package name
   id        -- module name
   (fmap (mapGenUnit f)) -- instantiating modules

newtype PackageId   = PackageId    FastString deriving (Eq)
newtype PackageName = PackageName
   { unPackageName :: FastString
   }
   deriving (Eq)

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
      field "import-dirs"          (fsep (map (text . ST.unpack) unitImportDirs)),
      field "library-dirs"         (fsep (map (text . ST.unpack) unitLibraryDirs)),
      field "dynamic-library-dirs" (fsep (map (text . ST.unpack) unitLibraryDynDirs)),
      field "hs-libraries"         (fsep (map (text . ST.unpack) unitLibraries)),
      field "extra-libraries"      (fsep (map (text . ST.unpack) unitExtDepLibsSys)),
      field "extra-ghci-libraries" (fsep (map (text . ST.unpack) unitExtDepLibsGhc)),
      field "include-dirs"         (fsep (map (text . ST.unpack) unitIncludeDirs)),
      field "includes"             (fsep (map (text . ST.unpack) unitIncludes)),
      field "depends"              (fsep (map ppr  unitDepends)),
      field "cc-options"           (fsep (map (text . ST.unpack) unitCcOptions)),
      field "ld-options"           (fsep (map (text . ST.unpack) unitLinkerOptions)),
      field "framework-dirs"       (fsep (map (text . ST.unpack) unitExtDepFrameworkDirs)),
      field "frameworks"           (fsep (map (text . ST.unpack) unitExtDepFrameworks)),
      field "haddock-interfaces"   (fsep (map (text . ST.unpack) unitHaddockInterfaces)),
      field "haddock-html"         (fsep (map (text . ST.unpack) unitHaddockHTMLs))
    ]
  where
    field name body = text name <> colon <+> nest 4 body

-- | Make a `Unit` from a `UnitInfo`
--
-- If the unit is definite, make a `RealUnit` from `unitId` field.
--
-- If the unit is indefinite, make a `VirtUnit` from `unitInstanceOf` and
-- `unitInstantiations` fields. Note that in this case we don't keep track of
-- `unitId`. It can be retrieved later with "improvement", i.e. matching on
-- `unitInstanceOf/unitInstantiations` fields (see Note [About units] in
-- GHC.Unit).
mkUnit :: UnitInfo -> Unit
mkUnit p
   | unitIsIndefinite p = mkVirtUnit (unitInstanceOf p) (unitInstantiations p)
   | otherwise          = RealUnit (Definite (unitId p))

-- | Create a UnitPprInfo from a UnitInfo
mkUnitPprInfo :: (u -> FastString) -> GenUnitInfo u -> UnitPprInfo
mkUnitPprInfo ufs i = UnitPprInfo
   (ufs (unitId i))
   (unitPackageNameString i)
   (unitPackageVersion i)
   ((unpackFS . unPackageName) <$> unitComponentName i)
