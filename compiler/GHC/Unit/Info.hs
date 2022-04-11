{-# LANGUAGE RecordWildCards, FlexibleInstances, MultiParamTypeClasses #-}

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

   , collectIncludeDirs
   , collectExtraCcOpts
   , collectLibraryDirs
   , collectFrameworks
   , collectFrameworksDirs
   , unitHsLibs
   )
where

import GHC.Prelude
import GHC.Platform.Ways

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Types.Unique

import GHC.Data.FastString
import qualified GHC.Data.ShortText as ST

import GHC.Unit.Module as Module
import GHC.Unit.Ppr
import GHC.Unit.Database

import GHC.Settings

import Data.Version
import Data.Bifunctor
import Data.List (isPrefixOf, stripPrefix)


-- | Information about an installed unit
--
-- We parameterize on the unit identifier:
--    * UnitKey: identifier used in the database (cf 'UnitKeyInfo')
--    * UnitId: identifier used to generate code (cf 'UnitInfo')
--
-- These two identifiers are different for wired-in packages. See Note [About
-- units] in "GHC.Unit"
type GenUnitInfo unit = GenericUnitInfo PackageId PackageName unit ModuleName (GenModule (GenUnit unit))

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
   mkPackageIdentifier'
   mkPackageName'
   mkModuleName'
   mkModule'
   where
     mkPackageIdentifier' = PackageId      . mkFastStringByteString
     mkPackageName'       = PackageName    . mkFastStringByteString
     mkUnitKey'           = UnitKey        . mkFastStringByteString
     mkModuleName'        = mkModuleNameFS . mkFastStringByteString
     mkVirtUnitKey' i = case i of
      DbInstUnitId cid insts -> mkVirtUnit (mkUnitKey' cid) (fmap (bimap mkModuleName' mkModule') insts)
      DbUnitId uid           -> RealUnit (Definite (mkUnitKey' uid))
     mkModule' m = case m of
       DbModule uid n -> mkModule (mkVirtUnitKey' uid) (mkModuleName' n)
       DbModuleVar  n -> mkHoleModule (mkModuleName' n)

-- | Map over the unit parameter
mapUnitInfo :: IsUnitId v => (u -> v) -> GenUnitInfo u -> GenUnitInfo v
mapUnitInfo f = mapGenericUnitInfo
   f         -- unit identifier
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

-- | Find all the include directories in the given units
collectIncludeDirs :: [UnitInfo] -> [FilePath]
collectIncludeDirs ps = map ST.unpack $ ordNub (filter (not . ST.null) (concatMap unitIncludeDirs ps))

-- | Find all the C-compiler options in the given units
collectExtraCcOpts :: [UnitInfo] -> [String]
collectExtraCcOpts ps = map ST.unpack (concatMap unitCcOptions ps)

-- | Find all the library directories in the given units for the given ways
collectLibraryDirs :: Ways -> [UnitInfo] -> [FilePath]
collectLibraryDirs ws = ordNub . filter notNull . concatMap (libraryDirsForWay ws)

-- | Find all the frameworks in the given units
collectFrameworks :: [UnitInfo] -> [String]
collectFrameworks ps = map ST.unpack (concatMap unitExtDepFrameworks ps)

-- | Find all the package framework paths in these and the preload packages
collectFrameworksDirs :: [UnitInfo] -> [String]
collectFrameworksDirs ps = map ST.unpack (ordNub (filter (not . ST.null) (concatMap unitExtDepFrameworkDirs ps)))

-- | Either the 'unitLibraryDirs' or 'unitLibraryDynDirs' as appropriate for the way.
libraryDirsForWay :: Ways -> UnitInfo -> [String]
libraryDirsForWay ws
  | hasWay ws WayDyn = map ST.unpack . unitLibraryDynDirs
  | otherwise        = map ST.unpack . unitLibraryDirs

unitHsLibs :: GhcNameVersion -> Ways -> UnitInfo -> [String]
unitHsLibs namever ways0 p = map (mkDynName . addSuffix . ST.unpack) (unitLibraries p)
  where
        ways1 = removeWay WayDyn ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        tag     = waysTag (fullWays ways1)
        rts_tag = waysTag ways1

        mkDynName x
         | not (ways0 `hasWay` WayDyn) = x
         | "HS" `isPrefixOf` x         = x ++ dynLibSuffix namever
           -- For non-Haskell libraries, we use the name "Cfoo". The .a
           -- file is libCfoo.a, and the .so is libfoo.so. That way the
           -- linker knows what we mean for the vanilla (-lCfoo) and dyn
           -- (-lfoo) ways. We therefore need to strip the 'C' off here.
         | Just x' <- stripPrefix "C" x = x'
         | otherwise
            = panic ("Don't understand library name " ++ x)

        -- Add _thr and other rts suffixes to packages named
        -- `rts` or `rts-1.0`. Why both?  Traditionally the rts
        -- package is called `rts` only.  However the tooling
        -- usually expects a package name to have a version.
        -- As such we will gradually move towards the `rts-1.0`
        -- package name, at which point the `rts` package name
        -- will eventually be unused.
        --
        -- This change elevates the need to add custom hooks
        -- and handling specifically for the `rts` package for
        -- example in ghc-cabal.
        addSuffix rts@"HSrts"       = rts       ++ (expandTag rts_tag)
        addSuffix rts@"HSrts-1.0.2" = rts       ++ (expandTag rts_tag)
        addSuffix other_lib         = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t
