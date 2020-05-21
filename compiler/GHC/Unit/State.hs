-- (c) The University of Glasgow, 2006

{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns, FlexibleContexts #-}

-- | Package manipulation
module GHC.Unit.State (
        module GHC.Unit.Info,

        -- * Reading the package config, and processing cmdline args
        PackageState(..),
        PackageDatabase (..),
        UnitInfoMap,
        emptyPackageState,
        initPackages,
        readPackageDatabases,
        readPackageDatabase,
        getPackageConfRefs,
        resolvePackageDatabase,
        listUnitInfoMap,

        -- * Querying the package config
        lookupUnit,
        lookupUnit',
        lookupInstalledPackage,
        lookupPackageName,
        improveUnit,
        searchPackageId,
        unsafeGetUnitInfo,
        getInstalledPackageDetails,
        displayUnitId,
        listVisibleModuleNames,
        lookupModuleInAllPackages,
        lookupModuleWithSuggestions,
        lookupPluginModuleWithSuggestions,
        LookupResult(..),
        ModuleSuggestion(..),
        ModuleOrigin(..),
        UnusablePackageReason(..),
        pprReason,

        -- * Inspecting the set of packages in scope
        getPackageIncludePath,
        getPackageLibraryPath,
        getPackageLinkOpts,
        getPackageExtraCcOpts,
        getPackageFrameworkPath,
        getPackageFrameworks,
        getPreloadPackagesAnd,

        collectArchives,
        collectIncludeDirs, collectLibraryPaths, collectLinkOpts,
        packageHsLibs, getLibs,

        -- * Utils
        mkIndefUnitId,
        updateIndefUnitId,
        unwireUnit,
        pprFlag,
        pprPackages,
        pprPackagesSimple,
        pprModuleMap,
        isIndefinite,
    )
where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Unit.Database
import GHC.Unit.Info
import GHC.Unit.Types
import GHC.Unit.Module
import GHC.Unit.Subst
import GHC.Driver.Session
import GHC.Driver.Ways
import GHC.Types.Unique.FM
import GHC.Types.Unique.DFM
import GHC.Types.Unique.Set
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable as Outputable
import GHC.Data.Maybe

import System.Environment ( getEnv )
import GHC.Data.FastString
import GHC.Utils.Error  ( debugTraceMsg, MsgDoc, dumpIfSet_dyn,
                          withTiming, DumpFormat (..) )
import GHC.Utils.Exception

import System.Directory
import System.FilePath as FilePath
import Control.Monad
import Data.Graph (stronglyConnComp, SCC(..))
import Data.Char ( toUpper )
import Data.List as List
import Data.Map (Map)
import Data.Set (Set)
import Data.Monoid (First(..))
import qualified Data.Semigroup as Semigroup
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- The Package state

-- | Package state is all stored in 'DynFlags', including the details of
-- all packages, which packages are exposed, and which modules they
-- provide.
--
-- The package state is computed by 'initPackages', and kept in DynFlags.
-- It is influenced by various package flags:
--
--   * @-package <pkg>@ and @-package-id <pkg>@ cause @<pkg>@ to become exposed.
--     If @-hide-all-packages@ was not specified, these commands also cause
--      all other packages with the same name to become hidden.
--
--   * @-hide-package <pkg>@ causes @<pkg>@ to become hidden.
--
--   * (there are a few more flags, check below for their semantics)
--
-- The package state has the following properties.
--
--   * Let @exposedPackages@ be the set of packages thus exposed.
--     Let @depExposedPackages@ be the transitive closure from @exposedPackages@ of
--     their dependencies.
--
--   * When searching for a module from a preload import declaration,
--     only the exposed modules in @exposedPackages@ are valid.
--
--   * When searching for a module from an implicit import, all modules
--     from @depExposedPackages@ are valid.
--
--   * When linking in a compilation manager mode, we link in packages the
--     program depends on (the compiler knows this list by the
--     time it gets to the link step).  Also, we link in all packages
--     which were mentioned with preload @-package@ flags on the command-line,
--     or are a transitive dependency of same, or are \"base\"\/\"rts\".
--     The reason for this is that we might need packages which don't
--     contain any Haskell modules, and therefore won't be discovered
--     by the normal mechanism of dependency tracking.

-- Notes on DLLs
-- ~~~~~~~~~~~~~
-- When compiling module A, which imports module B, we need to
-- know whether B will be in the same DLL as A.
--      If it's in the same DLL, we refer to B_f_closure
--      If it isn't, we refer to _imp__B_f_closure
-- When compiling A, we record in B's Module value whether it's
-- in a different DLL, by setting the DLL flag.

-- | Given a module name, there may be multiple ways it came into scope,
-- possibly simultaneously.  This data type tracks all the possible ways
-- it could have come into scope.  Warning: don't use the record functions,
-- they're partial!
data ModuleOrigin =
    -- | Module is hidden, and thus never will be available for import.
    -- (But maybe the user didn't realize), so we'll still keep track
    -- of these modules.)
    ModHidden
    -- | Module is unavailable because the package is unusable.
  | ModUnusable UnusablePackageReason
    -- | Module is public, and could have come from some places.
  | ModOrigin {
        -- | @Just False@ means that this module is in
        -- someone's @exported-modules@ list, but that package is hidden;
        -- @Just True@ means that it is available; @Nothing@ means neither
        -- applies.
        fromOrigPackage :: Maybe Bool
        -- | Is the module available from a reexport of an exposed package?
        -- There could be multiple.
      , fromExposedReexport :: [UnitInfo]
        -- | Is the module available from a reexport of a hidden package?
      , fromHiddenReexport :: [UnitInfo]
        -- | Did the module export come from a package flag? (ToDo: track
        -- more information.
      , fromPackageFlag :: Bool
      }

instance Outputable ModuleOrigin where
    ppr ModHidden = text "hidden module"
    ppr (ModUnusable _) = text "unusable module"
    ppr (ModOrigin e res rhs f) = sep (punctuate comma (
        (case e of
            Nothing -> []
            Just False -> [text "hidden package"]
            Just True -> [text "exposed package"]) ++
        (if null res
            then []
            else [text "reexport by" <+>
                    sep (map (ppr . mkUnit) res)]) ++
        (if null rhs
            then []
            else [text "hidden reexport by" <+>
                    sep (map (ppr . mkUnit) res)]) ++
        (if f then [text "package flag"] else [])
        ))

-- | Smart constructor for a module which is in @exposed-modules@.  Takes
-- as an argument whether or not the defining package is exposed.
fromExposedModules :: Bool -> ModuleOrigin
fromExposedModules e = ModOrigin (Just e) [] [] False

-- | Smart constructor for a module which is in @reexported-modules@.  Takes
-- as an argument whether or not the reexporting package is exposed, and
-- also its 'UnitInfo'.
fromReexportedModules :: Bool -> UnitInfo -> ModuleOrigin
fromReexportedModules True pkg = ModOrigin Nothing [pkg] [] False
fromReexportedModules False pkg = ModOrigin Nothing [] [pkg] False

-- | Smart constructor for a module which was bound by a package flag.
fromFlag :: ModuleOrigin
fromFlag = ModOrigin Nothing [] [] True

instance Semigroup ModuleOrigin where
    ModOrigin e res rhs f <> ModOrigin e' res' rhs' f' =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = panic "ModOrigin: package both exposed/hidden"
            g Nothing x = x
            g x Nothing = x
    _x <> _y = panic "ModOrigin: hidden module redefined"

instance Monoid ModuleOrigin where
    mempty = ModOrigin Nothing [] [] False
    mappend = (Semigroup.<>)

-- | Is the name from the import actually visible? (i.e. does it cause
-- ambiguity, or is it only relevant when we're making suggestions?)
originVisible :: ModuleOrigin -> Bool
originVisible ModHidden = False
originVisible (ModUnusable _) = False
originVisible (ModOrigin b res _ f) = b == Just True || not (null res) || f

-- | Are there actually no providers for this module?  This will never occur
-- except when we're filtering based on package imports.
originEmpty :: ModuleOrigin -> Bool
originEmpty (ModOrigin Nothing [] [] False) = True
originEmpty _ = False

-- | Map from 'UnitId' to 'UnitInfo', plus
-- the transitive closure of preload units.
data UnitInfoMap = UnitInfoMap
   { unUnitInfoMap :: UniqDFM UnitInfo
      -- ^ Map from 'UnitId' to 'UnitInfo'

   , preloadClosure :: UniqSet UnitId
     -- ^ The set of transitively reachable units according
     -- to the explicitly provided command line arguments.
     -- A fully instantiated VirtUnit may only be replaced by a RealUnit from
     -- this set.
     -- See Note [VirtUnit to RealUnit improvement]
   }

-- | 'UniqFM' map from 'Unit' to a 'UnitVisibility'.
type VisibilityMap = Map Unit UnitVisibility

-- | 'UnitVisibility' records the various aspects of visibility of a particular
-- 'Unit'.
data UnitVisibility = UnitVisibility
    { uv_expose_all :: Bool
      --  ^ Should all modules in exposed-modules should be dumped into scope?
    , uv_renamings :: [(ModuleName, ModuleName)]
      -- ^ Any custom renamings that should bring extra 'ModuleName's into
      -- scope.
    , uv_package_name :: First FastString
      -- ^ The package name associated with the 'Unit'.  This is used
      -- to implement legacy behavior where @-package foo-0.1@ implicitly
      -- hides any packages named @foo@
    , uv_requirements :: Map ModuleName (Set InstantiatedModule)
      -- ^ The signatures which are contributed to the requirements context
      -- from this unit ID.
    , uv_explicit :: Bool
      -- ^ Whether or not this unit was explicitly brought into scope,
      -- as opposed to implicitly via the 'exposed' fields in the
      -- package database (when @-hide-all-packages@ is not passed.)
    }

instance Outputable UnitVisibility where
    ppr (UnitVisibility {
        uv_expose_all = b,
        uv_renamings = rns,
        uv_package_name = First mb_pn,
        uv_requirements = reqs,
        uv_explicit = explicit
    }) = ppr (b, rns, mb_pn, reqs, explicit)

instance Semigroup UnitVisibility where
    uv1 <> uv2
        = UnitVisibility
          { uv_expose_all = uv_expose_all uv1 || uv_expose_all uv2
          , uv_renamings = uv_renamings uv1 ++ uv_renamings uv2
          , uv_package_name = mappend (uv_package_name uv1) (uv_package_name uv2)
          , uv_requirements = Map.unionWith Set.union (uv_requirements uv1) (uv_requirements uv2)
          , uv_explicit = uv_explicit uv1 || uv_explicit uv2
          }

instance Monoid UnitVisibility where
    mempty = UnitVisibility
             { uv_expose_all = False
             , uv_renamings = []
             , uv_package_name = First Nothing
             , uv_requirements = Map.empty
             , uv_explicit = False
             }
    mappend = (Semigroup.<>)

type WiredUnitId = DefUnitId
type PreloadUnitId = UnitId

-- | Map from 'ModuleName' to a set of of module providers (i.e. a 'Module' and
-- its 'ModuleOrigin').
--
-- NB: the set is in fact a 'Map Module ModuleOrigin', probably to keep only one
-- origin for a given 'Module'
type ModuleNameProvidersMap =
    Map ModuleName (Map Module ModuleOrigin)

data PackageState = PackageState {
  -- | A mapping of 'Unit' to 'UnitInfo'.  This list is adjusted
  -- so that only valid packages are here.  'UnitInfo' reflects
  -- what was stored *on disk*, except for the 'trusted' flag, which
  -- is adjusted at runtime.  (In particular, some packages in this map
  -- may have the 'exposed' flag be 'False'.)
  unitInfoMap :: UnitInfoMap,

  -- | A mapping of 'PackageName' to 'IndefUnitId'.  This is used when
  -- users refer to packages in Backpack includes.
  packageNameMap            :: Map PackageName IndefUnitId,

  -- | A mapping from wired in names to the original names from the
  -- package database.
  unwireMap :: Map WiredUnitId WiredUnitId,

  -- | The packages we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a package
  -- is always mentioned before the packages it depends on.
  preloadPackages      :: [PreloadUnitId],

  -- | Packages which we explicitly depend on (from a command line flag).
  -- We'll use this to generate version macros.
  explicitPackages      :: [Unit],

  -- | This is a full map from 'ModuleName' to all modules which may possibly
  -- be providing it.  These providers may be hidden (but we'll still want
  -- to report them in error messages), or it may be an ambiguous import.
  moduleNameProvidersMap    :: !ModuleNameProvidersMap,

  -- | A map, like 'moduleNameProvidersMap', but controlling plugin visibility.
  pluginModuleNameProvidersMap    :: !ModuleNameProvidersMap,

  -- | A map saying, for each requirement, what interfaces must be merged
  -- together when we use them.  For example, if our dependencies
  -- are @p[A=<A>]@ and @q[A=<A>,B=r[C=<A>]:B]@, then the interfaces
  -- to merge for A are @p[A=<A>]:A@, @q[A=<A>,B=r[C=<A>]:B]:A@
  -- and @r[C=<A>]:C@.
  --
  -- There's an entry in this map for each hole in our home library.
  requirementContext :: Map ModuleName [InstantiatedModule]
  }

emptyPackageState :: PackageState
emptyPackageState = PackageState {
    unitInfoMap = emptyUnitInfoMap,
    packageNameMap = Map.empty,
    unwireMap = Map.empty,
    preloadPackages = [],
    explicitPackages = [],
    moduleNameProvidersMap = Map.empty,
    pluginModuleNameProvidersMap = Map.empty,
    requirementContext = Map.empty
    }

-- | Package database
data PackageDatabase unit = PackageDatabase
   { packageDatabasePath  :: FilePath
   , packageDatabaseUnits :: [GenUnitInfo unit]
   }

type InstalledPackageIndex = Map UnitId UnitInfo

-- | Empty package configuration map
emptyUnitInfoMap :: UnitInfoMap
emptyUnitInfoMap = UnitInfoMap emptyUDFM emptyUniqSet

-- | Find the unit we know about with the given unit id, if any
lookupUnit :: DynFlags -> Unit -> Maybe UnitInfo
lookupUnit dflags = lookupUnit' (isIndefinite dflags) (unitInfoMap (pkgState dflags))

-- | A more specialized interface, which takes a boolean specifying
-- whether or not to look for on-the-fly renamed interfaces, and
-- just a 'UnitInfoMap' rather than a 'DynFlags' (so it can
-- be used while we're initializing 'DynFlags'
lookupUnit' :: Bool -> UnitInfoMap -> Unit -> Maybe UnitInfo
lookupUnit' False (UnitInfoMap pkg_map _) uid  = lookupUDFM pkg_map uid
lookupUnit' True m@(UnitInfoMap pkg_map _) uid = case uid of
   HoleUnit   -> error "Hole unit"
   RealUnit _ -> lookupUDFM pkg_map uid
   VirtUnit i -> fmap (renamePackage m (instUnitInsts i))
                      (lookupUDFM pkg_map (instUnitInstanceOf i))

{-
-- | Find the indefinite package for a given 'IndefUnitId'.
-- The way this works is just by fiat'ing that every indefinite package's
-- unit key is precisely its component ID; and that they share uniques.
lookupIndefUnitId :: PackageState -> IndefUnitId -> Maybe UnitInfo
lookupIndefUnitId pkgstate (IndefUnitId cid_fs) = lookupUDFM pkg_map cid_fs
  where
    UnitInfoMap pkg_map = unitInfoMap pkgstate
-}

-- | Find the package we know about with the given package name (e.g. @foo@), if any
-- (NB: there might be a locally defined unit name which overrides this)
lookupPackageName :: PackageState -> PackageName -> Maybe IndefUnitId
lookupPackageName pkgstate n = Map.lookup n (packageNameMap pkgstate)

-- | Search for packages with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: PackageState -> PackageId -> [UnitInfo]
searchPackageId pkgstate pid = filter ((pid ==) . unitPackageId)
                               (listUnitInfoMap pkgstate)

-- | Extends the package configuration map with a list of package configs.
extendUnitInfoMap
   :: UnitInfoMap -> [UnitInfo] -> UnitInfoMap
extendUnitInfoMap (UnitInfoMap pkg_map closure) new_pkgs
  = UnitInfoMap (foldl' add pkg_map new_pkgs) closure
    -- We also add the expanded version of the mkUnit, so that
    -- 'improveUnit' can find it.
  where add pkg_map p = addToUDFM (addToUDFM pkg_map (expandedUnitInfoId p) p)
                                  (unitId p) p

-- | Looks up the package with the given id in the package state, panicing if it is
-- not found
unsafeGetUnitInfo :: HasDebugCallStack => DynFlags -> Unit -> UnitInfo
unsafeGetUnitInfo dflags pid =
    case lookupUnit dflags pid of
      Just config -> config
      Nothing -> pprPanic "unsafeGetUnitInfo" (ppr pid)

lookupInstalledPackage :: PackageState -> UnitId -> Maybe UnitInfo
lookupInstalledPackage pkgstate uid = lookupInstalledPackage' (unitInfoMap pkgstate) uid

lookupInstalledPackage' :: UnitInfoMap -> UnitId -> Maybe UnitInfo
lookupInstalledPackage' (UnitInfoMap db _) uid = lookupUDFM db uid

getInstalledPackageDetails :: HasDebugCallStack => PackageState -> UnitId -> UnitInfo
getInstalledPackageDetails pkgstate uid =
    case lookupInstalledPackage pkgstate uid of
      Just config -> config
      Nothing -> pprPanic "getInstalledPackageDetails" (ppr uid)

-- | Get a list of entries from the package database.  NB: be careful with
-- this function, although all packages in this map are "visible", this
-- does not imply that the exposed-modules of the package are available
-- (they may have been thinned or renamed).
listUnitInfoMap :: PackageState -> [UnitInfo]
listUnitInfoMap pkgstate = eltsUDFM pkg_map
  where
    UnitInfoMap pkg_map _ = unitInfoMap pkgstate

-- ----------------------------------------------------------------------------
-- Loading the package db files and building up the package state

-- | Read the package database files, and sets up various internal tables of
-- package information, according to the package-related flags on the
-- command-line (@-package@, @-hide-package@ etc.)
--
-- Returns a list of packages to link in if we're doing dynamic linking.
-- This list contains the packages that the user explicitly mentioned with
-- @-package@ flags.
--
-- 'initPackages' can be called again subsequently after updating the
-- 'packageFlags' field of the 'DynFlags', and it will update the
-- 'pkgState' in 'DynFlags' and return a list of packages to
-- link in.
initPackages :: DynFlags -> IO (DynFlags, [PreloadUnitId])
initPackages dflags = withTiming dflags
                                  (text "initializing package database")
                                  forcePkgDb $ do
  read_pkg_dbs <-
    case pkgDatabase dflags of
        Nothing  -> readPackageDatabases dflags
        Just dbs -> return dbs

  let
      distrust_all db = db { packageDatabaseUnits = distrustAllUnits (packageDatabaseUnits db) }

      pkg_dbs
         | gopt Opt_DistrustAllPackages dflags = map distrust_all read_pkg_dbs
         | otherwise                           = read_pkg_dbs

  (pkg_state, preload, insts)
        <- mkPackageState dflags pkg_dbs []
  return (dflags{ pkgDatabase = Just read_pkg_dbs,
                  pkgState = pkg_state,
                  thisUnitIdInsts_ = insts },
          preload)
  where
    forcePkgDb (dflags, _) = unitInfoMap (pkgState dflags) `seq` ()

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageDatabases :: DynFlags -> IO [PackageDatabase UnitId]
readPackageDatabases dflags = do
  conf_refs <- getPackageConfRefs dflags
  confs     <- liftM catMaybes $ mapM (resolvePackageDatabase dflags) conf_refs
  mapM (readPackageDatabase dflags) confs


getPackageConfRefs :: DynFlags -> IO [PkgDbRef]
getPackageConfRefs dflags = do
  let system_conf_refs = [UserPkgDb, GlobalPkgDb]

  e_pkg_path <- tryIO (getEnv $ map toUpper (programName dflags) ++ "_PACKAGE_PATH")
  let base_conf_refs = case e_pkg_path of
        Left _ -> system_conf_refs
        Right path
         | not (null path) && isSearchPathSeparator (last path)
         -> map PkgDbPath (splitSearchPath (init path)) ++ system_conf_refs
         | otherwise
         -> map PkgDbPath (splitSearchPath path)

  -- Apply the package DB-related flags from the command line to get the
  -- final list of package DBs.
  --
  -- Notes on ordering:
  --  * The list of flags is reversed (later ones first)
  --  * We work with the package DB list in "left shadows right" order
  --  * and finally reverse it at the end, to get "right shadows left"
  --
  return $ reverse (foldr doFlag base_conf_refs (packageDBFlags dflags))
 where
  doFlag (PackageDB p) dbs = p : dbs
  doFlag NoUserPackageDB dbs = filter isNotUser dbs
  doFlag NoGlobalPackageDB dbs = filter isNotGlobal dbs
  doFlag ClearPackageDBs _ = []

  isNotUser UserPkgDb = False
  isNotUser _ = True

  isNotGlobal GlobalPkgDb = False
  isNotGlobal _ = True

-- | Return the path of a package database from a 'PkgDbRef'. Return 'Nothing'
-- when the user database filepath is expected but the latter doesn't exist.
--
-- NB: This logic is reimplemented in Cabal, so if you change it,
-- make sure you update Cabal. (Or, better yet, dump it in the
-- compiler info so Cabal can use the info.)
resolvePackageDatabase :: DynFlags -> PkgDbRef -> IO (Maybe FilePath)
resolvePackageDatabase dflags GlobalPkgDb = return $ Just (globalPackageDatabasePath dflags)
resolvePackageDatabase dflags UserPkgDb = runMaybeT $ do
  dir <- versionedAppDir dflags
  let pkgconf = dir </> "package.conf.d"
  exist <- tryMaybeT $ doesDirectoryExist pkgconf
  if exist then return pkgconf else mzero
resolvePackageDatabase _ (PkgDbPath name) = return $ Just name

readPackageDatabase :: DynFlags -> FilePath -> IO (PackageDatabase UnitId)
readPackageDatabase dflags conf_file = do
  isdir <- doesDirectoryExist conf_file

  proto_pkg_configs <-
    if isdir
       then readDirStyleUnitInfo conf_file
       else do
            isfile <- doesFileExist conf_file
            if isfile
               then do
                 mpkgs <- tryReadOldFileStyleUnitInfo
                 case mpkgs of
                   Just pkgs -> return pkgs
                   Nothing   -> throwGhcExceptionIO $ InstallationError $
                      "ghc no longer supports single-file style package " ++
                      "databases (" ++ conf_file ++
                      ") use 'ghc-pkg init' to create the database with " ++
                      "the correct format."
               else throwGhcExceptionIO $ InstallationError $
                      "can't find a package database at " ++ conf_file

  let
      -- Fix #16360: remove trailing slash from conf_file before calculating pkgroot
      conf_file' = dropTrailingPathSeparator conf_file
      top_dir = topDir dflags
      pkgroot = takeDirectory conf_file'
      pkg_configs1 = map (mungeUnitInfo top_dir pkgroot . mapUnitInfo (\(UnitKey x) -> UnitId x) unitIdFS . mkUnitKeyInfo)
                         proto_pkg_configs
  --
  return $ PackageDatabase conf_file' pkg_configs1
  where
    readDirStyleUnitInfo conf_dir = do
      let filename = conf_dir </> "package.cache"
      cache_exists <- doesFileExist filename
      if cache_exists
        then do
          debugTraceMsg dflags 2 $ text "Using binary package database:"
                                    <+> text filename
          readPackageDbForGhc filename
        else do
          -- If there is no package.cache file, we check if the database is not
          -- empty by inspecting if the directory contains any .conf file. If it
          -- does, something is wrong and we fail. Otherwise we assume that the
          -- database is empty.
          debugTraceMsg dflags 2 $ text "There is no package.cache in"
                               <+> text conf_dir
                                <> text ", checking if the database is empty"
          db_empty <- all (not . isSuffixOf ".conf")
                   <$> getDirectoryContents conf_dir
          if db_empty
            then do
              debugTraceMsg dflags 3 $ text "There are no .conf files in"
                                   <+> text conf_dir <> text ", treating"
                                   <+> text "package database as empty"
              return []
            else do
              throwGhcExceptionIO $ InstallationError $
                "there is no package.cache in " ++ conf_dir ++
                " even though package database is not empty"


    -- Single-file style package dbs have been deprecated for some time, but
    -- it turns out that Cabal was using them in one place. So this is a
    -- workaround to allow older Cabal versions to use this newer ghc.
    -- We check if the file db contains just "[]" and if so, we look for a new
    -- dir-style db in conf_file.d/, ie in a dir next to the given file.
    -- We cannot just replace the file with a new dir style since Cabal still
    -- assumes it's a file and tries to overwrite with 'writeFile'.
    -- ghc-pkg also cooperates with this workaround.
    tryReadOldFileStyleUnitInfo = do
      content <- readFile conf_file `catchIO` \_ -> return ""
      if take 2 content == "[]"
        then do
          let conf_dir = conf_file <.> "d"
          direxists <- doesDirectoryExist conf_dir
          if direxists
             then do debugTraceMsg dflags 2 (text "Ignoring old file-style db and trying:" <+> text conf_dir)
                     liftM Just (readDirStyleUnitInfo conf_dir)
             else return (Just []) -- ghc-pkg will create it when it's updated
        else return Nothing

distrustAllUnits :: [UnitInfo] -> [UnitInfo]
distrustAllUnits pkgs = map distrust pkgs
  where
    distrust pkg = pkg{ unitIsTrusted = False }

mungeUnitInfo :: FilePath -> FilePath
                   -> UnitInfo -> UnitInfo
mungeUnitInfo top_dir pkgroot =
    mungeDynLibFields
  . mungeUnitInfoPaths top_dir pkgroot

mungeDynLibFields :: UnitInfo -> UnitInfo
mungeDynLibFields pkg =
    pkg {
      unitLibraryDynDirs = case unitLibraryDynDirs pkg of
         [] -> unitLibraryDirs pkg
         ds -> ds
    }

-- -----------------------------------------------------------------------------
-- Modify our copy of the package database based on trust flags,
-- -trust and -distrust.

applyTrustFlag
   :: DynFlags
   -> PackagePrecedenceIndex
   -> UnusablePackages
   -> [UnitInfo]
   -> TrustFlag
   -> IO [UnitInfo]
applyTrustFlag dflags prec_map unusable pkgs flag =
  case flag of
    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (map trust ps ++ qs)
          where trust p = p {unitIsTrusted=True}

    DistrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (distrustAllUnits ps ++ qs)

-- | A little utility to tell if the 'thisPackage' is indefinite
-- (if it is not, we should never use on-the-fly renaming.)
isIndefinite :: DynFlags -> Bool
isIndefinite dflags = not (unitIsDefinite (thisPackage dflags))

applyPackageFlag
   :: DynFlags
   -> PackagePrecedenceIndex
   -> UnitInfoMap
   -> UnusablePackages
   -> Bool -- if False, if you expose a package, it implicitly hides
           -- any previously exposed packages with the same name
   -> [UnitInfo]
   -> VisibilityMap           -- Initially exposed
   -> PackageFlag               -- flag to apply
   -> IO VisibilityMap        -- Now exposed

applyPackageFlag dflags prec_map pkg_db unusable no_hide_others pkgs vm flag =
  case flag of
    ExposePackage _ arg (ModRenaming b rns) ->
       case findPackages prec_map pkg_db arg pkgs unusable of
         Left ps         -> packageFlagErr dflags flag ps
         Right (p:_) -> return vm'
          where
           n = fsPackageName p

           -- If a user says @-unit-id p[A=<A>]@, this imposes
           -- a requirement on us: whatever our signature A is,
           -- it must fulfill all of p[A=<A>]:A's requirements.
           -- This method is responsible for computing what our
           -- inherited requirements are.
           reqs | UnitIdArg orig_uid <- arg = collectHoles orig_uid
                | otherwise                 = Map.empty

           collectHoles uid = case uid of
             HoleUnit       -> Map.empty
             RealUnit {}    -> Map.empty -- definite units don't have holes
             VirtUnit indef ->
                  let local = [ Map.singleton
                                  (moduleName mod)
                                  (Set.singleton $ Module indef mod_name)
                              | (mod_name, mod) <- instUnitInsts indef
                              , isHoleModule mod ]
                      recurse = [ collectHoles (moduleUnit mod)
                                | (_, mod) <- instUnitInsts indef ]
                  in Map.unionsWith Set.union $ local ++ recurse

           uv = UnitVisibility
                { uv_expose_all = b
                , uv_renamings = rns
                , uv_package_name = First (Just n)
                , uv_requirements = reqs
                , uv_explicit = True
                }
           vm' = Map.insertWith mappend (mkUnit p) uv vm_cleared
           -- In the old days, if you said `ghc -package p-0.1 -package p-0.2`
           -- (or if p-0.1 was registered in the pkgdb as exposed: True),
           -- the second package flag would override the first one and you
           -- would only see p-0.2 in exposed modules.  This is good for
           -- usability.
           --
           -- However, with thinning and renaming (or Backpack), there might be
           -- situations where you legitimately want to see two versions of a
           -- package at the same time, and this behavior would make it
           -- impossible to do so.  So we decided that if you pass
           -- -hide-all-packages, this should turn OFF the overriding behavior
           -- where an exposed package hides all other packages with the same
           -- name.  This should not affect Cabal at all, which only ever
           -- exposes one package at a time.
           --
           -- NB: Why a variable no_hide_others?  We have to apply this logic to
           -- -plugin-package too, and it's more consistent if the switch in
           -- behavior is based off of
           -- -hide-all-packages/-hide-all-plugin-packages depending on what
           -- flag is in question.
           vm_cleared | no_hide_others = vm
                      -- NB: renamings never clear
                      | (_:_) <- rns = vm
                      | otherwise = Map.filterWithKey
                            (\k uv -> k == mkUnit p
                                   || First (Just n) /= uv_package_name uv) vm
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case findPackages prec_map pkg_db (PackageArg str) pkgs unusable of
         Left ps  -> packageFlagErr dflags flag ps
         Right ps -> return vm'
          where vm' = foldl' (flip Map.delete) vm (map mkUnit ps)

-- | Like 'selectPackages', but doesn't return a list of unmatched
-- packages.  Furthermore, any packages it returns are *renamed*
-- if the 'UnitArg' has a renaming associated with it.
findPackages :: PackagePrecedenceIndex
             -> UnitInfoMap -> PackageArg -> [UnitInfo]
             -> UnusablePackages
             -> Either [(UnitInfo, UnusablePackageReason)]
                [UnitInfo]
findPackages prec_map pkg_db arg pkgs unusable
  = let ps = mapMaybe (finder arg) pkgs
    in if null ps
        then Left (mapMaybe (\(x,y) -> finder arg x >>= \x' -> return (x',y))
                            (Map.elems unusable))
        else Right (sortByPreference prec_map ps)
  where
    finder (PackageArg str) p
      = if str == unitPackageIdString p || str == unitPackageNameString p
          then Just p
          else Nothing
    finder (UnitIdArg uid) p
      = case uid of
          RealUnit (Definite iuid)
            | iuid == unitId p
            -> Just p
          VirtUnit inst
            | indefUnit (instUnitInstanceOf inst) == unitId p
            -> Just (renamePackage pkg_db (instUnitInsts inst) p)
          _ -> Nothing

selectPackages :: PackagePrecedenceIndex -> PackageArg -> [UnitInfo]
               -> UnusablePackages
               -> Either [(UnitInfo, UnusablePackageReason)]
                  ([UnitInfo], [UnitInfo])
selectPackages prec_map arg pkgs unusable
  = let matches = matching arg
        (ps,rest) = partition matches pkgs
    in if null ps
        then Left (filter (matches.fst) (Map.elems unusable))
        else Right (sortByPreference prec_map ps, rest)

-- | Rename a 'UnitInfo' according to some module instantiation.
renamePackage :: UnitInfoMap -> [(ModuleName, Module)]
              -> UnitInfo -> UnitInfo
renamePackage pkg_map insts conf =
    let hsubst = listToUFM insts
        smod  = renameHoleModule' pkg_map hsubst
        new_insts = map (\(k,v) -> (k,smod v)) (unitInstantiations conf)
    in conf {
        unitInstantiations = new_insts,
        unitExposedModules = map (\(mod_name, mb_mod) -> (mod_name, fmap smod mb_mod))
                             (unitExposedModules conf)
    }


-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> UnitInfo -> Bool
matchingStr str p
        =  str == unitPackageIdString p
        || str == unitPackageNameString p

matchingId :: UnitId -> UnitInfo -> Bool
matchingId uid p = uid == unitId p

matching :: PackageArg -> UnitInfo -> Bool
matching (PackageArg str) = matchingStr str
matching (UnitIdArg (RealUnit (Definite uid))) = matchingId uid
matching (UnitIdArg _)  = \_ -> False -- TODO: warn in this case

-- | This sorts a list of packages, putting "preferred" packages first.
-- See 'compareByPreference' for the semantics of "preference".
sortByPreference :: PackagePrecedenceIndex -> [UnitInfo] -> [UnitInfo]
sortByPreference prec_map = sortBy (flip (compareByPreference prec_map))

-- | Returns 'GT' if @pkg@ should be preferred over @pkg'@ when picking
-- which should be "active".  Here is the order of preference:
--
--      1. First, prefer the latest version
--      2. If the versions are the same, prefer the package that
--      came in the latest package database.
--
-- Pursuant to #12518, we could change this policy to, for example, remove
-- the version preference, meaning that we would always prefer the packages
-- in later package database.
--
-- Instead, we use that preference based policy only when one of the packages
-- is integer-gmp and the other is integer-simple.
-- This currently only happens when we're looking up which concrete
-- package to use in place of @integer-wired-in@ and that two different
-- package databases supply a different integer library. For more about
-- the fake @integer-wired-in@ package, see Note [The integer library]
-- in the @GHC.Builtin.Names@ module.
compareByPreference
    :: PackagePrecedenceIndex
    -> UnitInfo
    -> UnitInfo
    -> Ordering
compareByPreference prec_map pkg pkg'
  | Just prec  <- Map.lookup (unitId pkg)  prec_map
  , Just prec' <- Map.lookup (unitId pkg') prec_map
  , differentIntegerPkgs pkg pkg'
  = compare prec prec'

  | otherwise
  = case comparing unitPackageVersion pkg pkg' of
        GT -> GT
        EQ | Just prec  <- Map.lookup (unitId pkg)  prec_map
           , Just prec' <- Map.lookup (unitId pkg') prec_map
           -- Prefer the package from the later DB flag (i.e., higher
           -- precedence)
           -> compare prec prec'
           | otherwise
           -> EQ
        LT -> LT

  where isIntegerPkg p = unitPackageNameString p `elem`
          ["integer-simple", "integer-gmp"]
        differentIntegerPkgs p p' =
          isIntegerPkg p && isIntegerPkg p' &&
          (unitPackageName p /= unitPackageName p')

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

packageFlagErr :: DynFlags
               -> PackageFlag
               -> [(UnitInfo, UnusablePackageReason)]
               -> IO a
packageFlagErr dflags flag reasons
  = packageFlagErr' dflags (pprFlag flag) reasons

trustFlagErr :: DynFlags
             -> TrustFlag
             -> [(UnitInfo, UnusablePackageReason)]
             -> IO a
trustFlagErr dflags flag reasons
  = packageFlagErr' dflags (pprTrustFlag flag) reasons

packageFlagErr' :: DynFlags
               -> SDoc
               -> [(UnitInfo, UnusablePackageReason)]
               -> IO a
packageFlagErr' dflags flag_doc reasons
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ err))
  where err = text "cannot satisfy " <> flag_doc <>
                (if null reasons then Outputable.empty else text ": ") $$
              nest 4 (ppr_reasons $$
                      text "(use -v for more information)")
        ppr_reasons = vcat (map ppr_reason reasons)
        ppr_reason (p, reason) =
            pprReason (ppr (unitId p) <+> text "is") reason

pprFlag :: PackageFlag -> SDoc
pprFlag flag = case flag of
    HidePackage p   -> text "-hide-package " <> text p
    ExposePackage doc _ _ -> text doc

pprTrustFlag :: TrustFlag -> SDoc
pprTrustFlag flag = case flag of
    TrustPackage p    -> text "-trust " <> text p
    DistrustPackage p -> text "-distrust " <> text p

-- -----------------------------------------------------------------------------
-- Wired-in units
--
-- See Note [Wired-in units] in GHC.Unit.Module

type WiredInUnitId = String
type WiredPackagesMap = Map WiredUnitId WiredUnitId

wired_in_unitids :: [WiredInUnitId]
wired_in_unitids = map unitString wiredInUnitIds

findWiredInPackages
   :: DynFlags
   -> PackagePrecedenceIndex
   -> [UnitInfo]           -- database
   -> VisibilityMap             -- info on what packages are visible
                                -- for wired in selection
   -> IO ([UnitInfo],  -- package database updated for wired in
          WiredPackagesMap) -- map from unit id to wired identity

findWiredInPackages dflags prec_map pkgs vis_map = do
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base), as described
  -- in Note [Wired-in units] in GHC.Unit.Module
  let
        matches :: UnitInfo -> WiredInUnitId -> Bool
        pc `matches` pid
            -- See Note [The integer library] in GHC.Builtin.Names
            | pid == unitString integerUnitId
            = unitPackageNameString pc `elem` ["integer-gmp", "integer-simple"]
        pc `matches` pid = unitPackageNameString pc == pid

        -- find which package corresponds to each wired-in package
        -- delete any other packages with the same name
        -- update the package and any dependencies to point to the new
        -- one.
        --
        -- When choosing which package to map to a wired-in package
        -- name, we try to pick the latest version of exposed packages.
        -- However, if there are no exposed wired in packages available
        -- (e.g. -hide-all-packages was used), we can't bail: we *have*
        -- to assign a package for the wired-in package: so we try again
        -- with hidden packages included to (and pick the latest
        -- version).
        --
        -- You can also override the default choice by using -ignore-package:
        -- this works even when there is no exposed wired in package
        -- available.
        --
        findWiredInPackage :: [UnitInfo] -> WiredInUnitId
                           -> IO (Maybe (WiredInUnitId, UnitInfo))
        findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ]
               all_exposed_ps =
                    [ p | p <- all_ps
                        , Map.member (mkUnit p) vis_map ] in
           case all_exposed_ps of
            [] -> case all_ps of
                       []   -> notfound
                       many -> pick (head (sortByPreference prec_map many))
            many -> pick (head (sortByPreference prec_map many))
          where
                notfound = do
                          debugTraceMsg dflags 2 $
                            text "wired-in package "
                                 <> text wired_pkg
                                 <> text " not found."
                          return Nothing
                pick :: UnitInfo
                     -> IO (Maybe (WiredInUnitId, UnitInfo))
                pick pkg = do
                        debugTraceMsg dflags 2 $
                            text "wired-in package "
                                 <> text wired_pkg
                                 <> text " mapped to "
                                 <> ppr (unitId pkg)
                        return (Just (wired_pkg, pkg))


  mb_wired_in_pkgs <- mapM (findWiredInPackage pkgs) wired_in_unitids
  let
        wired_in_pkgs = catMaybes mb_wired_in_pkgs
        pkgstate = pkgState dflags

        -- this is old: we used to assume that if there were
        -- multiple versions of wired-in packages installed that
        -- they were mutually exclusive.  Now we're assuming that
        -- you have one "main" version of each wired-in package
        -- (the latest version), and the others are backward-compat
        -- wrappers that depend on this one.  e.g. base-4.0 is the
        -- latest, base-3.0 is a compat wrapper depending on base-4.0.
        {-
        deleteOtherWiredInPackages pkgs = filterOut bad pkgs
          where bad p = any (p `matches`) wired_in_unitids
                      && package p `notElem` map fst wired_in_ids
        -}

        wiredInMap :: Map WiredUnitId WiredUnitId
        wiredInMap = Map.fromList
          [ (key, Definite (stringToUnitId wiredInUnitId))
          | (wiredInUnitId, pkg) <- wired_in_pkgs
          , Just key <- pure $ definiteUnitInfoId pkg
          ]

        updateWiredInDependencies pkgs = map (upd_deps . upd_pkg) pkgs
          where upd_pkg pkg
                  | Just def_uid <- definiteUnitInfoId pkg
                  , Just wiredInUnitId <- Map.lookup def_uid wiredInMap
                  = let fs = unitIdFS (unDefinite wiredInUnitId)
                    in pkg {
                      unitId = UnitId fs,
                      unitInstanceOf = mkIndefUnitId pkgstate fs
                    }
                  | otherwise
                  = pkg
                upd_deps pkg = pkg {
                      -- temporary harmless DefUnitId invariant violation
                      unitDepends = map (unDefinite . upd_wired_in wiredInMap . Definite) (unitDepends pkg),
                      unitExposedModules
                        = map (\(k,v) -> (k, fmap (upd_wired_in_mod wiredInMap) v))
                              (unitExposedModules pkg)
                    }


  return (updateWiredInDependencies pkgs, wiredInMap)

-- Helper functions for rewiring Module and Unit.  These
-- rewrite Units of modules in wired-in packages to the form known to the
-- compiler, as described in Note [Wired-in units] in GHC.Unit.Module.
--
-- For instance, base-4.9.0.0 will be rewritten to just base, to match
-- what appears in GHC.Builtin.Names.

upd_wired_in_mod :: WiredPackagesMap -> Module -> Module
upd_wired_in_mod wiredInMap (Module uid m) = Module (upd_wired_in_uid wiredInMap uid) m

upd_wired_in_uid :: WiredPackagesMap -> Unit -> Unit
upd_wired_in_uid wiredInMap u = case u of
   HoleUnit           -> HoleUnit
   RealUnit def_uid   -> RealUnit (upd_wired_in wiredInMap def_uid)
   VirtUnit indef_uid ->
      VirtUnit $ mkInstantiatedUnit
        (instUnitInstanceOf indef_uid)
        (map (\(x,y) -> (x,upd_wired_in_mod wiredInMap y)) (instUnitInsts indef_uid))

upd_wired_in :: WiredPackagesMap -> DefUnitId -> DefUnitId
upd_wired_in wiredInMap key
    | Just key' <- Map.lookup key wiredInMap = key'
    | otherwise = key

updateVisibilityMap :: WiredPackagesMap -> VisibilityMap -> VisibilityMap
updateVisibilityMap wiredInMap vis_map = foldl' f vis_map (Map.toList wiredInMap)
  where f vm (from, to) = case Map.lookup (RealUnit from) vis_map of
                    Nothing -> vm
                    Just r -> Map.insert (RealUnit to) r
                                (Map.delete (RealUnit from) vm)


-- ----------------------------------------------------------------------------

-- | The reason why a package is unusable.
data UnusablePackageReason
  = -- | We ignored it explicitly using @-ignore-package@.
    IgnoredWithFlag
    -- | This package transitively depends on a package that was never present
    -- in any of the provided databases.
  | BrokenDependencies   [UnitId]
    -- | This package transitively depends on a package involved in a cycle.
    -- Note that the list of 'UnitId' reports the direct dependencies
    -- of this package that (transitively) depended on the cycle, and not
    -- the actual cycle itself (which we report separately at high verbosity.)
  | CyclicDependencies   [UnitId]
    -- | This package transitively depends on a package which was ignored.
  | IgnoredDependencies  [UnitId]
    -- | This package transitively depends on a package which was
    -- shadowed by an ABI-incompatible package.
  | ShadowedDependencies [UnitId]

instance Outputable UnusablePackageReason where
    ppr IgnoredWithFlag = text "[ignored with flag]"
    ppr (BrokenDependencies uids)   = brackets (text "broken" <+> ppr uids)
    ppr (CyclicDependencies uids)   = brackets (text "cyclic" <+> ppr uids)
    ppr (IgnoredDependencies uids)  = brackets (text "ignored" <+> ppr uids)
    ppr (ShadowedDependencies uids) = brackets (text "shadowed" <+> ppr uids)

type UnusablePackages = Map UnitId
                            (UnitInfo, UnusablePackageReason)

pprReason :: SDoc -> UnusablePackageReason -> SDoc
pprReason pref reason = case reason of
  IgnoredWithFlag ->
      pref <+> text "ignored due to an -ignore-package flag"
  BrokenDependencies deps ->
      pref <+> text "unusable due to missing dependencies:" $$
        nest 2 (hsep (map ppr deps))
  CyclicDependencies deps ->
      pref <+> text "unusable due to cyclic dependencies:" $$
        nest 2 (hsep (map ppr deps))
  IgnoredDependencies deps ->
      pref <+> text ("unusable because the -ignore-package flag was used to " ++
                     "ignore at least one of its dependencies:") $$
        nest 2 (hsep (map ppr deps))
  ShadowedDependencies deps ->
      pref <+> text "unusable due to shadowed dependencies:" $$
        nest 2 (hsep (map ppr deps))

reportCycles :: DynFlags -> [SCC UnitInfo] -> IO ()
reportCycles dflags sccs = mapM_ report sccs
  where
    report (AcyclicSCC _) = return ()
    report (CyclicSCC vs) =
        debugTraceMsg dflags 2 $
          text "these packages are involved in a cycle:" $$
            nest 2 (hsep (map (ppr . unitId) vs))

reportUnusable :: DynFlags -> UnusablePackages -> IO ()
reportUnusable dflags pkgs = mapM_ report (Map.toList pkgs)
  where
    report (ipid, (_, reason)) =
       debugTraceMsg dflags 2 $
         pprReason
           (text "package" <+> ppr ipid <+> text "is") reason

-- ----------------------------------------------------------------------------
--
-- Utilities on the database
--

-- | A reverse dependency index, mapping an 'UnitId' to
-- the 'UnitId's which have a dependency on it.
type RevIndex = Map UnitId [UnitId]

-- | Compute the reverse dependency index of a package database.
reverseDeps :: InstalledPackageIndex -> RevIndex
reverseDeps db = Map.foldl' go Map.empty db
  where
    go r pkg = foldl' (go' (unitId pkg)) r (unitDepends pkg)
    go' from r to = Map.insertWith (++) to [from] r

-- | Given a list of 'UnitId's to remove, a database,
-- and a reverse dependency index (as computed by 'reverseDeps'),
-- remove those packages, plus any packages which depend on them.
-- Returns the pruned database, as well as a list of 'UnitInfo's
-- that was removed.
removePackages :: [UnitId] -> RevIndex
               -> InstalledPackageIndex
               -> (InstalledPackageIndex, [UnitInfo])
removePackages uids index m = go uids (m,[])
  where
    go [] (m,pkgs) = (m,pkgs)
    go (uid:uids) (m,pkgs)
        | Just pkg <- Map.lookup uid m
        = case Map.lookup uid index of
            Nothing    -> go uids (Map.delete uid m, pkg:pkgs)
            Just rdeps -> go (rdeps ++ uids) (Map.delete uid m, pkg:pkgs)
        | otherwise
        = go uids (m,pkgs)

-- | Given a 'UnitInfo' from some 'InstalledPackageIndex',
-- return all entries in 'depends' which correspond to packages
-- that do not exist in the index.
depsNotAvailable :: InstalledPackageIndex
                 -> UnitInfo
                 -> [UnitId]
depsNotAvailable pkg_map pkg = filter (not . (`Map.member` pkg_map)) (unitDepends pkg)

-- | Given a 'UnitInfo' from some 'InstalledPackageIndex'
-- return all entries in 'unitAbiDepends' which correspond to packages
-- that do not exist, OR have mismatching ABIs.
depsAbiMismatch :: InstalledPackageIndex
                -> UnitInfo
                -> [UnitId]
depsAbiMismatch pkg_map pkg = map fst . filter (not . abiMatch) $ unitAbiDepends pkg
  where
    abiMatch (dep_uid, abi)
        | Just dep_pkg <- Map.lookup dep_uid pkg_map
        = unitAbiHash dep_pkg == abi
        | otherwise
        = False

-- -----------------------------------------------------------------------------
-- Ignore packages

ignorePackages :: [IgnorePackageFlag] -> [UnitInfo] -> UnusablePackages
ignorePackages flags pkgs = Map.fromList (concatMap doit flags)
  where
  doit (IgnorePackage str) =
     case partition (matchingStr str) pkgs of
         (ps, _) -> [ (unitId p, (p, IgnoredWithFlag))
                    | p <- ps ]
        -- missing package is not an error for -ignore-package,
        -- because a common usage is to -ignore-package P as
        -- a preventative measure just in case P exists.

-- ----------------------------------------------------------------------------
--
-- Merging databases
--

-- | For each package, a mapping from uid -> i indicates that this
-- package was brought into GHC by the ith @-package-db@ flag on
-- the command line.  We use this mapping to make sure we prefer
-- packages that were defined later on the command line, if there
-- is an ambiguity.
type PackagePrecedenceIndex = Map UnitId Int

-- | Given a list of databases, merge them together, where
-- packages with the same unit id in later databases override
-- earlier ones.  This does NOT check if the resulting database
-- makes sense (that's done by 'validateDatabase').
mergeDatabases :: DynFlags -> [PackageDatabase UnitId]
               -> IO (InstalledPackageIndex, PackagePrecedenceIndex)
mergeDatabases dflags = foldM merge (Map.empty, Map.empty) . zip [1..]
  where
    merge (pkg_map, prec_map) (i, PackageDatabase db_path db) = do
      debugTraceMsg dflags 2 $
          text "loading package database" <+> text db_path
      forM_ (Set.toList override_set) $ \pkg ->
          debugTraceMsg dflags 2 $
              text "package" <+> ppr pkg <+>
              text "overrides a previously defined package"
      return (pkg_map', prec_map')
     where
      db_map = mk_pkg_map db
      mk_pkg_map = Map.fromList . map (\p -> (unitId p, p))

      -- The set of UnitIds which appear in both db and pkgs.  These are the
      -- ones that get overridden.  Compute this just to give some
      -- helpful debug messages at -v2
      override_set :: Set UnitId
      override_set = Set.intersection (Map.keysSet db_map)
                                      (Map.keysSet pkg_map)

      -- Now merge the sets together (NB: in case of duplicate,
      -- first argument preferred)
      pkg_map' :: InstalledPackageIndex
      pkg_map' = Map.union db_map pkg_map

      prec_map' :: PackagePrecedenceIndex
      prec_map' = Map.union (Map.map (const i) db_map) prec_map

-- | Validates a database, removing unusable packages from it
-- (this includes removing packages that the user has explicitly
-- ignored.)  Our general strategy:
--
-- 1. Remove all broken packages (dangling dependencies)
-- 2. Remove all packages that are cyclic
-- 3. Apply ignore flags
-- 4. Remove all packages which have deps with mismatching ABIs
--
validateDatabase :: DynFlags -> InstalledPackageIndex
                 -> (InstalledPackageIndex, UnusablePackages, [SCC UnitInfo])
validateDatabase dflags pkg_map1 =
    (pkg_map5, unusable, sccs)
  where
    ignore_flags = reverse (ignorePackageFlags dflags)

    -- Compute the reverse dependency index
    index = reverseDeps pkg_map1

    -- Helper function
    mk_unusable mk_err dep_matcher m uids =
      Map.fromList [ (unitId pkg, (pkg, mk_err (dep_matcher m pkg)))
                   | pkg <- uids ]

    -- Find broken packages
    directly_broken = filter (not . null . depsNotAvailable pkg_map1)
                             (Map.elems pkg_map1)
    (pkg_map2, broken) = removePackages (map unitId directly_broken) index pkg_map1
    unusable_broken = mk_unusable BrokenDependencies depsNotAvailable pkg_map2 broken

    -- Find recursive packages
    sccs = stronglyConnComp [ (pkg, unitId pkg, unitDepends pkg)
                            | pkg <- Map.elems pkg_map2 ]
    getCyclicSCC (CyclicSCC vs) = map unitId vs
    getCyclicSCC (AcyclicSCC _) = []
    (pkg_map3, cyclic) = removePackages (concatMap getCyclicSCC sccs) index pkg_map2
    unusable_cyclic = mk_unusable CyclicDependencies depsNotAvailable pkg_map3 cyclic

    -- Apply ignore flags
    directly_ignored = ignorePackages ignore_flags (Map.elems pkg_map3)
    (pkg_map4, ignored) = removePackages (Map.keys directly_ignored) index pkg_map3
    unusable_ignored = mk_unusable IgnoredDependencies depsNotAvailable pkg_map4 ignored

    -- Knock out packages whose dependencies don't agree with ABI
    -- (i.e., got invalidated due to shadowing)
    directly_shadowed = filter (not . null . depsAbiMismatch pkg_map4)
                               (Map.elems pkg_map4)
    (pkg_map5, shadowed) = removePackages (map unitId directly_shadowed) index pkg_map4
    unusable_shadowed = mk_unusable ShadowedDependencies depsAbiMismatch pkg_map5 shadowed

    unusable = directly_ignored `Map.union` unusable_ignored
                                `Map.union` unusable_broken
                                `Map.union` unusable_cyclic
                                `Map.union` unusable_shadowed

-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our package
-- settings and populate the package state.

mkPackageState
    :: DynFlags
    -- initial databases, in the order they were specified on
    -- the command line (later databases shadow earlier ones)
    -> [PackageDatabase UnitId]
    -> [PreloadUnitId]              -- preloaded packages
    -> IO (PackageState,
           [PreloadUnitId],         -- new packages to preload
           Maybe [(ModuleName, Module)])

mkPackageState dflags dbs preload0 = do
{-
   Plan.

   There are two main steps for making the package state:

    1. We want to build a single, unified package database based
       on all of the input databases, which upholds the invariant that
       there is only one package per any UnitId and there are no
       dangling dependencies.  We'll do this by merging, and
       then successively filtering out bad dependencies.

       a) Merge all the databases together.
          If an input database defines unit ID that is already in
          the unified database, that package SHADOWS the existing
          package in the current unified database.  Note that
          order is important: packages defined later in the list of
          command line arguments shadow those defined earlier.

       b) Remove all packages with missing dependencies, or
          mutually recursive dependencies.

       b) Remove packages selected by -ignore-package from input database

       c) Remove all packages which depended on packages that are now
          shadowed by an ABI-incompatible package

       d) report (with -v) any packages that were removed by steps 1-3

    2. We want to look at the flags controlling package visibility,
       and build a mapping of what module names are in scope and
       where they live.

       a) on the final, unified database, we apply -trust/-distrust
          flags directly, modifying the database so that the 'trusted'
          field has the correct value.

       b) we use the -package/-hide-package flags to compute a
          visibility map, stating what packages are "exposed" for
          the purposes of computing the module map.
          * if any flag refers to a package which was removed by 1-5, then
            we can give an error message explaining why
          * if -hide-all-packages was not specified, this step also
            hides packages which are superseded by later exposed packages
          * this step is done TWICE if -plugin-package/-hide-all-plugin-packages
            are used

       c) based on the visibility map, we pick wired packages and rewrite
          them to have the expected unitId.

       d) finally, using the visibility map and the package database,
          we build a mapping saying what every in scope module name points to.
-}

  -- This, and the other reverse's that you will see, are due to the fact that
  -- packageFlags, pluginPackageFlags, etc. are all specified in *reverse* order
  -- than they are on the command line.
  let other_flags = reverse (packageFlags dflags)
  debugTraceMsg dflags 2 $
      text "package flags" <+> ppr other_flags

  -- Merge databases together, without checking validity
  (pkg_map1, prec_map) <- mergeDatabases dflags dbs

  -- Now that we've merged everything together, prune out unusable
  -- packages.
  let (pkg_map2, unusable, sccs) = validateDatabase dflags pkg_map1

  reportCycles dflags sccs
  reportUnusable dflags unusable

  -- Apply trust flags (these flags apply regardless of whether
  -- or not packages are visible or not)
  pkgs1 <- foldM (applyTrustFlag dflags prec_map unusable)
                 (Map.elems pkg_map2) (reverse (trustFlags dflags))
  let prelim_pkg_db = extendUnitInfoMap emptyUnitInfoMap pkgs1

  --
  -- Calculate the initial set of units from package databases, prior to any package flags.
  --
  -- Conceptually, we select the latest versions of all valid (not unusable) *packages*
  -- (not units). This is empty if we have -hide-all-packages.
  --
  -- Then we create an initial visibility map with default visibilities for all
  -- exposed, definite units which belong to the latest valid packages.
  --
  let preferLater unit unit' =
        case compareByPreference prec_map unit unit' of
            GT -> unit
            _  -> unit'
      addIfMorePreferable m unit = addToUDFM_C preferLater m (fsPackageName unit) unit
      -- This is the set of maximally preferable packages. In fact, it is a set of
      -- most preferable *units* keyed by package name, which act as stand-ins in
      -- for "a package in a database". We use units here because we don't have
      -- "a package in a database" as a type currently.
      mostPreferablePackageReps = if gopt Opt_HideAllPackages dflags
                    then emptyUDFM
                    else foldl' addIfMorePreferable emptyUDFM pkgs1
      -- When exposing units, we want to consider all of those in the most preferable
      -- packages. We can implement that by looking for units that are equi-preferable
      -- with the most preferable unit for package. Being equi-preferable means that
      -- they must be in the same database, with the same version, and the same package name.
      --
      -- We must take care to consider all these units and not just the most
      -- preferable one, otherwise we can end up with problems like #16228.
      mostPreferable u =
        case lookupUDFM mostPreferablePackageReps (fsPackageName u) of
          Nothing -> False
          Just u' -> compareByPreference prec_map u u' == EQ
      vis_map1 = foldl' (\vm p ->
                            -- Note: we NEVER expose indefinite packages by
                            -- default, because it's almost assuredly not
                            -- what you want (no mix-in linking has occurred).
                            if unitIsExposed p && unitIsDefinite (mkUnit p) && mostPreferable p
                               then Map.insert (mkUnit p)
                                               UnitVisibility {
                                                 uv_expose_all = True,
                                                 uv_renamings = [],
                                                 uv_package_name = First (Just (fsPackageName p)),
                                                 uv_requirements = Map.empty,
                                                 uv_explicit = False
                                               }
                                               vm
                               else vm)
                         Map.empty pkgs1

  --
  -- Compute a visibility map according to the command-line flags (-package,
  -- -hide-package).  This needs to know about the unusable packages, since if a
  -- user tries to enable an unusable package, we should let them know.
  --
  vis_map2 <- foldM (applyPackageFlag dflags prec_map prelim_pkg_db unusable
                        (gopt Opt_HideAllPackages dflags) pkgs1)
                            vis_map1 other_flags

  --
  -- Sort out which packages are wired in. This has to be done last, since
  -- it modifies the unit ids of wired in packages, but when we process
  -- package arguments we need to key against the old versions.
  --
  (pkgs2, wired_map) <- findWiredInPackages dflags prec_map pkgs1 vis_map2
  let pkg_db = extendUnitInfoMap emptyUnitInfoMap pkgs2

  -- Update the visibility map, so we treat wired packages as visible.
  let vis_map = updateVisibilityMap wired_map vis_map2

  let hide_plugin_pkgs = gopt Opt_HideAllPluginPackages dflags
  plugin_vis_map <-
    case pluginPackageFlags dflags of
        -- common case; try to share the old vis_map
        [] | not hide_plugin_pkgs -> return vis_map
           | otherwise -> return Map.empty
        _ -> do let plugin_vis_map1
                        | hide_plugin_pkgs = Map.empty
                        -- Use the vis_map PRIOR to wired in,
                        -- because otherwise applyPackageFlag
                        -- won't work.
                        | otherwise = vis_map2
                plugin_vis_map2
                    <- foldM (applyPackageFlag dflags prec_map prelim_pkg_db unusable
                                (gopt Opt_HideAllPluginPackages dflags) pkgs1)
                             plugin_vis_map1
                             (reverse (pluginPackageFlags dflags))
                -- Updating based on wired in packages is mostly
                -- good hygiene, because it won't matter: no wired in
                -- package has a compiler plugin.
                -- TODO: If a wired in package had a compiler plugin,
                -- and you tried to pick different wired in packages
                -- with the plugin flags and the normal flags... what
                -- would happen?  I don't know!  But this doesn't seem
                -- likely to actually happen.
                return (updateVisibilityMap wired_map plugin_vis_map2)

  --
  -- Here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "preload"
  -- packages.  we link these packages in eagerly.  The preload set
  -- should contain at least rts & base, which is why we pretend that
  -- the command line contains -package rts & -package base.
  --
  -- NB: preload IS important even for type-checking, because we
  -- need the correct include path to be set.
  --
  let preload1 = Map.keys (Map.filter uv_explicit vis_map)

  let pkgname_map = foldl' add Map.empty pkgs2
        where add pn_map p
                = Map.insert (unitPackageName p) (unitInstanceOf p) pn_map

  -- The explicitPackages accurately reflects the set of packages we have turned
  -- on; as such, it also is the only way one can come up with requirements.
  -- The requirement context is directly based off of this: we simply
  -- look for nested unit IDs that are directly fed holes: the requirements
  -- of those units are precisely the ones we need to track
  let explicit_pkgs = Map.keys vis_map
      req_ctx = Map.map (Set.toList)
              $ Map.unionsWith Set.union (map uv_requirements (Map.elems vis_map))


  let preload2 = preload1

  let
      -- add base & rts to the preload packages
      basicLinkedPackages
       | gopt Opt_AutoLinkPackages dflags
          = filter (flip elemUDFM (unUnitInfoMap pkg_db))
                [baseUnitId, rtsUnitId]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      -- (NB: since this is only relevant for base/rts it doesn't matter
      -- that thisUnitIdInsts_ is not wired yet)
      --
      preload3 = ordNub $ filter (/= thisPackage dflags)
                        $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db (zip (map toUnitId preload3) (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let mod_map1 = mkModuleNameProvidersMap dflags pkg_db vis_map
      mod_map2 = mkUnusableModuleNameProvidersMap unusable
      mod_map = Map.union mod_map1 mod_map2

  dumpIfSet_dyn (dflags { pprCols = 200 }) Opt_D_dump_mod_map "Mod Map"
    FormatText
    (pprModuleMap mod_map)

  -- Force pstate to avoid leaking the dflags0 passed to mkPackageState
  let !pstate = PackageState{
    preloadPackages     = dep_preload,
    explicitPackages    = explicit_pkgs,
    unitInfoMap            = pkg_db,
    moduleNameProvidersMap  = mod_map,
    pluginModuleNameProvidersMap = mkModuleNameProvidersMap dflags pkg_db plugin_vis_map,
    packageNameMap          = pkgname_map,
    unwireMap = Map.fromList [ (v,k) | (k,v) <- Map.toList wired_map ],
    requirementContext = req_ctx
    }
  let new_insts = fmap (map (fmap (upd_wired_in_mod wired_map))) (thisUnitIdInsts_ dflags)
  return (pstate, new_dep_preload, new_insts)

-- | Given a wired-in 'Unit', "unwire" it into the 'Unit'
-- that it was recorded as in the package database.
unwireUnit :: DynFlags -> Unit-> Unit
unwireUnit dflags uid@(RealUnit def_uid) =
    maybe uid RealUnit (Map.lookup def_uid (unwireMap (pkgState dflags)))
unwireUnit _ uid = uid

-- -----------------------------------------------------------------------------
-- | Makes the mapping from module to package info

-- Slight irritation: we proceed by leafing through everything
-- in the installed package database, which makes handling indefinite
-- packages a bit bothersome.

mkModuleNameProvidersMap
  :: DynFlags
  -> UnitInfoMap
  -> VisibilityMap
  -> ModuleNameProvidersMap
mkModuleNameProvidersMap dflags pkg_db vis_map =
    -- What should we fold on?  Both situations are awkward:
    --
    --    * Folding on the visibility map means that we won't create
    --      entries for packages that aren't mentioned in vis_map
    --      (e.g., hidden packages, causing #14717)
    --
    --    * Folding on pkg_db is awkward because if we have an
    --      Backpack instantiation, we need to possibly add a
    --      package from pkg_db multiple times to the actual
    --      ModuleNameProvidersMap.  Also, we don't really want
    --      definite package instantiations to show up in the
    --      list of possibilities.
    --
    -- So what will we do instead?  We'll extend vis_map with
    -- entries for every definite (for non-Backpack) and
    -- indefinite (for Backpack) package, so that we get the
    -- hidden entries we need.
    Map.foldlWithKey extend_modmap emptyMap vis_map_extended
 where
  vis_map_extended = Map.union vis_map {- preferred -} default_vis

  default_vis = Map.fromList
                  [ (mkUnit pkg, mempty)
                  | pkg <- eltsUDFM (unUnitInfoMap pkg_db)
                  -- Exclude specific instantiations of an indefinite
                  -- package
                  , unitIsIndefinite pkg || null (unitInstantiations pkg)
                  ]

  emptyMap = Map.empty
  setOrigins m os = fmap (const os) m
  extend_modmap modmap uid
    UnitVisibility { uv_expose_all = b, uv_renamings = rns }
    = addListTo modmap theBindings
   where
    pkg = unit_lookup uid

    theBindings :: [(ModuleName, Map Module ModuleOrigin)]
    theBindings = newBindings b rns

    newBindings :: Bool
                -> [(ModuleName, ModuleName)]
                -> [(ModuleName, Map Module ModuleOrigin)]
    newBindings e rns  = es e ++ hiddens ++ map rnBinding rns

    rnBinding :: (ModuleName, ModuleName)
              -> (ModuleName, Map Module ModuleOrigin)
    rnBinding (orig, new) = (new, setOrigins origEntry fromFlag)
     where origEntry = case lookupUFM esmap orig of
            Just r -> r
            Nothing -> throwGhcException (CmdLineError (showSDoc dflags
                        (text "package flag: could not find module name" <+>
                            ppr orig <+> text "in package" <+> ppr pk)))

    es :: Bool -> [(ModuleName, Map Module ModuleOrigin)]
    es e = do
     (m, exposedReexport) <- exposed_mods
     let (pk', m', origin') =
          case exposedReexport of
           Nothing -> (pk, m, fromExposedModules e)
           Just (Module pk' m') ->
            let pkg' = unit_lookup pk'
            in (pk', m', fromReexportedModules e pkg')
     return (m, mkModMap pk' m' origin')

    esmap :: UniqFM (Map Module ModuleOrigin)
    esmap = listToUFM (es False) -- parameter here doesn't matter, orig will
                                 -- be overwritten

    hiddens = [(m, mkModMap pk m ModHidden) | m <- hidden_mods]

    pk = mkUnit pkg
    unit_lookup uid = lookupUnit' (isIndefinite dflags) pkg_db uid
                        `orElse` pprPanic "unit_lookup" (ppr uid)

    exposed_mods = unitExposedModules pkg
    hidden_mods  = unitHiddenModules pkg

-- | Make a 'ModuleNameProvidersMap' covering a set of unusable packages.
mkUnusableModuleNameProvidersMap :: UnusablePackages -> ModuleNameProvidersMap
mkUnusableModuleNameProvidersMap unusables =
    Map.foldl' extend_modmap Map.empty unusables
 where
    extend_modmap modmap (pkg, reason) = addListTo modmap bindings
      where bindings :: [(ModuleName, Map Module ModuleOrigin)]
            bindings = exposed ++ hidden

            origin = ModUnusable reason
            pkg_id = mkUnit pkg

            exposed = map get_exposed exposed_mods
            hidden = [(m, mkModMap pkg_id m origin) | m <- hidden_mods]

            get_exposed (mod, Just mod') = (mod, Map.singleton mod' origin)
            get_exposed (mod, _)         = (mod, mkModMap pkg_id mod origin)

            exposed_mods = unitExposedModules pkg
            hidden_mods  = unitHiddenModules pkg

-- | Add a list of key/value pairs to a nested map.
--
-- The outer map is processed with 'Data.Map.Strict' to prevent memory leaks
-- when reloading modules in GHCi (see #4029). This ensures that each
-- value is forced before installing into the map.
addListTo :: (Monoid a, Ord k1, Ord k2)
          => Map k1 (Map k2 a)
          -> [(k1, Map k2 a)]
          -> Map k1 (Map k2 a)
addListTo = foldl' merge
  where merge m (k, v) = MapStrict.insertWith (Map.unionWith mappend) k v m

-- | Create a singleton module mapping
mkModMap :: Unit -> ModuleName -> ModuleOrigin -> Map Module ModuleOrigin
mkModMap pkg mod = Map.singleton (mkModule pkg mod)

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of preload (command-line) packages to determine which packages to
-- use.

-- | Find all the include directories in these and the preload packages
getPackageIncludePath :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageIncludePath dflags pkgs =
  collectIncludeDirs `fmap` getPreloadPackagesAnd dflags pkgs

collectIncludeDirs :: [UnitInfo] -> [FilePath]
collectIncludeDirs ps = ordNub (filter notNull (concatMap unitIncludeDirs ps))

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageLibraryPath dflags pkgs =
  collectLibraryPaths dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLibraryPaths :: DynFlags -> [UnitInfo] -> [FilePath]
collectLibraryPaths dflags = ordNub . filter notNull
                           . concatMap (libraryDirsForWay dflags)

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getPackageLinkOpts :: DynFlags -> [PreloadUnitId] -> IO ([String], [String], [String])
getPackageLinkOpts dflags pkgs =
  collectLinkOpts dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLinkOpts :: DynFlags -> [UnitInfo] -> ([String], [String], [String])
collectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . packageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . unitExtDepLibsSys) ps,
        concatMap unitLinkerOptions ps
    )
collectArchives :: DynFlags -> UnitInfo -> IO [FilePath]
collectArchives dflags pc =
  filterM doesFileExist [ searchPath </> ("lib" ++ lib ++ ".a")
                        | searchPath <- searchPaths
                        , lib <- libs ]
  where searchPaths = ordNub . filter notNull . libraryDirsForWay dflags $ pc
        libs        = packageHsLibs dflags pc ++ unitExtDepLibsSys pc

getLibs :: DynFlags -> [PreloadUnitId] -> IO [(String,String)]
getLibs dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  fmap concat . forM ps $ \p -> do
    let candidates = [ (l </> f, f) | l <- collectLibraryPaths dflags [p]
                                    , f <- (\n -> "lib" ++ n ++ ".a") <$> packageHsLibs dflags p ]
    filterM (doesFileExist . fst) candidates

packageHsLibs :: DynFlags -> UnitInfo -> [String]
packageHsLibs dflags p = map (mkDynName . addSuffix) (unitLibraries p)
  where
        ways0 = ways dflags

        ways1 = Set.filter (/= WayDyn) ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        -- debug and profiled RTSs include support for -eventlog
        ways2 | WayDebug `Set.member` ways1 || WayProf `Set.member` ways1
              = Set.filter (/= WayEventLog) ways1
              | otherwise
              = ways1

        tag     = waysTag (Set.filter (not . wayRTSOnly) ways2)
        rts_tag = waysTag ways2

        mkDynName x
         | WayDyn `Set.notMember` ways dflags = x
         | "HS" `isPrefixOf` x                =
              x ++ '-':programName dflags ++ projectVersion dflags
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
        addSuffix rts@"HSrts"    = rts       ++ (expandTag rts_tag)
        addSuffix rts@"HSrts-1.0"= rts       ++ (expandTag rts_tag)
        addSuffix other_lib      = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t

-- | Either the 'unitLibraryDirs' or 'unitLibraryDynDirs' as appropriate for the way.
libraryDirsForWay :: DynFlags -> UnitInfo -> [String]
libraryDirsForWay dflags
  | WayDyn `elem` ways dflags = unitLibraryDynDirs
  | otherwise                 = unitLibraryDirs

-- | Find all the C-compiler options in these and the preload packages
getPackageExtraCcOpts :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap unitCcOptions ps)

-- | Find all the package framework paths in these and the preload packages
getPackageFrameworkPath  :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (ordNub (filter notNull (concatMap unitExtDepFrameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getPackageFrameworks  :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageFrameworks dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap unitExtDepFrameworks ps)

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a 'ModuleName', and if the module is in any package returns
-- list of modules which take that name.
lookupModuleInAllPackages :: DynFlags
                          -> ModuleName
                          -> [(Module, UnitInfo)]
lookupModuleInAllPackages dflags m
  = case lookupModuleWithSuggestions dflags m Nothing of
      LookupFound a b -> [(a,b)]
      LookupMultiple rs -> map f rs
        where f (m,_) = (m, expectJust "lookupModule" (lookupUnit dflags
                                                         (moduleUnit m)))
      _ -> []

-- | The result of performing a lookup
data LookupResult =
    -- | Found the module uniquely, nothing else to do
    LookupFound Module UnitInfo
    -- | Multiple modules with the same name in scope
  | LookupMultiple [(Module, ModuleOrigin)]
    -- | No modules found, but there were some hidden ones with
    -- an exact name match.  First is due to package hidden, second
    -- is due to module being hidden
  | LookupHidden [(Module, ModuleOrigin)] [(Module, ModuleOrigin)]
    -- | No modules found, but there were some unusable ones with
    -- an exact name match
  | LookupUnusable [(Module, ModuleOrigin)]
    -- | Nothing found, here are some suggested different names
  | LookupNotFound [ModuleSuggestion] -- suggestions

data ModuleSuggestion = SuggestVisible ModuleName Module ModuleOrigin
                      | SuggestHidden ModuleName Module ModuleOrigin

lookupModuleWithSuggestions :: DynFlags
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions dflags
  = lookupModuleWithSuggestions' dflags
        (moduleNameProvidersMap (pkgState dflags))

lookupPluginModuleWithSuggestions :: DynFlags
                                  -> ModuleName
                                  -> Maybe FastString
                                  -> LookupResult
lookupPluginModuleWithSuggestions dflags
  = lookupModuleWithSuggestions' dflags
        (pluginModuleNameProvidersMap (pkgState dflags))

lookupModuleWithSuggestions' :: DynFlags
                            -> ModuleNameProvidersMap
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions' dflags mod_map m mb_pn
  = case Map.lookup m mod_map of
        Nothing -> LookupNotFound suggestions
        Just xs ->
          case foldl' classify ([],[],[], []) (Map.toList xs) of
            ([], [], [], []) -> LookupNotFound suggestions
            (_, _, _, [(m, _)])             -> LookupFound m (mod_unit m)
            (_, _, _, exposed@(_:_))        -> LookupMultiple exposed
            ([], [], unusable@(_:_), [])    -> LookupUnusable unusable
            (hidden_pkg, hidden_mod, _, []) ->
              LookupHidden hidden_pkg hidden_mod
  where
    classify (hidden_pkg, hidden_mod, unusable, exposed) (m, origin0) =
      let origin = filterOrigin mb_pn (mod_unit m) origin0
          x = (m, origin)
      in case origin of
          ModHidden
            -> (hidden_pkg, x:hidden_mod, unusable, exposed)
          ModUnusable _
            -> (hidden_pkg, hidden_mod, x:unusable, exposed)
          _ | originEmpty origin
            -> (hidden_pkg,   hidden_mod, unusable, exposed)
            | originVisible origin
            -> (hidden_pkg, hidden_mod, unusable, x:exposed)
            | otherwise
            -> (x:hidden_pkg, hidden_mod, unusable, exposed)

    unit_lookup p = lookupUnit dflags p `orElse` pprPanic "lookupModuleWithSuggestions" (ppr p <+> ppr m)
    mod_unit = unit_lookup . moduleUnit

    -- Filters out origins which are not associated with the given package
    -- qualifier.  No-op if there is no package qualifier.  Test if this
    -- excluded all origins with 'originEmpty'.
    filterOrigin :: Maybe FastString
                 -> UnitInfo
                 -> ModuleOrigin
                 -> ModuleOrigin
    filterOrigin Nothing _ o = o
    filterOrigin (Just pn) pkg o =
      case o of
          ModHidden -> if go pkg then ModHidden else mempty
          (ModUnusable _) -> if go pkg then o else mempty
          ModOrigin { fromOrigPackage = e, fromExposedReexport = res,
                      fromHiddenReexport = rhs }
            -> ModOrigin {
                  fromOrigPackage = if go pkg then e else Nothing
                , fromExposedReexport = filter go res
                , fromHiddenReexport = filter go rhs
                , fromPackageFlag = False -- always excluded
                }
      where go pkg = pn == fsPackageName pkg

    suggestions
      | gopt Opt_HelpfulErrors dflags =
           fuzzyLookup (moduleNameString m) all_mods
      | otherwise = []

    all_mods :: [(String, ModuleSuggestion)]     -- All modules
    all_mods = sortBy (comparing fst) $
        [ (moduleNameString m, suggestion)
        | (m, e) <- Map.toList (moduleNameProvidersMap (pkgState dflags))
        , suggestion <- map (getSuggestion m) (Map.toList e)
        ]
    getSuggestion name (mod, origin) =
        (if originVisible origin then SuggestVisible else SuggestHidden)
            name mod origin

listVisibleModuleNames :: DynFlags -> [ModuleName]
listVisibleModuleNames dflags =
    map fst (filter visible (Map.toList (moduleNameProvidersMap (pkgState dflags))))
  where visible (_, ms) = any originVisible (Map.elems ms)

-- | Find all the 'UnitInfo' in both the preload packages from 'DynFlags' and corresponding to the list of
-- 'UnitInfo's
getPreloadPackagesAnd :: DynFlags -> [PreloadUnitId] -> IO [UnitInfo]
getPreloadPackagesAnd dflags pkgids0 =
  let
      pkgids  = pkgids0 ++
                  -- An indefinite package will have insts to HOLE,
                  -- which is not a real package. Don't look it up.
                  -- Fixes #14525
                  if isIndefinite dflags
                    then []
                    else map (toUnitId . moduleUnit . snd)
                             (thisUnitIdInsts dflags)
      state   = pkgState dflags
      pkg_map = unitInfoMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr dflags (foldM (add_package dflags pkg_map) preload pairs)
  return (map (getInstalledPackageDetails state) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: DynFlags
          -> UnitInfoMap
          -> [(UnitId, Maybe UnitId)]
          -> IO [UnitId]
closeDeps dflags pkg_map ps
    = throwErr dflags (closeDepsErr dflags pkg_map ps)

throwErr :: DynFlags -> MaybeErr MsgDoc a -> IO a
throwErr dflags m
              = case m of
                Failed e    -> throwGhcExceptionIO (CmdLineError (showSDoc dflags e))
                Succeeded r -> return r

closeDepsErr :: DynFlags
             -> UnitInfoMap
             -> [(UnitId,Maybe UnitId)]
             -> MaybeErr MsgDoc [UnitId]
closeDepsErr dflags pkg_map ps = foldM (add_package dflags pkg_map) [] ps

-- internal helper
add_package :: DynFlags
            -> UnitInfoMap
            -> [PreloadUnitId]
            -> (PreloadUnitId,Maybe PreloadUnitId)
            -> MaybeErr MsgDoc [PreloadUnitId]
add_package dflags pkg_db ps (p, mb_parent)
  | p `elem` ps = return ps     -- Check if we've already added this package
  | otherwise =
      case lookupInstalledPackage' pkg_db p of
        Nothing -> Failed (missingPackageMsg p <>
                           missingDependencyMsg mb_parent)
        Just pkg -> do
           -- Add the package's dependents also
           ps' <- foldM add_unit_key ps (unitDepends pkg)
           return (p : ps')
          where
            add_unit_key ps key
              = add_package dflags pkg_db ps (key, Just p)

missingPackageMsg :: Outputable pkgid => pkgid -> SDoc
missingPackageMsg p = text "unknown package:" <+> ppr p

missingDependencyMsg :: Maybe UnitId -> SDoc
missingDependencyMsg Nothing = Outputable.empty
missingDependencyMsg (Just parent)
  = space <> parens (text "dependency of" <+> ftext (unitIdFS parent))

-- -----------------------------------------------------------------------------

-- Cabal packages may contain several components (programs, libraries, etc.).
-- As far as GHC is concerned, installed package components ("units") are
-- identified by an opaque IndefUnitId string provided by Cabal. As the string
-- contains a hash, we don't want to display it to users so GHC queries the
-- database to retrieve some infos about the original source package (name,
-- version, component name).
--
-- Instead we want to display: packagename-version[:componentname]
--
-- Component name is only displayed if it isn't the default library
--
-- To do this we need to query the database (cached in DynFlags). We cache
-- these details in the IndefUnitId itself because we don't want to query
-- DynFlags each time we pretty-print the IndefUnitId
--
mkIndefUnitId :: PackageState -> FastString -> IndefUnitId
mkIndefUnitId pkgstate raw =
    let uid = UnitId raw
    in case lookupInstalledPackage pkgstate uid of
         Nothing -> Indefinite uid Nothing -- we didn't find the unit at all
         Just c  -> Indefinite uid $ Just $ mkUnitPprInfo c

-- | Update component ID details from the database
updateIndefUnitId :: PackageState -> IndefUnitId -> IndefUnitId
updateIndefUnitId pkgstate uid = mkIndefUnitId pkgstate (unitIdFS (indefUnit uid))


displayUnitId :: PackageState -> UnitId -> Maybe String
displayUnitId pkgstate uid =
    fmap unitPackageIdString (lookupInstalledPackage pkgstate uid)

-- -----------------------------------------------------------------------------
-- Displaying packages

-- | Show (very verbose) package info
pprPackages :: PackageState -> SDoc
pprPackages = pprPackagesWith pprUnitInfo

pprPackagesWith :: (UnitInfo -> SDoc) -> PackageState -> SDoc
pprPackagesWith pprIPI pkgstate =
    vcat (intersperse (text "---") (map pprIPI (listUnitInfoMap pkgstate)))

-- | Show simplified package info.
--
-- The idea is to only print package id, and any information that might
-- be different from the package databases (exposure, trust)
pprPackagesSimple :: PackageState -> SDoc
pprPackagesSimple = pprPackagesWith pprIPI
    where pprIPI ipi = let i = unitIdFS (unitId ipi)
                           e = if unitIsExposed ipi then text "E" else text " "
                           t = if unitIsTrusted ipi then text "T" else text " "
                       in e <> t <> text "  " <> ftext i

-- | Show the mapping of modules to where they come from.
pprModuleMap :: ModuleNameProvidersMap -> SDoc
pprModuleMap mod_map =
  vcat (map pprLine (Map.toList mod_map))
    where
      pprLine (m,e) = ppr m $$ nest 50 (vcat (map (pprEntry m) (Map.toList e)))
      pprEntry :: Outputable a => ModuleName -> (Module, a) -> SDoc
      pprEntry m (m',o)
        | m == moduleName m' = ppr (moduleUnit m') <+> parens (ppr o)
        | otherwise = ppr m' <+> parens (ppr o)

fsPackageName :: UnitInfo -> FastString
fsPackageName info = fs
   where
      PackageName fs = unitPackageName info

-- | Given a fully instantiated 'InstantiatedUnit', improve it into a
-- 'RealUnit' if we can find it in the package database.
improveUnit :: UnitInfoMap -> Unit -> Unit
improveUnit _ uid@(RealUnit _) = uid -- short circuit
improveUnit pkg_map uid =
    -- Do NOT lookup indefinite ones, they won't be useful!
    case lookupUnit' False pkg_map uid of
        Nothing  -> uid
        Just pkg ->
            -- Do NOT improve if the indefinite unit id is not
            -- part of the closure unique set.  See
            -- Note [VirtUnit to RealUnit improvement]
            if unitId pkg `elementOfUniqSet` preloadClosure pkg_map
                then mkUnit pkg
                else uid
