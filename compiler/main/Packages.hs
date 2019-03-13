-- (c) The University of Glasgow, 2006

{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns, FlexibleContexts #-}

-- | Package manipulation
module Packages (
        module PackageConfig,

        -- * Reading the package config, and processing cmdline args
        PackageState(preloadPackages, explicitPackages, moduleToPkgConfAll, requirementContext),
        PackageConfigMap,
        emptyPackageState,
        initPackages,
        readPackageConfigs,
        getPackageConfRefs,
        resolvePackageConfig,
        readPackageConfig,
        listPackageConfigMap,

        -- * Querying the package config
        lookupPackage,
        lookupPackage',
        lookupInstalledPackage,
        lookupPackageName,
        improveUnitId,
        searchPackageId,
        getPackageDetails,
        getInstalledPackageDetails,
        componentIdString,
        displayInstalledUnitId,
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
        getPackageConfigMap,
        getPreloadPackagesAnd,

        collectArchives,
        collectIncludeDirs, collectLibraryPaths, collectLinkOpts,
        packageHsLibs, getLibs,

        -- * Utils
        unwireUnitId,
        pprFlag,
        pprPackages,
        pprPackagesSimple,
        pprModuleMap,
        isIndefinite,
        isDllName
    )
where

#include "HsVersions.h"

import GhcPrelude

import GHC.PackageDb
import PackageConfig
import DynFlags
import Name             ( Name, nameModule_maybe )
import UniqFM
import UniqDFM
import UniqSet
import Module
import Util
import Panic
import Platform
import Outputable
import Maybes

import System.Environment ( getEnv )
import FastString
import ErrUtils         ( debugTraceMsg, MsgDoc, dumpIfSet_dyn )
import Exception

import System.Directory
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
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
import Data.Version

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
      , fromExposedReexport :: [PackageConfig]
        -- | Is the module available from a reexport of a hidden package?
      , fromHiddenReexport :: [PackageConfig]
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
                    sep (map (ppr . packageConfigId) res)]) ++
        (if null rhs
            then []
            else [text "hidden reexport by" <+>
                    sep (map (ppr . packageConfigId) res)]) ++
        (if f then [text "package flag"] else [])
        ))

-- | Smart constructor for a module which is in @exposed-modules@.  Takes
-- as an argument whether or not the defining package is exposed.
fromExposedModules :: Bool -> ModuleOrigin
fromExposedModules e = ModOrigin (Just e) [] [] False

-- | Smart constructor for a module which is in @reexported-modules@.  Takes
-- as an argument whether or not the reexporting package is expsed, and
-- also its 'PackageConfig'.
fromReexportedModules :: Bool -> PackageConfig -> ModuleOrigin
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

-- | 'UniqFM' map from 'InstalledUnitId'
type InstalledUnitIdMap = UniqDFM

-- | 'UniqFM' map from 'UnitId' to 'PackageConfig', plus
-- the transitive closure of preload packages.
data PackageConfigMap = PackageConfigMap {
        unPackageConfigMap :: InstalledUnitIdMap PackageConfig,
        -- | The set of transitively reachable packages according
        -- to the explicitly provided command line arguments.
        -- See Note [UnitId to InstalledUnitId improvement]
        preloadClosure :: UniqSet InstalledUnitId
    }

-- | 'UniqFM' map from 'UnitId' to a 'UnitVisibility'.
type VisibilityMap = Map UnitId UnitVisibility

-- | 'UnitVisibility' records the various aspects of visibility of a particular
-- 'UnitId'.
data UnitVisibility = UnitVisibility
    { uv_expose_all :: Bool
      --  ^ Should all modules in exposed-modules should be dumped into scope?
    , uv_renamings :: [(ModuleName, ModuleName)]
      -- ^ Any custom renamings that should bring extra 'ModuleName's into
      -- scope.
    , uv_package_name :: First FastString
      -- ^ The package name is associated with the 'UnitId'.  This is used
      -- to implement legacy behavior where @-package foo-0.1@ implicitly
      -- hides any packages named @foo@
    , uv_requirements :: Map ModuleName (Set IndefModule)
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
type PreloadUnitId = InstalledUnitId

-- | Map from 'ModuleName' to 'Module' to all the origins of the bindings
-- in scope.  The 'PackageConf' is not cached, mostly for convenience reasons
-- (since this is the slow path, we'll just look it up again).
type ModuleToPkgConfAll =
    Map ModuleName (Map Module ModuleOrigin)

data PackageState = PackageState {
  -- | A mapping of 'UnitId' to 'PackageConfig'.  This list is adjusted
  -- so that only valid packages are here.  'PackageConfig' reflects
  -- what was stored *on disk*, except for the 'trusted' flag, which
  -- is adjusted at runtime.  (In particular, some packages in this map
  -- may have the 'exposed' flag be 'False'.)
  pkgIdMap              :: PackageConfigMap,

  -- | A mapping of 'PackageName' to 'ComponentId'.  This is used when
  -- users refer to packages in Backpack includes.
  packageNameMap            :: Map PackageName ComponentId,

  -- | A mapping from wired in names to the original names from the
  -- package database.
  unwireMap :: Map WiredUnitId WiredUnitId,

  -- | The packages we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a package
  -- is always mentioned before the packages it depends on.
  preloadPackages      :: [PreloadUnitId],

  -- | Packages which we explicitly depend on (from a command line flag).
  -- We'll use this to generate version macros.
  explicitPackages      :: [UnitId],

  -- | This is a full map from 'ModuleName' to all modules which may possibly
  -- be providing it.  These providers may be hidden (but we'll still want
  -- to report them in error messages), or it may be an ambiguous import.
  moduleToPkgConfAll    :: !ModuleToPkgConfAll,

  -- | A map, like 'moduleToPkgConfAll', but controlling plugin visibility.
  pluginModuleToPkgConfAll    :: !ModuleToPkgConfAll,

  -- | A map saying, for each requirement, what interfaces must be merged
  -- together when we use them.  For example, if our dependencies
  -- are @p[A=<A>]@ and @q[A=<A>,B=r[C=<A>]:B]@, then the interfaces
  -- to merge for A are @p[A=<A>]:A@, @q[A=<A>,B=r[C=<A>]:B]:A@
  -- and @r[C=<A>]:C@.
  --
  -- There's an entry in this map for each hole in our home library.
  requirementContext :: Map ModuleName [IndefModule]
  }

emptyPackageState :: PackageState
emptyPackageState = PackageState {
    pkgIdMap = emptyPackageConfigMap,
    packageNameMap = Map.empty,
    unwireMap = Map.empty,
    preloadPackages = [],
    explicitPackages = [],
    moduleToPkgConfAll = Map.empty,
    pluginModuleToPkgConfAll = Map.empty,
    requirementContext = Map.empty
    }

type InstalledPackageIndex = Map InstalledUnitId PackageConfig

-- | Empty package configuration map
emptyPackageConfigMap :: PackageConfigMap
emptyPackageConfigMap = PackageConfigMap emptyUDFM emptyUniqSet

-- | Find the package we know about with the given unit id, if any
lookupPackage :: DynFlags -> UnitId -> Maybe PackageConfig
lookupPackage dflags = lookupPackage' (isIndefinite dflags) (pkgIdMap (pkgState dflags))

-- | A more specialized interface, which takes a boolean specifying
-- whether or not to look for on-the-fly renamed interfaces, and
-- just a 'PackageConfigMap' rather than a 'DynFlags' (so it can
-- be used while we're initializing 'DynFlags'
lookupPackage' :: Bool -> PackageConfigMap -> UnitId -> Maybe PackageConfig
lookupPackage' False (PackageConfigMap pkg_map _) uid = lookupUDFM pkg_map uid
lookupPackage' True m@(PackageConfigMap pkg_map _) uid =
    case splitUnitIdInsts uid of
        (iuid, Just indef) ->
            fmap (renamePackage m (indefUnitIdInsts indef))
                 (lookupUDFM pkg_map iuid)
        (_, Nothing) -> lookupUDFM pkg_map uid

{-
-- | Find the indefinite package for a given 'ComponentId'.
-- The way this works is just by fiat'ing that every indefinite package's
-- unit key is precisely its component ID; and that they share uniques.
lookupComponentId :: DynFlags -> ComponentId -> Maybe PackageConfig
lookupComponentId dflags (ComponentId cid_fs) = lookupUDFM pkg_map cid_fs
  where
    PackageConfigMap pkg_map = pkgIdMap (pkgState dflags)
-}

-- | Find the package we know about with the given package name (e.g. @foo@), if any
-- (NB: there might be a locally defined unit name which overrides this)
lookupPackageName :: DynFlags -> PackageName -> Maybe ComponentId
lookupPackageName dflags n = Map.lookup n (packageNameMap (pkgState dflags))

-- | Search for packages with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: DynFlags -> SourcePackageId -> [PackageConfig]
searchPackageId dflags pid = filter ((pid ==) . sourcePackageId)
                               (listPackageConfigMap dflags)

-- | Extends the package configuration map with a list of package configs.
extendPackageConfigMap
   :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPackageConfigMap (PackageConfigMap pkg_map closure) new_pkgs
  = PackageConfigMap (foldl' add pkg_map new_pkgs) closure
    -- We also add the expanded version of the packageConfigId, so that
    -- 'improveUnitId' can find it.
  where add pkg_map p = addToUDFM (addToUDFM pkg_map (expandedPackageConfigId p) p)
                                  (installedPackageConfigId p) p

-- | Looks up the package with the given id in the package state, panicing if it is
-- not found
getPackageDetails :: DynFlags -> UnitId -> PackageConfig
getPackageDetails dflags pid =
    expectJust "getPackageDetails" (lookupPackage dflags pid)

lookupInstalledPackage :: DynFlags -> InstalledUnitId -> Maybe PackageConfig
lookupInstalledPackage dflags uid = lookupInstalledPackage' (pkgIdMap (pkgState dflags)) uid

lookupInstalledPackage' :: PackageConfigMap -> InstalledUnitId -> Maybe PackageConfig
lookupInstalledPackage' (PackageConfigMap db _) uid = lookupUDFM db uid

getInstalledPackageDetails :: DynFlags -> InstalledUnitId -> PackageConfig
getInstalledPackageDetails dflags uid =
    expectJust "getInstalledPackageDetails" $
        lookupInstalledPackage dflags uid

-- | Get a list of entries from the package database.  NB: be careful with
-- this function, although all packages in this map are "visible", this
-- does not imply that the exposed-modules of the package are available
-- (they may have been thinned or renamed).
listPackageConfigMap :: DynFlags -> [PackageConfig]
listPackageConfigMap dflags = eltsUDFM pkg_map
  where
    PackageConfigMap pkg_map _ = pkgIdMap (pkgState dflags)

-- ----------------------------------------------------------------------------
-- Loading the package db files and building up the package state

-- | Call this after 'DynFlags.parseDynFlags'.  It reads the package
-- database files, and sets up various internal tables of package
-- information, according to the package-related flags on the
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
initPackages dflags0 = do
  dflags <- interpretPackageEnv dflags0
  pkg_db <-
    case pkgDatabase dflags of
        Nothing -> readPackageConfigs dflags
        Just db -> return $ map (\(p, pkgs)
                                    -> (p, setBatchPackageFlags dflags pkgs)) db
  (pkg_state, preload, insts)
        <- mkPackageState dflags pkg_db []
  return (dflags{ pkgDatabase = Just pkg_db,
                  pkgState = pkg_state,
                  thisUnitIdInsts_ = insts },
          preload)

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageConfigs :: DynFlags -> IO [(FilePath, [PackageConfig])]
readPackageConfigs dflags = do
  conf_refs <- getPackageConfRefs dflags
  confs     <- liftM catMaybes $ mapM (resolvePackageConfig dflags) conf_refs
  mapM (readPackageConfig dflags) confs


getPackageConfRefs :: DynFlags -> IO [PkgConfRef]
getPackageConfRefs dflags = do
  let system_conf_refs = [UserPkgConf, GlobalPkgConf]

  e_pkg_path <- tryIO (getEnv $ map toUpper (programName dflags) ++ "_PACKAGE_PATH")
  let base_conf_refs = case e_pkg_path of
        Left _ -> system_conf_refs
        Right path
         | not (null path) && isSearchPathSeparator (last path)
         -> map PkgConfFile (splitSearchPath (init path)) ++ system_conf_refs
         | otherwise
         -> map PkgConfFile (splitSearchPath path)

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

  isNotUser UserPkgConf = False
  isNotUser _ = True

  isNotGlobal GlobalPkgConf = False
  isNotGlobal _ = True

resolvePackageConfig :: DynFlags -> PkgConfRef -> IO (Maybe FilePath)
resolvePackageConfig dflags GlobalPkgConf = return $ Just (systemPackageConfig dflags)
-- NB: This logic is reimplemented in Cabal, so if you change it,
-- make sure you update Cabal.  (Or, better yet, dump it in the
-- compiler info so Cabal can use the info.)
resolvePackageConfig dflags UserPkgConf = runMaybeT $ do
  dir <- versionedAppDir dflags
  let pkgconf = dir </> "package.conf.d"
  exist <- tryMaybeT $ doesDirectoryExist pkgconf
  if exist then return pkgconf else mzero
resolvePackageConfig _ (PkgConfFile name) = return $ Just name

readPackageConfig :: DynFlags -> FilePath -> IO (FilePath, [PackageConfig])
readPackageConfig dflags conf_file = do
  isdir <- doesDirectoryExist conf_file

  proto_pkg_configs <-
    if isdir
       then readDirStylePackageConfig conf_file
       else do
            isfile <- doesFileExist conf_file
            if isfile
               then do
                 mpkgs <- tryReadOldFileStylePackageConfig
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
      top_dir = topDir dflags
      pkgroot = takeDirectory conf_file
      pkg_configs1 = map (mungePackageConfig top_dir pkgroot)
                         proto_pkg_configs
      pkg_configs2 = setBatchPackageFlags dflags pkg_configs1
  --
  return (conf_file, pkg_configs2)
  where
    readDirStylePackageConfig conf_dir = do
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
    tryReadOldFileStylePackageConfig = do
      content <- readFile conf_file `catchIO` \_ -> return ""
      if take 2 content == "[]"
        then do
          let conf_dir = conf_file <.> "d"
          direxists <- doesDirectoryExist conf_dir
          if direxists
             then do debugTraceMsg dflags 2 (text "Ignoring old file-style db and trying:" <+> text conf_dir)
                     liftM Just (readDirStylePackageConfig conf_dir)
             else return (Just []) -- ghc-pkg will create it when it's updated
        else return Nothing

setBatchPackageFlags :: DynFlags -> [PackageConfig] -> [PackageConfig]
setBatchPackageFlags dflags pkgs = maybeDistrustAll pkgs
  where
    maybeDistrustAll pkgs'
      | gopt Opt_DistrustAllPackages dflags = map distrust pkgs'
      | otherwise                           = pkgs'

    distrust pkg = pkg{ trusted = False }

mungePackageConfig :: FilePath -> FilePath
                   -> PackageConfig -> PackageConfig
mungePackageConfig top_dir pkgroot =
    mungeDynLibFields
  . mungePackagePaths top_dir pkgroot

mungeDynLibFields :: PackageConfig -> PackageConfig
mungeDynLibFields pkg =
    pkg {
      libraryDynDirs     = libraryDynDirs pkg
                `orIfNull` libraryDirs pkg
    }
  where
    orIfNull [] flags = flags
    orIfNull flags _  = flags

-- TODO: This code is duplicated in utils/ghc-pkg/Main.hs
mungePackagePaths :: FilePath -> FilePath -> PackageConfig -> PackageConfig
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths top_dir pkgroot pkg =
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      libraryDynDirs = munge_paths (libraryDynDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
      haddockHTMLs = munge_urls (haddockHTMLs pkg)
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url

    munge_path p
      | Just p' <- stripVarPrefix "${pkgroot}" p = pkgroot ++ p'
      | Just p' <- stripVarPrefix "$topdir"    p = top_dir ++ p'
      | otherwise                                = p

    munge_url p
      | Just p' <- stripVarPrefix "${pkgrooturl}" p = toUrlPath pkgroot p'
      | Just p' <- stripVarPrefix "$httptopdir"   p = toUrlPath top_dir p'
      | otherwise                                   = p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath
                        (r : -- We need to drop a leading "/" or "\\"
                             -- if there is one:
                             dropWhile (all isPathSeparator)
                                       (FilePath.splitDirectories p))

    -- We could drop the separator here, and then use </> above. However,
    -- by leaving it in and using ++ we keep the same path separator
    -- rather than letting FilePath change it to use \ as the separator
    stripVarPrefix var path = case stripPrefix var path of
                              Just [] -> Just []
                              Just cs@(c : _) | isPathSeparator c -> Just cs
                              _ -> Nothing


-- -----------------------------------------------------------------------------
-- Modify our copy of the package database based on trust flags,
-- -trust and -distrust.

applyTrustFlag
   :: DynFlags
   -> PackagePrecedenceIndex
   -> UnusablePackages
   -> [PackageConfig]
   -> TrustFlag
   -> IO [PackageConfig]
applyTrustFlag dflags prec_map unusable pkgs flag =
  case flag of
    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (map trust ps ++ qs)
          where trust p = p {trusted=True}

    DistrustPackage str ->
       case selectPackages prec_map (PackageArg str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (map distrust ps ++ qs)
          where distrust p = p {trusted=False}

-- | A little utility to tell if the 'thisPackage' is indefinite
-- (if it is not, we should never use on-the-fly renaming.)
isIndefinite :: DynFlags -> Bool
isIndefinite dflags = not (unitIdIsDefinite (thisPackage dflags))

applyPackageFlag
   :: DynFlags
   -> PackagePrecedenceIndex
   -> PackageConfigMap
   -> UnusablePackages
   -> Bool -- if False, if you expose a package, it implicitly hides
           -- any previously exposed packages with the same name
   -> [PackageConfig]
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

           collectHoles uid = case splitUnitIdInsts uid of
                (_, Just indef) ->
                  let local = [ Map.singleton
                                  (moduleName mod)
                                  (Set.singleton $ IndefModule indef mod_name)
                              | (mod_name, mod) <- indefUnitIdInsts indef
                              , isHoleModule mod ]
                      recurse = [ collectHoles (moduleUnitId mod)
                                | (_, mod) <- indefUnitIdInsts indef ]
                  in Map.unionsWith Set.union $ local ++ recurse
                -- Other types of unit identities don't have holes
                (_, Nothing) -> Map.empty


           uv = UnitVisibility
                { uv_expose_all = b
                , uv_renamings = rns
                , uv_package_name = First (Just n)
                , uv_requirements = reqs
                , uv_explicit = True
                }
           vm' = Map.insertWith mappend (packageConfigId p) uv vm_cleared
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
                            (\k uv -> k == packageConfigId p
                                   || First (Just n) /= uv_package_name uv) vm
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case findPackages prec_map pkg_db (PackageArg str) pkgs unusable of
         Left ps  -> packageFlagErr dflags flag ps
         Right ps -> return vm'
          where vm' = foldl' (flip Map.delete) vm (map packageConfigId ps)

-- | Like 'selectPackages', but doesn't return a list of unmatched
-- packages.  Furthermore, any packages it returns are *renamed*
-- if the 'UnitArg' has a renaming associated with it.
findPackages :: PackagePrecedenceIndex
             -> PackageConfigMap -> PackageArg -> [PackageConfig]
             -> UnusablePackages
             -> Either [(PackageConfig, UnusablePackageReason)]
                [PackageConfig]
findPackages prec_map pkg_db arg pkgs unusable
  = let ps = mapMaybe (finder arg) pkgs
    in if null ps
        then Left (mapMaybe (\(x,y) -> finder arg x >>= \x' -> return (x',y))
                            (Map.elems unusable))
        else Right (sortByPreference prec_map ps)
  where
    finder (PackageArg str) p
      = if str == sourcePackageIdString p || str == packageNameString p
          then Just p
          else Nothing
    finder (UnitIdArg uid) p
      = let (iuid, mb_indef) = splitUnitIdInsts uid
        in if iuid == installedPackageConfigId p
              then Just (case mb_indef of
                            Nothing    -> p
                            Just indef -> renamePackage pkg_db (indefUnitIdInsts indef) p)
              else Nothing

selectPackages :: PackagePrecedenceIndex -> PackageArg -> [PackageConfig]
               -> UnusablePackages
               -> Either [(PackageConfig, UnusablePackageReason)]
                  ([PackageConfig], [PackageConfig])
selectPackages prec_map arg pkgs unusable
  = let matches = matching arg
        (ps,rest) = partition matches pkgs
    in if null ps
        then Left (filter (matches.fst) (Map.elems unusable))
        else Right (sortByPreference prec_map ps, rest)

-- | Rename a 'PackageConfig' according to some module instantiation.
renamePackage :: PackageConfigMap -> [(ModuleName, Module)]
              -> PackageConfig -> PackageConfig
renamePackage pkg_map insts conf =
    let hsubst = listToUFM insts
        smod  = renameHoleModule' pkg_map hsubst
        new_insts = map (\(k,v) -> (k,smod v)) (instantiatedWith conf)
    in conf {
        instantiatedWith = new_insts,
        exposedModules = map (\(mod_name, mb_mod) -> (mod_name, fmap smod mb_mod))
                             (exposedModules conf)
    }


-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> PackageConfig -> Bool
matchingStr str p
        =  str == sourcePackageIdString p
        || str == packageNameString p

matchingId :: InstalledUnitId -> PackageConfig -> Bool
matchingId uid p = uid == installedPackageConfigId p

matching :: PackageArg -> PackageConfig -> Bool
matching (PackageArg str) = matchingStr str
matching (UnitIdArg (DefiniteUnitId (DefUnitId uid)))  = matchingId uid
matching (UnitIdArg _)  = \_ -> False -- TODO: warn in this case

-- | This sorts a list of packages, putting "preferred" packages first.
-- See 'compareByPreference' for the semantics of "preference".
sortByPreference :: PackagePrecedenceIndex -> [PackageConfig] -> [PackageConfig]
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
-- in the @PrelNames@ module.
compareByPreference
    :: PackagePrecedenceIndex
    -> PackageConfig
    -> PackageConfig
    -> Ordering
compareByPreference prec_map pkg pkg'
  | Just prec  <- Map.lookup (unitId pkg)  prec_map
  , Just prec' <- Map.lookup (unitId pkg') prec_map
  , differentIntegerPkgs pkg pkg'
  = compare prec prec'

  | otherwise
  = case comparing packageVersion pkg pkg' of
        GT -> GT
        EQ | Just prec  <- Map.lookup (unitId pkg)  prec_map
           , Just prec' <- Map.lookup (unitId pkg') prec_map
           -- Prefer the package from the later DB flag (i.e., higher
           -- precedence)
           -> compare prec prec'
           | otherwise
           -> EQ
        LT -> LT

  where isIntegerPkg p = packageNameString p `elem`
          ["integer-simple", "integer-gmp"]
        differentIntegerPkgs p p' =
          isIntegerPkg p && isIntegerPkg p' &&
          (packageName p /= packageName p')

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

packageFlagErr :: DynFlags
               -> PackageFlag
               -> [(PackageConfig, UnusablePackageReason)]
               -> IO a
packageFlagErr dflags flag reasons
  = packageFlagErr' dflags (pprFlag flag) reasons

trustFlagErr :: DynFlags
             -> TrustFlag
             -> [(PackageConfig, UnusablePackageReason)]
             -> IO a
trustFlagErr dflags flag reasons
  = packageFlagErr' dflags (pprTrustFlag flag) reasons

packageFlagErr' :: DynFlags
               -> SDoc
               -> [(PackageConfig, UnusablePackageReason)]
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
-- Wired-in packages
--
-- See Note [Wired-in packages] in Module

type WiredInUnitId = String
type WiredPackagesMap = Map WiredUnitId WiredUnitId

wired_in_pkgids :: [WiredInUnitId]
wired_in_pkgids = map unitIdString wiredInUnitIds

findWiredInPackages
   :: DynFlags
   -> PackagePrecedenceIndex
   -> [PackageConfig]           -- database
   -> VisibilityMap             -- info on what packages are visible
                                -- for wired in selection
   -> IO ([PackageConfig],  -- package database updated for wired in
          WiredPackagesMap) -- map from unit id to wired identity

findWiredInPackages dflags prec_map pkgs vis_map = do
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base), as described
  -- in Note [Wired-in packages] in Module
  let
        matches :: PackageConfig -> WiredInUnitId -> Bool
        pc `matches` pid
            -- See Note [The integer library] in PrelNames
            | pid == unitIdString integerUnitId
            = packageNameString pc `elem` ["integer-gmp", "integer-simple"]
        pc `matches` pid = packageNameString pc == pid

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
        findWiredInPackage :: [PackageConfig] -> WiredInUnitId
                           -> IO (Maybe (WiredInUnitId, PackageConfig))
        findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ]
               all_exposed_ps =
                    [ p | p <- all_ps
                        , Map.member (packageConfigId p) vis_map ] in
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
                pick :: PackageConfig
                     -> IO (Maybe (WiredInUnitId, PackageConfig))
                pick pkg = do
                        debugTraceMsg dflags 2 $
                            text "wired-in package "
                                 <> text wired_pkg
                                 <> text " mapped to "
                                 <> ppr (unitId pkg)
                        return (Just (wired_pkg, pkg))


  mb_wired_in_pkgs <- mapM (findWiredInPackage pkgs) wired_in_pkgids
  let
        wired_in_pkgs = catMaybes mb_wired_in_pkgs

        -- this is old: we used to assume that if there were
        -- multiple versions of wired-in packages installed that
        -- they were mutually exclusive.  Now we're assuming that
        -- you have one "main" version of each wired-in package
        -- (the latest version), and the others are backward-compat
        -- wrappers that depend on this one.  e.g. base-4.0 is the
        -- latest, base-3.0 is a compat wrapper depending on base-4.0.
        {-
        deleteOtherWiredInPackages pkgs = filterOut bad pkgs
          where bad p = any (p `matches`) wired_in_pkgids
                      && package p `notElem` map fst wired_in_ids
        -}

        wiredInMap :: Map WiredUnitId WiredUnitId
        wiredInMap = Map.fromList
          [ (key, DefUnitId (stringToInstalledUnitId wiredInUnitId))
          | (wiredInUnitId, pkg) <- wired_in_pkgs
          , Just key <- pure $ definitePackageConfigId pkg
          ]

        updateWiredInDependencies pkgs = map (upd_deps . upd_pkg) pkgs
          where upd_pkg pkg
                  | Just def_uid <- definitePackageConfigId pkg
                  , Just wiredInUnitId <- Map.lookup def_uid wiredInMap
                  = let fs = installedUnitIdFS (unDefUnitId wiredInUnitId)
                    in pkg {
                      unitId = fsToInstalledUnitId fs,
                      componentId = ComponentId fs
                    }
                  | otherwise
                  = pkg
                upd_deps pkg = pkg {
                      -- temporary harmless DefUnitId invariant violation
                      depends = map (unDefUnitId . upd_wired_in wiredInMap . DefUnitId) (depends pkg),
                      exposedModules
                        = map (\(k,v) -> (k, fmap (upd_wired_in_mod wiredInMap) v))
                              (exposedModules pkg)
                    }


  return (updateWiredInDependencies pkgs, wiredInMap)

-- Helper functions for rewiring Module and UnitId.  These
-- rewrite UnitIds of modules in wired-in packages to the form known to the
-- compiler, as described in Note [Wired-in packages] in Module.
--
-- For instance, base-4.9.0.0 will be rewritten to just base, to match
-- what appears in PrelNames.

upd_wired_in_mod :: WiredPackagesMap -> Module -> Module
upd_wired_in_mod wiredInMap (Module uid m) = Module (upd_wired_in_uid wiredInMap uid) m

upd_wired_in_uid :: WiredPackagesMap -> UnitId -> UnitId
upd_wired_in_uid wiredInMap (DefiniteUnitId def_uid) =
    DefiniteUnitId (upd_wired_in wiredInMap def_uid)
upd_wired_in_uid wiredInMap (IndefiniteUnitId indef_uid) =
    IndefiniteUnitId $ newIndefUnitId
        (indefUnitIdComponentId indef_uid)
        (map (\(x,y) -> (x,upd_wired_in_mod wiredInMap y)) (indefUnitIdInsts indef_uid))

upd_wired_in :: WiredPackagesMap -> DefUnitId -> DefUnitId
upd_wired_in wiredInMap key
    | Just key' <- Map.lookup key wiredInMap = key'
    | otherwise = key

updateVisibilityMap :: WiredPackagesMap -> VisibilityMap -> VisibilityMap
updateVisibilityMap wiredInMap vis_map = foldl' f vis_map (Map.toList wiredInMap)
  where f vm (from, to) = case Map.lookup (DefiniteUnitId from) vis_map of
                    Nothing -> vm
                    Just r -> Map.insert (DefiniteUnitId to) r
                                (Map.delete (DefiniteUnitId from) vm)


-- ----------------------------------------------------------------------------

-- | The reason why a package is unusable.
data UnusablePackageReason
  = -- | We ignored it explicitly using @-ignore-package@.
    IgnoredWithFlag
    -- | This package transitively depends on a package that was never present
    -- in any of the provided databases.
  | BrokenDependencies   [InstalledUnitId]
    -- | This package transitively depends on a package involved in a cycle.
    -- Note that the list of 'InstalledUnitId' reports the direct dependencies
    -- of this package that (transitively) depended on the cycle, and not
    -- the actual cycle itself (which we report separately at high verbosity.)
  | CyclicDependencies   [InstalledUnitId]
    -- | This package transitively depends on a package which was ignored.
  | IgnoredDependencies  [InstalledUnitId]
    -- | This package transitively depends on a package which was
    -- shadowed by an ABI-incompatible package.
  | ShadowedDependencies [InstalledUnitId]

instance Outputable UnusablePackageReason where
    ppr IgnoredWithFlag = text "[ignored with flag]"
    ppr (BrokenDependencies uids)   = brackets (text "broken" <+> ppr uids)
    ppr (CyclicDependencies uids)   = brackets (text "cyclic" <+> ppr uids)
    ppr (IgnoredDependencies uids)  = brackets (text "ignored" <+> ppr uids)
    ppr (ShadowedDependencies uids) = brackets (text "shadowed" <+> ppr uids)

type UnusablePackages = Map InstalledUnitId
                            (PackageConfig, UnusablePackageReason)

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

reportCycles :: DynFlags -> [SCC PackageConfig] -> IO ()
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

-- | A reverse dependency index, mapping an 'InstalledUnitId' to
-- the 'InstalledUnitId's which have a dependency on it.
type RevIndex = Map InstalledUnitId [InstalledUnitId]

-- | Compute the reverse dependency index of a package database.
reverseDeps :: InstalledPackageIndex -> RevIndex
reverseDeps db = Map.foldl' go Map.empty db
  where
    go r pkg = foldl' (go' (unitId pkg)) r (depends pkg)
    go' from r to = Map.insertWith (++) to [from] r

-- | Given a list of 'InstalledUnitId's to remove, a database,
-- and a reverse dependency index (as computed by 'reverseDeps'),
-- remove those packages, plus any packages which depend on them.
-- Returns the pruned database, as well as a list of 'PackageConfig's
-- that was removed.
removePackages :: [InstalledUnitId] -> RevIndex
               -> InstalledPackageIndex
               -> (InstalledPackageIndex, [PackageConfig])
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

-- | Given a 'PackageConfig' from some 'InstalledPackageIndex',
-- return all entries in 'depends' which correspond to packages
-- that do not exist in the index.
depsNotAvailable :: InstalledPackageIndex
                 -> PackageConfig
                 -> [InstalledUnitId]
depsNotAvailable pkg_map pkg = filter (not . (`Map.member` pkg_map)) (depends pkg)

-- | Given a 'PackageConfig' from some 'InstalledPackageIndex'
-- return all entries in 'abiDepends' which correspond to packages
-- that do not exist, OR have mismatching ABIs.
depsAbiMismatch :: InstalledPackageIndex
                -> PackageConfig
                -> [InstalledUnitId]
depsAbiMismatch pkg_map pkg = map fst . filter (not . abiMatch) $ abiDepends pkg
  where
    abiMatch (dep_uid, abi)
        | Just dep_pkg <- Map.lookup dep_uid pkg_map
        = abiHash dep_pkg == abi
        | otherwise
        = False

-- -----------------------------------------------------------------------------
-- Ignore packages

ignorePackages :: [IgnorePackageFlag] -> [PackageConfig] -> UnusablePackages
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
type PackagePrecedenceIndex = Map InstalledUnitId Int

-- | Given a list of databases, merge them together, where
-- packages with the same unit id in later databases override
-- earlier ones.  This does NOT check if the resulting database
-- makes sense (that's done by 'validateDatabase').
mergeDatabases :: DynFlags -> [(FilePath, [PackageConfig])]
               -> IO (InstalledPackageIndex, PackagePrecedenceIndex)
mergeDatabases dflags = foldM merge (Map.empty, Map.empty) . zip [1..]
  where
    merge (pkg_map, prec_map) (i, (db_path, db)) = do
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
      override_set :: Set InstalledUnitId
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
                 -> (InstalledPackageIndex, UnusablePackages, [SCC PackageConfig])
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
    sccs = stronglyConnComp [ (pkg, unitId pkg, depends pkg)
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
    -> [(FilePath, [PackageConfig])]
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
          * if -hide-all-packages what not specified, this step also
            hides packages which are superseded by later exposed packages
          * this step is done TWICE if -plugin-package/-hide-all-plugin-packages
            are used

       c) based on the visibility map, we pick wired packages and rewrite
          them to have the expected unitId.

       d) finally, using the visibility map and the package database,
          we build a mapping saying what every in scope module name points to.
-}

  -- This, and the other reverse's that you will see, are due to the face that
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
  let prelim_pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs1

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
      -- they must be in the same database, with the same version, and the same pacakge name.
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
                            if exposed p && unitIdIsDefinite (packageConfigId p) && mostPreferable p
                               then Map.insert (packageConfigId p)
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
  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs2

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
                = Map.insert (packageName p) (componentId p) pn_map

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
          = filter (flip elemUDFM (unPackageConfigMap pkg_db))
                [baseUnitId, rtsUnitId]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      -- (NB: since this is only relevant for base/rts it doesn't matter
      -- that thisUnitIdInsts_ is not wired yet)
      --
      preload3 = nub $ filter (/= thisPackage dflags)
                     $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db (zip (map toInstalledUnitId preload3) (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let mod_map1 = mkModuleToPkgConfAll dflags pkg_db vis_map
      mod_map2 = mkUnusableModuleToPkgConfAll unusable
      mod_map = Map.union mod_map1 mod_map2

  dumpIfSet_dyn (dflags { pprCols = 200 }) Opt_D_dump_mod_map "Mod Map"
    (pprModuleMap mod_map)

  -- Force pstate to avoid leaking the dflags0 passed to mkPackageState
  let !pstate = PackageState{
    preloadPackages     = dep_preload,
    explicitPackages    = explicit_pkgs,
    pkgIdMap            = pkg_db,
    moduleToPkgConfAll  = mod_map,
    pluginModuleToPkgConfAll = mkModuleToPkgConfAll dflags pkg_db plugin_vis_map,
    packageNameMap          = pkgname_map,
    unwireMap = Map.fromList [ (v,k) | (k,v) <- Map.toList wired_map ],
    requirementContext = req_ctx
    }
  let new_insts = fmap (map (fmap (upd_wired_in_mod wired_map))) (thisUnitIdInsts_ dflags)
  return (pstate, new_dep_preload, new_insts)

-- | Given a wired-in 'UnitId', "unwire" it into the 'UnitId'
-- that it was recorded as in the package database.
unwireUnitId :: DynFlags -> UnitId -> UnitId
unwireUnitId dflags uid@(DefiniteUnitId def_uid) =
    maybe uid DefiniteUnitId (Map.lookup def_uid (unwireMap (pkgState dflags)))
unwireUnitId _ uid = uid

-- -----------------------------------------------------------------------------
-- | Makes the mapping from module to package info

-- Slight irritation: we proceed by leafing through everything
-- in the installed package database, which makes handling indefinite
-- packages a bit bothersome.

mkModuleToPkgConfAll
  :: DynFlags
  -> PackageConfigMap
  -> VisibilityMap
  -> ModuleToPkgConfAll
mkModuleToPkgConfAll dflags pkg_db vis_map =
    -- What should we fold on?  Both situations are awkward:
    --
    --    * Folding on the visibility map means that we won't create
    --      entries for packages that aren't mentioned in vis_map
    --      (e.g., hidden packages, causing #14717)
    --
    --    * Folding on pkg_db is awkward because if we have an
    --      Backpack instantiation, we need to possibly add a
    --      package from pkg_db multiple times to the actual
    --      ModuleToPkgConfAll.  Also, we don't really want
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
                  [ (packageConfigId pkg, mempty)
                  | pkg <- eltsUDFM (unPackageConfigMap pkg_db)
                  -- Exclude specific instantiations of an indefinite
                  -- package
                  , indefinite pkg || null (instantiatedWith pkg)
                  ]

  emptyMap = Map.empty
  setOrigins m os = fmap (const os) m
  extend_modmap modmap uid
    UnitVisibility { uv_expose_all = b, uv_renamings = rns }
    = addListTo modmap theBindings
   where
    pkg = pkg_lookup uid

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
            let pkg' = pkg_lookup pk'
            in (pk', m', fromReexportedModules e pkg')
     return (m, mkModMap pk' m' origin')

    esmap :: UniqFM (Map Module ModuleOrigin)
    esmap = listToUFM (es False) -- parameter here doesn't matter, orig will
                                 -- be overwritten

    hiddens = [(m, mkModMap pk m ModHidden) | m <- hidden_mods]

    pk = packageConfigId pkg
    pkg_lookup uid = lookupPackage' (isIndefinite dflags) pkg_db uid
                        `orElse` pprPanic "pkg_lookup" (ppr uid)

    exposed_mods = exposedModules pkg
    hidden_mods = hiddenModules pkg

-- | Make a 'ModuleToPkgConfAll' covering a set of unusable packages.
mkUnusableModuleToPkgConfAll :: UnusablePackages -> ModuleToPkgConfAll
mkUnusableModuleToPkgConfAll unusables =
    Map.foldl' extend_modmap Map.empty unusables
 where
    extend_modmap modmap (pkg, reason) = addListTo modmap bindings
      where bindings :: [(ModuleName, Map Module ModuleOrigin)]
            bindings = exposed ++ hidden

            origin = ModUnusable reason
            pkg_id = packageConfigId pkg

            exposed = map get_exposed exposed_mods
            hidden = [(m, mkModMap pkg_id m origin) | m <- hidden_mods]

            get_exposed (mod, Just mod') = (mod, Map.singleton mod' origin)
            get_exposed (mod, _)         = (mod, mkModMap pkg_id mod origin)

            exposed_mods = exposedModules pkg
            hidden_mods = hiddenModules pkg

-- | Add a list of key/value pairs to a nested map.
--
-- The outer map is processed with 'Data.Map.Strict' to prevent memory leaks
-- when reloading modules in GHCi (see Trac #4029). This ensures that each
-- value is forced before installing into the map.
addListTo :: (Monoid a, Ord k1, Ord k2)
          => Map k1 (Map k2 a)
          -> [(k1, Map k2 a)]
          -> Map k1 (Map k2 a)
addListTo = foldl' merge
  where merge m (k, v) = MapStrict.insertWith (Map.unionWith mappend) k v m

-- | Create a singleton module mapping
mkModMap :: UnitId -> ModuleName -> ModuleOrigin -> Map Module ModuleOrigin
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

collectIncludeDirs :: [PackageConfig] -> [FilePath]
collectIncludeDirs ps = nub (filter notNull (concatMap includeDirs ps))

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageLibraryPath dflags pkgs =
  collectLibraryPaths dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLibraryPaths :: DynFlags -> [PackageConfig] -> [FilePath]
collectLibraryPaths dflags = nub . filter notNull
                           . concatMap (libraryDirsForWay dflags)

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getPackageLinkOpts :: DynFlags -> [PreloadUnitId] -> IO ([String], [String], [String])
getPackageLinkOpts dflags pkgs =
  collectLinkOpts dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLinkOpts :: DynFlags -> [PackageConfig] -> ([String], [String], [String])
collectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . packageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . extraLibraries) ps,
        concatMap ldOptions ps
    )
collectArchives :: DynFlags -> PackageConfig -> IO [FilePath]
collectArchives dflags pc =
  filterM doesFileExist [ searchPath </> ("lib" ++ lib ++ ".a")
                        | searchPath <- searchPaths
                        , lib <- libs ]
  where searchPaths = nub . filter notNull . libraryDirsForWay dflags $ pc
        libs        = packageHsLibs dflags pc ++ extraLibraries pc

getLibs :: DynFlags -> [PreloadUnitId] -> IO [(String,String)]
getLibs dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  fmap concat . forM ps $ \p -> do
    let candidates = [ (l </> f, f) | l <- collectLibraryPaths dflags [p]
                                    , f <- (\n -> "lib" ++ n ++ ".a") <$> packageHsLibs dflags p ]
    filterM (doesFileExist . fst) candidates

packageHsLibs :: DynFlags -> PackageConfig -> [String]
packageHsLibs dflags p = map (mkDynName . addSuffix) (hsLibraries p)
  where
        ways0 = ways dflags

        ways1 = filter (/= WayDyn) ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        -- debug and profiled RTSs include support for -eventlog
        ways2 | WayDebug `elem` ways1 || WayProf `elem` ways1
              = filter (/= WayEventLog) ways1
              | otherwise
              = ways1

        tag     = mkBuildTag (filter (not . wayRTSOnly) ways2)
        rts_tag = mkBuildTag ways2

        mkDynName x
         | WayDyn `notElem` ways dflags = x
         | "HS" `isPrefixOf` x          =
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

-- | Either the 'libraryDirs' or 'libraryDynDirs' as appropriate for the way.
libraryDirsForWay :: DynFlags -> PackageConfig -> [String]
libraryDirsForWay dflags
  | WayDyn `elem` ways dflags = libraryDynDirs
  | otherwise                 = libraryDirs

-- | Find all the C-compiler options in these and the preload packages
getPackageExtraCcOpts :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap ccOptions ps)

-- | Find all the package framework paths in these and the preload packages
getPackageFrameworkPath  :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap frameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getPackageFrameworks  :: DynFlags -> [PreloadUnitId] -> IO [String]
getPackageFrameworks dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap frameworks ps)

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a 'ModuleName', and if the module is in any package returns
-- list of modules which take that name.
lookupModuleInAllPackages :: DynFlags
                          -> ModuleName
                          -> [(Module, PackageConfig)]
lookupModuleInAllPackages dflags m
  = case lookupModuleWithSuggestions dflags m Nothing of
      LookupFound a b -> [(a,b)]
      LookupMultiple rs -> map f rs
        where f (m,_) = (m, expectJust "lookupModule" (lookupPackage dflags
                                                         (moduleUnitId m)))
      _ -> []

-- | The result of performing a lookup
data LookupResult =
    -- | Found the module uniquely, nothing else to do
    LookupFound Module PackageConfig
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
        (moduleToPkgConfAll (pkgState dflags))

lookupPluginModuleWithSuggestions :: DynFlags
                                  -> ModuleName
                                  -> Maybe FastString
                                  -> LookupResult
lookupPluginModuleWithSuggestions dflags
  = lookupModuleWithSuggestions' dflags
        (pluginModuleToPkgConfAll (pkgState dflags))

lookupModuleWithSuggestions' :: DynFlags
                            -> ModuleToPkgConfAll
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions' dflags mod_map m mb_pn
  = case Map.lookup m mod_map of
        Nothing -> LookupNotFound suggestions
        Just xs ->
          case foldl' classify ([],[],[], []) (Map.toList xs) of
            ([], [], [], []) -> LookupNotFound suggestions
            (_, _, _, [(m, _)])             -> LookupFound m (mod_pkg m)
            (_, _, _, exposed@(_:_))        -> LookupMultiple exposed
            ([], [], unusable@(_:_), [])    -> LookupUnusable unusable
            (hidden_pkg, hidden_mod, _, []) ->
              LookupHidden hidden_pkg hidden_mod
  where
    classify (hidden_pkg, hidden_mod, unusable, exposed) (m, origin0) =
      let origin = filterOrigin mb_pn (mod_pkg m) origin0
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

    pkg_lookup p = lookupPackage dflags p `orElse` pprPanic "lookupModuleWithSuggestions" (ppr p <+> ppr m)
    mod_pkg = pkg_lookup . moduleUnitId

    -- Filters out origins which are not associated with the given package
    -- qualifier.  No-op if there is no package qualifier.  Test if this
    -- excluded all origins with 'originEmpty'.
    filterOrigin :: Maybe FastString
                 -> PackageConfig
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
        | (m, e) <- Map.toList (moduleToPkgConfAll (pkgState dflags))
        , suggestion <- map (getSuggestion m) (Map.toList e)
        ]
    getSuggestion name (mod, origin) =
        (if originVisible origin then SuggestVisible else SuggestHidden)
            name mod origin

listVisibleModuleNames :: DynFlags -> [ModuleName]
listVisibleModuleNames dflags =
    map fst (filter visible (Map.toList (moduleToPkgConfAll (pkgState dflags))))
  where visible (_, ms) = any originVisible (Map.elems ms)

-- | Find all the 'PackageConfig' in both the preload packages from 'DynFlags' and corresponding to the list of
-- 'PackageConfig's
getPreloadPackagesAnd :: DynFlags -> [PreloadUnitId] -> IO [PackageConfig]
getPreloadPackagesAnd dflags pkgids0 =
  let
      pkgids  = pkgids0 ++
                  -- An indefinite package will have insts to HOLE,
                  -- which is not a real package. Don't look it up.
                  -- Fixes #14525
                  if isIndefinite dflags
                    then []
                    else map (toInstalledUnitId . moduleUnitId . snd)
                             (thisUnitIdInsts dflags)
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr dflags (foldM (add_package dflags pkg_map) preload pairs)
  return (map (getInstalledPackageDetails dflags) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: DynFlags
          -> PackageConfigMap
          -> [(InstalledUnitId, Maybe InstalledUnitId)]
          -> IO [InstalledUnitId]
closeDeps dflags pkg_map ps
    = throwErr dflags (closeDepsErr dflags pkg_map ps)

throwErr :: DynFlags -> MaybeErr MsgDoc a -> IO a
throwErr dflags m
              = case m of
                Failed e    -> throwGhcExceptionIO (CmdLineError (showSDoc dflags e))
                Succeeded r -> return r

closeDepsErr :: DynFlags
             -> PackageConfigMap
             -> [(InstalledUnitId,Maybe InstalledUnitId)]
             -> MaybeErr MsgDoc [InstalledUnitId]
closeDepsErr dflags pkg_map ps = foldM (add_package dflags pkg_map) [] ps

-- internal helper
add_package :: DynFlags
            -> PackageConfigMap
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
           ps' <- foldM add_unit_key ps (depends pkg)
           return (p : ps')
          where
            add_unit_key ps key
              = add_package dflags pkg_db ps (key, Just p)

missingPackageMsg :: Outputable pkgid => pkgid -> SDoc
missingPackageMsg p = text "unknown package:" <+> ppr p

missingDependencyMsg :: Maybe InstalledUnitId -> SDoc
missingDependencyMsg Nothing = Outputable.empty
missingDependencyMsg (Just parent)
  = space <> parens (text "dependency of" <+> ftext (installedUnitIdFS parent))

-- -----------------------------------------------------------------------------

componentIdString :: DynFlags -> ComponentId -> Maybe String
componentIdString dflags cid = do
    conf <- lookupInstalledPackage dflags (componentIdToInstalledUnitId cid)
    return $
        case sourceLibName conf of
            Nothing -> sourcePackageIdString conf
            Just (PackageName libname) ->
                packageNameString conf
                    ++ "-" ++ showVersion (packageVersion conf)
                    ++ ":" ++ unpackFS libname

displayInstalledUnitId :: DynFlags -> InstalledUnitId -> Maybe String
displayInstalledUnitId dflags uid =
    fmap sourcePackageIdString (lookupInstalledPackage dflags uid)

-- | Will the 'Name' come from a dynamically linked library?
isDllName :: DynFlags -> Module -> Name -> Bool
-- Despite the "dll", I think this function just means that
-- the symbol comes from another dynamically-linked package,
-- and applies on all platforms, not just Windows
isDllName dflags this_mod name
  | not (gopt Opt_ExternalDynamicRefs dflags) = False
  | Just mod <- nameModule_maybe name
    -- Issue #8696 - when GHC is dynamically linked, it will attempt
    -- to load the dynamic dependencies of object files at compile
    -- time for things like QuasiQuotes or
    -- TemplateHaskell. Unfortunately, this interacts badly with
    -- intra-package linking, because we don't generate indirect
    -- (dynamic) symbols for intra-package calls. This means that if a
    -- module with an intra-package call is loaded without its
    -- dependencies, then GHC fails to link. This is the cause of #
    --
    -- In the mean time, always force dynamic indirections to be
    -- generated: when the module name isn't the module being
    -- compiled, references are dynamic.
    = case platformOS $ targetPlatform dflags of
        -- On Windows the hack for #8696 makes it unlinkable.
        -- As the entire setup of the code from Cmm down to the RTS expects
        -- the use of trampolines for the imported functions only when
        -- doing intra-package linking, e.g. refering to a symbol defined in the same
        -- package should not use a trampoline.
        -- I much rather have dynamic TH not supported than the entire Dynamic linking
        -- not due to a hack.
        -- Also not sure this would break on Windows anyway.
        OSMinGW32 -> moduleUnitId mod /= moduleUnitId this_mod

        -- For the other platforms, still perform the hack
        _         -> mod /= this_mod

  | otherwise = False  -- no, it is not even an external name

-- -----------------------------------------------------------------------------
-- Displaying packages

-- | Show (very verbose) package info
pprPackages :: DynFlags -> SDoc
pprPackages = pprPackagesWith pprPackageConfig

pprPackagesWith :: (PackageConfig -> SDoc) -> DynFlags -> SDoc
pprPackagesWith pprIPI dflags =
    vcat (intersperse (text "---") (map pprIPI (listPackageConfigMap dflags)))

-- | Show simplified package info.
--
-- The idea is to only print package id, and any information that might
-- be different from the package databases (exposure, trust)
pprPackagesSimple :: DynFlags -> SDoc
pprPackagesSimple = pprPackagesWith pprIPI
    where pprIPI ipi = let i = installedUnitIdFS (unitId ipi)
                           e = if exposed ipi then text "E" else text " "
                           t = if trusted ipi then text "T" else text " "
                       in e <> t <> text "  " <> ftext i

-- | Show the mapping of modules to where they come from.
pprModuleMap :: ModuleToPkgConfAll -> SDoc
pprModuleMap mod_map =
  vcat (map pprLine (Map.toList mod_map))
    where
      pprLine (m,e) = ppr m $$ nest 50 (vcat (map (pprEntry m) (Map.toList e)))
      pprEntry :: Outputable a => ModuleName -> (Module, a) -> SDoc
      pprEntry m (m',o)
        | m == moduleName m' = ppr (moduleUnitId m') <+> parens (ppr o)
        | otherwise = ppr m' <+> parens (ppr o)

fsPackageName :: PackageConfig -> FastString
fsPackageName = mkFastString . packageNameString

-- | Given a fully instantiated 'UnitId', improve it into a
-- 'InstalledUnitId' if we can find it in the package database.
improveUnitId :: PackageConfigMap -> UnitId -> UnitId
improveUnitId _ uid@(DefiniteUnitId _) = uid -- short circuit
improveUnitId pkg_map uid =
    -- Do NOT lookup indefinite ones, they won't be useful!
    case lookupPackage' False pkg_map uid of
        Nothing  -> uid
        Just pkg ->
            -- Do NOT improve if the indefinite unit id is not
            -- part of the closure unique set.  See
            -- Note [UnitId to InstalledUnitId improvement]
            if installedPackageConfigId pkg `elementOfUniqSet` preloadClosure pkg_map
                then packageConfigId pkg
                else uid

-- | Retrieve the 'PackageConfigMap' from 'DynFlags'; used
-- in the @hs-boot@ loop-breaker.
getPackageConfigMap :: DynFlags -> PackageConfigMap
getPackageConfigMap = pkgIdMap . pkgState
