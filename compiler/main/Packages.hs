-- (c) The University of Glasgow, 2006

{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns #-}

-- | Package manipulation
module Packages (
        module PackageConfig,

        -- * Reading the package config, and processing cmdline args
        PackageState(preloadPackages, explicitPackages),
        emptyPackageState,
        initPackages,
        readPackageConfigs,
        getPackageConfRefs,
        resolvePackageConfig,
        readPackageConfig,
        listPackageConfigMap,

        -- * Querying the package config
        lookupPackage,
        searchPackageId,
        getPackageDetails,
        listVisibleModuleNames,
        lookupModuleInAllPackages,
        lookupModuleWithSuggestions,
        lookupPluginModuleWithSuggestions,
        LookupResult(..),
        ModuleSuggestion(..),
        ModuleOrigin(..),

        -- * Inspecting the set of packages in scope
        getPackageIncludePath,
        getPackageLibraryPath,
        getPackageLinkOpts,
        getPackageExtraCcOpts,
        getPackageFrameworkPath,
        getPackageFrameworks,
        getPreloadPackagesAnd,

        collectIncludeDirs, collectLibraryPaths, collectLinkOpts,
        packageHsLibs,

        -- * Utils
        unitIdPackageIdString,
        pprFlag,
        pprPackages,
        pprPackagesSimple,
        pprModuleMap,
        isDllName
    )
where

#include "HsVersions.h"

import GHC.PackageDb
import PackageConfig
import DynFlags
import Name             ( Name, nameModule_maybe )
import UniqFM
import UniqDFM
import Module
import Util
import Panic
import Outputable
import Maybes

import System.Environment ( getEnv )
import FastString
import ErrUtils         ( debugTraceMsg, MsgDoc )
import Exception
import Unique

import System.Directory
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import Control.Monad
import Data.Char ( toUpper )
import Data.List as List
import Data.Map (Map)
import Data.Set (Set)
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid hiding ((<>))
#endif
#if __GLASGOW_HASKELL__ > 710
import Data.Semigroup   ( Semigroup )
import qualified Data.Semigroup as Semigroup
#endif
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import qualified FiniteMap as Map
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
--   * When searching for a module from an preload import declaration,
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

#if __GLASGOW_HASKELL__ > 710
instance Semigroup ModuleOrigin where
    ModOrigin e res rhs f <> ModOrigin e' res' rhs' f' =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = panic "ModOrigin: package both exposed/hidden"
            g Nothing x = x
            g x Nothing = x
    _x <> _y = panic "ModOrigin: hidden module redefined"
#endif

instance Monoid ModuleOrigin where
    mempty = ModOrigin Nothing [] [] False
    mappend (ModOrigin e res rhs f) (ModOrigin e' res' rhs' f') =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = panic "ModOrigin: package both exposed/hidden"
            g Nothing x = x
            g x Nothing = x
    mappend _ _ = panic "ModOrigin: hidden module redefined"

-- | Is the name from the import actually visible? (i.e. does it cause
-- ambiguity, or is it only relevant when we're making suggestions?)
originVisible :: ModuleOrigin -> Bool
originVisible ModHidden = False
originVisible (ModOrigin b res _ f) = b == Just True || not (null res) || f

-- | Are there actually no providers for this module?  This will never occur
-- except when we're filtering based on package imports.
originEmpty :: ModuleOrigin -> Bool
originEmpty (ModOrigin Nothing [] [] False) = True
originEmpty _ = False

-- | 'UniqFM' map from 'UnitId'
type UnitIdMap = UniqDFM

-- | 'UniqFM' map from 'UnitId' to 'PackageConfig'
type PackageConfigMap = UnitIdMap PackageConfig

-- | 'UniqFM' map from 'UnitId' to (1) whether or not all modules which
-- are exposed should be dumped into scope, (2) any custom renamings that
-- should also be apply, and (3) what package name is associated with the
-- key, if it might be hidden
type VisibilityMap =
    UnitIdMap (Bool, [(ModuleName, ModuleName)], FastString)

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

  -- | The packages we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a package
  -- is always mentioned before the packages it depends on.
  preloadPackages      :: [UnitId],

  -- | Packages which we explicitly depend on (from a command line flag).
  -- We'll use this to generate version macros.
  explicitPackages      :: [UnitId],

  -- | This is a full map from 'ModuleName' to all modules which may possibly
  -- be providing it.  These providers may be hidden (but we'll still want
  -- to report them in error messages), or it may be an ambiguous import.
  moduleToPkgConfAll    :: !ModuleToPkgConfAll,

  -- | A map, like 'moduleToPkgConfAll', but controlling plugin visibility.
  pluginModuleToPkgConfAll    :: !ModuleToPkgConfAll
  }

emptyPackageState :: PackageState
emptyPackageState = PackageState {
    pkgIdMap = emptyPackageConfigMap,
    preloadPackages = [],
    explicitPackages = [],
    moduleToPkgConfAll = Map.empty,
    pluginModuleToPkgConfAll = Map.empty
    }

type InstalledPackageIndex = Map UnitId PackageConfig

-- | Empty package configuration map
emptyPackageConfigMap :: PackageConfigMap
emptyPackageConfigMap = emptyUDFM

-- | Find the package we know about with the given key (e.g. @foo_HASH@), if any
lookupPackage :: DynFlags -> UnitId -> Maybe PackageConfig
lookupPackage dflags = lookupPackage' (pkgIdMap (pkgState dflags))

lookupPackage' :: PackageConfigMap -> UnitId -> Maybe PackageConfig
lookupPackage' = lookupUDFM

-- | Search for packages with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: DynFlags -> SourcePackageId -> [PackageConfig]
searchPackageId dflags pid = filter ((pid ==) . sourcePackageId)
                               (listPackageConfigMap dflags)

-- | Extends the package configuration map with a list of package configs.
extendPackageConfigMap
   :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPackageConfigMap pkg_map new_pkgs
  = foldl add pkg_map new_pkgs
  where add pkg_map p = addToUDFM pkg_map (packageConfigId p) p

-- | Looks up the package with the given id in the package state, panicing if it is
-- not found
getPackageDetails :: DynFlags -> UnitId -> PackageConfig
getPackageDetails dflags pid =
    expectJust "getPackageDetails" (lookupPackage dflags pid)

-- | Get a list of entries from the package database.  NB: be careful with
-- this function, although all packages in this map are "visible", this
-- does not imply that the exposed-modules of the package are available
-- (they may have been thinned or renamed).
listPackageConfigMap :: DynFlags -> [PackageConfig]
listPackageConfigMap dflags = eltsUDFM (pkgIdMap (pkgState dflags))

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
initPackages :: DynFlags -> IO (DynFlags, [UnitId])
initPackages dflags0 = do
  dflags <- interpretPackageEnv dflags0
  pkg_db <-
    case pkgDatabase dflags of
        Nothing -> readPackageConfigs dflags
        Just db -> return $ map (\(p, pkgs)
                                    -> (p, setBatchPackageFlags dflags pkgs)) db
  (pkg_state, preload, this_pkg)
        <- mkPackageState dflags pkg_db []
  return (dflags{ pkgDatabase = Just pkg_db,
                  pkgState = pkg_state,
                  thisPackage = this_pkg },
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

  return $ reverse (extraPkgConfs dflags base_conf_refs)
  -- later packages shadow earlier ones.  extraPkgConfs
  -- is in the opposite order to the flags on the
  -- command line.

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
      debugTraceMsg dflags 2 (text "Using binary package database:" <+> text filename)
      readPackageDbForGhc filename

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
   -> UnusablePackages
   -> [PackageConfig]
   -> TrustFlag
   -> IO [PackageConfig]
applyTrustFlag dflags unusable pkgs flag =
  case flag of
    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (map trust ps ++ qs)
          where trust p = p {trusted=True}

    DistrustPackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> trustFlagErr dflags flag ps
         Right (ps,qs) -> return (map distrust ps ++ qs)
          where distrust p = p {trusted=False}

applyPackageFlag
   :: DynFlags
   -> UnusablePackages
   -> Bool -- if False, if you expose a package, it implicitly hides
           -- any previously exposed packages with the same name
   -> [PackageConfig]
   -> VisibilityMap           -- Initially exposed
   -> PackageFlag               -- flag to apply
   -> IO VisibilityMap        -- Now exposed

applyPackageFlag dflags unusable no_hide_others pkgs vm flag =
  case flag of
    ExposePackage _ arg (ModRenaming b rns) ->
       case selectPackages (matching arg) pkgs unusable of
         Left ps         -> packageFlagErr dflags flag ps
         Right (p:_,_) -> return vm'
          where
           n = fsPackageName p
           vm' = addToUDFM_C edit vm_cleared (packageConfigId p) (b, rns, n)
           edit (b, rns, n) (b', rns', _) = (b || b', rns ++ rns', n)
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
                      | otherwise = filterUDFM_Directly
                            (\k (_,_,n') -> k == getUnique (packageConfigId p)
                                                || n /= n') vm
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> packageFlagErr dflags flag ps
         Right (ps,_) -> return vm'
          where vm' = delListFromUDFM vm (map packageConfigId ps)

selectPackages :: (PackageConfig -> Bool) -> [PackageConfig]
               -> UnusablePackages
               -> Either [(PackageConfig, UnusablePackageReason)]
                  ([PackageConfig], [PackageConfig])
selectPackages matches pkgs unusable
  = let (ps,rest) = partition matches pkgs
    in if null ps
        then Left (filter (matches.fst) (Map.elems unusable))
        -- NB: packages from later package databases are LATER
        -- in the list.  We want to prefer the latest package.
        else Right (sortByVersion (reverse ps), rest)

-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> PackageConfig -> Bool
matchingStr str p
        =  str == sourcePackageIdString p
        || str == packageNameString p

matchingId :: String -> PackageConfig -> Bool
matchingId str p = str == unitIdString (packageConfigId p)

matching :: PackageArg -> PackageConfig -> Bool
matching (PackageArg str) = matchingStr str
matching (UnitIdArg str)  = matchingId str

sortByVersion :: [PackageConfig] -> [PackageConfig]
sortByVersion = sortBy (flip (comparing packageVersion))

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

packageFlagErr :: DynFlags
               -> PackageFlag
               -> [(PackageConfig, UnusablePackageReason)]
               -> IO a

-- for missing DPH package we emit a more helpful error message, because
-- this may be the result of using -fdph-par or -fdph-seq.
packageFlagErr dflags (ExposePackage _ (PackageArg pkg) _) []
  | is_dph_package pkg
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ dph_err))
  where dph_err = text "the " <> text pkg <> text " package is not installed."
                  $$ text "To install it: \"cabal install dph\"."
        is_dph_package pkg = "dph" `isPrefixOf` pkg
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

wired_in_pkgids :: [String]
wired_in_pkgids = map unitIdString wiredInUnitIds

type WiredPackagesMap = Map UnitId UnitId

findWiredInPackages
   :: DynFlags
   -> [PackageConfig]           -- database
   -> VisibilityMap             -- info on what packages are visible
                                -- for wired in selection
   -> IO ([PackageConfig],  -- package database updated for wired in
          WiredPackagesMap) -- map from unit id to wired identity

findWiredInPackages dflags pkgs vis_map = do
  --
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base).
  --
  let
        matches :: PackageConfig -> String -> Bool
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
        findWiredInPackage :: [PackageConfig] -> String
                           -> IO (Maybe PackageConfig)
        findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ]
               all_exposed_ps =
                    [ p | p <- all_ps
                        , elemUDFM (packageConfigId p) vis_map ] in
           case all_exposed_ps of
            [] -> case all_ps of
                       []   -> notfound
                       many -> pick (head (sortByVersion many))
            many -> pick (head (sortByVersion many))
          where
                notfound = do
                          debugTraceMsg dflags 2 $
                            text "wired-in package "
                                 <> text wired_pkg
                                 <> text " not found."
                          return Nothing
                pick :: PackageConfig
                     -> IO (Maybe PackageConfig)
                pick pkg = do
                        debugTraceMsg dflags 2 $
                            text "wired-in package "
                                 <> text wired_pkg
                                 <> text " mapped to "
                                 <> ppr (unitId pkg)
                        return (Just pkg)


  mb_wired_in_pkgs <- mapM (findWiredInPackage pkgs) wired_in_pkgids
  let
        wired_in_pkgs = catMaybes mb_wired_in_pkgs
        wired_in_ids = map unitId wired_in_pkgs

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

        wiredInMap :: Map UnitId UnitId
        wiredInMap = foldl' add_mapping Map.empty pkgs
          where add_mapping m pkg
                  | let key = unitId pkg
                  , key `elem` wired_in_ids
                  = Map.insert key (stringToUnitId (packageNameString pkg)) m
                  | otherwise = m

        updateWiredInDependencies pkgs = map (upd_deps . upd_pkg) pkgs
          where upd_pkg pkg
                  | unitId pkg `elem` wired_in_ids
                  = pkg {
                      unitId = stringToUnitId (packageNameString pkg)
                    }
                  | otherwise
                  = pkg
                upd_deps pkg = pkg {
                      depends = map upd_wired_in (depends pkg),
                      exposedModules
                        = map (\(ExposedModule k v) ->
                                (ExposedModule k (fmap upd_wired_in_mod v)))
                              (exposedModules pkg)
                    }
                upd_wired_in_mod (OriginalModule uid m)
                    = OriginalModule (upd_wired_in uid) m
                upd_wired_in key
                    | Just key' <- Map.lookup key wiredInMap = key'
                    | otherwise = key


  return (updateWiredInDependencies pkgs, wiredInMap)

updateVisibilityMap :: WiredPackagesMap -> VisibilityMap -> VisibilityMap
updateVisibilityMap wiredInMap vis_map = foldl' f vis_map (Map.toList wiredInMap)
  where f vm (from, to) = case lookupUDFM vis_map from of
                    Nothing -> vm
                    Just r -> addToUDFM vm to r


-- ----------------------------------------------------------------------------

data UnusablePackageReason
  = IgnoredWithFlag
  | MissingDependencies [UnitId]

type UnusablePackages = Map UnitId
                            (PackageConfig, UnusablePackageReason)

pprReason :: SDoc -> UnusablePackageReason -> SDoc
pprReason pref reason = case reason of
  IgnoredWithFlag ->
      pref <+> text "ignored due to an -ignore-package flag"
  MissingDependencies deps ->
      pref <+> text "unusable due to missing or recursive dependencies:" $$
        nest 2 (hsep (map ppr deps))

reportUnusable :: DynFlags -> UnusablePackages -> IO ()
reportUnusable dflags pkgs = mapM_ report (Map.toList pkgs)
  where
    report (ipid, (_, reason)) =
       debugTraceMsg dflags 2 $
         pprReason
           (text "package" <+> ppr ipid <+> text "is") reason

-- ----------------------------------------------------------------------------
--
-- Detect any packages that have missing dependencies, and also any
-- mutually-recursive groups of packages (loops in the package graph
-- are not allowed).  We do this by taking the least fixpoint of the
-- dependency graph, repeatedly adding packages whose dependencies are
-- satisfied until no more can be added.
--
findBroken :: [PackageConfig]
           -> Map UnitId PackageConfig
           -> UnusablePackages
findBroken pkgs pkg_map0 = go [] pkg_map0 pkgs
 where
   go avail pkg_map not_avail =
     case partitionWith (depsAvailable pkg_map) not_avail of
        ([], not_avail) ->
            Map.fromList [ (unitId p, (p, MissingDependencies deps))
                         | (p,deps) <- not_avail ]
        (new_avail, not_avail) ->
            go (new_avail ++ avail) pkg_map' (map fst not_avail)
            where pkg_map' = Map.insertList
                             [ (unitId p, p) | p <- new_avail ]
                             pkg_map

   depsAvailable :: InstalledPackageIndex
                 -> PackageConfig
                 -> Either PackageConfig (PackageConfig, [UnitId])
   depsAvailable pkg_map pkg
        | null dangling = Left pkg
        | otherwise     = Right (pkg, dangling)
        where dangling = filter (not . (`Map.member` pkg_map)) (depends pkg)

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

-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our package
-- settings and populate the package state.

mkPackageState
    :: DynFlags
    -> [(FilePath, [PackageConfig])]     -- initial databases
    -> [UnitId]              -- preloaded packages
    -> IO (PackageState,
           [UnitId],         -- new packages to preload
           UnitId) -- this package, might be modified if the current
                      -- package is a wired-in package.

mkPackageState dflags dbs preload0 = do
  -- Compute the unit id
  let this_package = thisPackage dflags

{-
   Plan.

   There are two main steps for making the package state:

    1. We want to build a single, unified package database based
       on all of the input databases, which upholds the invariant that
       there is only one package per any UnitId, and that there are no
       dangling dependencies. We'll do this by successively merging each
       input database into this unified database:

       a) if an input database defines unit ID that is already in
          the unified database, that package SHADOWS the existing
          package in the current unified database
            * for every such shadowed package, we remove it and any
              packages which transitively depend on it from the
              unified datbase

       b) remove packages selected by -ignore-package from input database

       c) remove any packages with missing dependencies or mutually recursive
          dependencies from the input database

       d) report (with -v) any packages that were removed by steps 1-3

       e) merge the input database into the unified database

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

  let other_flags = reverse (packageFlags dflags)
      ignore_flags = reverse (ignorePackageFlags dflags)

  let merge pkg_map (db_path, db) = do
        debugTraceMsg dflags 2 $
            text "loading package database" <+> text db_path
        forM_ (Set.toList override_set) $ \pkg ->
            debugTraceMsg dflags 2 $
                text "package" <+> ppr pkg <+>
                text "overrides a previously defined package"
        return pkg_map'
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
        pkg_map' :: Map UnitId PackageConfig
        pkg_map' = Map.union db_map pkg_map

  pkg_map1 <- foldM merge Map.empty dbs

  -- Now that we've merged everything together, prune out unusable
  -- packages.
  let pkgs01 = Map.elems pkg_map1

      -- First, apply ignore flags to db
      ignored = ignorePackages ignore_flags pkgs01
      pkgs02 = filter (not . (`Map.member` ignored) . unitId) pkgs01

      -- Look for broken packages (either from ignore, or possibly
      -- because the db was broken to begin with)
      broken = findBroken pkgs02 Map.empty
      pkgs03 = filter (not . (`Map.member` broken) . unitId) pkgs02

      unusable = ignored `Map.union` broken

  reportUnusable dflags unusable

  -- Apply trust flags (these flags apply regardless of whether
  -- or not packages are visible or not)
  pkgs1 <- foldM (applyTrustFlag dflags unusable)
                 pkgs03 (reverse (trustFlags dflags))

  --
  -- Calculate the initial set of packages, prior to any package flags.
  -- This set contains the latest version of all valid (not unusable) packages,
  -- or is empty if we have -hide-all-packages
  --
  let preferLater pkg pkg' =
        case comparing packageVersion pkg pkg' of
            GT -> pkg
            _  -> pkg'
      calcInitial m pkg = addToUDFM_C preferLater m (fsPackageName pkg) pkg
      initial = if gopt Opt_HideAllPackages dflags
                    then emptyUDFM
                    else foldl' calcInitial emptyUDFM pkgs1
      vis_map1 = foldUDFM (\p vm ->
                            if exposed p
                               then addToUDFM vm (packageConfigId p)
                                              (True, [], fsPackageName p)
                               else vm)
                         emptyUDFM initial

  --
  -- Compute a visibility map according to the command-line flags (-package,
  -- -hide-package).  This needs to know about the unusable packages, since if a
  -- user tries to enable an unusable package, we should let them know.
  --
  vis_map2 <- foldM (applyPackageFlag dflags unusable
                        (gopt Opt_HideAllPackages dflags) pkgs1)
                            vis_map1 other_flags

  --
  -- Sort out which packages are wired in. This has to be done last, since
  -- it modifies the unit ids of wired in packages, but when we process
  -- package arguments we need to key against the old versions.
  --
  (pkgs2, wired_map) <- findWiredInPackages dflags pkgs1 vis_map2

  -- Update the visibility map, so we treat wired packages as visible.
  let vis_map = updateVisibilityMap wired_map vis_map2

  let hide_plugin_pkgs = gopt Opt_HideAllPluginPackages dflags
  plugin_vis_map <-
    case pluginPackageFlags dflags of
        -- common case; try to share the old vis_map
        [] | not hide_plugin_pkgs -> return vis_map
           | otherwise -> return emptyUDFM
        _ -> do let plugin_vis_map1
                        | hide_plugin_pkgs = emptyUDFM
                        -- Use the vis_map PRIOR to wired in,
                        -- because otherwise applyPackageFlag
                        -- won't work.
                        | otherwise = vis_map2
                plugin_vis_map2
                    <- foldM (applyPackageFlag dflags unusable
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
  let preload1 = [ let key = unitId p
                   in fromMaybe key (Map.lookup key wired_map)
                 | f <- other_flags, p <- get_exposed f ]

      get_exposed (ExposePackage _ a _) = take 1 . sortByVersion
                                      . filter (matching a)
                                      $ pkgs1
      get_exposed _                 = []

  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs2

  let preload2 = preload1

  let
      -- add base & rts to the preload packages
      basicLinkedPackages
       | gopt Opt_AutoLinkPackages dflags
          = filter (flip elemUDFM pkg_db)
                [baseUnitId, rtsUnitId]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      preload3 = nub $ filter (/= this_package)
                     $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db (zip preload3 (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  -- Force pstate to avoid leaking the dflags0 passed to mkPackageState
  let !pstate = PackageState{
    preloadPackages     = dep_preload,
    explicitPackages    = foldUDFM (\pkg xs ->
                            if elemUDFM (packageConfigId pkg) vis_map
                                then packageConfigId pkg : xs
                                else xs) [] pkg_db,
    pkgIdMap            = pkg_db,
    moduleToPkgConfAll  = mkModuleToPkgConfAll dflags pkg_db vis_map,
    pluginModuleToPkgConfAll = mkModuleToPkgConfAll dflags pkg_db plugin_vis_map
    }
  return (pstate, new_dep_preload, this_package)


-- -----------------------------------------------------------------------------
-- | Makes the mapping from module to package info

mkModuleToPkgConfAll
  :: DynFlags
  -> PackageConfigMap
  -> VisibilityMap
  -> ModuleToPkgConfAll
mkModuleToPkgConfAll dflags pkg_db vis_map =
    foldl' extend_modmap emptyMap (eltsUDFM pkg_db)
 where
  emptyMap = Map.empty
  sing pk m _ = Map.singleton (mkModule pk m)
  addListTo = foldl' merge
  merge m (k, v) = MapStrict.insertWith (Map.unionWith mappend) k v m
  setOrigins m os = fmap (const os) m
  extend_modmap modmap pkg = addListTo modmap theBindings
   where
    theBindings :: [(ModuleName, Map Module ModuleOrigin)]
    theBindings | Just (b,rns,_) <- lookupUDFM vis_map (packageConfigId pkg)
                              = newBindings b rns
                | otherwise   = newBindings False []

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
     ExposedModule m exposedReexport <- exposed_mods
     let (pk', m', pkg', origin') =
          case exposedReexport of
           Nothing -> (pk, m, pkg, fromExposedModules e)
           Just (OriginalModule pk' m') ->
            let pkg' = pkg_lookup pk'
            in (pk', m', pkg', fromReexportedModules e pkg')
     return (m, sing pk' m' pkg' origin')

    esmap :: UniqFM (Map Module ModuleOrigin)
    esmap = listToUFM (es False) -- parameter here doesn't matter, orig will
                                 -- be overwritten

    hiddens = [(m, sing pk m pkg ModHidden) | m <- hidden_mods]

    pk = packageConfigId pkg
    pkg_lookup = expectJust "mkModuleToPkgConf" . lookupPackage' pkg_db

    exposed_mods = exposedModules pkg
    hidden_mods = hiddenModules pkg

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
getPackageIncludePath :: DynFlags -> [UnitId] -> IO [String]
getPackageIncludePath dflags pkgs =
  collectIncludeDirs `fmap` getPreloadPackagesAnd dflags pkgs

collectIncludeDirs :: [PackageConfig] -> [FilePath]
collectIncludeDirs ps = nub (filter notNull (concatMap includeDirs ps))

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [UnitId] -> IO [String]
getPackageLibraryPath dflags pkgs =
  collectLibraryPaths dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLibraryPaths :: DynFlags -> [PackageConfig] -> [FilePath]
collectLibraryPaths dflags = nub . filter notNull
                           . concatMap (libraryDirsForWay dflags)

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getPackageLinkOpts :: DynFlags -> [UnitId] -> IO ([String], [String], [String])
getPackageLinkOpts dflags pkgs =
  collectLinkOpts dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLinkOpts :: DynFlags -> [PackageConfig] -> ([String], [String], [String])
collectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . packageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . extraLibraries) ps,
        concatMap ldOptions ps
    )

packageHsLibs :: DynFlags -> PackageConfig -> [String]
packageHsLibs dflags p = map (mkDynName . addSuffix) (hsLibraries p)
  where
        ways0 = ways dflags

        ways1 = filter (/= WayDyn) ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        -- debug RTS includes support for -eventlog
        ways2 | WayDebug `elem` ways1
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

        addSuffix rts@"HSrts"    = rts       ++ (expandTag rts_tag)
        addSuffix other_lib      = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t

-- | Either the 'libraryDirs' or 'libraryDynDirs' as appropriate for the way.
libraryDirsForWay :: DynFlags -> PackageConfig -> [String]
libraryDirsForWay dflags
  | WayDyn `elem` ways dflags = libraryDynDirs
  | otherwise                 = libraryDirs

-- | Find all the C-compiler options in these and the preload packages
getPackageExtraCcOpts :: DynFlags -> [UnitId] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap ccOptions ps)

-- | Find all the package framework paths in these and the preload packages
getPackageFrameworkPath  :: DynFlags -> [UnitId] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap frameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getPackageFrameworks  :: DynFlags -> [UnitId] -> IO [String]
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
          case foldl' classify ([],[],[]) (Map.toList xs) of
            ([], [], []) -> LookupNotFound suggestions
            (_, _, [(m, _)])             -> LookupFound m (mod_pkg m)
            (_, _, exposed@(_:_))        -> LookupMultiple exposed
            (hidden_pkg, hidden_mod, []) -> LookupHidden hidden_pkg hidden_mod
  where
    classify (hidden_pkg, hidden_mod, exposed) (m, origin0) =
      let origin = filterOrigin mb_pn (mod_pkg m) origin0
          x = (m, origin)
      in case origin of
          ModHidden                  -> (hidden_pkg,   x:hidden_mod, exposed)
          _ | originEmpty origin     -> (hidden_pkg,   hidden_mod,   exposed)
            | originVisible origin   -> (hidden_pkg,   hidden_mod,   x:exposed)
            | otherwise              -> (x:hidden_pkg, hidden_mod,   exposed)

    pkg_lookup = expectJust "lookupModuleWithSuggestions" . lookupPackage dflags
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
getPreloadPackagesAnd :: DynFlags -> [UnitId] -> IO [PackageConfig]
getPreloadPackagesAnd dflags pkgids =
  let
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr dflags (foldM (add_package pkg_map) preload pairs)
  return (map (getPackageDetails dflags) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: DynFlags
          -> PackageConfigMap
          -> [(UnitId, Maybe UnitId)]
          -> IO [UnitId]
closeDeps dflags pkg_map ps
    = throwErr dflags (closeDepsErr pkg_map ps)

throwErr :: DynFlags -> MaybeErr MsgDoc a -> IO a
throwErr dflags m
              = case m of
                Failed e    -> throwGhcExceptionIO (CmdLineError (showSDoc dflags e))
                Succeeded r -> return r

closeDepsErr :: PackageConfigMap
             -> [(UnitId,Maybe UnitId)]
             -> MaybeErr MsgDoc [UnitId]
closeDepsErr pkg_map ps = foldM (add_package pkg_map) [] ps

-- internal helper
add_package :: PackageConfigMap
            -> [UnitId]
            -> (UnitId,Maybe UnitId)
            -> MaybeErr MsgDoc [UnitId]
add_package pkg_db ps (p, mb_parent)
  | p `elem` ps = return ps     -- Check if we've already added this package
  | otherwise =
      case lookupPackage' pkg_db p of
        Nothing -> Failed (missingPackageMsg p <>
                           missingDependencyMsg mb_parent)
        Just pkg -> do
           -- Add the package's dependents also
           ps' <- foldM add_unit_key ps (depends pkg)
           return (p : ps')
          where
            add_unit_key ps key
              = add_package pkg_db ps (key, Just p)

missingPackageMsg :: Outputable pkgid => pkgid -> SDoc
missingPackageMsg p = text "unknown package:" <+> ppr p

missingDependencyMsg :: Maybe UnitId -> SDoc
missingDependencyMsg Nothing = Outputable.empty
missingDependencyMsg (Just parent)
  = space <> parens (text "dependency of" <+> ftext (unitIdFS parent))

-- -----------------------------------------------------------------------------

unitIdPackageIdString :: DynFlags -> UnitId -> Maybe String
unitIdPackageIdString dflags pkg_key
    | pkg_key == mainUnitId = Just "main"
    | otherwise = fmap sourcePackageIdString (lookupPackage dflags pkg_key)

-- | Will the 'Name' come from a dynamically linked library?
isDllName :: DynFlags -> UnitId -> Module -> Name -> Bool
-- Despite the "dll", I think this function just means that
-- the symbol comes from another dynamically-linked package,
-- and applies on all platforms, not just Windows
isDllName dflags _this_pkg this_mod name
  | WayDyn `notElem` ways dflags = False
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
    = if mod /= this_mod
      then True
      else case dllSplit dflags of
           Nothing -> False
           Just ss ->
               let findMod m = let modStr = moduleNameString (moduleName m)
                               in case find (modStr `Set.member`) ss of
                                  Just i -> i
                                  Nothing -> panic ("Can't find " ++ modStr ++ "in DLL split")
               in findMod mod /= findMod this_mod

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
    where pprIPI ipi = let i = unitIdFS (unitId ipi)
                           e = if exposed ipi then text "E" else text " "
                           t = if trusted ipi then text "T" else text " "
                       in e <> t <> text "  " <> ftext i

-- | Show the mapping of modules to where they come from.
pprModuleMap :: DynFlags -> SDoc
pprModuleMap dflags =
  vcat (map pprLine (Map.toList (moduleToPkgConfAll (pkgState dflags))))
    where
      pprLine (m,e) = ppr m $$ nest 50 (vcat (map (pprEntry m) (Map.toList e)))
      pprEntry m (m',o)
        | m == moduleName m' = ppr (moduleUnitId m') <+> parens (ppr o)
        | otherwise = ppr m' <+> parens (ppr o)

fsPackageName :: PackageConfig -> FastString
fsPackageName = mkFastString . packageNameString
