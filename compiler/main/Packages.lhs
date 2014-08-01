%
% (c) The University of Glasgow, 2006
%
\begin{code}
{-# LANGUAGE CPP #-}

-- | Package manipulation
module Packages (
        module PackageConfig,

        -- * Reading the package config, and processing cmdline args
        PackageState(preloadPackages),
        initPackages,

        -- * Querying the package config
        lookupPackage,
        resolveInstalledPackageId,
        searchPackageId,
        dumpPackages,
        simpleDumpPackages,
        getPackageDetails,
        listVisibleModuleNames,
        lookupModuleInAllPackages,
        lookupModuleWithSuggestions,
        LookupResult(..),

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
        ModuleExport(..),

        -- * Utils
        packageKeyPackageIdString,
        isDllName
    )
where

#include "HsVersions.h"

import PackageConfig
import DynFlags
import Config           ( cProjectVersion )
import Name             ( Name, nameModule_maybe )
import UniqFM
import Module
import Util
import Panic
import Outputable
import Maybes

import System.Environment ( getEnv )
import Distribution.InstalledPackageInfo
import Distribution.InstalledPackageInfo.Binary
import Distribution.Package hiding (depends, PackageKey, mkPackageKey)
import Distribution.ModuleExport
import FastString
import ErrUtils         ( debugTraceMsg, putMsg, MsgDoc )
import Exception

import System.Directory
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import Control.Monad
import Data.Char (isSpace)
import Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
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
-- possibly simultaneously (which could lead to ambiguity.)
data ModuleOrigin =
    -- | This module name was in the exposed-modules list of a package
    FromExposedModules PackageConfig
    -- | This module name was in the hidden-modules list of a package
  | FromHiddenModules PackageConfig
    -- | This module name was in the reexported-modules list of a package
  | FromReexportedModules {
        theReexporter :: PackageConfig,
        theOriginal :: PackageConfig
    }
  -- FromFlagRenaming

-- | Is the name from the import actually visible? (i.e. does it cause
-- ambiguity, or is it only relevant when we're making suggestions?)
originVisible :: ModuleOrigin -> Maybe PackageConfig
originVisible (FromHiddenModules _) = Nothing
originVisible (FromExposedModules pkg)
    | exposed pkg = Just pkg
    | otherwise = Nothing
originVisible (FromReexportedModules{ theReexporter = pkg })
    | exposed pkg = Just pkg
    | otherwise = Nothing

-- | When we do a plain lookup (e.g. for an import), initially, all we want
-- to know is if we can find it or not (and if we do and it's a reexport,
-- what the real name is).  If the find fails, we'll want to investigate more
-- to give a good error message.
data SimpleModuleConf =
    SModConf Module PackageConfig [ModuleOrigin]
  | SModConfAmbiguous

-- | Map from 'ModuleName'
type ModuleNameMap = UniqFM

-- | Map from 'PackageKey'
type PackageKeyMap = UniqFM

type PackageConfigMap = PackageKeyMap PackageConfig
type ModuleToPkgConfAll = Map ModuleName (Map Module [ModuleOrigin])

data PackageState = PackageState {
  -- | A mapping of 'PackageKey' to 'PackageConfig'.  This list is adjusted
  -- so that only valid packages are here.  Currently, we also flip the
  -- exposed/trusted bits based on package flags; however, the hope is to
  -- stop doing that.
  pkgIdMap              :: PackageConfigMap,

  -- | The packages we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a package
  -- is always mentioned before the packages it depends on.
  preloadPackages      :: [PackageKey],

  -- | This is a simplified map from 'ModuleName' to original 'Module' and
  -- package configuration providing it.
  moduleToPkgConf       :: ModuleNameMap SimpleModuleConf,

  -- | This is a full map from 'ModuleName' to all modules which may possibly
  -- be providing it.  These providers may be hidden (but we'll still want
  -- to report them in error messages), or it may be an ambiguous import.
  moduleToPkgConfAll    :: ModuleToPkgConfAll,

  -- | This is a map from 'InstalledPackageId' to 'PackageKey', since GHC
  -- internally deals in package keys but the database may refer to installed
  -- package IDs.
  installedPackageIdMap :: InstalledPackageIdMap
  }

type InstalledPackageIdMap = Map InstalledPackageId PackageKey
type InstalledPackageIndex = Map InstalledPackageId PackageConfig

emptyPackageConfigMap :: PackageConfigMap
emptyPackageConfigMap = emptyUFM

-- | Find the package we know about with the given key (e.g. @foo_HASH@), if any
lookupPackage :: DynFlags -> PackageKey -> Maybe PackageConfig
lookupPackage dflags = lookupPackage' (pkgIdMap (pkgState dflags))

lookupPackage' :: PackageConfigMap -> PackageKey -> Maybe PackageConfig
lookupPackage' = lookupUFM

-- | Search for packages with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: DynFlags -> PackageId -> [PackageConfig]
searchPackageId dflags pid = filter ((pid ==) . sourcePackageId)
                               (listPackageConfigMap dflags)

-- | Extends the package configuration map with a list of package configs.
extendPackageConfigMap
   :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPackageConfigMap pkg_map new_pkgs
  = foldl add pkg_map new_pkgs
  where add pkg_map p = addToUFM pkg_map (packageConfigId p) p

-- | Looks up the package with the given id in the package state, panicing if it is
-- not found
getPackageDetails :: DynFlags -> PackageKey -> PackageConfig
getPackageDetails dflags pid =
    expectJust "getPackageDetails" (lookupPackage dflags pid)

-- | Get a list of entries from the package database.  NB: be careful with
-- this function, it may not do what you expect it to.
listPackageConfigMap :: DynFlags -> [PackageConfig]
listPackageConfigMap dflags = eltsUFM (pkgIdMap (pkgState dflags))

resolveInstalledPackageId :: DynFlags -> InstalledPackageId -> PackageKey
resolveInstalledPackageId dflags ipid =
    expectJust "resolveInstalledPackageId"
        (Map.lookup ipid (installedPackageIdMap (pkgState dflags)))

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
initPackages :: DynFlags -> IO (DynFlags, [PackageKey])
initPackages dflags = do
  pkg_db <- case pkgDatabase dflags of
                Nothing -> readPackageConfigs dflags
                Just db -> return $ setBatchPackageFlags dflags db
  (pkg_state, preload, this_pkg)
        <- mkPackageState dflags pkg_db [] (thisPackage dflags)
  return (dflags{ pkgDatabase = Just pkg_db,
                  pkgState = pkg_state,
                  thisPackage = this_pkg },
          preload)

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageConfigs :: DynFlags -> IO [PackageConfig]
readPackageConfigs dflags = do
  let system_conf_refs = [UserPkgConf, GlobalPkgConf]

  e_pkg_path <- tryIO (getEnv "GHC_PACKAGE_PATH")
  let base_conf_refs = case e_pkg_path of
        Left _ -> system_conf_refs
        Right path
         | null (last cs)
         -> map PkgConfFile (init cs) ++ system_conf_refs
         | otherwise
         -> map PkgConfFile cs
         where cs = parseSearchPath path
         -- if the path ends in a separator (eg. "/foo/bar:")
         -- then we tack on the system paths.

  let conf_refs = reverse (extraPkgConfs dflags base_conf_refs)
  -- later packages shadow earlier ones.  extraPkgConfs
  -- is in the opposite order to the flags on the
  -- command line.
  confs <- liftM catMaybes $ mapM (resolvePackageConfig dflags) conf_refs

  liftM concat $ mapM (readPackageConfig dflags) confs

resolvePackageConfig :: DynFlags -> PkgConfRef -> IO (Maybe FilePath)
resolvePackageConfig dflags GlobalPkgConf = return $ Just (systemPackageConfig dflags)
resolvePackageConfig _ UserPkgConf = handleIO (\_ -> return Nothing) $ do
  appdir <- getAppUserDataDirectory "ghc"
  let dir = appdir </> (TARGET_ARCH ++ '-':TARGET_OS ++ '-':cProjectVersion)
      pkgconf = dir </> "package.conf.d"
  exist <- doesDirectoryExist pkgconf
  return $ if exist then Just pkgconf else Nothing
resolvePackageConfig _ (PkgConfFile name) = return $ Just name

readPackageConfig :: DynFlags -> FilePath -> IO [PackageConfig]
readPackageConfig dflags conf_file = do
  isdir <- doesDirectoryExist conf_file

  proto_pkg_configs <-
    if isdir
       then do let filename = conf_file </> "package.cache"
               debugTraceMsg dflags 2 (text "Using binary package database:" <+> text filename)
               conf <- readBinPackageDB filename
               return (map installedPackageInfoToPackageConfig conf)

       else do
            isfile <- doesFileExist conf_file
            when (not isfile) $
              throwGhcExceptionIO $ InstallationError $
                "can't find a package database at " ++ conf_file
            debugTraceMsg dflags 2 (text "Using package config file:" <+> text conf_file)
            str <- readFile conf_file
            case reads str of
                [(configs, rest)]
                    | all isSpace rest -> return (map installedPackageInfoToPackageConfig configs)
                _ -> throwGhcExceptionIO $ InstallationError $
                        "invalid package database file " ++ conf_file

  let
      top_dir = topDir dflags
      pkgroot = takeDirectory conf_file
      pkg_configs1 = map (mungePackagePaths top_dir pkgroot) proto_pkg_configs
      pkg_configs2 = setBatchPackageFlags dflags pkg_configs1
  --
  return pkg_configs2

setBatchPackageFlags :: DynFlags -> [PackageConfig] -> [PackageConfig]
setBatchPackageFlags dflags pkgs = (maybeDistrustAll . maybeHideAll) pkgs
  where
    maybeHideAll pkgs'
      | gopt Opt_HideAllPackages dflags = map hide pkgs'
      | otherwise                       = pkgs'

    maybeDistrustAll pkgs'
      | gopt Opt_DistrustAllPackages dflags = map distrust pkgs'
      | otherwise                           = pkgs'

    hide pkg = pkg{ exposed = False }
    distrust pkg = pkg{ trusted = False }

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
-- Modify our copy of the package database based on a package flag
-- (-package, -hide-package, -ignore-package).

applyPackageFlag
   :: DynFlags
   -> UnusablePackages
   -> [PackageConfig]           -- Initial database
   -> PackageFlag               -- flag to apply
   -> IO [PackageConfig]        -- new database

applyPackageFlag dflags unusable pkgs flag =
  case flag of
    ExposePackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps         -> packageFlagErr dflags flag ps
         Right (p:ps,qs) -> return (p':ps')
          where p' = p {exposed=True}
                ps' = hideAll (pkgName (sourcePackageId p)) (ps++qs)
         _ -> panic "applyPackageFlag"

    ExposePackageId str ->
       case selectPackages (matchingId str) pkgs unusable of
         Left ps         -> packageFlagErr dflags flag ps
         Right (p:ps,qs) -> return (p':ps')
          where p' = p {exposed=True}
                ps' = hideAll (pkgName (sourcePackageId p)) (ps++qs)
         _ -> panic "applyPackageFlag"

    ExposePackageKey str ->
       case selectPackages (matchingKey str) pkgs unusable of
         Left ps         -> packageFlagErr dflags flag ps
         Right (p:ps,qs) -> return (p':ps')
          where p' = p {exposed=True}
                ps' = hideAll (pkgName (sourcePackageId p)) (ps++qs)
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> packageFlagErr dflags flag ps
         Right (ps,qs) -> return (map hide ps ++ qs)
          where hide p = p {exposed=False}

    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> packageFlagErr dflags flag ps
         Right (ps,qs) -> return (map trust ps ++ qs)
          where trust p = p {trusted=True}

    DistrustPackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> packageFlagErr dflags flag ps
         Right (ps,qs) -> return (map distrust ps ++ qs)
          where distrust p = p {trusted=False}

    _ -> panic "applyPackageFlag"

   where
        -- When a package is requested to be exposed, we hide all other
        -- packages with the same name if -hide-all-packages was not specified.
        -- If it was specified, we expect users to not try to expose a package
        -- multiple times, so don't hide things.
        hideAll name ps = map maybe_hide ps
          where maybe_hide p
                   | gopt Opt_HideAllPackages dflags     = p
                   | pkgName (sourcePackageId p) == name = p {exposed=False}
                   | otherwise                           = p


selectPackages :: (PackageConfig -> Bool) -> [PackageConfig]
               -> UnusablePackages
               -> Either [(PackageConfig, UnusablePackageReason)]
                  ([PackageConfig], [PackageConfig])
selectPackages matches pkgs unusable
  = let
        (ps,rest) = partition matches pkgs
        reasons = [ (p, Map.lookup (installedPackageId p) unusable)
                  | p <- ps ]
    in
    if all (isJust.snd) reasons
       then Left  [ (p, reason) | (p,Just reason) <- reasons ]
       else Right (sortByVersion [ p | (p,Nothing) <- reasons ], rest)

-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> PackageConfig -> Bool
matchingStr str p
        =  str == display (sourcePackageId p)
        || str == display (pkgName (sourcePackageId p))

matchingId :: String -> PackageConfig -> Bool
matchingId str p =  InstalledPackageId str == installedPackageId p

matchingKey :: String -> PackageConfig -> Bool
matchingKey str p = str == display (packageKey p)

sortByVersion :: [InstalledPackageInfo_ m] -> [InstalledPackageInfo_ m]
sortByVersion = sortBy (flip (comparing (pkgVersion.sourcePackageId)))

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

packageFlagErr :: DynFlags
               -> PackageFlag
               -> [(PackageConfig, UnusablePackageReason)]
               -> IO a

-- for missing DPH package we emit a more helpful error message, because
-- this may be the result of using -fdph-par or -fdph-seq.
packageFlagErr dflags (ExposePackage pkg) [] | is_dph_package pkg
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ dph_err))
  where dph_err = text "the " <> text pkg <> text " package is not installed."
                  $$ text "To install it: \"cabal install dph\"."
        is_dph_package pkg = "dph" `isPrefixOf` pkg

packageFlagErr dflags flag reasons
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ err))
  where err = text "cannot satisfy " <> ppr_flag <>
                (if null reasons then empty else text ": ") $$
              nest 4 (ppr_reasons $$
                      -- ToDo: this admonition seems a bit dodgy
                      text "(use -v for more information)")
        ppr_flag = case flag of
                     IgnorePackage p -> text "-ignore-package " <> text p
                     HidePackage p   -> text "-hide-package " <> text p
                     ExposePackage p -> text "-package " <> text p
                     ExposePackageId p -> text "-package-id " <> text p
                     ExposePackageKey p -> text "-package-key " <> text p
                     TrustPackage p    -> text "-trust " <> text p
                     DistrustPackage p -> text "-distrust " <> text p
        ppr_reasons = vcat (map ppr_reason reasons)
        ppr_reason (p, reason) = pprReason (pprIPkg p <+> text "is") reason

-- -----------------------------------------------------------------------------
-- Hide old versions of packages

--
-- hide all packages for which there is also a later version
-- that is already exposed.  This just makes it non-fatal to have two
-- versions of a package exposed, which can happen if you install a
-- later version of a package in the user database, for example.
-- However, don't do this if @-hide-all-packages@ was passed.
--
hideOldPackages :: DynFlags -> [PackageConfig] -> IO [PackageConfig]
hideOldPackages dflags pkgs = mapM maybe_hide pkgs
  where maybe_hide p
           | gopt Opt_HideAllPackages dflags = return p
           | not (exposed p) = return p
           | (p' : _) <- later_versions = do
                debugTraceMsg dflags 2 $
                   (ptext (sLit "hiding package") <+> pprSPkg p <+>
                    ptext (sLit "to avoid conflict with later version") <+>
                    pprSPkg p')
                return (p {exposed=False})
           | otherwise = return p
          where myname = pkgName (sourcePackageId p)
                myversion = pkgVersion (sourcePackageId p)
                later_versions = [ p | p <- pkgs, exposed p,
                                       let pkg = sourcePackageId p,
                                       pkgName pkg == myname,
                                       pkgVersion pkg > myversion ]

-- -----------------------------------------------------------------------------
-- Wired-in packages

findWiredInPackages
   :: DynFlags
   -> [PackageConfig]           -- database
   -> IO [PackageConfig]

findWiredInPackages dflags pkgs = do
  --
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base).
  --
  let
        wired_in_pkgids :: [String]
        wired_in_pkgids = map packageKeyString wiredInPackageKeys

        matches :: PackageConfig -> String -> Bool
        pc `matches` pid = display (pkgName (sourcePackageId pc)) == pid

        -- find which package corresponds to each wired-in package
        -- delete any other packages with the same name
        -- update the package and any dependencies to point to the new
        -- one.
        --
        -- When choosing which package to map to a wired-in package
        -- name, we prefer exposed packages, and pick the latest
        -- version.  To override the default choice, -hide-package
        -- could be used to hide newer versions.
        --
        findWiredInPackage :: [PackageConfig] -> String
                           -> IO (Maybe InstalledPackageId)
        findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ] in
           case all_ps of
                []   -> notfound
                many -> pick (head (sortByVersion many))
          where
                notfound = do
                          debugTraceMsg dflags 2 $
                            ptext (sLit "wired-in package ")
                                 <> text wired_pkg
                                 <> ptext (sLit " not found.")
                          return Nothing
                pick :: InstalledPackageInfo_ ModuleName
                     -> IO (Maybe InstalledPackageId)
                pick pkg = do
                        debugTraceMsg dflags 2 $
                            ptext (sLit "wired-in package ")
                                 <> text wired_pkg
                                 <> ptext (sLit " mapped to ")
                                 <> pprIPkg pkg
                        return (Just (installedPackageId pkg))


  mb_wired_in_ids <- mapM (findWiredInPackage pkgs) wired_in_pkgids
  let
        wired_in_ids = catMaybes mb_wired_in_ids

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

        updateWiredInDependencies pkgs = map upd_pkg pkgs
          where upd_pkg p
                  | installedPackageId p `elem` wired_in_ids
                  = let pid = (sourcePackageId p) { pkgVersion = Version [] [] }
                    in p { sourcePackageId = pid
                         , packageKey = OldPackageKey pid }
                  | otherwise
                  = p

  return $ updateWiredInDependencies pkgs

-- ----------------------------------------------------------------------------

data UnusablePackageReason
  = IgnoredWithFlag
  | MissingDependencies [InstalledPackageId]
  | ShadowedBy InstalledPackageId

type UnusablePackages = Map InstalledPackageId UnusablePackageReason

pprReason :: SDoc -> UnusablePackageReason -> SDoc
pprReason pref reason = case reason of
  IgnoredWithFlag ->
      pref <+> ptext (sLit "ignored due to an -ignore-package flag")
  MissingDependencies deps ->
      pref <+>
      ptext (sLit "unusable due to missing or recursive dependencies:") $$
        nest 2 (hsep (map (text.display) deps))
  ShadowedBy ipid ->
      pref <+> ptext (sLit "shadowed by package ") <> text (display ipid)

reportUnusable :: DynFlags -> UnusablePackages -> IO ()
reportUnusable dflags pkgs = mapM_ report (Map.toList pkgs)
  where
    report (ipid, reason) =
       debugTraceMsg dflags 2 $
         pprReason
           (ptext (sLit "package") <+>
            text (display ipid) <+> text "is") reason

-- ----------------------------------------------------------------------------
--
-- Detect any packages that have missing dependencies, and also any
-- mutually-recursive groups of packages (loops in the package graph
-- are not allowed).  We do this by taking the least fixpoint of the
-- dependency graph, repeatedly adding packages whose dependencies are
-- satisfied until no more can be added.
--
findBroken :: [PackageConfig] -> UnusablePackages
findBroken pkgs = go [] Map.empty pkgs
 where
   go avail ipids not_avail =
     case partitionWith (depsAvailable ipids) not_avail of
        ([], not_avail) ->
            Map.fromList [ (installedPackageId p, MissingDependencies deps)
                         | (p,deps) <- not_avail ]
        (new_avail, not_avail) ->
            go (new_avail ++ avail) new_ipids (map fst not_avail)
            where new_ipids = Map.insertList
                                [ (installedPackageId p, p) | p <- new_avail ]
                                ipids

   depsAvailable :: InstalledPackageIndex
                 -> PackageConfig
                 -> Either PackageConfig (PackageConfig, [InstalledPackageId])
   depsAvailable ipids pkg
        | null dangling = Left pkg
        | otherwise     = Right (pkg, dangling)
        where dangling = filter (not . (`Map.member` ipids)) (depends pkg)

-- -----------------------------------------------------------------------------
-- Eliminate shadowed packages, giving the user some feedback

-- later packages in the list should shadow earlier ones with the same
-- package name/version.  Additionally, a package may be preferred if
-- it is in the transitive closure of packages selected using -package-id
-- flags.
shadowPackages :: [PackageConfig] -> [InstalledPackageId] -> UnusablePackages
shadowPackages pkgs preferred
 = let (shadowed,_) = foldl check ([],emptyUFM) pkgs
   in  Map.fromList shadowed
 where
 check (shadowed,pkgmap) pkg
      | Just oldpkg <- lookupUFM pkgmap pkgid
      , let
            ipid_new = installedPackageId pkg
            ipid_old = installedPackageId oldpkg
        --
      , ipid_old /= ipid_new
      = if ipid_old `elem` preferred
           then ( (ipid_new, ShadowedBy ipid_old) : shadowed, pkgmap )
           else ( (ipid_old, ShadowedBy ipid_new) : shadowed, pkgmap' )
      | otherwise
      = (shadowed, pkgmap')
      where
        pkgid = mkFastString (display (sourcePackageId pkg))
        pkgmap' = addToUFM pkgmap pkgid pkg

-- -----------------------------------------------------------------------------

ignorePackages :: [PackageFlag] -> [PackageConfig] -> UnusablePackages
ignorePackages flags pkgs = Map.fromList (concatMap doit flags)
  where
  doit (IgnorePackage str) =
     case partition (matchingStr str) pkgs of
         (ps, _) -> [ (installedPackageId p, IgnoredWithFlag)
                    | p <- ps ]
        -- missing package is not an error for -ignore-package,
        -- because a common usage is to -ignore-package P as
        -- a preventative measure just in case P exists.
  doit _ = panic "ignorePackages"

-- -----------------------------------------------------------------------------

depClosure :: InstalledPackageIndex
           -> [InstalledPackageId]
           -> [InstalledPackageId]
depClosure index ipids = closure Map.empty ipids
  where
   closure set [] = Map.keys set
   closure set (ipid : ipids)
     | ipid `Map.member` set = closure set ipids
     | Just p <- Map.lookup ipid index = closure (Map.insert ipid p set)
                                                 (depends p ++ ipids)
     | otherwise = closure set ipids

-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our package
-- settings and populate the package state.

mkPackageState
    :: DynFlags
    -> [PackageConfig]          -- initial database
    -> [PackageKey]              -- preloaded packages
    -> PackageKey                -- this package
    -> IO (PackageState,
           [PackageKey],         -- new packages to preload
           PackageKey) -- this package, might be modified if the current
                      -- package is a wired-in package.

mkPackageState dflags pkgs0 preload0 this_package = do

{-
   Plan.

   1. P = transitive closure of packages selected by -package-id

   2. Apply shadowing.  When there are multiple packages with the same
      packageKey,
        * if one is in P, use that one
        * otherwise, use the one highest in the package stack
      [
       rationale: we cannot use two packages with the same packageKey
       in the same program, because packageKey is the symbol prefix.
       Hence we must select a consistent set of packages to use.  We have
       a default algorithm for doing this: packages higher in the stack
       shadow those lower down.  This default algorithm can be overriden
       by giving explicit -package-id flags; then we have to take these
       preferences into account when selecting which other packages are
       made available.

       Our simple algorithm throws away some solutions: there may be other
       consistent sets that would satisfy the -package flags, but it's
       not GHC's job to be doing constraint solving.
      ]

   3. remove packages selected by -ignore-package

   4. remove any packages with missing dependencies, or mutually recursive
      dependencies.

   5. report (with -v) any packages that were removed by steps 2-4

   6. apply flags to set exposed/hidden on the resulting packages
      - if any flag refers to a package which was removed by 2-4, then
        we can give an error message explaining why

   7. hide any packages which are superseded by later exposed packages
-}

  let
      flags = reverse (packageFlags dflags)

      -- pkgs0 with duplicate packages filtered out.  This is
      -- important: it is possible for a package in the global package
      -- DB to have the same IPID as a package in the user DB, and
      -- we want the latter to take precedence.  This is not the same
      -- as shadowing (below), since in this case the two packages
      -- have the same ABI and are interchangeable.
      --
      -- #4072: note that we must retain the ordering of the list here
      -- so that shadowing behaves as expected when we apply it later.
      pkgs0_unique = snd $ foldr del (Set.empty,[]) pkgs0
          where del p (s,ps)
                  | pid `Set.member` s = (s,ps)
                  | otherwise          = (Set.insert pid s, p:ps)
                  where pid = installedPackageId p
          -- XXX this is just a variant of nub

      ipid_map = Map.fromList [ (installedPackageId p, p) | p <- pkgs0 ]
      -- NB: Prefer the last one (i.e. the one highest in the package stack
      pk_map = Map.fromList [ (packageConfigId p, p) | p <- pkgs0 ]

      ipid_selected = depClosure ipid_map ([ InstalledPackageId i
                                           | ExposePackageId i <- flags ]
                                        ++ [ installedPackageId pkg
                                           | ExposePackageKey k <- flags
                                           , Just pkg <- [Map.lookup
                                                (stringToPackageKey k) pk_map]])

      (ignore_flags, other_flags) = partition is_ignore flags
      is_ignore IgnorePackage{} = True
      is_ignore _ = False

      shadowed = shadowPackages pkgs0_unique ipid_selected

      ignored  = ignorePackages ignore_flags pkgs0_unique

      pkgs0' = filter (not . (`Map.member` (Map.union shadowed ignored)) . installedPackageId) pkgs0_unique
      broken   = findBroken pkgs0'
      unusable = shadowed `Map.union` ignored `Map.union` broken

  reportUnusable dflags unusable

  --
  -- Modify the package database according to the command-line flags
  -- (-package, -hide-package, -ignore-package, -hide-all-packages).
  --
  pkgs1 <- foldM (applyPackageFlag dflags unusable) pkgs0_unique other_flags
  let pkgs2 = filter (not . (`Map.member` unusable) . installedPackageId) pkgs1

  -- Here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "preload"
  -- packages.  we link these packages in eagerly.  The preload set
  -- should contain at least rts & base, which is why we pretend that
  -- the command line contains -package rts & -package base.
  --
  let preload1 = [ installedPackageId p | f <- flags, p <- get_exposed f ]

      get_exposed (ExposePackage   s)
         = take 1 $ sortByVersion (filter (matchingStr s) pkgs2)
         --  -package P means "the latest version of P" (#7030)
      get_exposed (ExposePackageId s) = filter (matchingId  s) pkgs2
      get_exposed (ExposePackageKey s) = filter (matchingKey s) pkgs2
      get_exposed _                   = []

  -- hide packages that are subsumed by later versions
  pkgs3 <- hideOldPackages dflags pkgs2

  -- sort out which packages are wired in
  pkgs4 <- findWiredInPackages dflags pkgs3

  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs4

      ipid_map = Map.fromList [ (installedPackageId p, packageConfigId p)
                              | p <- pkgs4 ]

      lookupIPID ipid@(InstalledPackageId str)
         | Just pid <- Map.lookup ipid ipid_map = return pid
         | otherwise                            = missingPackageErr dflags str

  preload2 <- mapM lookupIPID preload1

  let
      -- add base & rts to the preload packages
      basicLinkedPackages
       | gopt Opt_AutoLinkPackages dflags
          = filter (flip elemUFM pkg_db)
                [basePackageKey, rtsPackageKey]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      preload3 = nub $ filter (/= this_package)
                     $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db ipid_map (zip preload3 (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let pstate = PackageState{
    preloadPackages     = dep_preload,
    pkgIdMap            = pkg_db,
    moduleToPkgConf     = mkModuleToPkgConf pkg_db ipid_map,
    moduleToPkgConfAll  = mkModuleToPkgConfAll pkg_db ipid_map, -- lazy!
    installedPackageIdMap = ipid_map
    }

  return (pstate, new_dep_preload, this_package)


-- -----------------------------------------------------------------------------
-- | Makes the mapping from module to package info

-- | Creates the minimal lookup, which is sufficient if we don't need to
-- report errors.
mkModuleToPkgConf
  :: PackageConfigMap
  -> InstalledPackageIdMap
  -> ModuleNameMap SimpleModuleConf
mkModuleToPkgConf pkg_db ipid_map =
    foldl' extend_modmap emptyUFM (eltsUFM pkg_db)
  where
    extend_modmap modmap pkg
        | exposed pkg = addListToUFM_C merge modmap es
        | otherwise   = modmap
      where merge (SModConf m pkg o) (SModConf m' _ o')
                | m == m' = SModConf m pkg (o ++ o')
                | otherwise = SModConfAmbiguous
            merge _ _ = SModConfAmbiguous
            es = [ (m, SModConf (mkModule pk  m ) pkg [FromExposedModules pkg])
                 | m <- exposed_mods] ++
                 [ (m, SModConf (mkModule pk' m') pkg' [FromReexportedModules{
                                                        theReexporter = pkg,
                                                        theOriginal = pkg'
                                                       }])
                 | ModuleExport{ exportName = m
                               , exportCachedTrueOrig = Just (ipid', m')}
                        <- reexported_mods
                 , Just pk' <- [Map.lookup ipid' ipid_map]
                 , let pkg' = pkg_lookup pk' ]
            pk = packageConfigId pkg
            pkg_lookup = expectJust "mkModuleToPkgConf" . lookupPackage' pkg_db
            exposed_mods = exposedModules pkg
            reexported_mods = reexportedModules pkg

-- | Creates the full lookup, which contains all information we know about
-- modules. Calculate this lazily!  (Note: this will get forced if you use
-- package imports.
mkModuleToPkgConfAll
  :: PackageConfigMap
  -> InstalledPackageIdMap
  -> ModuleToPkgConfAll
mkModuleToPkgConfAll pkg_db ipid_map =
    -- Uses a Map instead of a UniqFM so we don't have to also put
    -- the keys in the values.
    foldl' extend_modmap Map.empty (eltsUFM pkg_db)
 where
  extend_modmap m pkg = foldl' merge m es
   where
    merge m' (k, v) = Map.insertWith (Map.unionWith (++)) k v m'
    sing = Map.singleton
    es =
     [(m, sing (mkModule pk m) [FromExposedModules pkg]) | m <- exposed_mods] ++
     [(m, sing (mkModule pk m) [FromHiddenModules pkg])  | m <- hidden_mods] ++
     [(m, sing (mkModule pk' m') [FromReexportedModules{ theReexporter = pkg
                                                       , theOriginal   = pkg'}])
     | ModuleExport{ exportName = m
                   , exportCachedTrueOrig = Just (ipid', m')} <- reexported_mods
     , let pk' = expectJust "mkModuleToPkgConfAll/i" (Map.lookup ipid' ipid_map)
           pkg' = pkg_lookup pk' ]
    pk = packageConfigId pkg
    pkg_lookup = expectJust "mkModuleToPkgConfAll" . lookupPackage' pkg_db
    exposed_mods = exposedModules pkg
    reexported_mods = reexportedModules pkg
    hidden_mods  = hiddenModules pkg

pprSPkg :: PackageConfig -> SDoc
pprSPkg p = text (display (sourcePackageId p))

pprIPkg :: PackageConfig -> SDoc
pprIPkg p = text (display (installedPackageId p))

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
getPackageIncludePath :: DynFlags -> [PackageKey] -> IO [String]
getPackageIncludePath dflags pkgs =
  collectIncludeDirs `fmap` getPreloadPackagesAnd dflags pkgs

collectIncludeDirs :: [PackageConfig] -> [FilePath]
collectIncludeDirs ps = nub (filter notNull (concatMap includeDirs ps))

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [PackageKey] -> IO [String]
getPackageLibraryPath dflags pkgs =
  collectLibraryPaths `fmap` getPreloadPackagesAnd dflags pkgs

collectLibraryPaths :: [PackageConfig] -> [FilePath]
collectLibraryPaths ps = nub (filter notNull (concatMap libraryDirs ps))

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getPackageLinkOpts :: DynFlags -> [PackageKey] -> IO ([String], [String], [String])
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
         | gopt Opt_Static dflags       = x
         | "HS" `isPrefixOf` x          = x ++ "-ghc" ++ cProjectVersion
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

-- | Find all the C-compiler options in these and the preload packages
getPackageExtraCcOpts :: DynFlags -> [PackageKey] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap ccOptions ps)

-- | Find all the package framework paths in these and the preload packages
getPackageFrameworkPath  :: DynFlags -> [PackageKey] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap frameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getPackageFrameworks  :: DynFlags -> [PackageKey] -> IO [String]
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
      LookupMultiple rs -> rs
      _ -> []

-- | The result of performing a lookup
data LookupResult =
    -- | Found the module uniquely, nothing else to do
    LookupFound Module PackageConfig
    -- | Multiple modules with the same name in scope
  | LookupMultiple [(Module, PackageConfig)]
    -- | No modules found, but there were some hidden ones with
    -- an exact name match.  First is due to package hidden, second
    -- is due to module being hidden
  | LookupHidden [(Module, PackageConfig)] [(Module, PackageConfig)]
    -- | Nothing found, here are some suggested different names
  | LookupNotFound [Module] -- suggestions

lookupModuleWithSuggestions :: DynFlags
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions dflags m mb_pn
  = case lookupUFM (moduleToPkgConf pkg_state) m of
     Just (SModConf m pkg os) | any (matches mb_pn) os -> LookupFound m pkg
     _ -> case Map.lookup m (moduleToPkgConfAll pkg_state) of
        Nothing -> LookupNotFound suggestions
        Just xs0 ->
          let xs = filter (any (matches mb_pn)) (Map.elems xs0)
          in case concatMap (selectVisible m) xs of
                [] -> case [ (mkModule (packageConfigId pkg) m, pkg)
                           | origin <- concat xs
                           , mb_pn `matches` origin
                           , let pkg = extractPackage origin ] of
                        [] -> LookupNotFound suggestions
                        rs -> uncurry LookupHidden $ partition (exposed.snd) rs
                [_] -> panic "lookupModuleWithSuggestions"
                rs -> LookupMultiple rs
  where
    -- ToDo: this will be wrong when we add flag renaming

    -- NB: ignore the original module; we care about what's user-visible
    selectVisible mod_nm origins =
        [ (mkModule (packageConfigId pkg) mod_nm, pkg)
        | origin <- origins
        , mb_pn `matches` origin
        , Just pkg <- [originVisible origin] ]

    pkg_state = pkgState dflags

    suggestions
      | gopt Opt_HelpfulErrors dflags =
           fuzzyLookup (moduleNameString m) all_mods
      | otherwise = []

    all_mods :: [(String, Module)]     -- All modules
    all_mods =
        [ (moduleNameString mod_nm, from_mod)
        | (mod_nm, modmap) <- Map.toList (moduleToPkgConfAll (pkgState dflags))
        -- NB: ignore the original module; we care about what's user-visible
        , (_, origins) <- Map.toList modmap
        -- NB: do *not* filter on mb_pn; user might have passed an incorrect
        -- package name
        , from_mod <- map (flip mkModule mod_nm
                                . packageConfigId . extractPackage) origins ]

    extractPackage (FromExposedModules pkg) = pkg
    extractPackage (FromHiddenModules pkg) = pkg
    extractPackage (FromReexportedModules{ theReexporter = pkg }) = pkg

    Nothing `matches` _ = True
    Just pn `matches` origin = case packageName (extractPackage origin) of
                                PackageName pn' -> fsLit pn' == pn

listVisibleModuleNames :: DynFlags -> [ModuleName]
listVisibleModuleNames dflags =
    Map.keys (moduleToPkgConfAll (pkgState dflags))

-- | Find all the 'PackageConfig' in both the preload packages from 'DynFlags' and corresponding to the list of
-- 'PackageConfig's
getPreloadPackagesAnd :: DynFlags -> [PackageKey] -> IO [PackageConfig]
getPreloadPackagesAnd dflags pkgids =
  let
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      ipid_map = installedPackageIdMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr dflags (foldM (add_package pkg_map ipid_map) preload pairs)
  return (map (getPackageDetails dflags) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: DynFlags
          -> PackageConfigMap
          -> Map InstalledPackageId PackageKey
          -> [(PackageKey, Maybe PackageKey)]
          -> IO [PackageKey]
closeDeps dflags pkg_map ipid_map ps
    = throwErr dflags (closeDepsErr pkg_map ipid_map ps)

throwErr :: DynFlags -> MaybeErr MsgDoc a -> IO a
throwErr dflags m
              = case m of
                Failed e    -> throwGhcExceptionIO (CmdLineError (showSDoc dflags e))
                Succeeded r -> return r

closeDepsErr :: PackageConfigMap
             -> Map InstalledPackageId PackageKey
             -> [(PackageKey,Maybe PackageKey)]
             -> MaybeErr MsgDoc [PackageKey]
closeDepsErr pkg_map ipid_map ps = foldM (add_package pkg_map ipid_map) [] ps

-- internal helper
add_package :: PackageConfigMap
            -> Map InstalledPackageId PackageKey
            -> [PackageKey]
            -> (PackageKey,Maybe PackageKey)
            -> MaybeErr MsgDoc [PackageKey]
add_package pkg_db ipid_map ps (p, mb_parent)
  | p `elem` ps = return ps     -- Check if we've already added this package
  | otherwise =
      case lookupPackage' pkg_db p of
        Nothing -> Failed (missingPackageMsg (packageKeyString p) <>
                           missingDependencyMsg mb_parent)
        Just pkg -> do
           -- Add the package's dependents also
           ps' <- foldM add_package_ipid ps (depends pkg)
           return (p : ps')
          where
            add_package_ipid ps ipid@(InstalledPackageId str)
              | Just pid <- Map.lookup ipid ipid_map
              = add_package pkg_db ipid_map ps (pid, Just p)
              | otherwise
              = Failed (missingPackageMsg str <> missingDependencyMsg mb_parent)

missingPackageErr :: DynFlags -> String -> IO a
missingPackageErr dflags p
    = throwGhcExceptionIO (CmdLineError (showSDoc dflags (missingPackageMsg p)))

missingPackageMsg :: String -> SDoc
missingPackageMsg p = ptext (sLit "unknown package:") <+> text p

missingDependencyMsg :: Maybe PackageKey -> SDoc
missingDependencyMsg Nothing = empty
missingDependencyMsg (Just parent)
  = space <> parens (ptext (sLit "dependency of") <+> ftext (packageKeyFS parent))

-- -----------------------------------------------------------------------------

packageKeyPackageIdString :: DynFlags -> PackageKey -> String
packageKeyPackageIdString dflags pkg_key
    | pkg_key == mainPackageKey = "main"
    | otherwise = maybe "(unknown)"
                      (display . sourcePackageId)
                      (lookupPackage dflags pkg_key)

-- | Will the 'Name' come from a dynamically linked library?
isDllName :: DynFlags -> PackageKey -> Module -> Name -> Bool
-- Despite the "dll", I think this function just means that
-- the synbol comes from another dynamically-linked package,
-- and applies on all platforms, not just Windows
isDllName dflags _this_pkg this_mod name
  | gopt Opt_Static dflags = False
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

-- | Show (very verbose) package info on console, if verbosity is >= 5
dumpPackages :: DynFlags -> IO ()
dumpPackages = dumpPackages' showInstalledPackageInfo

dumpPackages' :: (InstalledPackageInfo -> String) -> DynFlags -> IO ()
dumpPackages' showIPI dflags
  = do putMsg dflags $
             vcat (map (text . showIPI
                             . packageConfigToInstalledPackageInfo)
                       (listPackageConfigMap dflags))

-- | Show simplified package info on console, if verbosity == 4.
-- The idea is to only print package id, and any information that might
-- be different from the package databases (exposure, trust)
simpleDumpPackages :: DynFlags -> IO ()
simpleDumpPackages = dumpPackages' showIPI
    where showIPI ipi = let InstalledPackageId i = installedPackageId ipi
                            e = if exposed ipi then "E" else " "
                            t = if trusted ipi then "T" else " "
                        in e ++ t ++ "  " ++ i

\end{code}
