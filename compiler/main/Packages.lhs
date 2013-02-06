%
% (c) The University of Glasgow, 2006
%
\begin{code}
-- | Package manipulation
module Packages (
        module PackageConfig,

        -- * The PackageConfigMap
        PackageConfigMap, emptyPackageConfigMap, lookupPackage,
        extendPackageConfigMap, dumpPackages,

        -- * Reading the package config, and processing cmdline args
        PackageState(..),
        initPackages,
        getPackageDetails,
        lookupModuleInAllPackages, lookupModuleWithSuggestions,

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
import Distribution.Package hiding (PackageId,depends)
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
--
--   * @-package <pkg>@ causes @<pkg>@ to become exposed, and all other packages
--      with the same name to become hidden.
--
--   * @-hide-package <pkg>@ causes @<pkg>@ to become hidden.
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

data PackageState = PackageState {
  pkgIdMap              :: PackageConfigMap, -- PackageId   -> PackageConfig
        -- The exposed flags are adjusted according to -package and
        -- -hide-package flags, and -ignore-package removes packages.

  preloadPackages      :: [PackageId],
        -- The packages we're going to link in eagerly.  This list
        -- should be in reverse dependency order; that is, a package
        -- is always mentioned before the packages it depends on.

  moduleToPkgConfAll    :: UniqFM [(PackageConfig,Bool)], -- ModuleEnv mapping
        -- Derived from pkgIdMap.
        -- Maps Module to (pkgconf,exposed), where pkgconf is the
        -- PackageConfig for the package containing the module, and
        -- exposed is True if the package exposes that module.

  installedPackageIdMap :: InstalledPackageIdMap
  }

-- | A PackageConfigMap maps a 'PackageId' to a 'PackageConfig'
type PackageConfigMap = UniqFM PackageConfig

type InstalledPackageIdMap = Map InstalledPackageId PackageId

type InstalledPackageIndex = Map InstalledPackageId PackageConfig

emptyPackageConfigMap :: PackageConfigMap
emptyPackageConfigMap = emptyUFM

-- | Find the package we know about with the given id (e.g. \"foo-1.0\"), if any
lookupPackage :: PackageConfigMap -> PackageId -> Maybe PackageConfig
lookupPackage = lookupUFM

extendPackageConfigMap
   :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPackageConfigMap pkg_map new_pkgs
  = foldl add pkg_map new_pkgs
  where add pkg_map p = addToUFM pkg_map (packageConfigId p) p

-- | Looks up the package with the given id in the package state, panicing if it is
-- not found
getPackageDetails :: PackageState -> PackageId -> PackageConfig
getPackageDetails ps pid = expectJust "getPackageDetails" (lookupPackage (pkgIdMap ps) pid)

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
initPackages :: DynFlags -> IO (DynFlags, [PackageId])
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
        -- packages with the same name.
        hideAll name ps = map maybe_hide ps
          where maybe_hide p
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
                      text "(use -v for more information)")
        ppr_flag = case flag of
                     IgnorePackage p -> text "-ignore-package " <> text p
                     HidePackage p   -> text "-hide-package " <> text p
                     ExposePackage p -> text "-package " <> text p
                     ExposePackageId p -> text "-package-id " <> text p
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
--
hideOldPackages :: DynFlags -> [PackageConfig] -> IO [PackageConfig]
hideOldPackages dflags pkgs = mapM maybe_hide pkgs
  where maybe_hide p
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
        wired_in_pkgids = map packageIdString
                          [ primPackageId,
                            integerPackageId,
                            basePackageId,
                            rtsPackageId,
                            thPackageId,
                            dphSeqPackageId,
                            dphParPackageId ]

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
                  = p { sourcePackageId = (sourcePackageId p){ pkgVersion = Version [] [] } }
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
      | Just oldpkg <- lookupUFM pkgmap (packageConfigId pkg)
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
        pkgmap' = addToUFM pkgmap (packageConfigId pkg) pkg

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
    -> [PackageId]              -- preloaded packages
    -> PackageId                -- this package
    -> IO (PackageState,
           [PackageId],         -- new packages to preload
           PackageId) -- this package, might be modified if the current
                      -- package is a wired-in package.

mkPackageState dflags pkgs0 preload0 this_package = do

{-
   Plan.

   1. P = transitive closure of packages selected by -package-id

   2. Apply shadowing.  When there are multiple packages with the same
      sourcePackageId,
        * if one is in P, use that one
        * otherwise, use the one highest in the package stack
      [
       rationale: we cannot use two packages with the same sourcePackageId
       in the same program, because sourcePackageId is the symbol prefix.
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

      ipid_selected = depClosure ipid_map [ InstalledPackageId i
                                          | ExposePackageId i <- flags ]

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
          = filter (flip elemUFM pkg_db) [basePackageId, rtsPackageId]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      preload3 = nub $ filter (/= this_package)
                     $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db ipid_map (zip preload3 (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let pstate = PackageState{ preloadPackages     = dep_preload,
                             pkgIdMap            = pkg_db,
                             moduleToPkgConfAll  = mkModuleMap pkg_db,
                             installedPackageIdMap = ipid_map
                           }

  return (pstate, new_dep_preload, this_package)


-- -----------------------------------------------------------------------------
-- Make the mapping from module to package info

mkModuleMap
  :: PackageConfigMap
  -> UniqFM [(PackageConfig, Bool)]
mkModuleMap pkg_db = foldr extend_modmap emptyUFM pkgids
  where
        pkgids = map packageConfigId (eltsUFM pkg_db)

        extend_modmap pkgid modmap =
                addListToUFM_C (++) modmap
                   ([(m, [(pkg, True)])  | m <- exposed_mods] ++
                    [(m, [(pkg, False)]) | m <- hidden_mods])
          where
                pkg = expectJust "mkModuleMap" (lookupPackage pkg_db pkgid)
                exposed_mods = exposedModules pkg
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
getPackageIncludePath :: DynFlags -> [PackageId] -> IO [String]
getPackageIncludePath dflags pkgs =
  collectIncludeDirs `fmap` getPreloadPackagesAnd dflags pkgs

collectIncludeDirs :: [PackageConfig] -> [FilePath]
collectIncludeDirs ps = nub (filter notNull (concatMap includeDirs ps))

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [PackageId] -> IO [String]
getPackageLibraryPath dflags pkgs =
  collectLibraryPaths `fmap` getPreloadPackagesAnd dflags pkgs

collectLibraryPaths :: [PackageConfig] -> [FilePath]
collectLibraryPaths ps = nub (filter notNull (concatMap libraryDirs ps))

-- | Find all the link options in these and the preload packages
getPackageLinkOpts :: DynFlags -> [PackageId] -> IO [String]
getPackageLinkOpts dflags pkgs =
  collectLinkOpts dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLinkOpts :: DynFlags -> [PackageConfig] -> [String]
collectLinkOpts dflags ps = concat (map all_opts ps)
  where
        libs p     = packageHsLibs dflags p ++ extraLibraries p
        all_opts p = map ("-l" ++) (libs p) ++ ldOptions p

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

        mkDynName | gopt Opt_Static dflags = id
                  | otherwise = (++ ("-ghc" ++ cProjectVersion))

        addSuffix rts@"HSrts"    = rts       ++ (expandTag rts_tag)
        addSuffix other_lib      = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t

-- | Find all the C-compiler options in these and the preload packages
getPackageExtraCcOpts :: DynFlags -> [PackageId] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap ccOptions ps)

-- | Find all the package framework paths in these and the preload packages
getPackageFrameworkPath  :: DynFlags -> [PackageId] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap frameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getPackageFrameworks  :: DynFlags -> [PackageId] -> IO [String]
getPackageFrameworks dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap frameworks ps)

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a 'Module', and if the module is in a package returns
-- @(pkgconf, exposed)@ where pkgconf is the PackageConfig for that package,
-- and exposed is @True@ if the package exposes the module.
lookupModuleInAllPackages :: DynFlags -> ModuleName -> [(PackageConfig,Bool)]
lookupModuleInAllPackages dflags m
  = case lookupModuleWithSuggestions dflags m of
      Right pbs -> pbs
      Left  _   -> []

lookupModuleWithSuggestions
  :: DynFlags -> ModuleName
  -> Either [Module] [(PackageConfig,Bool)]
         -- Lookup module in all packages
         -- Right pbs   =>   found in pbs
         -- Left  ms    =>   not found; but here are sugestions
lookupModuleWithSuggestions dflags m
  = case lookupUFM (moduleToPkgConfAll pkg_state) m of
        Nothing -> Left suggestions
        Just ps -> Right ps
  where
    pkg_state = pkgState dflags
    suggestions
      | gopt Opt_HelpfulErrors dflags = fuzzyLookup (moduleNameString m) all_mods
      | otherwise                     = []

    all_mods :: [(String, Module)]     -- All modules
    all_mods = [ (moduleNameString mod_nm, mkModule pkg_id mod_nm)
               | pkg_config <- eltsUFM (pkgIdMap pkg_state)
               , let pkg_id = packageConfigId pkg_config
               , mod_nm <- exposedModules pkg_config ]

-- | Find all the 'PackageConfig' in both the preload packages from 'DynFlags' and corresponding to the list of
-- 'PackageConfig's
getPreloadPackagesAnd :: DynFlags -> [PackageId] -> IO [PackageConfig]
getPreloadPackagesAnd dflags pkgids =
  let
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      ipid_map = installedPackageIdMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr dflags (foldM (add_package pkg_map ipid_map) preload pairs)
  return (map (getPackageDetails state) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: DynFlags
          -> PackageConfigMap
          -> Map InstalledPackageId PackageId
          -> [(PackageId, Maybe PackageId)]
          -> IO [PackageId]
closeDeps dflags pkg_map ipid_map ps
    = throwErr dflags (closeDepsErr pkg_map ipid_map ps)

throwErr :: DynFlags -> MaybeErr MsgDoc a -> IO a
throwErr dflags m
              = case m of
                Failed e    -> throwGhcExceptionIO (CmdLineError (showSDoc dflags e))
                Succeeded r -> return r

closeDepsErr :: PackageConfigMap
             -> Map InstalledPackageId PackageId
             -> [(PackageId,Maybe PackageId)]
             -> MaybeErr MsgDoc [PackageId]
closeDepsErr pkg_map ipid_map ps = foldM (add_package pkg_map ipid_map) [] ps

-- internal helper
add_package :: PackageConfigMap
            -> Map InstalledPackageId PackageId
            -> [PackageId]
            -> (PackageId,Maybe PackageId)
            -> MaybeErr MsgDoc [PackageId]
add_package pkg_db ipid_map ps (p, mb_parent)
  | p `elem` ps = return ps     -- Check if we've already added this package
  | otherwise =
      case lookupPackage pkg_db p of
        Nothing -> Failed (missingPackageMsg (packageIdString p) <>
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

missingDependencyMsg :: Maybe PackageId -> SDoc
missingDependencyMsg Nothing = empty
missingDependencyMsg (Just parent)
  = space <> parens (ptext (sLit "dependency of") <+> ftext (packageIdFS parent))

-- -----------------------------------------------------------------------------

-- | Will the 'Name' come from a dynamically linked library?
isDllName :: DynFlags -> PackageId -> Name -> Bool
-- Despite the "dll", I think this function just means that
-- the synbol comes from another dynamically-linked package,
-- and applies on all platforms, not just Windows
isDllName dflags this_pkg name
  | gopt Opt_Static dflags = False
  | Just mod <- nameModule_maybe name = modulePackageId mod /= this_pkg
  | otherwise = False  -- no, it is not even an external name

-- -----------------------------------------------------------------------------
-- Displaying packages

-- | Show package info on console, if verbosity is >= 3
dumpPackages :: DynFlags -> IO ()
dumpPackages dflags
  = do let pkg_map = pkgIdMap (pkgState dflags)
       putMsg dflags $
             vcat (map (text . showInstalledPackageInfo
                             . packageConfigToInstalledPackageInfo)
                       (eltsUFM pkg_map))
\end{code}
