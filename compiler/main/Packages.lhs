%
% (c) The University of Glasgow, 2006
%
\begin{code}
-- | Package manipulation
module Packages (
	module PackageConfig,

	-- * The PackageConfigMap
	PackageConfigMap, emptyPackageConfigMap, lookupPackage,
	extendPackageConfigMap,	dumpPackages,

	-- * Reading the package config, and processing cmdline args
	PackageState(..),
	initPackages,
	getPackageDetails,
	lookupModuleInAllPackages,

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
import ParsePkgConf	( loadPackageConfig )
import DynFlags		( dopt, DynFlag(..), DynFlags(..), PackageFlag(..) )
import StaticFlags	( opt_Static )
import Config		( cProjectVersion )
import Name		( Name, nameModule_maybe )
import UniqFM
import Module
import Util
import Maybes		( expectJust, MaybeErr(..) )
import Panic
import Outputable

import System.Environment ( getEnv )
import Distribution.InstalledPackageInfo hiding (depends)
import Distribution.Package hiding (depends, PackageId)
import Distribution.Text
import Distribution.Version
import FastString
import ErrUtils         ( debugTraceMsg, putMsg, Message )
import Exception

import System.Directory
import System.FilePath
import Data.Maybe
import Control.Monad
import Data.List

-- ---------------------------------------------------------------------------
-- The Package state

-- | Package state is all stored in 'DynFlag's, including the details of
-- all packages, which packages are exposed, and which modules they
-- provide.
--
-- The package state is computed by 'initPackages', and kept in DynFlags.
--
--   * @-package <pkg>@ causes @<pkg>@ to become exposed, and all other packages 
--	with the same name to become hidden.
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
-- 	If it's in the same DLL, we refer to B_f_closure
-- 	If it isn't, we refer to _imp__B_f_closure
-- When compiling A, we record in B's Module value whether it's
-- in a different DLL, by setting the DLL flag.

data PackageState = PackageState {
  pkgIdMap		:: PackageConfigMap, -- PackageId   -> PackageConfig
	-- The exposed flags are adjusted according to -package and
	-- -hide-package flags, and -ignore-package removes packages.

  preloadPackages      :: [PackageId],
	-- The packages we're going to link in eagerly.  This list
	-- should be in reverse dependency order; that is, a package
	-- is always mentioned before the packages it depends on.

  moduleToPkgConfAll 	:: UniqFM [(PackageConfig,Bool)] -- ModuleEnv mapping
	-- Derived from pkgIdMap.	
	-- Maps Module to (pkgconf,exposed), where pkgconf is the
	-- PackageConfig for the package containing the module, and
	-- exposed is True if the package exposes that module.
  }

-- | A PackageConfigMap maps a 'PackageId' to a 'PackageConfig'
type PackageConfigMap = UniqFM PackageConfig

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
-- Loading the package config files and building up the package state

-- | Call this after 'DynFlags.parseDynFlags'.  It reads the package
-- configuration files, and sets up various internal tables of package
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
                Just db -> return db
  (pkg_state, preload, this_pkg)       
        <- mkPackageState dflags pkg_db [] (thisPackage dflags)
  return (dflags{ pkgDatabase = Just pkg_db,
		  pkgState = pkg_state,
                  thisPackage = this_pkg },
          preload)

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageConfigs :: DynFlags -> IO PackageConfigMap
readPackageConfigs dflags = do
   e_pkg_path <- tryIO (getEnv "GHC_PACKAGE_PATH")
   system_pkgconfs <- getSystemPackageConfigs dflags

   let pkgconfs = case e_pkg_path of
		    Left _   -> system_pkgconfs
		    Right path
		     | last cs == "" -> init cs ++ system_pkgconfs
		     | otherwise     -> cs
		     where cs = parseSearchPath path
		     -- if the path ends in a separator (eg. "/foo/bar:")
		     -- the we tack on the system paths.

	-- Read all the ones mentioned in -package-conf flags
   pkg_map <- foldM (readPackageConfig dflags) emptyPackageConfigMap
		 (reverse pkgconfs ++ extraPkgConfs dflags)

   return pkg_map


getSystemPackageConfigs :: DynFlags -> IO [FilePath]
getSystemPackageConfigs dflags = do
	-- System one always comes first
   let system_pkgconf = systemPackageConfig dflags

	-- allow package.conf.d to contain a bunch of .conf files
	-- containing package specifications.  This is an easier way
	-- to maintain the package database on systems with a package
	-- management system, or systems that don't want to run ghc-pkg
	-- to register or unregister packages.  Undocumented feature for now.
   let system_pkgconf_dir = system_pkgconf <.> "d"
   system_pkgconf_dir_exists <- doesDirectoryExist system_pkgconf_dir
   system_pkgconfs <-
     if system_pkgconf_dir_exists
       then do files <- getDirectoryContents system_pkgconf_dir
               return [ system_pkgconf_dir </> file
                      | file <- files
                      , takeExtension file == ".conf" ]
       else return []

	-- Read user's package conf (eg. ~/.ghc/i386-linux-6.3/package.conf)
	-- unless the -no-user-package-conf flag was given.
	-- We only do this when getAppUserDataDirectory is available 
	-- (GHC >= 6.3).
   user_pkgconf <- do
      appdir <- getAppUserDataDirectory "ghc"
      let 
     	 pkgconf = appdir
		   </> (TARGET_ARCH ++ '-':TARGET_OS ++ '-':cProjectVersion)
		   </> "package.conf"
      flg <- doesFileExist pkgconf
      if (flg && dopt Opt_ReadUserPackageConf dflags)
	then return [pkgconf]
	else return []
    `catchIO` (\_ -> return [])

   return (user_pkgconf ++ system_pkgconfs ++ [system_pkgconf])


readPackageConfig
   :: DynFlags -> PackageConfigMap -> FilePath -> IO PackageConfigMap
readPackageConfig dflags pkg_map conf_file = do
  debugTraceMsg dflags 2 (text "Using package config file:" <+> text conf_file)
  proto_pkg_configs <- loadPackageConfig dflags conf_file
  let top_dir = topDir dflags
      pkg_configs1 = mungePackagePaths top_dir proto_pkg_configs
      pkg_configs2 = maybeHidePackages dflags pkg_configs1
  return (extendPackageConfigMap pkg_map pkg_configs2)

maybeHidePackages :: DynFlags -> [PackageConfig] -> [PackageConfig]
maybeHidePackages dflags pkgs
  | dopt Opt_HideAllPackages dflags = map hide pkgs
  | otherwise 			    = pkgs
  where
    hide pkg = pkg{ exposed = False }

mungePackagePaths :: String -> [PackageConfig] -> [PackageConfig]
-- Replace the string "$topdir" at the beginning of a path
-- with the current topdir (obtained from the -B option).
mungePackagePaths top_dir ps = map munge_pkg ps
 where 
  munge_pkg p = p{ importDirs  = munge_paths (importDirs p),
		   includeDirs = munge_paths (includeDirs p),
    		   libraryDirs = munge_paths (libraryDirs p),
		   frameworkDirs = munge_paths (frameworkDirs p),
                   haddockInterfaces = munge_paths (haddockInterfaces p),
	           haddockHTMLs = munge_paths (haddockHTMLs p)
                    }

  munge_paths = map munge_path

  munge_path p 
	  | Just p' <- maybePrefixMatch "$topdir"     p =            top_dir ++ p'
	  | Just p' <- maybePrefixMatch "$httptopdir" p = toHttpPath top_dir ++ p'
	  | otherwise				    = p

  toHttpPath p = "file:///" ++ p


-- -----------------------------------------------------------------------------
-- Modify our copy of the package database based on a package flag
-- (-package, -hide-package, -ignore-package).

applyPackageFlag
   :: [PackageConfig]           -- Initial database
   -> PackageFlag               -- flag to apply
   -> IO [PackageConfig]        -- new database

applyPackageFlag pkgs flag = 
  case flag of
        ExposePackage str ->
	   case matchingPackages str pkgs of
		Nothing -> missingPackageErr str
		Just ([], _) -> panic "applyPackageFlag"
		Just (p:ps,qs) -> return (p':ps')
		  where p' = p {exposed=True}
		        ps' = hideAll (pkgName (package p)) (ps++qs)

	HidePackage str ->
           case matchingPackages str pkgs of
                Nothing -> missingPackageErr str
                Just (ps,qs) -> return (map hide ps ++ qs)
		  where hide p = p {exposed=False}

	IgnorePackage str ->
           case matchingPackages str pkgs of
                Nothing -> return pkgs
                Just (_, qs) -> return qs
		-- missing package is not an error for -ignore-package,
		-- because a common usage is to -ignore-package P as
		-- a preventative measure just in case P exists.
   where
	-- When a package is requested to be exposed, we hide all other
	-- packages with the same name.
	hideAll name ps = map maybe_hide ps
	  where maybe_hide p | pkgName (package p) == name = p {exposed=False}
			     | otherwise                   = p


matchingPackages :: String -> [PackageConfig]
         -> Maybe ([PackageConfig], [PackageConfig])
matchingPackages str pkgs
  = case partition (packageMatches str) pkgs of
	([],_)    -> Nothing
	(ps,rest) -> Just (sortByVersion ps, rest)

-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
packageMatches :: String -> PackageConfig -> Bool
packageMatches str p
	=  str == display (package p)
	|| str == display (pkgName (package p))

pickPackages :: [PackageConfig] -> [String] -> [PackageConfig]
pickPackages pkgs strs = 
  [ p | p <- strs, Just (p:_, _) <- [matchingPackages p pkgs] ]

sortByVersion :: [InstalledPackageInfo_ m] -> [InstalledPackageInfo_ m]
sortByVersion = sortBy (flip (comparing (pkgVersion.package)))

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

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
		   (ptext (sLit "hiding package") <+> 
                    text (display (package p)) <+>
		    ptext (sLit "to avoid conflict with later version") <+>
		    text (display (package p')))
		return (p {exposed=False})
	   | otherwise = return p
	  where myname = pkgName (package p)
		myversion = pkgVersion (package p)
		later_versions = [ p | p <- pkgs, exposed p,
				    let pkg = package p,
				    pkgName pkg == myname,
				    pkgVersion pkg > myversion ]

-- -----------------------------------------------------------------------------
-- Wired-in packages

findWiredInPackages
   :: DynFlags
   -> [PackageConfig]           -- database
   -> [PackageIdentifier]       -- preload packages
   -> PackageId 		-- this package
   -> IO ([PackageConfig],
          [PackageIdentifier],
          PackageId)

findWiredInPackages dflags pkgs preload this_package = do
  --
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base).
  --
  let
        wired_in_pkgids :: [(PackageId, [String])]
        wired_in_pkgids = [ (primPackageId, [""]),
                            (integerPackageId, [""]),
                            (basePackageId, [""]),
                            (rtsPackageId, [""]),
                            (haskell98PackageId, [""]),
                            (sybPackageId, [""]),
                            (thPackageId, [""]),
                            (dphSeqPackageId, [""]),
                            (dphParPackageId, [""])]

        matches :: PackageConfig -> (PackageId, [String]) -> Bool
        pc `matches` (pid, suffixes)
            = display (pkgName (package pc)) `elem`
              (map (packageIdString pid ++) suffixes)

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
	findWiredInPackage :: [PackageConfig] -> (PackageId, [String])
			   -> IO (Maybe (PackageIdentifier, PackageId))
	findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ] in
	   case all_ps of
		[]   -> notfound
		many -> pick (head (sortByVersion many))
          where
                suffixes = snd wired_pkg
                notfound = do
			  debugTraceMsg dflags 2 $
			    ptext (sLit "wired-in package ")
				 <> ppr (fst wired_pkg)
                                 <> (if null suffixes
                                     then empty
                                     else text (show suffixes))
				 <> ptext (sLit " not found.")
			  return Nothing
		pick :: InstalledPackageInfo_ ModuleName
                     -> IO (Maybe (PackageIdentifier, PackageId))
                pick pkg = do
                        debugTraceMsg dflags 2 $
			    ptext (sLit "wired-in package ")
				 <> ppr (fst wired_pkg)
				 <> ptext (sLit " mapped to ")
				 <> text (display (package pkg))
			return (Just (package pkg, fst wired_pkg))


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
	  where upd_pkg p = p{ package = upd_pid (package p),
			       depends = map upd_pid (depends p) }

	upd_pid pid = case filter ((== pid) . fst) wired_in_ids of
				[] -> pid
				((x, y):_) -> x{ pkgName = PackageName (packageIdString y),
                                                 pkgVersion = Version [] [] }

        -- pkgs1 = deleteOtherWiredInPackages pkgs

        pkgs2 = updateWiredInDependencies pkgs

        preload1 = map upd_pid preload

        -- we must return an updated thisPackage, just in case we
        -- are actually compiling one of the wired-in packages
        Just old_this_pkg = unpackPackageId this_package
        new_this_pkg = mkPackageId (upd_pid old_this_pkg)

  return (pkgs2, preload1, new_this_pkg)

-- ----------------------------------------------------------------------------
--
-- Detect any packages that have missing dependencies, and also any
-- mutually-recursive groups of packages (loops in the package graph
-- are not allowed).  We do this by taking the least fixpoint of the
-- dependency graph, repeatedly adding packages whose dependencies are
-- satisfied until no more can be added.
--
elimDanglingDeps
   :: DynFlags
   -> [PackageConfig]
   -> [PackageId]       -- ignored packages
   -> IO [PackageConfig]

elimDanglingDeps dflags pkgs ignored = go [] pkgs'
 where
   pkgs' = filter (\p -> packageConfigId p `notElem` ignored) pkgs

   go avail not_avail =
     case partitionWith (depsAvailable avail) not_avail of
        ([],        not_avail) -> do mapM_ reportElim not_avail; return avail
        (new_avail, not_avail) -> go (new_avail ++ avail) (map fst not_avail)

   depsAvailable :: [PackageConfig] -> PackageConfig
                 -> Either PackageConfig (PackageConfig, [PackageIdentifier])
   depsAvailable pkgs_ok pkg 
        | null dangling = Left pkg
        | otherwise     = Right (pkg, dangling)
        where dangling = filter (`notElem` pids) (depends pkg)
              pids = map package pkgs_ok

   reportElim (p, deps) = 
        debugTraceMsg dflags 2 $
             (ptext (sLit "package") <+> pprPkg p <+> 
                  ptext (sLit "will be ignored due to missing or recursive dependencies:") $$ 
	      nest 2 (hsep (map (text.display) deps)))

-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our package
-- settings and populate the package state.

mkPackageState
    :: DynFlags
    -> PackageConfigMap         -- initial database
    -> [PackageId]              -- preloaded packages
    -> PackageId                -- this package
    -> IO (PackageState,
           [PackageId],         -- new packages to preload
           PackageId) -- this package, might be modified if the current

                      -- package is a wired-in package.

mkPackageState dflags orig_pkg_db preload0 this_package = do
  --
  -- Modify the package database according to the command-line flags
  -- (-package, -hide-package, -ignore-package, -hide-all-packages).
  --
  let flags = reverse (packageFlags dflags)
  let pkgs0 = eltsUFM orig_pkg_db
  pkgs1 <- foldM applyPackageFlag pkgs0 flags

  -- Here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "preload"
  -- packages.  we link these packages in eagerly.  The preload set
  -- should contain at least rts & base, which is why we pretend that
  -- the command line contains -package rts & -package base.
  --
  let new_preload_packages = 
        map package (pickPackages pkgs0 [ p | ExposePackage p <- flags ])

  -- hide packages that are subsumed by later versions
  pkgs2 <- hideOldPackages dflags pkgs1

  -- sort out which packages are wired in
  (pkgs3, preload1, new_this_pkg)
        <- findWiredInPackages dflags pkgs2 new_preload_packages this_package

  let ignored = map packageConfigId $
                   pickPackages pkgs0 [ p | IgnorePackage p <- flags ]
  pkgs <- elimDanglingDeps dflags pkgs3 ignored

  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs

      -- add base & rts to the preload packages
      basicLinkedPackages
       | dopt Opt_AutoLinkPackages dflags
          = filter (flip elemUFM pkg_db) [basePackageId, rtsPackageId]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      preload2 = nub (filter (/= new_this_pkg)
		             (basicLinkedPackages ++ map mkPackageId preload1))

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps pkg_db (zip preload2 (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let pstate = PackageState{ preloadPackages     = dep_preload,
		             pkgIdMap   	 = pkg_db,
		             moduleToPkgConfAll  = mkModuleMap pkg_db
		           }

  return (pstate, new_dep_preload, new_this_pkg)


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

pprPkg :: PackageConfig -> SDoc
pprPkg p = text (display (package p))

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
        tag = buildTag dflags
        rts_tag = rtsBuildTag dflags

	mkDynName | opt_Static = id
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
lookupModuleInAllPackages dflags m =
  case lookupUFM (moduleToPkgConfAll (pkgState dflags)) m of
	Nothing -> []
	Just ps -> ps

-- | Find all the 'PackageConfig' in both the preload packages from 'DynFlags' and corresponding to the list of
-- 'PackageConfig's
getPreloadPackagesAnd :: DynFlags -> [PackageId] -> IO [PackageConfig]
getPreloadPackagesAnd dflags pkgids =
  let 
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr (foldM (add_package pkg_map) preload pairs)
  return (map (getPackageDetails state) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: PackageConfigMap -> [(PackageId, Maybe PackageId)]
        -> IO [PackageId]
closeDeps pkg_map ps = throwErr (closeDepsErr pkg_map ps)

throwErr :: MaybeErr Message a -> IO a
throwErr m = case m of
		Failed e    -> ghcError (CmdLineError (showSDoc e))
		Succeeded r -> return r

closeDepsErr :: PackageConfigMap -> [(PackageId,Maybe PackageId)]
	-> MaybeErr Message [PackageId]
closeDepsErr pkg_map ps = foldM (add_package pkg_map) [] ps

-- internal helper
add_package :: PackageConfigMap -> [PackageId] -> (PackageId,Maybe PackageId)
	-> MaybeErr Message [PackageId]
add_package pkg_db ps (p, mb_parent)
  | p `elem` ps = return ps	-- Check if we've already added this package
  | otherwise =
      case lookupPackage pkg_db p of
        Nothing -> Failed (missingPackageMsg (packageIdString p) <> 
                           missingDependencyMsg mb_parent)
        Just pkg -> do
    	   -- Add the package's dependents also
	   let deps = map mkPackageId (depends pkg)
    	   ps' <- foldM (add_package pkg_db) ps (zip deps (repeat (Just p)))
    	   return (p : ps')

missingPackageErr :: String -> IO [PackageConfig]
missingPackageErr p = ghcError (CmdLineError (showSDoc (missingPackageMsg p)))

missingPackageMsg :: String -> SDoc
missingPackageMsg p = ptext (sLit "unknown package:") <+> text p

missingDependencyMsg :: Maybe PackageId -> SDoc
missingDependencyMsg Nothing = empty
missingDependencyMsg (Just parent)
  = space <> parens (ptext (sLit "dependency of") <+> ftext (packageIdFS parent))

-- -----------------------------------------------------------------------------

-- | Will the 'Name' come from a dynamically linked library?
isDllName :: PackageId -> Name -> Bool
isDllName this_pkg name
  | opt_Static = False
  | Just mod <- nameModule_maybe name = modulePackageId mod /= this_pkg
  | otherwise = False  -- no, it is not even an external name

-- -----------------------------------------------------------------------------
-- Displaying packages

-- | Show package info on console, if verbosity is >= 3
dumpPackages :: DynFlags -> IO ()
dumpPackages dflags
  = do  let pkg_map = pkgIdMap (pkgState dflags)
	putMsg dflags $
              vcat (map (text . showInstalledPackageInfo
                              . packageConfigToInstalledPackageInfo)
                        (eltsUFM pkg_map))
\end{code}
