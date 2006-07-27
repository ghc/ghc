%
% (c) The University of Glasgow, 2000
%
\section{Package manipulation}

\begin{code}
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
	getPackageCIncludes,
	getPackageLibraryPath,
	getPackageLinkOpts,
	getPackageExtraCcOpts,
	getPackageFrameworkPath,
	getPackageFrameworks,
	getExplicitPackagesAnd,

	-- * Utils
	isDllName
    )
where

#include "HsVersions.h"

import PackageConfig	
import SysTools		( getTopDir, getPackageConfigPath )
import ParsePkgConf	( loadPackageConfig )
import DynFlags		( dopt, DynFlag(..), DynFlags(..), PackageFlag(..) )
import StaticFlags	( opt_Static )
import Config		( cProjectVersion )
import Name		( Name, nameModule_maybe )
import UniqFM
import Module
import UniqSet
import Util
import Maybes		( expectJust, MaybeErr(..) )
import Panic
import Outputable

#if __GLASGOW_HASKELL__ >= 603
import System.Directory	( getAppUserDataDirectory )
#else
import Compat.Directory	( getAppUserDataDirectory )
#endif

import System.Environment ( getEnv )
import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Version
import System.Directory	( doesFileExist, doesDirectoryExist,
			  getDirectoryContents )
import Data.Maybe	( catMaybes )
import Control.Monad	( foldM )
import Data.List	( nub, partition, sortBy, isSuffixOf )
import FastString
import EXCEPTION	( throwDyn )
import ErrUtils         ( debugTraceMsg, putMsg, Message )

-- ---------------------------------------------------------------------------
-- The Package state

-- Package state is all stored in DynFlags, including the details of
-- all packages, which packages are exposed, and which modules they
-- provide.

-- The package state is computed by initPackages, and kept in DynFlags.
--
--   * -package <pkg> causes <pkg> to become exposed, and all other packages 
--	with the same name to become hidden.
-- 
--   * -hide-package <pkg> causes <pkg> to become hidden.
-- 
--   * Let exposedPackages be the set of packages thus exposed.  
--     Let depExposedPackages be the transitive closure from exposedPackages of
--     their dependencies.
--
--   * When searching for a module from an explicit import declaration,
--     only the exposed modules in exposedPackages are valid.
--
--   * When searching for a module from an implicit import, all modules
--     from depExposedPackages are valid.
--
--   * When linking in a comp manager mode, we link in packages the
--     program depends on (the compiler knows this list by the
--     time it gets to the link step).  Also, we link in all packages
--     which were mentioned with explicit -package flags on the command-line,
--     or are a transitive dependency of same, or are "base"/"rts".
--     The reason for (b) is that we might need packages which don't
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

  explicitPackages      :: [PackageId],
	-- The packages we're going to link in eagerly.  This list
	-- should be in reverse dependency order; that is, a package
	-- is always mentioned before the packages it depends on.

  origPkgIdMap	        :: PackageConfigMap, -- PackageId   -> PackageConfig
	-- the full package database

  pkgIdMap		:: PackageConfigMap, -- PackageId   -> PackageConfig
	-- Derived from origPkgIdMap.
	-- The exposed flags are adjusted according to -package and
	-- -hide-package flags, and -ignore-package removes packages.

  moduleToPkgConfAll 	:: UniqFM [(PackageConfig,Bool)] -- ModuleEnv mapping
	-- Derived from pkgIdMap.	
	-- Maps Module to (pkgconf,exposed), where pkgconf is the
	-- PackageConfig for the package containing the module, and
	-- exposed is True if the package exposes that module.
  }

-- A PackageConfigMap maps a PackageId to a PackageConfig
type PackageConfigMap = UniqFM PackageConfig

emptyPackageConfigMap :: PackageConfigMap
emptyPackageConfigMap = emptyUFM

lookupPackage :: PackageConfigMap -> PackageId -> Maybe PackageConfig
lookupPackage = lookupUFM

extendPackageConfigMap
   :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPackageConfigMap pkg_map new_pkgs 
  = foldl add pkg_map new_pkgs
  where add pkg_map p = addToUFM pkg_map (packageConfigId p) p

getPackageDetails :: PackageState -> PackageId -> PackageConfig
getPackageDetails dflags ps = expectJust "getPackageDetails" (lookupPackage (pkgIdMap dflags) ps)

-- ----------------------------------------------------------------------------
-- Loading the package config files and building up the package state

-- | Call this after parsing the DynFlags.  It reads the package
-- configuration files, and sets up various internal tables of package
-- information, according to the package-related flags on the
-- command-line (@-package@, @-hide-package@ etc.)
initPackages :: DynFlags -> IO DynFlags
initPackages dflags = do 
  pkg_map <- readPackageConfigs dflags; 
  mkPackageState dflags pkg_map

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageConfigs :: DynFlags -> IO PackageConfigMap
readPackageConfigs dflags = do
   e_pkg_path <- try (getEnv "GHC_PACKAGE_PATH")
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
   system_pkgconf <- getPackageConfigPath

	-- allow package.conf.d to contain a bunch of .conf files
	-- containing package specifications.  This is an easier way
	-- to maintain the package database on systems with a package
	-- management system, or systems that don't want to run ghc-pkg
	-- to register or unregister packages.  Undocumented feature for now.
   let system_pkgconf_dir = system_pkgconf ++ ".d"
   system_pkgconf_dir_exists <- doesDirectoryExist system_pkgconf_dir
   system_pkgconfs <-
     if system_pkgconf_dir_exists
       then do files <- getDirectoryContents system_pkgconf_dir
               return [ system_pkgconf_dir ++ '/' : file
                      | file <- files
                      , isSuffixOf ".conf" file]
       else return []

	-- Read user's package conf (eg. ~/.ghc/i386-linux-6.3/package.conf)
	-- unless the -no-user-package-conf flag was given.
	-- We only do this when getAppUserDataDirectory is available 
	-- (GHC >= 6.3).
   user_pkgconf <- handle (\_ -> return []) $ do
      appdir <- getAppUserDataDirectory "ghc"
      let 
     	 pkgconf = appdir
		   `joinFileName` (TARGET_ARCH ++ '-':TARGET_OS ++ '-':cProjectVersion)
		   `joinFileName` "package.conf"
      flg <- doesFileExist pkgconf
      if (flg && dopt Opt_ReadUserPackageConf dflags)
	then return [pkgconf]
	else return []

   return (user_pkgconf ++ system_pkgconfs ++ [system_pkgconf])


readPackageConfig
   :: DynFlags -> PackageConfigMap -> FilePath -> IO PackageConfigMap
readPackageConfig dflags pkg_map conf_file = do
  debugTraceMsg dflags 2 (text "Using package config file:" <+> text conf_file)
  proto_pkg_configs <- loadPackageConfig conf_file
  top_dir 	    <- getTopDir
  let pkg_configs1 = mungePackagePaths top_dir proto_pkg_configs
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
		   frameworkDirs = munge_paths (frameworkDirs p) }

  munge_paths = map munge_path

  munge_path p 
	  | Just p' <- maybePrefixMatch "$topdir" p = top_dir ++ p'
	  | otherwise				    = p


-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our package
-- settings and populate the package state.

mkPackageState :: DynFlags -> PackageConfigMap -> IO DynFlags
mkPackageState dflags orig_pkg_db = do
  --
  -- Modify the package database according to the command-line flags
  -- (-package, -hide-package, -ignore-package, -hide-all-packages).
  --
  -- Also, here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "explicit" packages.
  -- we link these packages in eagerly.  The explicit set should contain
  -- at least rts & base, which is why we pretend that the command line
  -- contains -package rts & -package base.
  --
  let
	flags = reverse (packageFlags dflags)

	procflags pkgs expl [] = return (pkgs,expl)
	procflags pkgs expl (ExposePackage str : flags) = do
	   case pick str pkgs of
		Nothing -> missingPackageErr str
		Just (p,ps) -> procflags (p':ps') expl' flags
		  where p' = p {exposed=True}
		        ps' = hideAll (pkgName (package p)) ps
			expl' = package p : expl
	procflags pkgs expl (HidePackage str : flags) = do
	   case partition (matches str) pkgs of
		([],_)   -> missingPackageErr str
		(ps,qs) -> procflags (map hide ps ++ qs) expl flags
		  where hide p = p {exposed=False}
	procflags pkgs expl (IgnorePackage str : flags) = do
	   case partition (matches str) pkgs of
		(ps,qs) -> procflags qs expl flags
		-- missing package is not an error for -ignore-package,
		-- because a common usage is to -ignore-package P as
		-- a preventative measure just in case P exists.

	pick str pkgs
	  = case partition (matches str) pkgs of
		([],_) -> Nothing
		(ps,rest) -> 
		   case sortByVersion ps of
			(p:ps) -> Just (p, ps ++ rest)
			_ -> panic "Packages.pick"

        sortByVersion = sortBy (flip (comparing (pkgVersion.package)))
        comparing f a b = f a `compare` f b

	-- A package named on the command line can either include the
	-- version, or just the name if it is unambiguous.
	matches str p
		=  str == showPackageId (package p)
		|| str == pkgName (package p)

	-- When a package is requested to be exposed, we hide all other
	-- packages with the same name.
	hideAll name ps = map maybe_hide ps
	  where maybe_hide p | pkgName (package p) == name = p {exposed=False}
			     | otherwise                   = p
  --
  (pkgs1,explicit) <- procflags (eltsUFM orig_pkg_db) [] flags
  --
  -- hide all packages for which there is also a later version
  -- that is already exposed.  This just makes it non-fatal to have two
  -- versions of a package exposed, which can happen if you install a
  -- later version of a package in the user database, for example.
  --
  let maybe_hide p
	   | not (exposed p) = return p
	   | (p' : _) <- later_versions = do
		debugTraceMsg dflags 2 $
		   (ptext SLIT("hiding package") <+> text (showPackageId (package p)) <+>
		    ptext SLIT("to avoid conflict with later version") <+>
		    text (showPackageId (package p')))
		return (p {exposed=False})
	   | otherwise = return p
	  where myname = pkgName (package p)
		myversion = pkgVersion (package p)
		later_versions = [ p | p <- pkgs1, exposed p,
				    let pkg = package p,
				    pkgName pkg == myname,
				    pkgVersion pkg > myversion ]

  pkgs2 <- mapM maybe_hide pkgs1
  --
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base).
  --
  let
	wired_in_pkgids = [ basePackageId,
			    rtsPackageId,
			    haskell98PackageId,
			    thPackageId ]

	wired_in_names = map packageIdString wired_in_pkgids

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
			   -> IO (Maybe PackageIdentifier)
	findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, pkgName (package p) == wired_pkg ] in
	   case filter exposed all_ps of
		[] -> case all_ps of
                        []   -> notfound
                        many -> pick (head (sortByVersion many))
		many  -> pick (head (sortByVersion many))
          where
                notfound = do
			  debugTraceMsg dflags 2 $
			    ptext SLIT("wired-in package ")
				 <> text wired_pkg
				 <> ptext SLIT(" not found.")
			  return Nothing
                pick pkg = do
                        debugTraceMsg dflags 2 $
			    ptext SLIT("wired-in package ")
				 <> text wired_pkg
				 <> ptext SLIT(" mapped to ")
				 <> text (showPackageId (package pkg))
			return (Just (package pkg))


  mb_wired_in_ids <- mapM (findWiredInPackage pkgs2) wired_in_names
  let 
        wired_in_ids = catMaybes mb_wired_in_ids

	deleteOtherWiredInPackages pkgs = filter ok pkgs
	  where ok p = pkgName (package p) `notElem` wired_in_names
                     || package p `elem` wired_in_ids

	updateWiredInDependencies pkgs = map upd_pkg pkgs
	  where upd_pkg p = p{ package = upd_pid (package p),
			       depends = map upd_pid (depends p) }

	upd_pid pid = case filter (== pid) wired_in_ids of
				[] -> pid
				(x:_) -> x{ pkgVersion = Version [] [] }

        pkgs3 = deleteOtherWiredInPackages pkgs2

        pkgs4 = updateWiredInDependencies pkgs3

        explicit1 = map upd_pid explicit

        -- we must return an updated thisPackage, just in case we
        -- are actually compiling one of the wired-in packages
        Just old_this_pkg = unpackPackageId (thisPackage dflags)
        new_this_pkg = mkPackageId (upd_pid old_this_pkg)

  --
  -- Eliminate any packages which have dangling dependencies (perhaps
  -- because the package was removed by -ignore-package).
  --
  let
	elimDanglingDeps pkgs = 
	   case partition (not.null.snd) (map (getDanglingDeps pkgs) pkgs) of
	      ([],ps) -> return (map fst ps)
	      (ps,qs) -> do
		 mapM_ reportElim ps
		 elimDanglingDeps (map fst qs)

	reportElim (p, deps) = 
		debugTraceMsg dflags 2 $
		   (ptext SLIT("package") <+> pprPkg p <+> 
			ptext SLIT("will be ignored due to missing dependencies:") $$ 
		    nest 2 (hsep (map (text.showPackageId) deps)))

	getDanglingDeps pkgs p = (p, filter dangling (depends p))
	  where dangling pid = pid `notElem` all_pids
		all_pids = map package pkgs
  --
  pkgs <- elimDanglingDeps pkgs4
  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs
  --
  -- Find the transitive closure of dependencies of exposed
  --
  let exposed_pkgids = [ packageConfigId p | p <- pkgs, exposed p ]
  dep_exposed <- closeDeps pkg_db exposed_pkgids
  let
	-- add base & rts to the explicit packages
	basicLinkedPackages = filter (flip elemUFM pkg_db)
				 [basePackageId,rtsPackageId]
	explicit2 = addListToUniqSet (mkUniqSet (map mkPackageId explicit1))
                                     basicLinkedPackages
  --
  -- Close the explicit packages with their dependencies
  --
  dep_explicit <- closeDeps pkg_db (uniqSetToList explicit2)
  --
  -- Build up a mapping from Module -> PackageConfig for all modules.
  -- Discover any conflicts at the same time, and factor in the new exposed
  -- status of each package.
  --
  let mod_map = mkModuleMap pkg_db dep_exposed

      pstate = PackageState{ explicitPackages     = dep_explicit,
		             origPkgIdMap	    = orig_pkg_db,
		             pkgIdMap   	    = pkg_db,
		             moduleToPkgConfAll   = mod_map
		           }

  return dflags{ pkgState = pstate, thisPackage = new_this_pkg }
  -- done!


mkModuleMap
  :: PackageConfigMap
  -> [PackageId]
  -> UniqFM [(PackageConfig, Bool)]
mkModuleMap pkg_db pkgs = foldr extend_modmap emptyUFM pkgs
  where
	extend_modmap pkgid modmap =
		addListToUFM_C (++) modmap 
		    [(m, [(pkg, m `elem` exposed_mods)]) | m <- all_mods]
	  where
		pkg = expectJust "mkModuleMap" (lookupPackage pkg_db pkgid)
	        exposed_mods = map mkModuleName (exposedModules pkg)
	        hidden_mods  = map mkModuleName (hiddenModules pkg)
		all_mods = exposed_mods ++ hidden_mods

pprPkg :: PackageConfig -> SDoc
pprPkg p = text (showPackageId (package p))

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of explicit (command-line) packages to determine which packages to
-- use.

getPackageIncludePath :: DynFlags -> [PackageId] -> IO [String]
getPackageIncludePath dflags pkgs = do
  ps <- getExplicitPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap includeDirs ps)))

	-- includes are in reverse dependency order (i.e. rts first)
getPackageCIncludes :: [PackageConfig] -> IO [String]
getPackageCIncludes pkg_configs = do
  return (reverse (nub (filter notNull (concatMap includes pkg_configs))))

getPackageLibraryPath :: DynFlags -> [PackageId] -> IO [String]
getPackageLibraryPath dflags pkgs = do 
  ps <- getExplicitPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap libraryDirs ps)))

getPackageLinkOpts :: DynFlags -> [PackageId] -> IO [String]
getPackageLinkOpts dflags pkgs = do
  ps <- getExplicitPackagesAnd dflags pkgs
  let tag = buildTag dflags
      rts_tag = rtsBuildTag dflags
  let 
	imp        = if opt_Static then "" else "_dyn"
      	libs p     = map ((++imp) . addSuffix) (hsLibraries p)
      	                 ++ hACK_dyn (extraLibraries p)
	all_opts p = map ("-l" ++) (libs p) ++ ldOptions p

	suffix     = if null tag then "" else  '_':tag
	rts_suffix = if null rts_tag then "" else  '_':rts_tag

        addSuffix rts@"HSrts"    = rts       ++ rts_suffix
        addSuffix other_lib      = other_lib ++ suffix

        -- This is a hack that's even more horrible (and hopefully more temporary)
        -- than the one below [referring to previous splittage of HSbase into chunks
	-- to work around GNU ld bug]. HSbase_cbits and friends require the _dyn suffix
        -- for dynamic linking, but not _p or other 'way' suffix. So we just add
        -- _dyn to extraLibraries if they already have a _cbits suffix.
        
        hACK_dyn = map hack
          where hack lib | not opt_Static && "_cbits" `isSuffixOf` lib = lib ++ "_dyn"
                         | otherwise = lib

  return (concat (map all_opts ps))

getPackageExtraCcOpts :: DynFlags -> [PackageId] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getExplicitPackagesAnd dflags pkgs
  return (concatMap ccOptions ps)

getPackageFrameworkPath  :: DynFlags -> [PackageId] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getExplicitPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap frameworkDirs ps)))

getPackageFrameworks  :: DynFlags -> [PackageId] -> IO [String]
getPackageFrameworks dflags pkgs = do
  ps <- getExplicitPackagesAnd dflags pkgs
  return (concatMap frameworks ps)

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a Module, and if the module is in a package returns 
-- @(pkgconf,exposed)@ where pkgconf is the PackageConfig for that package,
-- and exposed is True if the package exposes the module.
lookupModuleInAllPackages :: DynFlags -> ModuleName -> [(PackageConfig,Bool)]
lookupModuleInAllPackages dflags m =
  case lookupUFM (moduleToPkgConfAll (pkgState dflags)) m of
	Nothing -> []
	Just ps -> ps

getExplicitPackagesAnd :: DynFlags -> [PackageId] -> IO [PackageConfig]
getExplicitPackagesAnd dflags pkgids =
  let 
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      expl    = explicitPackages state
  in do
  all_pkgs <- throwErr (foldM (add_package pkg_map) expl pkgids)
  return (map (getPackageDetails state) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: PackageConfigMap -> [PackageId] -> IO [PackageId]
closeDeps pkg_map ps = throwErr (closeDepsErr pkg_map ps)

throwErr :: MaybeErr Message a -> IO a
throwErr m = case m of
		Failed e    -> throwDyn (CmdLineError (showSDoc e))
		Succeeded r -> return r

closeDepsErr :: PackageConfigMap -> [PackageId]
	-> MaybeErr Message [PackageId]
closeDepsErr pkg_map ps = foldM (add_package pkg_map) [] ps

-- internal helper
add_package :: PackageConfigMap -> [PackageId] -> PackageId 
	-> MaybeErr Message [PackageId]
add_package pkg_db ps p
  | p `elem` ps = return ps	-- Check if we've already added this package
  | otherwise =
      case lookupPackage pkg_db p of
        Nothing -> Failed (missingPackageMsg (packageIdString p))
        Just pkg -> do
    	   -- Add the package's dependents also
	   let deps = map mkPackageId (depends pkg)
    	   ps' <- foldM (add_package pkg_db) ps deps
    	   return (p : ps')

missingPackageErr p = throwDyn (CmdLineError (showSDoc (missingPackageMsg p)))
missingPackageMsg p = ptext SLIT("unknown package:") <+> text p

-- -----------------------------------------------------------------------------

isDllName :: PackageId -> Name -> Bool
isDllName this_pkg name
  | opt_Static = False
  | Just mod <- nameModule_maybe name = modulePackageId mod /= this_pkg
  | otherwise = False  -- no, it is not even an external name

-- -----------------------------------------------------------------------------
-- Displaying packages

dumpPackages :: DynFlags -> IO ()
-- Show package info on console, if verbosity is >= 3
dumpPackages dflags
  = do  let pkg_map = pkgIdMap (pkgState dflags)
	putMsg dflags $
	      vcat (map (text.showInstalledPackageInfo) (eltsUFM pkg_map))
\end{code}
