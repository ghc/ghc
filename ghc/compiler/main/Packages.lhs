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
	PackageIdH(..), isHomePackage,
	PackageState(..),
	mkPackageState,
	initPackages,
	getPackageDetails,
	checkForPackageConflicts,
	lookupModuleInAllPackages,

	HomeModules, mkHomeModules, isHomeModule,

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
import FiniteMap
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

import Distribution.InstalledPackageInfo
import Distribution.Package
import Distribution.Version
import Data.Maybe	( isNothing )
import System.Directory	( doesFileExist )
import Control.Monad	( foldM )
import Data.List	( nub, partition, sortBy )

#ifdef mingw32_TARGET_OS
import Data.List	( isPrefixOf )
#endif
import Data.List        ( isSuffixOf )

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
--   * It is an error for any two packages in depExposedPackages to provide the
--     same module.
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


-- One important thing that the package state provides is a way to
-- tell, for a given module, whether it is part of the current package
-- or not.  We need to know this for two reasons:
--
--  * generating cross-DLL calls is different from intra-DLL calls 
--    (see below).
--  * we don't record version information in interface files for entities
--    in a different package.
-- 
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

  moduleToPkgConfAll 	:: ModuleEnv [(PackageConfig,Bool)],
	-- Derived from pkgIdMap.	
	-- Maps Module to (pkgconf,exposed), where pkgconf is the
	-- PackageConfig for the package containing the module, and
	-- exposed is True if the package exposes that module.

  -- The PackageIds of some known packages
  basePackageId		:: PackageIdH,
  rtsPackageId		:: PackageIdH,
  haskell98PackageId	:: PackageIdH,
  thPackageId		:: PackageIdH
  }

data PackageIdH 
   = HomePackage 		-- The "home" package is the package curently
				-- being compiled
   | ExtPackage PackageId	-- An "external" package is any other package


isHomePackage :: PackageIdH -> Bool
isHomePackage HomePackage    = True
isHomePackage (ExtPackage _) = False

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
  state <- mkPackageState dflags pkg_map
  return dflags{ pkgState = state }

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageConfigs :: DynFlags -> IO PackageConfigMap
readPackageConfigs dflags = do
	-- System one always comes first
   system_pkgconf <- getPackageConfigPath
   pkg_map1 <- readPackageConfig dflags emptyPackageConfigMap system_pkgconf

	-- Read user's package conf (eg. ~/.ghc/i386-linux-6.3/package.conf)
	-- unless the -no-user-package-conf flag was given.
	-- We only do this when getAppUserDataDirectory is available 
	-- (GHC >= 6.3).
   (exists, pkgconf) <- catch (do
      appdir <- getAppUserDataDirectory "ghc"
      let 
     	 pkgconf = appdir
		   `joinFileName` (TARGET_ARCH ++ '-':TARGET_OS ++ '-':cProjectVersion)
		   `joinFileName` "package.conf"
      flg <- doesFileExist pkgconf
      return (flg, pkgconf))
       -- gobble them all up and turn into False.
      (\ _ -> return (False, ""))
   pkg_map2 <- if (dopt Opt_ReadUserPackageConf dflags && exists)
		  then readPackageConfig dflags pkg_map1 pkgconf
		  else return pkg_map1

	-- Read all the ones mentioned in -package-conf flags
   pkg_map <- foldM (readPackageConfig dflags) pkg_map2
		 (extraPkgConfs dflags)

   return pkg_map


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

mkPackageState :: DynFlags -> PackageConfigMap -> IO PackageState
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
		  where pkgid = packageConfigId p
			p' = p {exposed=True}
		        ps' = hideAll (pkgName (package p)) ps
			expl' = addOneToUniqSet expl pkgid
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
		   case sortBy (flip (comparing (pkgVersion.package))) ps of
			(p:ps) -> Just (p, ps ++ rest)
			_ -> panic "Packages.pick"

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
  (pkgs1,explicit) <- procflags (eltsUFM orig_pkg_db) emptyUniqSet flags
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
		a_later_version_is_exposed
		  = not (null later_versions)

  pkgs2 <- mapM maybe_hide pkgs1
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
  pkgs <- elimDanglingDeps pkgs2
  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs
  --
  -- Find the transitive closure of dependencies of exposed
  --
  let exposed_pkgids = [ packageConfigId p | p <- pkgs, exposed p ]
  dep_exposed <- closeDeps pkg_db exposed_pkgids
  --
  -- Look up some known PackageIds
  --
  let
	lookupPackageByName :: FastString -> PackageIdH
	lookupPackageByName nm = 
	  case [ conf | p <- dep_exposed,
			Just conf <- [lookupPackage pkg_db p],
			nm == mkFastString (pkgName (package conf)) ] of
		[]     -> HomePackage
		(p:ps) -> ExtPackage (mkPackageId (package p))

	-- Get the PackageIds for some known packages (we know the names,
	-- but we don't know the versions).  Some of these packages might
	-- not exist in the database, so they are Maybes.
	basePackageId		= lookupPackageByName basePackageName
	rtsPackageId		= lookupPackageByName rtsPackageName
	haskell98PackageId	= lookupPackageByName haskell98PackageName
	thPackageId		= lookupPackageByName thPackageName

	-- add base & rts to the explicit packages
	basicLinkedPackages = [basePackageId,rtsPackageId]
	explicit' = addListToUniqSet explicit 
			[ p | ExtPackage p <- basicLinkedPackages ]
  --
  -- Close the explicit packages with their dependencies
  --
  dep_explicit <- closeDeps pkg_db (uniqSetToList explicit')
  --
  -- Build up a mapping from Module -> PackageConfig for all modules.
  -- Discover any conflicts at the same time, and factor in the new exposed
  -- status of each package.
  --
  let mod_map = mkModuleMap pkg_db dep_exposed

  return PackageState{ explicitPackages     = dep_explicit,
		       origPkgIdMap	    = orig_pkg_db,
		       pkgIdMap   	    = pkg_db,
		       moduleToPkgConfAll   = mod_map,
		       basePackageId	    = basePackageId,
		       rtsPackageId	    = rtsPackageId,
  		       haskell98PackageId   = haskell98PackageId,
  		       thPackageId          = thPackageId
		     }
  -- done!

basePackageName      = FSLIT("base")
rtsPackageName	     = FSLIT("rts")
haskell98PackageName = FSLIT("haskell98")
thPackageName        = FSLIT("template-haskell")
				-- Template Haskell libraries in here

mkModuleMap
  :: PackageConfigMap
  -> [PackageId]
  -> ModuleEnv [(PackageConfig, Bool)]
mkModuleMap pkg_db pkgs = foldr extend_modmap emptyUFM pkgs
  where
	extend_modmap pkgname modmap =
		addListToUFM_C (++) modmap 
		    [(m, [(pkg, m `elem` exposed_mods)]) | m <- all_mods]
	  where
		pkg = expectJust "mkModuleMap" (lookupPackage pkg_db pkgname)
	        exposed_mods = map mkModule (exposedModules pkg)
	        hidden_mods  = map mkModule (hiddenModules pkg)
		all_mods = exposed_mods ++ hidden_mods

-- -----------------------------------------------------------------------------
-- Check for conflicts in the program.

-- | A conflict arises if the program contains two modules with the same
-- name, which can arise if the program depends on multiple packages that
-- expose the same module, or if the program depends on a package that
-- contains a module also present in the program (the "home package").
--
checkForPackageConflicts
   :: DynFlags
   -> [Module]		-- modules in the home package
   -> [PackageId]	-- packages on which the program depends
   -> MaybeErr Message ()

checkForPackageConflicts dflags mods pkgs = do
    let 
	state   = pkgState dflags
	pkg_db  = pkgIdMap state
    --
    dep_pkgs <- closeDepsErr pkg_db pkgs

    let 
	extend_modmap pkgname modmap  =
		addListToFM_C (++) modmap
		    [(m, [(pkg, m `elem` exposed_mods)]) | m <- all_mods]
	  where
		pkg = expectJust "checkForPackageConflicts" 
				(lookupPackage pkg_db pkgname)
	        exposed_mods = map mkModule (exposedModules pkg)
	        hidden_mods  = map mkModule (hiddenModules pkg)
		all_mods = exposed_mods ++ hidden_mods

        mod_map = foldr extend_modmap emptyFM pkgs
	mod_map_list :: [(Module,[(PackageConfig,Bool)])]
        mod_map_list = fmToList mod_map

	overlaps = [ (m, map fst ps) | (m,ps@(_:_:_)) <- mod_map_list ]
    --
    if not (null overlaps)
	then Failed (pkgOverlapError overlaps)
	else do

    let 
	overlap_mods = [ (mod,pkg)
		       | mod <- mods,
		         Just ((pkg,_):_) <- [lookupFM mod_map mod] ]    
				-- will be only one package here
    if not (null overlap_mods)
	then Failed (modOverlapError overlap_mods)
	else do

    return ()
       
pkgOverlapError overlaps =  vcat (map msg overlaps)
  where 
	msg (mod,pkgs) =
	   text "conflict: module" <+> quotes (ppr mod)
		 <+> ptext SLIT("is present in multiple packages:")
		 <+> hsep (punctuate comma (map pprPkg pkgs))

modOverlapError overlaps =   vcat (map msg overlaps)
  where 
	msg (mod,pkg) = fsep [
	   	text "conflict: module",
		quotes (ppr mod),
		ptext SLIT("belongs to the current program/library"),
		ptext SLIT("and also to package"),
		pprPkg pkg ]

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
lookupModuleInAllPackages :: DynFlags -> Module -> [(PackageConfig,Bool)]
lookupModuleInAllPackages dflags m =
  case lookupModuleEnv (moduleToPkgConfAll (pkgState dflags)) m of
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
-- The home module set

newtype HomeModules = HomeModules ModuleSet

mkHomeModules :: [Module] -> HomeModules
mkHomeModules = HomeModules . mkModuleSet

isHomeModule :: HomeModules -> Module -> Bool
isHomeModule (HomeModules set) mod  = elemModuleSet mod set

-- Determining whether a Name refers to something in another package or not.
-- Cross-package references need to be handled differently when dynamically-
-- linked libraries are involved.

isDllName :: HomeModules -> Name -> Bool
isDllName pdeps name
  | opt_Static = False
  | Just mod <- nameModule_maybe name = not (isHomeModule pdeps mod)
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
