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
	initPackages,
	moduleToPackageConfig,
	getPackageDetails,
	isHomeModule,

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
import Module		( Module, mkModule )
import UniqFM
import UniqSet
import Util
import Maybes		( expectJust )
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
import Control.Monad	( when, foldM )
import Data.List	( nub, partition )

#ifdef mingw32_TARGET_OS
import Data.List	( isPrefixOf )
#endif

import FastString
import DATA_IOREF
import EXCEPTION	( throwDyn )
import ErrUtils         ( debugTraceMsg, putMsg )

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

  pkgIdMap		:: PackageConfigMap, -- PackageId   -> PackageConfig
	-- mapping derived from the package databases and
	-- command-line package flags.

  moduleToPkgConf       :: UniqFM (PackageConfig,Bool),
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
     	 pkgconf = appdir ++ '/':TARGET_ARCH ++ '-':TARGET_OS
			++ '-':cProjectVersion ++ "/package.conf"
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
  debugTraceMsg dflags 2 ("Using package config file: " ++ conf_file)
  proto_pkg_configs <- loadPackageConfig conf_file
  top_dir 	    <- getTopDir
  let pkg_configs = mungePackagePaths top_dir proto_pkg_configs
  return (extendPackageConfigMap pkg_map pkg_configs)


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
mkPackageState dflags pkg_db = do
  --
  -- Modify the package database according to the command-line flags
  -- (-package, -hide-package, -ignore-package).
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
	   case partition (matches str) pkgs of
		([],_)   -> missingPackageErr str
		([p],ps) -> procflags (p':ps) (addOneToUniqSet expl pkgid) flags
		  where pkgid = packageConfigId p
			p' = p {exposed=True}
		(ps,_)   -> multiplePackagesErr str ps
	procflags pkgs expl (HidePackage str : flags) = do
	   case partition (matches str) pkgs of
		([],_)   -> missingPackageErr str
		([p],ps) -> procflags (p':ps) expl flags
		  where p' = p {exposed=False}
		(ps,_)   -> multiplePackagesErr str ps
	procflags pkgs expl (IgnorePackage str : flags) = do
	   case partition (matches str) pkgs of
		(ps,qs) -> procflags qs expl flags
		-- missing package is not an error for -ignore-package,
		-- because a common usage is to -ignore-package P as
		-- a preventative measure just in case P exists.

	-- A package named on the command line can either include the
	-- version, or just the name if it is unambiguous.
	matches str p
		=  str == showPackageId (package p)
		|| str == pkgName (package p)
  --
  (pkgs1,explicit) <- procflags (eltsUFM pkg_db) emptyUniqSet flags
  --
  let
	elimDanglingDeps pkgs = 
	   case partition (hasDanglingDeps pkgs) pkgs of
	      ([],ps) -> ps
	      (ps,qs) -> elimDanglingDeps qs

	hasDanglingDeps pkgs p = any dangling (depends p)
	  where dangling pid = pid `notElem` all_pids
		all_pids = map package pkgs
  --
  -- Eliminate any packages which have dangling dependencies (perhaps
  -- because the package was removed by -ignore-package).
  --
  let pkgs = elimDanglingDeps pkgs1
      pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs
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
  let
	extend_modmap modmap pkgname = do
	  let 
		pkg = expectJust "mkPackageState" (lookupPackage pkg_db pkgname)
	        exposed_mods = map mkModule (exposedModules pkg)
	        hidden_mods  = map mkModule (hiddenModules pkg)
		all_mods = exposed_mods ++ hidden_mods
	  --
	  -- check for overlaps
	  --
	  let
		overlaps = [ (m,pkg) | m <- all_mods, 
				       Just (pkg,_) <- [lookupUFM modmap m] ]
	  --
	  when (not (null overlaps)) $ overlappingError pkg overlaps
	  --
	  return (addListToUFM modmap 
		    [(m, (pkg, m `elem` exposed_mods)) 
		    | m <- all_mods])
  --
  mod_map <- foldM extend_modmap emptyUFM dep_exposed

  return PackageState{ explicitPackages    = dep_explicit,
		       pkgIdMap   	   = pkg_db,
		       moduleToPkgConf 	   = mod_map,
		       basePackageId	   = basePackageId,
		       rtsPackageId	   = rtsPackageId,
  		       haskell98PackageId  = haskell98PackageId,
  		       thPackageId         = thPackageId
		     }
  -- done!

basePackageName      = FSLIT("base")
rtsPackageName	     = FSLIT("rts")
haskell98PackageName = FSLIT("haskell98")
thPackageName        = FSLIT("template-haskell")
				-- Template Haskell libraries in here

overlappingError pkg overlaps
  = throwDyn (CmdLineError (showSDoc (vcat (map msg overlaps))))
  where 
	this_pkg = text (showPackageId (package pkg))
	msg (mod,other_pkg) =
	   text "Error: module '" <> ppr mod
		 <> text "' is exposed by package "
		 <> this_pkg <> text " and package "
		 <> text (showPackageId (package other_pkg))

multiplePackagesErr str ps =
  throwDyn (CmdLineError (showSDoc (
		   text "Error; multiple packages match" <+> 
			text str <> colon <+>
		    sep (punctuate comma (map (text.showPackageId.package) ps))
		)))

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
      	libs p     = map ((++imp) . addSuffix) (hACK (hsLibraries p)) ++ extraLibraries p
	all_opts p = map ("-l" ++) (libs p) ++ ldOptions p

	suffix     = if null tag then "" else  '_':tag
	rts_suffix = if null rts_tag then "" else  '_':rts_tag

        addSuffix rts@"HSrts"    = rts       ++ rts_suffix
        addSuffix other_lib      = other_lib ++ suffix

  return (concat (map all_opts ps))
  where

     -- This is a totally horrible (temporary) hack, for Win32.  Problem is
     -- that package.conf for Win32 says that the main prelude lib is 
     -- split into HSbase1, HSbase2 and HSbase3, which is needed due to a bug
     -- in the GNU linker (PEi386 backend). However, we still only
     -- have HSbase.a for static linking, not HSbase{1,2,3}.a
     -- getPackageLibraries is called to find the .a's to add to the static
     -- link line.  On Win32, this hACK detects HSbase{1,2,3} and 
     -- replaces them with HSbase, so static linking still works.
     -- Libraries needed for dynamic (GHCi) linking are discovered via
     -- different route (in InteractiveUI.linkPackage).
     -- See driver/PackageSrc.hs for the HSbase1/HSbase2 split definition.
     -- THIS IS A STRICTLY TEMPORARY HACK (famous last words ...)
     -- JRS 04 Sept 01: Same appalling hack for HSwin32[1,2]
     -- KAA 29 Mar  02: Same appalling hack for HSobjectio[1,2,3,4]
     --
     -- [sof 03/05: Renamed the (moribund) HSwin32 to HSwin_32 so as to
     --  avoid filename conflicts with the 'Win32' package on a case-insensitive filesystem]
     hACK libs
#      if !defined(mingw32_TARGET_OS) && !defined(cygwin32_TARGET_OS)
       = libs
#      else
       = if   "HSbase1" `elem` libs && "HSbase2" `elem` libs && "HSbase3" `elem` libs
         then "HSbase" : filter (not.(isPrefixOf "HSbase")) libs
         else
         if   "HSwin_321" `elem` libs && "HSwin_322" `elem` libs
         then "HSwin_32" : filter (not.(isPrefixOf "HSwin_32")) libs
         else 
         if   "HSobjectio1" `elem` libs && "HSobjectio2" `elem` libs && "HSobjectio3" `elem` libs && "HSobjectio4" `elem` libs
	 then "HSobjectio" : filter (not.(isPrefixOf "HSobjectio")) libs
         else 
         libs
#      endif

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

-- Takes a Module, and if the module is in a package returns 
-- (pkgconf,exposed) where pkgconf is the PackageConfig for that package,
-- and exposed is True if the package exposes the module.
moduleToPackageConfig :: DynFlags -> Module -> Maybe (PackageConfig,Bool)
moduleToPackageConfig dflags m = 
  lookupUFM (moduleToPkgConf (pkgState dflags)) m

isHomeModule :: DynFlags -> Module -> Bool
isHomeModule dflags mod = isNothing (moduleToPackageConfig dflags mod)

getExplicitPackagesAnd :: DynFlags -> [PackageId] -> IO [PackageConfig]
getExplicitPackagesAnd dflags pkgids =
  let 
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      expl    = explicitPackages state
  in do
  all_pkgs <- foldM (add_package pkg_map) expl pkgids
  return (map (getPackageDetails state) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: PackageConfigMap -> [PackageId] -> IO [PackageId]
closeDeps pkg_map ps = foldM (add_package pkg_map) [] ps

-- internal helper
add_package :: PackageConfigMap -> [PackageId] -> PackageId -> IO [PackageId]
add_package pkg_db ps p
  | p `elem` ps = return ps	-- Check if we've already added this package
  | otherwise =
      case lookupPackage pkg_db p of
        Nothing -> missingPackageErr (packageIdString p)
        Just pkg -> do
    	   -- Add the package's dependents also
	   let deps = map mkPackageId (depends pkg)
    	   ps' <- foldM (add_package pkg_db) ps deps
    	   return (p : ps')

missingPackageErr p =  throwDyn (CmdLineError ("unknown package: " ++ p))

-- -----------------------------------------------------------------------------
-- Determining whether a Name refers to something in another package or not.
-- Cross-package references need to be handled differently when dynamically-
-- linked libraries are involved.

isDllName :: DynFlags -> Name -> Bool
isDllName dflags name
  | opt_Static = False
  | otherwise =
    case nameModule_maybe name of
        Nothing -> False  -- no, it is not even an external name
        Just mod ->
            case lookupUFM (moduleToPkgConf (pkgState dflags)) mod of
                Just _  -> True   -- yes, its a package module
                Nothing -> False  -- no, must be a home module

-- -----------------------------------------------------------------------------
-- Displaying packages

dumpPackages :: DynFlags -> IO ()
-- Show package info on console, if verbosity is >= 3
dumpPackages dflags
  = do  let pkg_map = pkgIdMap (pkgState dflags)
	putMsg $ showSDoc $
	      vcat (map (text.showInstalledPackageInfo) (eltsUFM pkg_map))
\end{code}
