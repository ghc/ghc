%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    flushFinderCache,	-- :: IO ()
    FindResult(..),
    findModule,		-- :: ModuleName -> Bool -> IO FindResult
    findPackageModule,  -- :: ModuleName -> Bool -> IO FindResult
    mkHomeModLocation,	-- :: ModuleName -> FilePath -> IO ModLocation
    findLinkable,	-- :: ModuleName -> ModLocation -> IO (Maybe Linkable)

    hiBootExt,		-- :: String
    hiBootVerExt,	-- :: String

  ) where

#include "HsVersions.h"

import Module
import UniqFM		( filterUFM )
import HscTypes		( Linkable(..), Unlinked(..), IfacePackage(..) )
import Packages
import DriverState
import DriverUtil
import FastString
import Config
import Util
import CmdLineOpts	( DynFlags(..) )

import DATA_IOREF	( IORef, writeIORef, readIORef )

import Data.List
import System.Directory
import System.IO
import Control.Monad
import Data.Maybe	( isNothing )

-- -----------------------------------------------------------------------------
-- The Finder

-- The Finder provides a thin filesystem abstraction to the rest of
-- the compiler.  For a given module, it can tell you where the
-- source, interface, and object files for that module live.
-- 
-- It does *not* know which particular package a module lives in.  Use
-- Packages.moduleToPackageConfig for that.

-- -----------------------------------------------------------------------------
-- The finder's cache

GLOBAL_VAR(finder_cache, emptyModuleEnv, ModuleEnv FinderCacheEntry)

type FinderCacheEntry = (ModLocation,Maybe (PackageConfig,Bool))

-- remove all the home modules from the cache; package modules are
-- assumed to not move around during a session.
flushFinderCache :: IO ()
flushFinderCache = do
  fm <- readIORef finder_cache
  writeIORef finder_cache (filterUFM (\(loc,m) -> isNothing m) fm)

addToFinderCache :: Module -> FinderCacheEntry -> IO ()
addToFinderCache mod_name entry = do
  fm <- readIORef finder_cache
  writeIORef finder_cache (extendModuleEnv fm mod_name entry)

lookupFinderCache :: Module -> IO (Maybe FinderCacheEntry)
lookupFinderCache mod_name = do
  fm <- readIORef finder_cache
  return $! lookupModuleEnv fm mod_name

-- -----------------------------------------------------------------------------
-- Locating modules

-- This is the main interface to the finder, which maps ModuleNames to
-- Modules and ModLocations.
--
-- The Module contains one crucial bit of information about a module:
-- whether it lives in the current ("home") package or not (see Module
-- for more details).
--
-- The ModLocation contains the names of all the files associated with
-- that module: its source file, .hi file, object file, etc.

data FindResult
  = Found ModLocation IfacePackage
	-- the module was found
  | PackageHidden PackageId
	-- for an explicit source import: the package containing the module is
	-- not exposed.
  | ModuleHidden  PackageId
	-- for an explicit source import: the package containing the module is
	-- exposed, but the module itself is hidden.
  | NotFound [FilePath]
	-- the module was not found, the specified places were searched.

findModule :: DynFlags -> Module -> Bool -> IO FindResult
findModule = cached findModule'
  
findModule' :: DynFlags -> Module -> Bool -> IO FindResult
findModule' dflags name explicit = do
    r <- findPackageModule' dflags name explicit
    case r of
	NotFound pkg_files -> do
	   j <- maybeHomeModule dflags name
	   case j of
		NotFound home_files -> 
			return (NotFound (home_files ++ pkg_files))
		other_result
			-> return other_result
	other_result
		-> return other_result

cached fn dflags name explicit = do
  m <- lookupFinderCache name
  case m of
    Nothing -> fn dflags name explicit
    Just (loc,maybe_pkg)
	| Just err <- visible explicit maybe_pkg  ->  return err
	| otherwise -> return (Found loc (pkgInfoToId maybe_pkg))
  
pkgInfoToId :: Maybe (PackageConfig,Bool) -> IfacePackage
pkgInfoToId (Just (pkg,_)) = ExternalPackage (mkPackageId (package pkg))
pkgInfoToId Nothing = ThisPackage

-- Is a module visible or not?  Returns Nothing if the import is ok,
-- or Just err if there's a visibility error.
visible :: Bool -> Maybe (PackageConfig,Bool) -> Maybe FindResult
visible explicit maybe_pkg
   | Nothing <- maybe_pkg  =  Nothing	-- home module ==> YES
   | not explicit          =  Nothing	-- implicit import ==> YES
   | Just (pkg, exposed_module) <- maybe_pkg 
    = case () of
	_ | not exposed_module -> Just (ModuleHidden pkgname)
	  | not (exposed pkg)  -> Just (PackageHidden pkgname)
	  | otherwise          -> Nothing
	  where 
		pkgname = packageConfigId pkg
     

hiBootExt = "hi-boot"
hiBootVerExt = "hi-boot-" ++ cHscIfaceFileVersion

maybeHomeModule :: DynFlags -> Module -> IO FindResult
maybeHomeModule dflags mod = do
   let home_path = importPaths dflags
   hisuf     <- readIORef v_Hi_suf
   mode      <- readIORef v_GhcMode

   let
     source_exts = 
      [ ("hs",   mkHomeModLocationSearched mod)
      , ("lhs",  mkHomeModLocationSearched mod)
      ]
     
     hi_exts = [ (hisuf,  mkHiOnlyModLocation hisuf mod) ]
     
     boot_exts =
       [ (hiBootVerExt, mkHiOnlyModLocation hisuf mod)
       , (hiBootExt,    mkHiOnlyModLocation hisuf mod)
       ]

     	-- In compilation manager modes, we look for source files in the home
     	-- package because we can compile these automatically.  In one-shot
     	-- compilation mode we look for .hi and .hi-boot files only.
     	--
     	-- When generating dependencies, we're interested in either category.
     	--
     exts
         | mode == DoMkDependHS   = hi_exts ++ source_exts ++ boot_exts
         | isCompManagerMode mode = source_exts
	 | otherwise {-one-shot-} = hi_exts ++ boot_exts

   searchPathExts home_path mod exts
   	
-- -----------------------------------------------------------------------------
-- Looking for a package module

findPackageModule :: DynFlags -> Module -> Bool -> IO FindResult
findPackageModule = cached findPackageModule'

findPackageModule' :: DynFlags -> Module -> Bool -> IO FindResult
findPackageModule' dflags mod explicit = do
  mode     <- readIORef v_GhcMode

  case moduleToPackageConfig dflags mod of
    Nothing -> return (NotFound [])
    pkg_info@(Just (pkg_conf, module_exposed))
	| Just err <- visible explicit pkg_info  ->  return err
	| otherwise  ->  findPackageIface mode mod paths pkg_info
      where 
	    paths   = importDirs pkg_conf

findPackageIface
	:: GhcMode
	-> Module
	-> [FilePath]
	-> Maybe (PackageConfig,Bool)
	-> IO FindResult
findPackageIface mode mod imp_dirs pkg_info = do
   -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef v_Build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")

  let
     hi_exts =
        [ (package_hisuf, 
	    mkPackageModLocation pkg_info package_hisuf mod) ]

     source_exts = 
       [ ("hs",   mkPackageModLocation pkg_info package_hisuf mod)
       , ("lhs",  mkPackageModLocation pkg_info package_hisuf mod)
       ]

     -- mkdependHS needs to look for source files in packages too, so
     -- that we can make dependencies between package before they have
     -- been built.
     exts 
      | mode == DoMkDependHS = hi_exts ++ source_exts
      | otherwise = hi_exts

      -- we never look for a .hi-boot file in an external package;
      -- .hi-boot files only make sense for the home package.
  searchPathExts imp_dirs mod exts

-- -----------------------------------------------------------------------------
-- General path searching

searchPathExts
  :: [FilePath]		-- paths to search
  -> Module		-- module name
  -> [ (
	String,					     -- suffix
	String -> String -> String -> IO FindResult  -- action
       )
     ] 
  -> IO FindResult

searchPathExts path mod exts = search to_search
  where
    basename = dots_to_slashes (moduleUserString mod)

    to_search :: [(FilePath, IO FindResult)]
    to_search = [ (file, fn p basename ext)
		| p <- path, 
		  (ext,fn) <- exts,
		  let base | p == "."  = basename
	     	           | otherwise = p ++ '/':basename
	              file = base ++ '.':ext
		]

    search [] = return (NotFound (map fst to_search))
    search ((file, result) : rest) = do
      b <- doesFileExist file
      if b 
	then result
	else search rest

-- -----------------------------------------------------------------------------
-- Building ModLocations

mkHiOnlyModLocation hisuf mod path basename _ext = do
  -- basename == dots_to_slashes (moduleNameUserString mod)
  loc <- hiOnlyModLocation path basename hisuf
  addToFinderCache mod (loc, Nothing)
  return (Found loc ThisPackage)

mkPackageModLocation pkg_info hisuf mod path basename _ext = do
  -- basename == dots_to_slashes (moduleNameUserString mod)
  loc <- hiOnlyModLocation path basename hisuf
  addToFinderCache mod (loc, pkg_info)
  return (Found loc (pkgInfoToId pkg_info))

hiOnlyModLocation path basename hisuf 
 = do let full_basename = path++'/':basename
      obj_fn <- mkObjPath full_basename basename
      return ModLocation{    ml_hspp_file = Nothing,
			     ml_hspp_buf  = Nothing,
 	        	     ml_hs_file   = Nothing,
 	        	     ml_hi_file   = full_basename ++ '.':hisuf,
		 		-- Remove the .hi-boot suffix from
		 		-- hi_file, if it had one.  We always
		 		-- want the name of the real .hi file
		 		-- in the ml_hi_file field.
	   	             ml_obj_file  = obj_fn
                  }

-- -----------------------------------------------------------------------------
-- Constructing a home module location

-- This is where we construct the ModLocation for a module in the home
-- package, for which we have a source file.  It is called from three
-- places:
--
--  (a) Here in the finder, when we are searching for a module to import,
--      using the search path (-i option).
--
--  (b) The compilation manager, when constructing the ModLocation for
--      a "root" module (a source file named explicitly on the command line
--      or in a :load command in GHCi).
--
--  (c) The driver in one-shot mode, when we need to construct a
--      ModLocation for a source file named on the command-line.
--
-- Parameters are:
--
-- mod
--      The name of the module
--
-- path
--      (a): The search path component where the source file was found.
--      (b) and (c): "."
--
-- src_basename
--      (a): dots_to_slashes (moduleNameUserString mod)
--      (b) and (c): The filename of the source file, minus its extension
--
-- ext
--	The filename extension of the source file (usually "hs" or "lhs").

mkHomeModLocation mod src_filename = do
   let (basename,extension) = splitFilename src_filename
   mkHomeModLocation' mod basename extension

mkHomeModLocationSearched mod path basename ext = do
   loc <- mkHomeModLocation' mod (path ++ '/':basename) ext
   return (Found loc ThisPackage)

mkHomeModLocation' mod src_basename ext = do
   let mod_basename = dots_to_slashes (moduleUserString mod)

   obj_fn <- mkObjPath src_basename mod_basename
   hi_fn  <- mkHiPath  src_basename mod_basename

   let loc = ModLocation{ ml_hspp_file = Nothing,
			  ml_hspp_buf  = Nothing,
	   		  ml_hs_file   = Just (src_basename ++ '.':ext),
			  ml_hi_file   = hi_fn,
			  ml_obj_file  = obj_fn }

   addToFinderCache mod (loc, Nothing)
   return loc

-- | Constructs the filename of a .o file for a given source file.
-- Does /not/ check whether the .o file exists
mkObjPath
  :: FilePath		-- the filename of the source file, minus the extension
  -> String		-- the module name with dots replaced by slashes
  -> IO FilePath
mkObjPath basename mod_basename
  = do  odir   <- readIORef v_Output_dir
	osuf   <- readIORef v_Object_suf

	let obj_basename | Just dir <- odir = dir ++ '/':mod_basename
	   	         | otherwise        = basename

        return (obj_basename ++ '.':osuf)

-- | Constructs the filename of a .hi file for a given source file.
-- Does /not/ check whether the .hi file exists
mkHiPath
  :: FilePath		-- the filename of the source file, minus the extension
  -> String		-- the module name with dots replaced by slashes
  -> IO FilePath
mkHiPath basename mod_basename
  = do  hidir   <- readIORef v_Hi_dir
	hisuf   <- readIORef v_Hi_suf

	let hi_basename | Just dir <- hidir = dir ++ '/':mod_basename
	   	        | otherwise         = basename

        return (hi_basename ++ '.':hisuf)

-- -----------------------------------------------------------------------------
-- findLinkable isn't related to the other stuff in here, 
-- but there's no other obvious place for it

findLinkable :: Module -> ModLocation -> IO (Maybe Linkable)
findLinkable mod locn
   = do let obj_fn = ml_obj_file locn
	obj_exist <- doesFileExist obj_fn
        if not obj_exist 
         then return Nothing 
         else 
         do let stub_fn = case splitFilename3 obj_fn of
                             (dir, base, ext) -> dir ++ "/" ++ base ++ "_stub.o"
            stub_exist <- doesFileExist stub_fn
            obj_time <- getModificationTime obj_fn
            if stub_exist
             then return (Just (LM obj_time mod [DotO obj_fn, DotO stub_fn]))
             else return (Just (LM obj_time mod [DotO obj_fn]))

-- -----------------------------------------------------------------------------
-- Utils

dots_to_slashes = map (\c -> if c == '.' then '/' else c)

\end{code}
