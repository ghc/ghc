%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    flushFinderCache,	-- :: IO ()

    findModule,		-- :: ModuleName 
			--   -> IO (Either [FilePath] (Module, ModLocation))

    findPackageModule,  -- :: ModuleName
			--   -> IO (Either [FilePath] (Module, ModLocation))

    mkHomeModLocation,	-- :: ModuleName -> FilePath -> IO ModLocation

    findLinkable,	-- :: ModuleName -> ModLocation -> IO (Maybe Linkable)

    hiBootExt,		-- :: String
    hiBootVerExt,	-- :: String

  ) where

#include "HsVersions.h"

import Module
import UniqFM		( filterUFM )
import HscTypes		( Linkable(..), Unlinked(..) )
import DriverState
import DriverUtil
import FastString
import Config
import Util

import DATA_IOREF	( IORef, writeIORef, readIORef )

import List
import Directory
import IO
import Monad

-- -----------------------------------------------------------------------------
-- The Finder

-- The Finder provides a thin filesystem abstraction to the rest of the
-- compiler.  For a given module, it knows (a) whether the module lives
-- in the home package or in another package, so it can make a Module
-- from a ModuleName, and (b) where the source, interface, and object
-- files for a module live.
-- 
-- It does *not* know which particular package a module lives in, because
-- that information is only contained in the interface file.

-- -----------------------------------------------------------------------------
-- The finder's cache

GLOBAL_VAR(finder_cache, emptyModuleEnv, ModuleEnv (Module,ModLocation))

-- remove all the home modules from the cache; package modules are
-- assumed to not move around during a session.
flushFinderCache :: IO ()
flushFinderCache = do
  fm <- readIORef finder_cache
  writeIORef finder_cache (filterUFM (not . isHomeModule . fst) fm)

addToFinderCache :: ModuleName -> (Module,ModLocation) -> IO ()
addToFinderCache mod_name stuff = do
  fm <- readIORef finder_cache
  writeIORef finder_cache (extendModuleEnvByName fm mod_name stuff)

lookupFinderCache :: ModuleName -> IO (Maybe (Module,ModLocation))
lookupFinderCache mod_name = do
  fm <- readIORef finder_cache
  return $! lookupModuleEnvByName fm mod_name

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

findModule :: ModuleName -> IO (Either [FilePath] (Module, ModLocation))
findModule name = do
  r <- lookupFinderCache name
  case r of
   Just result -> return (Right result)
   Nothing -> do  
       j <- maybeHomeModule name
       case j of
	 Right home_module -> return (Right home_module)
	 Left home_files   -> do
	    r <- findPackageMod name
	    case r of
		Right pkg_module -> return (Right pkg_module)
		Left pkg_files   -> return (Left (home_files ++ pkg_files))

findPackageModule :: ModuleName -> IO (Either [FilePath] (Module, ModLocation))
findPackageModule name = do
  r <- lookupFinderCache name
  case r of
   Just result -> return (Right result)
   Nothing     -> findPackageMod name

hiBootExt = "hi-boot"
hiBootVerExt = "hi-boot-" ++ cHscIfaceFileVersion

maybeHomeModule :: ModuleName -> IO (Either [FilePath] (Module, ModLocation))
maybeHomeModule mod_name = do
   home_path <- readIORef v_Import_paths
   hisuf     <- readIORef v_Hi_suf
   mode      <- readIORef v_GhcMode

   let
     source_exts = 
      [ ("hs",   mkHomeModLocationSearched mod_name)
      , ("lhs",  mkHomeModLocationSearched mod_name)
      ]
     
     hi_exts = [ (hisuf,  mkHiOnlyModLocation hisuf mod_name) ]
     
     boot_exts =
       [ (hiBootVerExt, mkHiOnlyModLocation hisuf mod_name)
       , (hiBootExt,    mkHiOnlyModLocation hisuf mod_name)
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

   searchPathExts home_path mod_name exts
   	
-- -----------------------------------------------------------------------------
-- Looking for a package module

findPackageMod :: ModuleName -> IO (Either [FilePath] (Module, ModLocation))
findPackageMod mod_name = do
  mode     <- readIORef v_GhcMode
  imp_dirs <- getPackageImportPath -- including the 'auto' ones

   -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef v_Build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")

  let
     hi_exts =
        [ (package_hisuf, mkPackageModLocation package_hisuf mod_name) ]

     source_exts = 
       [ ("hs",   mkPackageModLocation package_hisuf mod_name)
       , ("lhs",  mkPackageModLocation package_hisuf mod_name)
       ]
     
     -- mkdependHS needs to look for source files in packages too, so
     -- that we can make dependencies between package before they have
     -- been built.
     exts 
      | mode == DoMkDependHS = hi_exts ++ source_exts
      | otherwise = hi_exts

      -- we never look for a .hi-boot file in an external package;
      -- .hi-boot files only make sense for the home package.
  searchPathExts imp_dirs mod_name exts

-- -----------------------------------------------------------------------------
-- General path searching

searchPathExts
  :: [FilePath]		-- paths to search
  -> ModuleName		-- module name
  -> [ (
	String,						-- suffix
	String -> String -> String -> IO (Module, ModLocation)  -- action
       )
     ] 
  -> IO (Either [FilePath] (Module, ModLocation))

searchPathExts path mod_name exts = search to_search
  where
    basename = dots_to_slashes (moduleNameUserString mod_name)

    to_search :: [(FilePath, IO (Module,ModLocation))]
    to_search = [ (file, fn p basename ext)
		| p <- path, 
		  (ext,fn) <- exts,
		  let base | p == "."  = basename
	     	           | otherwise = p ++ '/':basename
	              file = base ++ '.':ext
		]

    search [] = return (Left (map fst to_search))
    search ((file, result) : rest) = do
      b <- doesFileExist file
      if b 
	then Right `liftM` result
	else search rest

-- -----------------------------------------------------------------------------
-- Building ModLocations

mkHiOnlyModLocation hisuf mod_name path basename _ext = do
  -- basename == dots_to_slashes (moduleNameUserString mod_name)
  loc <- hiOnlyModLocation path basename hisuf
  let result = (mkHomeModule mod_name, loc)
  addToFinderCache mod_name result
  return result

mkPackageModLocation hisuf mod_name path basename _ext = do
  -- basename == dots_to_slashes (moduleNameUserString mod_name)
  loc <- hiOnlyModLocation path basename hisuf
  let result = (mkPackageModule mod_name, loc)
  addToFinderCache mod_name result
  return result

hiOnlyModLocation path basename hisuf 
 = do let full_basename = path++'/':basename
      obj_fn <- mkObjPath full_basename basename
      return ModLocation{ ml_hspp_file = Nothing,
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
-- mod_name
--      The name of the module
--
-- path
--      (a): The search path component where the source file was found.
--      (b) and (c): "."
--
-- src_basename
--      (a): dots_to_slashes (moduleNameUserString mod_name)
--      (b) and (c): The filename of the source file, minus its extension
--
-- ext
--	The filename extension of the source file (usually "hs" or "lhs").

mkHomeModLocation mod_name src_filename = do
   let (basename,extension) = splitFilename src_filename
   mkHomeModLocation' mod_name basename extension

mkHomeModLocationSearched mod_name path basename ext =
   mkHomeModLocation' mod_name (path ++ '/':basename) ext

mkHomeModLocation' mod_name src_basename ext = do
   let mod_basename = dots_to_slashes (moduleNameUserString mod_name)

   obj_fn <- mkObjPath src_basename mod_basename
   hi_fn  <- mkHiPath  src_basename mod_basename

   let result = ( mkHomeModule mod_name,
           	   ModLocation{ ml_hspp_file = Nothing,
	   		        ml_hs_file   = Just (src_basename ++ '.':ext),
			        ml_hi_file   = hi_fn,
			        ml_obj_file  = obj_fn
		       })

   addToFinderCache mod_name result
   return result

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

findLinkable :: ModuleName -> ModLocation -> IO (Maybe Linkable)
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
