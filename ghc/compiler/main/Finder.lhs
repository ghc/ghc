%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    flushFinderCache,	-- :: IO ()

    findModule,		-- :: ModuleName -> IO (Maybe (Module, ModLocation))
    findPackageModule,  -- :: ModuleName -> IO (Maybe (Module, ModLocation))

    mkHomeModLocation,	-- :: ModuleName -> String -> FilePath 
			--	-> IO ModLocation

    findLinkable,	-- :: ModuleName -> ModLocation -> IO (Maybe Linkable)

    hiBootExt,		-- :: String
    hiBootVerExt,	-- :: String

  ) where

#include "HsVersions.h"

import Module
import UniqFM		( filterUFM )
import Packages		( PackageConfig(..) )
import HscTypes		( Linkable(..), Unlinked(..) )
import DriverState
import DriverUtil	( split_longest_prefix, splitFilename3 )
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

findModule :: ModuleName -> IO (Maybe (Module, ModLocation))
findModule name = do
  r <- lookupFinderCache name
  case r of
   Just result -> return (Just result)
   Nothing -> do  
       j <- maybeHomeModule name
       case j of
	 Just home_module -> return (Just home_module)
	 Nothing	  -> findPackageMod name

findPackageModule :: ModuleName -> IO (Maybe (Module, ModLocation))
findPackageModule name = do
  r <- lookupFinderCache name
  case r of
   Just result -> return (Just result)
   Nothing     -> findPackageMod name

hiBootExt = "hi-boot"
hiBootVerExt = "hi-boot-" ++ cHscIfaceFileVersion

maybeHomeModule :: ModuleName -> IO (Maybe (Module, ModLocation))
maybeHomeModule mod_name = do
   home_path <- readIORef v_Import_paths
   hisuf     <- readIORef v_Hi_suf
   mode      <- readIORef v_GhcMode

   let
     source_exts = 
      [ ("hs",   mkHomeModLocation mod_name False)
      , ("lhs",  mkHomeModLocation mod_name False)
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

findPackageMod :: ModuleName -> IO (Maybe (Module, ModLocation))
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
  -> IO (Maybe (Module, ModLocation))

searchPathExts path mod_name exts = search path
  where
    mod_str = moduleNameUserString mod_name
    basename = map (\c -> if c == '.' then '/' else c) mod_str

    search [] = return Nothing
    search (p:ps) = loop exts
      where	
	base | p == "."  = basename
	     | otherwise = p ++ '/':basename

	loop [] = search ps
	loop ((ext,fn):exts) = do
	    let file = base ++ '.':ext
	    b <- doesFileExist file
	    if b then Just `liftM` fn p basename ext
		 else loop exts

-- -----------------------------------------------------------------------------
-- Building ModLocations

mkHiOnlyModLocation hisuf mod_name path basename extension = do
  loc <- hiOnlyModLocation path basename hisuf
  let result = (mkHomeModule mod_name, loc)
  addToFinderCache mod_name result
  return result

mkPackageModLocation hisuf mod_name path basename _extension = do
  loc <- hiOnlyModLocation path basename hisuf
  let result = (mkPackageModule mod_name, loc)
  addToFinderCache mod_name result
  return result

hiOnlyModLocation path basename hisuf 
 = do { obj_fn <- mkObjPath path basename ;
        return (ModLocation{ ml_hspp_file = Nothing,
 	        	     ml_hs_file   = Nothing,
 	        	     ml_hi_file   = path ++ '/':basename ++ '.':hisuf,
		 		    -- Remove the .hi-boot suffix from hi_file, if it
		 		    -- had one.  We always want the name of the real
		 		    -- .hi file in the ml_hi_file field.
	   	             ml_obj_file  = obj_fn
                 })}

-- -----------------------------------------------------------------------------
-- Constructing a home module location

-- The .hi file always follows the module name, whereas the object
-- file may follow the name of the source file in the case where the
-- two differ (see summariseFile in compMan/CompManager.lhs).

-- The source filename is specified in three components.  For example,
-- if we have a module "A.B.C" which was found along the patch "/P/Q/R"
-- with extension ".hs", then the full filename is "/P/Q/R/A/B/C.hs".  The
-- components passed to mkHomeModLocation are
--
--   path:      "/P/Q/R"
--   basename:  "A/B/C"
--   extension: "hs"
--
-- the object file and interface file are constructed by possibly
-- replacing the path component with the values of the -odir or the
-- -hidr options respectively, and the extension with the values of
-- the -osuf and -hisuf options respectively.  That is, the basename
-- always remains intact.
--
-- mkHomeModLocation is called directly by the compilation manager to
-- construct the information for a root module.  For a "root" module,
-- the rules are slightly different. The filename is allowed to
-- diverge from the module name, but we have to name the interface
-- file after the module name.  For example, a root module
-- "/P/Q/R/foo.hs" will have components
--
--  path:       "/P/Q/R"
--  basename:   "foo"
--  extension:  "hs"
-- 
-- and we set the flag is_root to True, to indicate that the basename
-- portion for the .hi file should be replaced by the last component
-- of the module name.  eg. if the module name is "A.B.C" then basename
-- will be replaced by "C" for the .hi file only, resulting in an
-- .hi file like "/P/Q/R/C.hi" (subject to -hidir and -hisuf as usual).

mkHomeModLocation mod_name is_root path basename extension = do

   hisuf  <- readIORef v_Hi_suf
   hidir  <- readIORef v_Hi_dir

   obj_fn <- mkObjPath path basename

   let  -- hi filename
       mod_str = moduleNameUserString mod_name
       (_,mod_suf) = split_longest_prefix mod_str (=='.')

       hi_basename
	  | is_root   = mod_suf
	  | otherwise = basename

       hi_path | Just d <- hidir = d
	       | otherwise       = path
       hi_fn = hi_path ++ '/':hi_basename ++ '.':hisuf

	-- source filename (extension is always .hs or .lhs)
       source_fn
	 | path == "."  = basename ++ '.':extension
	 | otherwise    = path ++ '/':basename ++ '.':extension

       result = ( mkHomeModule mod_name,
           	  ModLocation{ ml_hspp_file = Nothing,
	   		       ml_hs_file   = Just source_fn,
			       ml_hi_file   = hi_fn,
			       ml_obj_file  = obj_fn,
		       })

   addToFinderCache mod_name result
   return result

mkObjPath :: String -> FilePath -> IO FilePath
-- Construct the filename of a .o file from the path/basename
-- derived either from a .hs file or a .hi file.
--
-- Does *not* check whether the .o file exists
mkObjPath path basename
  = do  odir   <- readIORef v_Output_dir
	osuf   <- readIORef v_Object_suf
	let obj_path | Just d <- odir = d
	   	     | otherwise      = path
        return (obj_path ++ '/':basename ++ '.':osuf)

  

-- -----------------------------------------------------------------------------
-- findLinkable isn't related to the other stuff in here, 
-- but there' no other obvious place for it

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
\end{code}
