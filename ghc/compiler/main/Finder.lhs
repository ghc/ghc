%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    Finder, 		-- =  ModuleName -> IO (Maybe (Module, ModuleLocation))
    newFinder, 		-- :: PackageConfigInfo -> IO Finder, 
    ModuleLocation(..),
    mkHomeModuleLocn
  ) where

#include "HsVersions.h"

import HscTypes		( Finder, ModuleLocation(..) )
import CmStaticInfo
import DriverPhases
import DriverState
import Module
import FiniteMap
import Util
import Panic

import IOExts
import Directory
import List
import IO
import Monad
\end{code}

The Finder provides a thin filesystem abstraction to the rest of the
compiler.  For a given module, it knows (a) which package the module
lives in, so it can make a Module from a ModuleName, and (b) where the
source, interface, and object files for a module live.

\begin{code}

-- caches contents of package directories, never expunged
GLOBAL_VAR(v_PkgDirCache,    Nothing,  Maybe (FiniteMap String (PackageName, FilePath)))

-- caches contents of home directories, expunged whenever we
-- create a new finder.
GLOBAL_VAR(v_HomeDirCache,   Nothing,  Maybe (FiniteMap String FilePath))


newFinder :: PackageConfigInfo -> IO Finder
newFinder (PackageConfigInfo pkgs) = do
  -- expunge our home cache
  writeIORef v_HomeDirCache Nothing

  -- populate the package cache, if necessary
  pkg_cache <- readIORef v_PkgDirCache
  case pkg_cache of 
    Nothing -> do

	let extendFM fm pkg = do
		let dirs = import_dirs pkg
		    pkg_name = _PK_ (name pkg)
		let addDir fm dir = do
			contents <- getDirectoryContents' dir
			return (addListToFM fm (zip contents 
						   (repeat (pkg_name,dir))))
                foldM addDir fm dirs

  	pkg_map <- foldM extendFM emptyFM pkgs
	writeIORef v_PkgDirCache (Just pkg_map)

    Just _ -> 
        return ()

  -- and return the finder
  return finder

  
finder :: ModuleName -> IO (Maybe (Module, ModuleLocation))
finder name = do
  j <- maybeHomeModule name
  case j of
	Just home_module -> return (Just home_module)
	Nothing -> maybePackageModule name

maybeHomeModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
maybeHomeModule mod_name = do
   home_cache <- readIORef v_HomeDirCache

   home_map <- 
     case home_cache of
	Nothing -> do
	   -- populate the home dir cache, using the import path (the import 
	   -- path is changed by -i flags on the command line, and defaults 
	   -- to ["."]).
	   home_imports <- readIORef v_Import_paths
	   let extendFM fm path = do
		   contents <- getDirectoryContents' path
		   return (addListToFM fm (zip contents (repeat path)))
	   home_map <- foldM extendFM emptyFM home_imports
	   writeIORef v_HomeDirCache (Just home_map)
	   return home_map

        Just home_map -> return home_map

   let basename = moduleNameString mod_name
       hs  = basename ++ ".hs"
       lhs = basename ++ ".lhs"

   case lookupFM home_map hs of {
	Just path -> mkHomeModuleLocn mod_name (path ++ '/':basename) hs;
	Nothing ->

   case lookupFM home_map lhs of {
	Just path ->  mkHomeModuleLocn mod_name (path ++ '/':basename) lhs;
	Nothing -> return Nothing

   }}

mkHomeModuleLocn mod_name basename source_fn = do

   -- figure out the .hi file name: it lives in the same dir as the
   -- source, unless there's a -ohi flag on the command line.
   ohi    <- readIORef v_Output_hi
   hisuf  <- readIORef v_Hi_suf
   let hifile = case ohi of
		   Nothing -> basename ++ '.':hisuf
		   Just fn -> fn

   -- figure out the .o file name.  It also lives in the same dir
   -- as the source, but can be overriden by a -odir flag.
   o_file <- odir_ify (basename ++ '.':phaseInputExt Ln) >>= osuf_ify

   return (Just (mkHomeModule mod_name,
                 ModuleLocation{
		    hs_file  = source_fn,
		    hi_file  = hifile,
		    obj_file = o_file
	         }
	))

maybePackageModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
maybePackageModule mod_name = do
  maybe_pkg_cache <- readIORef v_PkgDirCache
  case maybe_pkg_cache of {
     Nothing -> panic "maybePackageModule: no pkg_cache";
     Just pkg_cache -> do

  -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef v_Build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")

  let basename = moduleNameString mod_name
      hi  = basename ++ '.':package_hisuf

  case lookupFM pkg_cache hi of
	Nothing -> return Nothing
	Just (pkg_name,path) -> 
	    return (Just (mkModule mod_name pkg_name,
			  ModuleLocation{ 
				hs_file  = error "package module; no source",
				hi_file  = hi,
				obj_file = error "package module; no object"
			   }
		   ))

   }

getDirectoryContents' d
   = IO.catch (getDirectoryContents d)
	  (\_ -> do hPutStr stderr 
		          ("WARNING: error while reading directory " ++ d)
		    return []
	  )
	 
\end{code}
