%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    initFinder, 	-- :: PackageConfigInfo -> IO (), 
    findModule,		-- :: ModuleName -> IO (Maybe (Module, ModuleLocation))
    emptyHomeDirCache	-- :: IO ()
  ) where

#include "HsVersions.h"

import HscTypes		( ModuleLocation(..) )
import CmStaticInfo
import DriverPhases
import DriverState
import Module
import FiniteMap
import Util
import Panic		( panic )

import IOExts
import Directory
import List
import IO
import Monad
import Outputable	( showSDoc, ppr )	-- debugging only
\end{code}

The Finder provides a thin filesystem abstraction to the rest of the
compiler.  For a given module, it knows (a) which package the module
lives in, so it can make a Module from a ModuleName, and (b) where the
source, interface, and object files for a module live.

\begin{code}

-- v_PkgDirCache caches contents of package directories, never expunged
GLOBAL_VAR(v_PkgDirCache, panic "no pkg cache!", 
           FiniteMap String (PackageName, FilePath))

-- v_HomeDirCache caches contents of home directories, 
-- expunged whenever we create a new finder.
GLOBAL_VAR(v_HomeDirCache, Nothing, Maybe (FiniteMap String FilePath))


initFinder :: PackageConfigInfo -> IO ()
initFinder pkgs 
  = do	{	-- expunge our home cache
	; writeIORef v_HomeDirCache Nothing
		-- lazilly fill in the package cache
	; writeIORef v_PkgDirCache (unsafePerformIO (newPkgCache pkgs))
	
-- Debug output
--	; pkg_dbg_info <- readIORef v_PkgDirCache
--	; putStrLn (unlines (map show (fmToList pkg_dbg_info)))
	}

emptyHomeDirCache :: IO ()
emptyHomeDirCache
   = writeIORef v_HomeDirCache Nothing

findModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
findModule name
  = do 	{ hPutStr stderr ("findModule: " ++ moduleNameUserString name ++ " ... ")
	; maybe_m <- findModule_wrk name
	; case maybe_m of
	     Nothing -> hPutStrLn stderr "Not Found"
	     Just mm -> hPutStrLn stderr (showSDoc (ppr (snd mm)))
	; return maybe_m
	}

findModule_wrk :: ModuleName -> IO (Maybe (Module, ModuleLocation))
findModule_wrk name
  = do	{ j <- maybeHomeModule name
	; case j of
	    Just home_module -> return (Just home_module)
	    Nothing	     -> maybePackageModule name
	}

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
                   let clean_contents = filter isUsefulFile contents
		   return (addListToFM fm (zip clean_contents (repeat path)))
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
                    ml_hspp_file = Nothing,
		    ml_hs_file   = Just source_fn,
		    ml_hi_file   = Just hifile,
		    ml_obj_file  = Just o_file
	         }
	))


newPkgCache :: [Package] -> IO (FiniteMap String (PackageName, FilePath))
newPkgCache pkgs = do
    let extendFM fm pkg = do
    	    let dirs = import_dirs pkg
    		pkg_name = _PK_ (name pkg)
    	    let addDir fm dir = do
    		    contents <- getDirectoryContents' dir
		    let clean_contents = filter isUsefulFile contents
    		    return (addListToFM fm (zip clean_contents 
    					       (repeat (pkg_name,dir))))
    	    foldM addDir fm dirs
    
    pkg_map <- foldM extendFM emptyFM pkgs
    return pkg_map


maybePackageModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
maybePackageModule mod_name = do
  pkg_cache <- readIORef v_PkgDirCache

  -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef v_Build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")

  let basename = moduleNameString mod_name
      hi = basename ++ '.':package_hisuf

  case lookupFM pkg_cache hi of
	Nothing -> return Nothing
	Just (pkg_name,path) -> 
	    return (Just (mkModule mod_name pkg_name,
			  ModuleLocation{ 
                                ml_hspp_file = Nothing,
				ml_hs_file   = Nothing,
				ml_hi_file   = Just (path ++ '/':hi),
				ml_obj_file  = Nothing
			   }
		   ))

isUsefulFile fn
   = let suffix = (reverse . takeWhile (/= '.') . reverse) fn
     in  suffix `elem` ["hi", "hs", "lhs", "hi-boot", "hi-boot-5"]

getDirectoryContents' d
   = IO.catch (getDirectoryContents d)
	  (\_ -> do hPutStr stderr 
		          ("WARNING: error while reading directory " ++ d)
		    return []
	  )
	 
\end{code}
