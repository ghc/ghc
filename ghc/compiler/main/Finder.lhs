%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    initFinder, 	-- :: [PackageConfig] -> IO (), 
    findModule,		-- :: ModuleName -> IO (Maybe (Module, ModuleLocation))
    mkHomeModuleLocn,	-- :: ModuleName -> String -> Maybe FilePath 
			--	-> IO ModuleLocation
    emptyHomeDirCache,	-- :: IO ()
    flushPackageCache   -- :: [PackageConfig] -> IO ()
  ) where

#include "HsVersions.h"

import HscTypes		( ModuleLocation(..) )
import CmStaticInfo
import DriverPhases
import DriverState
import DriverUtil
import Module
import FiniteMap
import FastString
import Util
import Panic		( panic )
import Config

import IOExts
import List
import Directory
import IO
import Monad
import Outputable
\end{code}

The Finder provides a thin filesystem abstraction to the rest of the
compiler.  For a given module, it knows (a) which package the module
lives in, so it can make a Module from a ModuleName, and (b) where the
source, interface, and object files for a module live.

\begin{code}
initFinder :: [PackageConfig] -> IO ()
initFinder pkgs = return ()

-- empty, and lazilly fill in the package cache
flushPackageCache :: [PackageConfig] -> IO ()
flushPackageCache pkgs = return ()

emptyHomeDirCache :: IO ()
emptyHomeDirCache = return ()

findModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
findModule name
  = do	{ j <- maybeHomeModule name
	; case j of
	    Just home_module -> return (Just home_module)
	    Nothing	     -> maybePackageModule name
	}

maybeHomeModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
maybeHomeModule mod_name = do
   home_path <- readIORef v_Import_paths

   let mod_str  = moduleNameUserString mod_name 
       basename = map (\c -> if c == '.' then '/' else c) mod_str
       hs  = basename ++ ".hs"
       lhs = basename ++ ".lhs"

   found <- findOnPath home_path hs
   case found of {
	  -- special case to avoid getting "./foo.hs" all the time
	Just "."  -> mkHomeModuleLocn mod_name basename (Just hs);
	Just path -> mkHomeModuleLocn mod_name 
			(path ++ '/':basename) (Just (path ++ '/':hs));
	Nothing -> do

   found <- findOnPath home_path lhs
   case found of {
	  -- special case to avoid getting "./foo.hs" all the time
	Just "."  -> mkHomeModuleLocn mod_name basename (Just lhs);
	Just path ->  mkHomeModuleLocn mod_name
			(path ++ '/':basename) (Just (path ++ '/':lhs));
	Nothing -> do

   -- can't find a source file anywhere, check for a lone .hi file.
   hisuf <- readIORef v_Hi_suf
   let hi = basename ++ '.':hisuf
   found <- findOnPath home_path hi
   case found of {
	Just path ->  mkHiOnlyModuleLocn mod_name hi;
	Nothing -> do

   -- last chance: .hi-boot-<ver> and .hi-boot
   let hi_boot = basename ++ ".hi-boot"
   let hi_boot_ver = basename ++ ".hi-boot-" ++ cHscIfaceFileVersion
   found <- findOnPath home_path hi_boot_ver
   case found of {
	Just path -> mkHiOnlyModuleLocn mod_name hi;
	Nothing -> do
   found <- findOnPath home_path hi_boot
   case found of {
	Just path -> mkHiOnlyModuleLocn mod_name hi;
	Nothing -> return Nothing
   }}}}}


mkHiOnlyModuleLocn mod_name hi_file = do
   return (Just (mkHomeModule mod_name,
                 ModuleLocation{
                    ml_hspp_file = Nothing,
		    ml_hs_file   = Nothing,
		    ml_hi_file   = hi_file,
		    ml_obj_file  = Nothing
	         }
	))

-- The .hi file always follows the module name, whereas the object
-- file may follow the name of the source file in the case where the
-- two differ (see summariseFile in compMan/CompManager.lhs).

mkHomeModuleLocn mod_name basename maybe_source_fn = do

   hisuf  <- readIORef v_Hi_suf
   hidir  <- readIORef v_Hi_dir

   let hi_rest = basename ++ '.':hisuf
       hi_file | Just d <- hidir = d ++ '/':hi_rest
	       | otherwise       = hi_rest

   -- figure out the .o file name.  It also lives in the same dir
   -- as the source, but can be overriden by a -odir flag.
   o_file <- odir_ify (basename ++ '.':phaseInputExt Ln) >>= osuf_ify

   return (Just (mkHomeModule mod_name,
                 ModuleLocation{
                    ml_hspp_file = Nothing,
		    ml_hs_file   = maybe_source_fn,
		    ml_hi_file   = hi_file,
		    ml_obj_file  = Just o_file
	         }
	))


maybePackageModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
maybePackageModule mod_name = do
  pkgs <- getPackageInfo

  -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef v_Build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")

  let basename = moduleNameUserString mod_name
      hi = basename ++ '.':package_hisuf

  found <- findOnPackagePath pkgs hi
  case found of
	Nothing -> return Nothing
	Just (pkg_name,path) -> 
	    return (Just (mkModule mod_name pkg_name,
			  ModuleLocation{ 
                                ml_hspp_file = Nothing,
				ml_hs_file   = Nothing,
				ml_hi_file   = path,
				ml_obj_file  = Nothing
			   }
		   ))

findOnPackagePath :: [PackageConfig] -> String
   -> IO (Maybe (PackageName,FilePath))
findOnPackagePath pkgs file = loop pkgs
 where
  loop [] = return Nothing
  loop (p:ps) = do
    found <- findOnPath (import_dirs p) file
    case found of
	Nothing -> loop ps
	Just f  -> return (Just (mkFastString (name p), f))

findOnPath :: [String] -> String -> IO (Maybe FilePath)
findOnPath path s = loop path
 where
  loop [] = return Nothing
  loop (d:ds) = do
    let file = d ++ '/':s
    b <- doesFileExist file
    if b then return (Just d) else loop ds
\end{code}
