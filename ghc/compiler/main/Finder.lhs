%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    initFinder, 	-- :: [PackageConfig] -> IO (), 
    findModule,		-- :: ModuleName -> IO (Maybe (Module, ModuleLocation))
    findModuleDep,	-- :: ModuleName -> Bool -> IO (Maybe (Module, ModuleLocation))
    findPackageModule,	-- :: ModuleName -> IO (Maybe (Module, ModuleLocation))
    mkHomeModuleLocn,	-- :: ModuleName -> String -> FilePath 
			--	-> IO ModuleLocation
    emptyHomeDirCache,	-- :: IO ()
    flushPackageCache   -- :: [PackageConfig] -> IO ()
  ) where

#include "HsVersions.h"

import HscTypes		( ModuleLocation(..) )
import Packages		( PackageConfig(..) )
import DriverPhases
import DriverState
import DriverUtil
import Module
import FastString
import Config

import IOExts
import List
import Directory
import IO
import Monad
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
findModule name = findModuleDep name False

findModuleDep :: ModuleName -> Bool -> IO (Maybe (Module, ModuleLocation))
findModuleDep name is_source
  = do	{ j <- maybeHomeModule name is_source
	; case j of
	    Just home_module -> return (Just home_module)
	    Nothing	     -> findPackageMod name False
	}

maybeHomeModule :: ModuleName -> Bool -> IO (Maybe (Module, ModuleLocation))
maybeHomeModule mod_name is_source = do
   home_path <- readIORef v_Import_paths
   hisuf     <- readIORef v_Hi_suf

   let mod_str  = moduleNameUserString mod_name 
       basename = map (\c -> if c == '.' then '/' else c) mod_str
       
       std_exts =
        [ ("hs",   \ _ fName path -> mkHomeModuleLocn mod_name path fName)
	, ("lhs",  \ _ fName path -> mkHomeModuleLocn mod_name path fName)
	, (hisuf,  \ _ fName path -> mkHiOnlyModuleLocn mod_name fName)
	]
	-- look for the .hi file last, because if there's a source file about
	-- we want to find it.

        -- last chance: .hi-boot-<ver> and .hi-boot
       hi_boot_ver = "hi-boot-" ++ cHscIfaceFileVersion

       boot_exts = 
       	[ (hi_boot_ver, \ _ fName path -> mkHiOnlyModuleLocn mod_name fName)
	, ("hi-boot",   \ _ fName path -> mkHiOnlyModuleLocn mod_name fName)
	]

   searchPathExts  
   	(map ((,) undefined) home_path)
	basename
	(if is_source then (boot_exts++std_exts) else std_exts ++ boot_exts)
			-- for SOURCE imports, check the hi-boot extensions
			-- before the source/iface ones, to avoid
			-- creating circ Makefile deps.

mkHiOnlyModuleLocn mod_name hi_file =
 return
   ( mkHomeModule mod_name
   , ModuleLocation{ ml_hspp_file = Nothing
    		   , ml_hs_file   = Nothing
		   , ml_hi_file   = hi_file
		   , ml_obj_file  = Nothing
	           }
   )

-- The .hi file always follows the module name, whereas the object
-- file may follow the name of the source file in the case where the
-- two differ (see summariseFile in compMan/CompManager.lhs).

mkHomeModuleLocn mod_name 
	basename		-- everything but the extension
	source_fn		-- full path to the source (required)
  = do

   hisuf  <- readIORef v_Hi_suf
   hidir  <- readIORef v_Hi_dir

   -- take the *last* component of the module name (if a hierarchical name),
   -- and append it to the directory to get the .hi file name.
   let (_,mod_str) = split_longest_prefix (moduleNameUserString mod_name) (=='.')
       hi_filename = mod_str ++ '.':hisuf
       hi_path | Just d <- hidir = d
	       | otherwise       = getdir basename
       hi = hi_path ++ '/':hi_filename

   -- figure out the .o file name.  It also lives in the same dir
   -- as the source, but can be overriden by a -odir flag.
   o_file <- odir_ify (basename ++ '.':phaseInputExt Ln) >>= osuf_ify

   return (mkHomeModule mod_name,
           ModuleLocation{ ml_hspp_file = Nothing
	   		 , ml_hs_file   = Just source_fn
			 , ml_hi_file   = hi
			 , ml_obj_file  = Just o_file
			 })

findPackageMod :: ModuleName
	       -> Bool
	       -> IO (Maybe (Module, ModuleLocation))
findPackageMod mod_name hiOnly = do
  pkgs <- getPackageInfo

   -- hi-suffix for packages depends on the build tag.
  package_hisuf <-
	do tag <- readIORef v_Build_tag
	   if null tag
		then return "hi"
		else return (tag ++ "_hi")
  let imp_dirs = concatMap (\ pkg -> map ((,) pkg) (import_dirs pkg)) pkgs
      mod_str  = moduleNameUserString mod_name 
      basename = map (\c -> if c == '.' then '/' else c) mod_str
  searchPathExts
  	imp_dirs basename
        ((package_hisuf,\ pkg fName path -> mkPackageModule mod_name pkg Nothing path) :
      	  -- can packages contain hi-boots?
	 (if hiOnly then [] else
	  [ ("hs",  \ pkg fName path -> mkPackageModule mod_name pkg (Just fName) path)
	  , ("lhs", \ pkg fName path -> mkPackageModule mod_name pkg (Just fName) path)
	  ]))
 where
  mkPackageModule mod_name pkg mbFName path =
    return ( mkModule mod_name (mkFastString (name pkg))
           , ModuleLocation{ ml_hspp_file = Nothing
		 	   , ml_hs_file   = mbFName
			   , ml_hi_file   = path ++".hi"
			   , ml_obj_file  = Nothing
			   })

findPackageModule :: ModuleName -> IO (Maybe (Module, ModuleLocation))
findPackageModule mod_name = findPackageMod mod_name True

searchPathExts :: [(a, FilePath)]
	       -> String
	       -> [(String, a -> FilePath -> String -> IO (Module, ModuleLocation))] 
	       -> IO (Maybe (Module, ModuleLocation))
searchPathExts path basename exts = search exts
  where
    search         [] = return Nothing
    search ((x,f):xs) = do
        let fName = (basename ++ '.':x)
        found <- findOnPath path fName
        case found of
   	    -- special case to avoid getting "./foo.<ext>" all the time
	  Just (v,".")  -> fmap Just (f v fName basename)
	  Just (v,path) -> fmap Just (f v (path ++ '/':fName)
	  		                  (path ++ '/':basename))
	  Nothing   -> search xs

findOnPath :: [(a,String)] -> String -> IO (Maybe (a, FilePath))
findOnPath path s = loop path
 where
  loop [] = return Nothing
  loop ((a,d):ds) = do
    let file = d ++ '/':s
    b <- doesFileExist file
    if b then return (Just (a,d)) else loop ds
\end{code}
