%
% (c) The University of Glasgow, 2000
%
\section[Finder]{Module Finder}

\begin{code}
module Finder (
    flushFinderCache,	-- :: IO ()
    FindResult(..),
    findModule,			-- :: ModuleName -> Bool -> IO FindResult
    findPackageModule,  	-- :: ModuleName -> Bool -> IO FindResult
    mkHomeModLocation,		-- :: ModuleName -> FilePath -> IO ModLocation
    mkHomeModLocation2,		-- :: ModuleName -> FilePath -> String -> IO ModLocation
    addHomeModuleToFinder, 	-- :: Module -> ModLocation -> IO ()

    findLinkable,	-- :: ModuleName -> ModLocation -> IO (Maybe Linkable)

    cantFindError, 	-- :: DynFlags -> Module -> FindResult -> SDoc
  ) where

#include "HsVersions.h"

import Module
import UniqFM		( filterUFM )
import HscTypes		( Linkable(..), Unlinked(..) )
import Packages
import FastString
import Util
import DynFlags		( DynFlags(..), isOneShot, GhcMode(..) )
import Outputable

import DATA_IOREF	( IORef, writeIORef, readIORef )

import Data.List
import System.Directory
import System.IO
import Control.Monad
import Maybes		( MaybeErr(..) )
import Data.Maybe	( isNothing )


type FileExt = String	-- Filename extension
type BaseName = String	-- Basename of file

-- -----------------------------------------------------------------------------
-- The Finder

-- The Finder provides a thin filesystem abstraction to the rest of
-- the compiler.  For a given module, it can tell you where the
-- source, interface, and object files for that module live.

-- It does *not* know which particular package a module lives in.  Use
-- Packages.moduleToPackageConfig for that.

-- -----------------------------------------------------------------------------
-- The finder's cache

GLOBAL_VAR(finder_cache, emptyModuleEnv, ModuleEnv FinderCacheEntry)

type FinderCacheEntry = (ModLocation, Maybe (PackageConfig,Bool))

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
  = Found ModLocation PackageIdH
	-- the module was found
  | PackageHidden PackageId
	-- for an explicit source import: the package containing the module is
	-- not exposed.
  | ModuleHidden  PackageId
	-- for an explicit source import: the package containing the module is
	-- exposed, but the module itself is hidden.
  | NotFound [FilePath]
	-- the module was not found, the specified places were searched.

type LocalFindResult = MaybeErr [FilePath] FinderCacheEntry
	-- LocalFindResult is used for internal functions which 
	-- return a more informative type; it's munged into
	-- the external FindResult by 'cached'

cached :: Bool
       -> (DynFlags -> Module -> IO LocalFindResult)
       -> DynFlags -> Module -> Bool -> IO FindResult
cached home_allowed wrapped_fn dflags name explicit 
  = do	{ 	-- First try the cache
	  mb_entry <- lookupFinderCache name
	; case mb_entry of {
	    Just old_entry -> return (found old_entry) ;
	    Nothing    -> do

	{ 	-- Now try the wrapped function
	  mb_entry <- wrapped_fn dflags name
	; case mb_entry of
	    Failed paths  	-> return (NotFound paths)
	    Succeeded new_entry -> do { addToFinderCache name new_entry
			      	      ; return (found new_entry) }
	}}} 
  where
	-- We've found the module, so the remaining question is
	-- whether it's visible or not
    found :: FinderCacheEntry -> FindResult
    found (loc, Nothing)
	| home_allowed  = Found loc HomePackage
	| otherwise     = NotFound []
    found (loc, Just (pkg, exposed_mod))
	| explicit && not exposed_mod   = ModuleHidden pkg_name
	| explicit && not (exposed pkg) = PackageHidden pkg_name
	| otherwise			= Found loc (ExtPackage (mkPackageId (package pkg)))
	where
	  pkg_name = packageConfigId pkg

addHomeModuleToFinder :: Module -> ModLocation -> IO ()
addHomeModuleToFinder mod loc = addToFinderCache mod (loc, Nothing)


-- -----------------------------------------------------------------------------
-- 	The two external entry points


findModule :: DynFlags -> Module -> Bool -> IO FindResult
findModule = cached True findModule' 
  
findPackageModule :: DynFlags -> Module -> Bool -> IO FindResult
findPackageModule = cached False findPackageModule'

-- -----------------------------------------------------------------------------
-- 	The internal workers

findModule' :: DynFlags -> Module -> IO LocalFindResult
-- Find home or package module
findModule' dflags name = do
    r <- findPackageModule' dflags name
    case r of
	Failed pkg_files -> do
	   j <- findHomeModule' dflags name
	   case j of
		Failed home_files -> 
			return (Failed (home_files ++ pkg_files))
		other_result
			-> return other_result
	other_result
		-> return other_result

findHomeModule' :: DynFlags -> Module -> IO LocalFindResult
findHomeModule' dflags mod = do
   let home_path = importPaths dflags
       hisuf = hiSuf dflags

   let
     source_exts = 
      [ ("hs",   mkHomeModLocationSearched dflags mod "hs")
      , ("lhs",  mkHomeModLocationSearched dflags  mod "lhs")
      ]
     
     hi_exts = [ (hisuf,  	 	mkHiOnlyModLocation dflags hisuf)
	       , (addBootSuffix hisuf,	mkHiOnlyModLocation dflags hisuf)
	       ]
     
     	-- In compilation manager modes, we look for source files in the home
     	-- package because we can compile these automatically.  In one-shot
     	-- compilation mode we look for .hi and .hi-boot files only.
     exts | isOneShot (ghcMode dflags) = hi_exts
          | otherwise      	       = source_exts

   searchPathExts home_path mod exts
   	
findPackageModule' :: DynFlags -> Module -> IO LocalFindResult
findPackageModule' dflags mod 
  = case moduleToPackageConfig dflags mod of
    	Nothing       -> return (Failed [])
	Just pkg_info -> findPackageIface dflags mod pkg_info

findPackageIface :: DynFlags -> Module -> (PackageConfig,Bool) -> IO LocalFindResult
findPackageIface dflags mod pkg_info@(pkg_conf, _) = do
  let
     tag = buildTag dflags

	   -- hi-suffix for packages depends on the build tag.
     package_hisuf | null tag  = "hi"
		   | otherwise = tag ++ "_hi"
     hi_exts =
        [ (package_hisuf, 
	    mkPackageModLocation dflags pkg_info package_hisuf) ]

     source_exts = 
       [ ("hs",   mkPackageModLocation dflags pkg_info package_hisuf)
       , ("lhs",  mkPackageModLocation dflags pkg_info package_hisuf)
       ]

     -- mkdependHS needs to look for source files in packages too, so
     -- that we can make dependencies between package before they have
     -- been built.
     exts 
      | MkDepend <- ghcMode dflags = hi_exts ++ source_exts
      | otherwise	 	   = hi_exts
      -- we never look for a .hi-boot file in an external package;
      -- .hi-boot files only make sense for the home package.

  searchPathExts (importDirs pkg_conf) mod exts

-- -----------------------------------------------------------------------------
-- General path searching

searchPathExts
  :: [FilePath]		-- paths to search
  -> Module		-- module name
  -> [ (
	FileExt,				     -- suffix
	FilePath -> BaseName -> IO FinderCacheEntry  -- action
       )
     ] 
  -> IO LocalFindResult

searchPathExts paths mod exts 
   = do result <- search to_search
{-
	hPutStrLn stderr (showSDoc $
		vcat [text "Search" <+> ppr mod <+> sep (map (text. fst) exts)
		    , nest 2 (vcat (map text paths))
		    , case result of
			Succeeded (loc, p) -> text "Found" <+> ppr loc
			Failed fs	   -> text "not found"])
-}	
	return result

  where
    basename = dots_to_slashes (moduleUserString mod)

    to_search :: [(FilePath, IO FinderCacheEntry)]
    to_search = [ (file, fn path basename)
		| path <- paths, 
		  (ext,fn) <- exts,
		  let base | path == "." = basename
	     	           | otherwise   = path ++ '/':basename
	              file = base ++ '.':ext
		]

    search [] = return (Failed (map fst to_search))
    search ((file, mk_result) : rest) = do
      b <- doesFileExist file
      if b 
	then do { res <- mk_result; return (Succeeded res) }
	else search rest

mkHomeModLocationSearched :: DynFlags -> Module -> FileExt
		          -> FilePath -> BaseName -> IO FinderCacheEntry
mkHomeModLocationSearched dflags mod suff path basename = do
   loc <- mkHomeModLocation2 dflags mod (path ++ '/':basename) suff
   return (loc, Nothing)

mkHiOnlyModLocation :: DynFlags -> FileExt -> FilePath -> BaseName
		    -> IO FinderCacheEntry
mkHiOnlyModLocation dflags hisuf path basename = do
  loc <- hiOnlyModLocation dflags path basename hisuf
  return (loc, Nothing)

mkPackageModLocation :: DynFlags -> (PackageConfig, Bool) -> FileExt
		     -> FilePath -> BaseName -> IO FinderCacheEntry
mkPackageModLocation dflags pkg_info hisuf path basename = do
  loc <- hiOnlyModLocation dflags path basename hisuf
  return (loc, Just pkg_info)

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

mkHomeModLocation :: DynFlags -> Module -> FilePath -> IO ModLocation
mkHomeModLocation dflags mod src_filename = do
   let (basename,extension) = splitFilename src_filename
   mkHomeModLocation2 dflags mod basename extension

mkHomeModLocation2 :: DynFlags
		   -> Module	
		   -> FilePath 	-- Of source module, without suffix
		   -> String 	-- Suffix
		   -> IO ModLocation
mkHomeModLocation2 dflags mod src_basename ext = do
   let mod_basename = dots_to_slashes (moduleUserString mod)

   obj_fn <- mkObjPath dflags src_basename mod_basename
   hi_fn  <- mkHiPath  dflags src_basename mod_basename

   return (ModLocation{ ml_hs_file   = Just (src_basename ++ '.':ext),
			ml_hi_file   = hi_fn,
			ml_obj_file  = obj_fn })

hiOnlyModLocation :: DynFlags -> FilePath -> String -> Suffix -> IO ModLocation
hiOnlyModLocation dflags path basename hisuf 
 = do let full_basename = path++'/':basename
      obj_fn <- mkObjPath dflags full_basename basename
      return ModLocation{    ml_hs_file   = Nothing,
 	        	     ml_hi_file   = full_basename ++ '.':hisuf,
		 		-- Remove the .hi-boot suffix from
		 		-- hi_file, if it had one.  We always
		 		-- want the name of the real .hi file
		 		-- in the ml_hi_file field.
	   	             ml_obj_file  = obj_fn
                  }

-- | Constructs the filename of a .o file for a given source file.
-- Does /not/ check whether the .o file exists
mkObjPath
  :: DynFlags
  -> FilePath		-- the filename of the source file, minus the extension
  -> String		-- the module name with dots replaced by slashes
  -> IO FilePath
mkObjPath dflags basename mod_basename
  = do  let
		odir = outputDir dflags
		osuf = objectSuf dflags
	
		obj_basename | Just dir <- odir = dir ++ '/':mod_basename
			     | otherwise        = basename

        return (obj_basename ++ '.':osuf)

-- | Constructs the filename of a .hi file for a given source file.
-- Does /not/ check whether the .hi file exists
mkHiPath
  :: DynFlags
  -> FilePath		-- the filename of the source file, minus the extension
  -> String		-- the module name with dots replaced by slashes
  -> IO FilePath
mkHiPath dflags basename mod_basename
  = do  let
		hidir = hiDir dflags
		hisuf = hiSuf dflags

		hi_basename | Just dir <- hidir = dir ++ '/':mod_basename
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


-- -----------------------------------------------------------------------------
-- Error messages

cantFindError :: DynFlags -> Module -> FindResult -> SDoc
cantFindError dflags mod_name find_result
  = hang (ptext SLIT("Could not find module") <+> quotes (ppr mod_name) <> colon)
       2 more_info
  where
    more_info
      = case find_result of
	    PackageHidden pkg 
		-> ptext SLIT("it is a member of package") <+> ppr pkg <> comma
		   <+> ptext SLIT("which is hidden")

	    ModuleHidden pkg
		-> ptext SLIT("it is hidden") <+> parens (ptext SLIT("in package")
		   <+> ppr pkg)

	    NotFound files
		| null files
		-> ptext SLIT("it is not a module in the current program, or in any known package.")
		| verbosity dflags < 3 
		-> ptext SLIT("use -v to see a list of the files searched for")
		| otherwise 
		-> hang (ptext SLIT("locations searched:")) 
		      2 (vcat (map text files))

	    Found _ _ -> panic "cantFindErr"
\end{code}
