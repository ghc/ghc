-----------------------------------------------------------------------------
-- $Id: DriverMkDepend.hs,v 1.42 2005/02/22 16:29:42 simonpj Exp $
--
-- GHC Driver
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

module DriverMkDepend (
	doMkDependHS
  ) where

#include "HsVersions.h"

import CompManager	( cmDownsweep, cmTopSort, cyclicModuleErr )
import CmdLineOpts	( DynFlags( verbosity ) )
import DriverState      ( getStaticOpts, v_Opt_dep )
import DriverUtil	( escapeSpaces, splitFilename, add )
import DriverFlags	( processArgs, OptKind(..) )
import HscTypes		( IsBootInterface, ModSummary(..), msObjFilePath, msHsFilePath )
import Packages		( PackageIdH(..) )
import SysTools		( newTempName )
import qualified SysTools
import Module		( Module, ModLocation(..), mkModule, moduleUserString, addBootSuffix_maybe )
import Digraph		( SCC(..) )
import Finder		( findModule, FindResult(..) )
import Util             ( global )
import Outputable
import Panic

import DATA_IOREF	( IORef, readIORef, writeIORef )
import EXCEPTION

import Directory
import IO
import Monad            ( when )
import Maybe            ( isJust )

#if __GLASGOW_HASKELL__ <= 408
import Panic		( catchJust, ioErrors )
#endif

-----------------------------------------------------------------
--
--		The main function
--
-----------------------------------------------------------------

doMkDependHS :: DynFlags -> [FilePath] -> IO ()
doMkDependHS dflags srcs
  = do	{ 	-- Initialisation
	  files <- beginMkDependHS

		-- Do the downsweep to find all the modules
	; excl_mods <- readIORef v_Dep_exclude_mods
	; mod_summaries <- cmDownsweep dflags srcs [] excl_mods

		-- Sort into dependency order
		-- There should be no cycles
	; let sorted = cmTopSort False mod_summaries

		-- Print out the dependencies if wanted
	; if verbosity dflags >= 2 then
		hPutStrLn stderr (showSDoc (text "Module dependencies" $$ ppr sorted))
	  else return ()
		
		-- Prcess them one by one, dumping results into makefile
		-- and complaining about cycles
	; mapM (processDeps dflags (mkd_tmp_hdl files)) sorted

		-- Tidy up
	; endMkDependHS dflags files }

-----------------------------------------------------------------
--
--		beginMkDependHs
--	Create a temporary file, 
--	find the Makefile, 
--	slurp through it, etc
--
-----------------------------------------------------------------

data MkDepFiles 
  = MkDep { mkd_make_file :: FilePath,		-- Name of the makefile
	    mkd_make_hdl  :: Maybe Handle, 	-- Handle for the open makefile 
	    mkd_tmp_file  :: FilePath,		-- Name of the temporary file
	    mkd_tmp_hdl   :: Handle }		-- Handle of the open temporary file

beginMkDependHS :: IO MkDepFiles
	
beginMkDependHS = do
  	-- slurp in the mkdependHS-style options
  flags <- getStaticOpts v_Opt_dep
  _ <- processArgs dep_opts flags []

     	-- open a new temp file in which to stuff the dependency info
     	-- as we go along.
  tmp_file <- newTempName "dep"
  tmp_hdl <- openFile tmp_file WriteMode

  	-- open the makefile
  makefile <- readIORef v_Dep_makefile
  exists <- doesFileExist makefile
  mb_make_hdl <- 
	if not exists
	then return Nothing
	else do
  	   makefile_hdl <- openFile makefile ReadMode

		-- slurp through until we get the magic start string,
		-- copying the contents into dep_makefile
  	   let slurp = do
		l <- hGetLine makefile_hdl
		if (l == depStartMarker)
			then return ()
			else do hPutStrLn tmp_hdl l; slurp
	 
		-- slurp through until we get the magic end marker,
		-- throwing away the contents
  	   let chuck = do
		l <- hGetLine makefile_hdl
		if (l == depEndMarker)
			then return ()
			else chuck
	 
	   catchJust ioErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)
	   catchJust ioErrors chuck
		(\e -> if isEOFError e then return () else ioError e)

	   return (Just makefile_hdl)


	-- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depStartMarker

  return (MkDep { mkd_make_file = makefile, mkd_make_hdl = mb_make_hdl, 
		  mkd_tmp_file  = tmp_file, mkd_tmp_hdl  = tmp_hdl})


-----------------------------------------------------------------
--
--		processDeps
--
-----------------------------------------------------------------

processDeps :: DynFlags
	    -> Handle		-- Write dependencies to here
	    -> SCC ModSummary
	    -> IO ()
-- Write suitable dependencies to handle
-- Always:
-- 			this.o : this.hs
--
-- If the dependency is on something other than a .hi file:
--   			this.o this.p_o ... : dep
-- otherwise
--   			this.o ...   : dep.hi
--   			this.p_o ... : dep.p_hi
--   			...
-- (where .o is $osuf, and the other suffixes come from
-- the cmdline -s options).
--
-- For {-# SOURCE #-} imports the "hi" will be "hi-boot".

processDeps dflags hdl (CyclicSCC nodes)
  =	-- There shouldn't be any cycles; report them	
    throwDyn (ProgramError (showSDoc $ cyclicModuleErr nodes))

processDeps dflags hdl (AcyclicSCC node)
  = do	{ extra_suffixes   <- readIORef v_Dep_suffixes
	; include_pkg_deps <- readIORef v_Dep_include_pkg_deps
	; let src_file  = msHsFilePath node
	      obj_file  = msObjFilePath node
	      obj_files = insertSuffixes obj_file extra_suffixes

	      do_imp is_boot imp_mod
		= do { mb_hi <- findDependency dflags src_file imp_mod 
					       is_boot include_pkg_deps
		     ; case mb_hi of {
			   Nothing      -> return () ;
			   Just hi_file -> do
		     { let hi_files = insertSuffixes hi_file extra_suffixes
			   write_dep (obj,hi) = writeDependency hdl [obj] hi

			-- Add one dependency for each suffix; 
			-- e.g.		A.o   : B.hi
			--		A.x_o : B.x_hi
		     ; mapM_ write_dep (obj_files `zip` hi_files) }}}

	     
	  	-- Emit std dependency of the object(s) on the source file
		-- Something like 	A.o : A.hs
	; writeDependency hdl obj_files src_file

		-- Emit a dependency for each import
	; mapM_ (do_imp True)  (ms_srcimps node)	-- SOURCE imports
	; mapM_ (do_imp False) (ms_imps node)		-- regular imports
	}


findDependency	:: DynFlags
		-> FilePath 		-- Importing module: used only for error msg
		-> Module		-- Imported module
		-> IsBootInterface	-- Source import
		-> Bool			-- Record dependency on package modules
		-> IO (Maybe FilePath)	-- Interface file file
findDependency dflags src imp is_boot include_pkg_deps
  = do	{ 	-- Find the module; this will be fast because
		-- we've done it once during downsweep
	  r <- findModule dflags imp True {-explicit-}
	; case r of 
	    Found loc pkg
		-- Not in this package: we don't need a dependency
		| ExtPackage _ <- pkg, not include_pkg_deps
		-> return Nothing

		-- Home package: just depend on the .hi or hi-boot file
		| otherwise
		-> return (Just (addBootSuffix_maybe is_boot (ml_hi_file loc)))

	    _ -> throwDyn (ProgramError 
		 (src ++ ": " ++ "can't locate import `" ++ (moduleUserString imp) ++ "'"
		  ++ if is_boot then " (SOURCE import)" else ""))
	}

-----------------------------
writeDependency :: Handle -> [FilePath] -> FilePath -> IO ()
-- (writeDependency h [t1,t2] dep) writes to handle h the dependency
--	t1 t2 : dep
writeDependency hdl targets dep
  = hPutStrLn hdl (unwords (map escapeSpaces targets) ++ " : "
		   ++ escapeSpaces dep)

-----------------------------
insertSuffixes	
	:: FilePath 	-- Original filename;	e.g. "foo.o"
	-> [String]	-- Extra suffices	e.g. ["x","y"]
	-> [FilePath]	-- Zapped filenames	e.g. ["foo.o", "foo.x_o", "foo.y_o"]
	-- Note that that the extra bit gets inserted *before* the old suffix
	-- We assume the old suffix contains no dots, so we can strip it with removeSuffix

	-- NOTE: we used to have this comment
		-- In order to construct hi files with alternate suffixes, we
		-- now have to find the "basename" of the hi file.  This is
		-- difficult because we can't just split the hi filename
		-- at the last dot - the hisuf might have dots in it.  So we
		-- check whether the hi filename ends in hisuf, and if it does,
		-- we strip off hisuf, otherwise we strip everything after the
		-- last dot.
	-- But I'm not sure we care about hisufs with dots in them. 
	-- Lots of other things will break first!

insertSuffixes file_name extras
  = file_name : [ basename ++ "." ++ extra ++ "_" ++ suffix | extra <- extras ]
  where
    (basename, suffix) = splitFilename file_name


-----------------------------------------------------------------
--
--		endMkDependHs
--	Complete the makefile, close the tmp file etc
--
-----------------------------------------------------------------

endMkDependHS :: DynFlags -> MkDepFiles -> IO ()

endMkDependHS dflags 
   (MkDep { mkd_make_file = makefile, mkd_make_hdl =  makefile_hdl,
            mkd_tmp_file  = tmp_file, mkd_tmp_hdl  =  tmp_hdl }) 
  = do
  -- write the magic marker into the tmp file
  hPutStrLn tmp_hdl depEndMarker

  case makefile_hdl of
     Nothing  -> return ()
     Just hdl -> do

	  -- slurp the rest of the original makefile and copy it into the output
  	let slurp = do
		l <- hGetLine hdl
		hPutStrLn tmp_hdl l
		slurp
	 
  	catchJust ioErrors slurp 
		(\e -> if isEOFError e then return () else ioError e)

	hClose hdl

  hClose tmp_hdl  -- make sure it's flushed

	-- Create a backup of the original makefile
  when (isJust makefile_hdl)
       (SysTools.copy dflags ("Backing up " ++ makefile) 
	  makefile (makefile++".bak"))

  	-- Copy the new makefile in place
  SysTools.copy dflags "Installing new makefile" tmp_file makefile


-----------------------------------------------------------------
--
--		Flags
--
-----------------------------------------------------------------

	-- Flags
GLOBAL_VAR(v_Dep_makefile, 		"Makefile", String);
GLOBAL_VAR(v_Dep_include_pkg_deps, 	False, Bool);
GLOBAL_VAR(v_Dep_exclude_mods,          [], [Module]);
GLOBAL_VAR(v_Dep_suffixes,		[], [String]);
GLOBAL_VAR(v_Dep_warnings,		True, Bool);

depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

-- for compatibility with the old mkDependHS, we accept options of the form
-- -optdep-f -optdep.depend, etc.
dep_opts = 
   [ (  "s", 			SepArg (add v_Dep_suffixes) )
   , (  "f", 			SepArg (writeIORef v_Dep_makefile) )
   , (  "w", 			NoArg (writeIORef v_Dep_warnings False) )
   , (  "-include-prelude",  	NoArg (writeIORef v_Dep_include_pkg_deps True) )
   , (  "-include-pkg-deps",  	NoArg (writeIORef v_Dep_include_pkg_deps True) )
   , (  "-exclude-module=",     Prefix (add v_Dep_exclude_mods . mkModule) )
   , (  "x",                    Prefix (add v_Dep_exclude_mods . mkModule) )
   ]
