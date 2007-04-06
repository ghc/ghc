-----------------------------------------------------------------------------
--
-- Makefile Dependency Generation
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module DriverMkDepend (
	doMkDependHS
  ) where

#include "HsVersions.h"

import qualified GHC
import GHC		( Session, ModSummary(..) )
import DynFlags
import Util		( escapeSpaces, splitFilename, joinFileExt )
import HscTypes		( HscEnv, IsBootInterface, msObjFilePath, msHsFilePath )
import SysTools		( newTempName )
import qualified SysTools
import Module
import Digraph		( SCC(..) )
import Finder		( findImportedModule, FindResult(..) )
import Util             ( global, consIORef )
import Outputable
import Panic
import SrcLoc
import Data.List
import CmdLineParser

import ErrUtils         ( debugTraceMsg, putMsg )

import Data.IORef	( IORef, readIORef, writeIORef )
import Control.Exception
import System.Exit	( ExitCode(..), exitWith )
import System.Directory
import System.IO
import SYSTEM_IO_ERROR  ( isEOFError )
import Control.Monad    ( when )
import Data.Maybe       ( isJust )

-----------------------------------------------------------------
--
--		The main function
--
-----------------------------------------------------------------

doMkDependHS :: Session -> [FilePath] -> IO ()
doMkDependHS session srcs
  = do	{ 	-- Initialisation
	  dflags <- GHC.getSessionDynFlags session
	; files <- beginMkDependHS dflags

		-- Do the downsweep to find all the modules
	; targets <- mapM (\s -> GHC.guessTarget s Nothing) srcs
	; GHC.setTargets session targets
	; excl_mods <- readIORef v_Dep_exclude_mods
	; r <- GHC.depanal session excl_mods True {- Allow dup roots -}
	; case r of
 	    Nothing -> exitWith (ExitFailure 1)
	    Just mod_summaries -> do {

		-- Sort into dependency order
		-- There should be no cycles
	  let sorted = GHC.topSortModuleGraph False mod_summaries Nothing

		-- Print out the dependencies if wanted
	; debugTraceMsg dflags 2 (text "Module dependencies" $$ ppr sorted)

		-- Prcess them one by one, dumping results into makefile
		-- and complaining about cycles
	; mapM (processDeps session excl_mods (mkd_tmp_hdl files)) sorted

		-- If -ddump-mod-cycles, show cycles in the module graph
	; dumpModCycles dflags mod_summaries

		-- Tidy up
	; endMkDependHS dflags files }}

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

beginMkDependHS :: DynFlags -> IO MkDepFiles
	
beginMkDependHS dflags = do
  	-- slurp in the mkdependHS-style options
  let flags = getOpts dflags opt_dep
  _ <- processArgs dep_opts flags

     	-- open a new temp file in which to stuff the dependency info
     	-- as we go along.
  tmp_file <- newTempName dflags "dep"
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

processDeps :: Session
	    -> [ModuleName]
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

processDeps session excl_mods hdl (CyclicSCC nodes)
  =	-- There shouldn't be any cycles; report them	
    throwDyn (ProgramError (showSDoc $ GHC.cyclicModuleErr nodes))

processDeps session excl_mods hdl (AcyclicSCC node)
  = do	{ extra_suffixes   <- readIORef v_Dep_suffixes
	; hsc_env <- GHC.sessionHscEnv session
	; include_pkg_deps <- readIORef v_Dep_include_pkg_deps
	; let src_file  = msHsFilePath node
	      obj_file  = msObjFilePath node
	      obj_files = insertSuffixes obj_file extra_suffixes

	      do_imp is_boot imp_mod
		= do { mb_hi <- findDependency hsc_env src_file imp_mod 
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

	-- SOURCE imports
	; mapM_ (do_imp True)  
		(filter (`notElem` excl_mods) (map unLoc (ms_srcimps node)))

	-- regular imports
	; mapM_ (do_imp False)
		(filter (`notElem` excl_mods) (map unLoc (ms_imps node)))
	}


findDependency	:: HscEnv
		-> FilePath 		-- Importing module: used only for error msg
		-> ModuleName		-- Imported module
		-> IsBootInterface	-- Source import
		-> Bool			-- Record dependency on package modules
		-> IO (Maybe FilePath)	-- Interface file file
findDependency hsc_env src imp is_boot include_pkg_deps
  = do	{ 	-- Find the module; this will be fast because
		-- we've done it once during downsweep
	  r <- findImportedModule hsc_env imp Nothing
	; case r of 
	    Found loc mod
		-- Home package: just depend on the .hi or hi-boot file
		| isJust (ml_hs_file loc)
		-> return (Just (addBootSuffix_maybe is_boot (ml_hi_file loc)))

		-- Not in this package: we don't need a dependency
		| otherwise
		-> return Nothing

	    _ -> panic "findDependency"
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
  = file_name : [ basename `joinFileExt` (extra ++ "_" ++ suffix) | extra <- extras ]
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
--		Module cycles
-----------------------------------------------------------------

dumpModCycles :: DynFlags -> [ModSummary] -> IO ()
dumpModCycles dflags mod_summaries
  | not (dopt Opt_D_dump_mod_cycles dflags) 
  = return ()

  | null cycles
  = putMsg dflags (ptext SLIT("No module cycles"))

  | otherwise
  = putMsg dflags (hang (ptext SLIT("Module cycles found:")) 2 pp_cycles)
  where

    cycles :: [[ModSummary]]
    cycles = [ c | CyclicSCC c <- GHC.topSortModuleGraph True mod_summaries Nothing ]

    pp_cycles = vcat [ (ptext SLIT("---------- Cycle") <+> int n <+> ptext SLIT("----------")) 
			$$ pprCycle c $$ text ""
		     | (n,c) <- [1..] `zip` cycles ]

pprCycle :: [ModSummary] -> SDoc
-- Print a cycle, but show only the imports within the cycle
pprCycle summaries = pp_group (CyclicSCC summaries)
  where
    cycle_mods :: [ModuleName]	-- The modules in this cycle
    cycle_mods = map (moduleName . ms_mod) summaries

    pp_group (AcyclicSCC ms) = pp_ms ms
    pp_group (CyclicSCC mss) 
	= ASSERT( not (null boot_only) )
		-- The boot-only list must be non-empty, else there would
		-- be an infinite chain of non-boot imoprts, and we've
		-- already checked for that in processModDeps
	  pp_ms loop_breaker $$ vcat (map pp_group groups)
	where
	  (boot_only, others) = partition is_boot_only mss
	  is_boot_only ms = not (any in_group (ms_imps ms))
	  in_group (L _ m) = m `elem` group_mods
	  group_mods = map (moduleName . ms_mod) mss
	  
	  loop_breaker = head boot_only
	  all_others   = tail boot_only ++ others
	  groups = GHC.topSortModuleGraph True all_others Nothing

    pp_ms summary = text mod_str <> text (take (20 - length mod_str) (repeat ' '))
		       <+> (pp_imps empty (ms_imps summary) $$
			    pp_imps (ptext SLIT("{-# SOURCE #-}")) (ms_srcimps summary))
	where
	  mod_str = moduleNameString (moduleName (ms_mod summary))

    pp_imps :: SDoc -> [Located ModuleName] -> SDoc
    pp_imps what [] = empty
    pp_imps what lms 
	= case [m | L _ m <- lms, m `elem` cycle_mods] of
	    [] -> empty
	    ms -> what <+> ptext SLIT("imports") <+> 
				pprWithCommas ppr ms

-----------------------------------------------------------------
--
--		Flags
--
-----------------------------------------------------------------

	-- Flags
GLOBAL_VAR(v_Dep_makefile, 		"Makefile", String);
GLOBAL_VAR(v_Dep_include_pkg_deps, 	False, Bool);
GLOBAL_VAR(v_Dep_exclude_mods,          [], [ModuleName]);
GLOBAL_VAR(v_Dep_suffixes,		[], [String]);
GLOBAL_VAR(v_Dep_warnings,		True, Bool);

depStartMarker = "# DO NOT DELETE: Beginning of Haskell dependencies"
depEndMarker   = "# DO NOT DELETE: End of Haskell dependencies"

-- for compatibility with the old mkDependHS, we accept options of the form
-- -optdep-f -optdep.depend, etc.
dep_opts = 
   [ (  "s", 			SepArg (consIORef v_Dep_suffixes) )
   , (  "f", 			SepArg (writeIORef v_Dep_makefile) )
   , (  "w", 			NoArg (writeIORef v_Dep_warnings False) )

   , (  "-include-prelude",  	NoArg (writeIORef v_Dep_include_pkg_deps True) )
	-- -include-prelude is the old name for -include-pkg-deps, kept around
	-- for backward compatibility, but undocumented

   , (  "-include-pkg-deps",  	NoArg (writeIORef v_Dep_include_pkg_deps True) )
   , (  "-exclude-module=",     Prefix (consIORef v_Dep_exclude_mods . mkModuleName) )
   , (  "x",                    Prefix (consIORef v_Dep_exclude_mods . mkModuleName) )
   ]
