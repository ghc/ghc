{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.145 2005/02/01 08:36:07 simonpj Exp $
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

module Main (main) where

#include "HsVersions.h"

#ifdef GHCI
import InteractiveUI( ghciWelcomeMsg, interactiveUI )
#endif


import CompManager	( cmInit, cmLoadModules, cmDepAnal )
import HscTypes		( GhciMode(..) )
import Config		( cBooterVersion, cGhcUnregisterised, cProjectVersion )
import SysTools		( initSysTools, cleanTempFiles, normalisePath )
import Packages		( dumpPackages, initPackages, haskell98PackageId, PackageIdH(..) )
import DriverPipeline	( staticLink, doMkDLL, compileFile )
import DriverState	( isLinkMode, isMakeMode, isInteractiveMode,
			  isCompManagerMode, isInterpretiveMode, 
			  buildStgToDo, findBuildTag, unregFlags, 
			  v_GhcMode, v_GhcModeFlag, GhcMode(..),
			  v_Keep_tmp_files, v_Ld_inputs, v_Ways, 
			  v_Output_file, v_Output_hi, v_GhcLink,
			  verifyOutputFiles, GhcLink(..)
			)
import DriverFlags

import DriverMkDepend	( doMkDependHS )
import DriverPhases	( Phase, isStopLn, isSourceFilename )

import DriverUtil	( add, handle, handleDyn, later, unknownFlagsErr )
import CmdLineOpts	( DynFlags(..), HscTarget(..), v_Static_hsc_opts,
			  defaultDynFlags )
import BasicTypes	( failed )
import Outputable
import Util
import Panic		( GhcException(..), panic, installSignalHandlers )

import DATA_IOREF	( readIORef, writeIORef )
import EXCEPTION	( throwDyn, Exception(..), 
			  AsyncException(StackOverflow) )

-- Standard Haskell libraries
import IO
import Directory	( doesFileExist )
import System		( getArgs, exitWith, ExitCode(..) )
import Monad
import List
import Maybe

-----------------------------------------------------------------------------
-- ToDo:

-- new mkdependHS doesn't support all the options that the old one did (-X et al.)
-- time commands when run with -v
-- split marker
-- java generation
-- user ways
-- Win32 support: proper signal handling
-- make sure OPTIONS in .hs file propogate to .hc file if -C or -keep-hc-file-too
-- reading the package configuration file is too slow
-- -K<size>

-----------------------------------------------------------------------------
-- Differences vs. old driver:

-- No more "Enter your Haskell program, end with ^D (on a line of its own):"
-- consistency checking removed (may do this properly later)
-- no -Ofile

-----------------------------------------------------------------------------
-- Main loop

main =
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  handle (\exception -> do
  	   hFlush stdout
	   case exception of
		-- an IO exception probably isn't our fault, so don't panic
		IOException _ ->  hPutStrLn stderr (show exception)
		AsyncException StackOverflow ->
			hPutStrLn stderr "stack overflow: use +RTS -K<size> to increase it"
		_other ->  hPutStr stderr (show (Panic (show exception)))
	   exitWith (ExitFailure 1)
         ) $ do

  -- all error messages are propagated as exceptions
  handleDyn (\dyn -> do
  		hFlush stdout
  		case dyn of
		     PhaseFailed _ code -> exitWith code
		     Interrupted -> exitWith (ExitFailure 1)
		     _ -> do hPutStrLn stderr (show (dyn :: GhcException))
			     exitWith (ExitFailure 1)
	    ) $ do

   installSignalHandlers

   argv <- getArgs
   let (minusB_args, argv') = partition (prefixMatch "-B") argv
   top_dir <- initSysTools minusB_args

	-- Process all the other arguments, and get the source files
   non_static <- processStaticFlags argv'
   mode <- readIORef v_GhcMode

	-- -O and --interactive are not a good combination
	-- ditto with any kind of way selection
   orig_ways <- readIORef v_Ways
   when (notNull orig_ways && isInterpretiveMode mode) $
      do throwDyn (UsageError 
                   "--interactive can't be used with -prof, -ticky, -unreg or -smp.")

	-- Find the build tag, and re-process the build-specific options.
	-- Also add in flags for unregisterised compilation, if 
	-- GhcUnregisterised=YES.
   way_opts <- findBuildTag
   let unreg_opts | cGhcUnregisterised == "YES" = unregFlags
		  | otherwise = []
   extra_non_static <- processStaticFlags (unreg_opts ++ way_opts)

	-- Give the static flags to hsc
   static_opts <- buildStaticHscOpts
   writeIORef v_Static_hsc_opts static_opts

   -- build the default DynFlags (these may be adjusted on a per
   -- module basis by OPTIONS pragmas and settings in the interpreter).

   stg_todo  <- buildStgToDo

   -- set the "global" HscTarget.  The HscTarget can be further adjusted on a module
   -- by module basis, using only the -fvia-C and -fasm flags.  If the global
   -- HscTarget is not HscC or HscAsm, -fvia-C and -fasm have no effect.
   let dflags0 = defaultDynFlags
   let lang = case mode of 
		 DoInteractive  -> HscInterpreted
		 DoEval _	-> HscInterpreted
		 _other		-> hscTarget dflags0

   let dflags1 = dflags0{ stgToDo  = stg_todo,
                  	  hscTarget  = lang,
			  -- leave out hscOutName for now
	                  hscOutName = panic "Main.main:hscOutName not set",
		  	  verbosity = case mode of
				 	 DoEval _ -> 0
				 	 _other   -> 1
			}

	-- The rest of the arguments are "dynamic"
	-- Leftover ones are presumably files
   (dflags2, fileish_args) <- processDynamicFlags 
				(extra_non_static ++ non_static) dflags1

	-- make sure we clean up after ourselves
   later (do  forget_it <- readIORef v_Keep_tmp_files
	      unless forget_it $ do
	      cleanTempFiles dflags2
     ) $ do
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.

	-- Read the package config(s), and process the package-related
	-- command-line flags
   dflags <- initPackages dflags2

   let
    {-
      We split out the object files (.o, .dll) and add them
      to v_Ld_inputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other 
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - and finally we consider everything not containing a '.' to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
    looks_like_an_input m =  isSourceFilename m 
			  || looksLikeModuleName m
			  || '.' `notElem` m

     -- To simplify the handling of filepaths, we normalise all filepaths right 
     -- away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    normal_fileish_paths = map normalisePath fileish_args
    (srcs, objs)         = partition looks_like_an_input normal_fileish_paths

    -- Note: have v_Ld_inputs maintain the order in which 'objs' occurred on 
    --       the command-line.
   mapM_ (add v_Ld_inputs) (reverse objs)

	---------------- Display banners and configuration -----------
   showBanners mode dflags static_opts

	---------------- Final sanity checking -----------
   checkOptions mode srcs objs

	---------------- Do the business -----------

   case mode of
	DoMake 	       -> doMake dflags srcs
	DoMkDependHS   -> doMkDependHS dflags srcs 
	StopBefore p   -> do { o_files <- compileFiles mode dflags srcs 
			     ; doLink dflags p o_files }
#ifndef GHCI
	DoInteractive -> noInteractiveError
	DoEval _      -> noInteractiveError
     where
       noInteractiveError = throwDyn (CmdLineError "not built for interactive use")
#else
	DoInteractive -> interactiveUI dflags srcs Nothing
	DoEval expr   -> interactiveUI dflags srcs (Just expr)
#endif

-- -----------------------------------------------------------------------------
-- Option sanity checks

checkOptions :: GhcMode -> [String] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions mode srcs objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | f@('-':_) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

	-- -ohi sanity check
   ohi <- readIORef v_Output_hi
   if (isJust ohi && 
      (isCompManagerMode mode || srcs `lengthExceeds` 1))
	then throwDyn (UsageError "-ohi can only be used when compiling a single source file")
	else do

	-- -o sanity checking
   o_file <- readIORef v_Output_file
   if (srcs `lengthExceeds` 1 && isJust o_file && not (isLinkMode mode))
	then throwDyn (UsageError "can't apply -o to multiple source files")
	else do

	-- Check that there are some input files
	-- (except in the interactive case)
   if null srcs && null objs && not (isInterpretiveMode mode)
	then throwDyn (UsageError "no input files")
	else do

     -- Verify that output files point somewhere sensible.
   verifyOutputFiles

-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

compileFiles :: GhcMode
	     -> DynFlags
	     -> [String]	-- Source files
	     -> IO [String]	-- Object files
compileFiles mode dflags srcs = mapM (compileFile mode dflags) srcs


doLink :: DynFlags -> Phase -> [FilePath] -> IO ()
doLink dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()		-- We stopped before the linking phase

  | otherwise
  = do 	{ ghc_link <- readIORef v_GhcLink
	; case ghc_link of
	    NoLink     -> return ()
	    StaticLink -> staticLink dflags o_files link_pkgs
	    MkDLL      -> doMkDLL dflags o_files link_pkgs
	}
  where
   -- Always link in the haskell98 package for static linking.  Other
   -- packages have to be specified via the -package flag.
    link_pkgs
	  | ExtPackage h98_id <- haskell98PackageId (pkgState dflags) = [h98_id]
	  | otherwise = []


-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: DynFlags -> [String] -> IO ()
doMake dflags []    = throwDyn (UsageError "no input files")
doMake dflags srcs  = do 
    state  <- cmInit Batch dflags
    graph  <- cmDepAnal state srcs
    (_, ok_flag, _) <- cmLoadModules state graph
    when (failed ok_flag) (exitWith (ExitFailure 1))
    return ()

-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanners :: GhcMode -> DynFlags -> [String] -> IO ()
showBanners mode dflags static_opts = do
   let verb = verbosity dflags

	-- Show the GHCi banner
#  ifdef GHCI
   when (isInteractiveMode mode && verb >= 1) $
      hPutStrLn stdout ghciWelcomeMsg
#  endif

	-- Display details of the configuration in verbose mode
   when (verb >= 2) $
	do hPutStr stderr "Glasgow Haskell Compiler, Version "
 	   hPutStr stderr cProjectVersion
	   hPutStr stderr ", for Haskell 98, compiled by GHC version "
	   hPutStrLn stderr cBooterVersion

   when (verb >= 3) $
	dumpPackages dflags

   when (verb >= 3) $
	hPutStrLn stderr ("Hsc static flags: " ++ unwords static_opts)
