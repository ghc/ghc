{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.125 2003/06/10 17:54:56 sof Exp $
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2002
--
-----------------------------------------------------------------------------

-- with path so that ghc -M can find config.h
#include "../includes/config.h"

module Main (main) where

#include "HsVersions.h"


#ifdef GHCI
import InteractiveUI( ghciWelcomeMsg, interactiveUI )
#endif


import CompManager	( cmInit, cmLoadModules, cmDepAnal )
import HscTypes		( GhciMode(..) )
import Config		( cBooterVersion, cGhcUnregisterised, cProjectVersion )
import SysTools		( getPackageConfigPath, initSysTools, cleanTempFiles,
			  normalisePath )
import Packages		( showPackages, getPackageConfigMap, basePackage,
			  haskell98Package
			)
import DriverPipeline	( staticLink, doMkDLL, runPipeline )
import DriverState	( buildCoreToDo, buildStgToDo,
			  findBuildTag, 
			  getPackageExtraGhcOpts, unregFlags, 
			  v_GhcMode, v_GhcModeFlag, GhcMode(..),
			  v_Keep_tmp_files, v_Ld_inputs, v_Ways, 
			  v_OptLevel, v_Output_file, v_Output_hi, 
			  readPackageConf, verifyOutputFiles, v_NoLink,
			  v_Build_tag
			)
import DriverFlags	( buildStaticHscOpts,
			  dynamic_flags, processArgs, static_flags)

import DriverMkDepend	( beginMkDependHS, endMkDependHS )
import DriverPhases	( isSourceFile )

import DriverUtil	( add, handle, handleDyn, later, unknownFlagsErr )
import CmdLineOpts	( dynFlag, restoreDynFlags,
			  saveDynFlags, setDynFlags, getDynFlags, dynFlag,
			  DynFlags(..), HscLang(..), v_Static_hsc_opts
			)
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
		IOException _ ->  hPutStr stderr (show exception)
		AsyncException StackOverflow ->
			hPutStrLn stderr "stack overflow: use +RTS -K<size> \ 
					 \to increase it"
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

   -- make sure we clean up after ourselves
   later (do  forget_it <- readIORef v_Keep_tmp_files
	      unless forget_it $ do
	      verb <- dynFlag verbosity
	      cleanTempFiles verb
     ) $ do
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.

   installSignalHandlers

   argv <- getArgs
   let (minusB_args, argv') = partition (prefixMatch "-B") argv
   top_dir <- initSysTools minusB_args

	-- Read the package configuration
   conf_file <- getPackageConfigPath
   readPackageConf conf_file

	-- Process all the other arguments, and get the source files
   non_static <- processArgs static_flags argv' []
   mode <- readIORef v_GhcMode

	-- -O and --interactive are not a good combination
	-- ditto with any kind of way selection
   orig_opt_level <- readIORef v_OptLevel
   when (orig_opt_level > 0 && mode == DoInteractive) $
      do putStr "warning: -O conflicts with --interactive; -O turned off.\n"
         writeIORef v_OptLevel 0
   orig_ways <- readIORef v_Ways
   when (notNull orig_ways && mode == DoInteractive) $
      do throwDyn (UsageError 
                   "--interactive can't be used with -prof, -ticky, -unreg or -smp.")

	-- Find the build tag, and re-process the build-specific options.
	-- Also add in flags for unregisterised compilation, if 
	-- GhcUnregisterised=YES.
   way_opts <- findBuildTag
   let unreg_opts | cGhcUnregisterised == "YES" = unregFlags
		  | otherwise = []
   pkg_extra_opts <- getPackageExtraGhcOpts
   extra_non_static <- processArgs static_flags 
			   (unreg_opts ++ way_opts ++ pkg_extra_opts) []

	-- give the static flags to hsc
   static_opts <- buildStaticHscOpts
   writeIORef v_Static_hsc_opts static_opts

   -- build the default DynFlags (these may be adjusted on a per
   -- module basis by OPTIONS pragmas and settings in the interpreter).

   core_todo <- buildCoreToDo
   stg_todo  <- buildStgToDo

   -- set the "global" HscLang.  The HscLang can be further adjusted on a module
   -- by module basis, using only the -fvia-C and -fasm flags.  If the global
   -- HscLang is not HscC or HscAsm, -fvia-C and -fasm have no effect.
   dyn_flags <- getDynFlags
   build_tag <- readIORef v_Build_tag
   let lang = case mode of 
		 DoInteractive  -> HscInterpreted
		 _other | build_tag /= "" -> HscC
			| otherwise       -> hscLang dyn_flags
		-- for ways other that the normal way, we must 
		-- compile via C.

   setDynFlags (dyn_flags{ coreToDo = core_todo,
			   stgToDo  = stg_todo,
                  	   hscLang  = lang,
			   -- leave out hscOutName for now
	                   hscOutName = panic "Main.main:hscOutName not set",
		  	   verbosity = 1
			})

	-- The rest of the arguments are "dynamic"
	-- Leftover ones are presumably files
   fileish_args <- processArgs dynamic_flags (extra_non_static ++ non_static) []

	-- save the "initial DynFlags" away
   saveDynFlags

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
    looks_like_an_input m =  isSourceFile m 
			  || looksLikeModuleName m
			  || '.' `notElem` m

    (raw_srcs, objs) = partition looks_like_an_input fileish_args

     -- To simplify the handling of filepaths, we normalise all source file
     -- paths right away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    srcs             = map normalisePath raw_srcs

   mapM_ (add v_Ld_inputs) objs

	---------------- Display banners and configuration -----------
   showBanners mode conf_file static_opts

	---------------- Final sanity checking -----------
   checkOptions mode srcs objs

	---------------- Do the business -----------
   case mode of
	DoMake 	       -> doMake srcs
			       
	DoMkDependHS   -> do { beginMkDependHS ; 
			       compileFiles mode srcs; 
			       endMkDependHS }
	StopBefore p   -> do { compileFiles mode srcs; return () }
	DoMkDLL	       -> do { o_files <- compileFiles mode srcs; 
			       doMkDLL o_files }
	DoLink	       -> do { o_files <- compileFiles mode srcs; 
			       omit_linking <- readIORef v_NoLink;
			       when (not omit_linking)
				    (staticLink o_files 
					[basePackage, haskell98Package]) }
			-- We always link in the base package in
			-- one-shot linking.  Any other packages
			-- required must be given using -package
			-- options on the command-line.

#ifndef GHCI
	DoInteractive -> throwDyn (CmdLineError "not built for interactive use")
#else
	DoInteractive -> interactiveUI srcs
#endif

-- -----------------------------------------------------------------------------
-- Option sanity checks

checkOptions :: GhcMode -> [String] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions mode srcs objs = do
	-- -ohi sanity check
   ohi <- readIORef v_Output_hi
   if (isJust ohi && 
      (mode == DoMake || mode == DoInteractive || srcs `lengthExceeds` 1))
	then throwDyn (UsageError "-ohi can only be used when compiling a single source file")
	else do

	-- -o sanity checking
   o_file <- readIORef v_Output_file
   if (srcs `lengthExceeds` 1 && isJust o_file && mode /= DoLink && mode /= DoMkDLL)
	then throwDyn (UsageError "can't apply -o to multiple source files")
	else do

	-- Check that there are some input files (except in the interactive 
	-- case)
   if null srcs && null objs && mode /= DoInteractive
	then throwDyn (UsageError "no input files")
	else do

     -- Complain about any unknown flags
   let unknown_opts = [ f | f@('-':_) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

     -- Verify that output files point somewhere sensible.
   verifyOutputFiles


-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

compileFiles :: GhcMode 
	     -> [String]	-- Source files
	     -> IO [String]	-- Object files
compileFiles mode srcs = do
   stop_flag <- readIORef v_GhcModeFlag
   mapM (compileFile mode stop_flag) srcs


compileFile mode stop_flag src = do
   restoreDynFlags
   
   exists <- doesFileExist src
   when (not exists) $ 
   	throwDyn (CmdLineError ("file `" ++ src ++ "' does not exist"))
   
   o_file   <- readIORef v_Output_file
	-- when linking, the -o argument refers to the linker's output.	
	-- otherwise, we use it as the name for the pipeline's output.
   let maybe_o_file
	  | mode==DoLink || mode==DoMkDLL  = Nothing
	  | otherwise                      = o_file

   runPipeline mode stop_flag True maybe_o_file src


-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: [String] -> IO ()
doMake []    = throwDyn (UsageError "no input files")
doMake srcs  = do 
    dflags <- getDynFlags 
    state  <- cmInit Batch
    graph  <- cmDepAnal state dflags srcs
    (_, ok_flag, _) <- cmLoadModules state dflags graph
    when (failed ok_flag) (exitWith (ExitFailure 1))
    return ()

-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanners :: GhcMode -> FilePath -> [String] -> IO ()
showBanners mode conf_file static_opts = do
   verb <- dynFlag verbosity

	-- Show the GHCi banner
#  ifdef GHCI
   when (mode == DoInteractive && verb >= 1) $
      hPutStrLn stdout ghciWelcomeMsg
#  endif

	-- Display details of the configuration in verbose mode
   when (verb >= 2) 
	(do hPutStr stderr "Glasgow Haskell Compiler, Version "
 	    hPutStr stderr cProjectVersion
	    hPutStr stderr ", for Haskell 98, compiled by GHC version "
	    hPutStrLn stderr cBooterVersion)

   when (verb >= 2) 
	(hPutStrLn stderr ("Using package config file: " ++ conf_file))

   pkg_details <- getPackageConfigMap
   showPackages pkg_details

   when (verb >= 3) 
	(hPutStrLn stderr ("Hsc static flags: " ++ unwords static_opts))
