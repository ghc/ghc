{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.98 2002/03/12 16:40:57 simonmar Exp $
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
import InteractiveUI(ghciWelcomeMsg, interactiveUI)
#endif


import Finder		( initFinder )
import CompManager	( cmInit, cmLoadModules, cmDepAnal )
import HscTypes		( GhciMode(..) )
import Config		( cBooterVersion, cGhcUnregisterised, cProjectVersion )
import SysTools		( getPackageConfigPath, initSysTools, cleanTempFiles )
import Packages		( showPackages )

import DriverPipeline	( doLink, doMkDLL, genPipeline, pipeLoop )
import DriverState	( buildCoreToDo, buildStgToDo, defaultHscLang,
			  findBuildTag, getPackageInfo, unregFlags, 
			  v_GhcMode, v_GhcModeFlag, GhcMode(..),
			  v_Cmdline_libraries, v_Keep_tmp_files, v_Ld_inputs,
			  v_OptLevel, v_Output_file, v_Output_hi, 
			  v_Package_details, v_Ways, getPackageExtraGhcOpts,
			  readPackageConf
			)
import DriverFlags	( buildStaticHscOpts,
			  dynamic_flags, processArgs, static_flags)

import DriverMkDepend	( beginMkDependHS, endMkDependHS )
import DriverPhases	( Phase(HsPp, Hsc), haskellish_src_file, objish_file )

import DriverUtil	( add, handle, handleDyn, later, splitFilename,
			  unknownFlagErr, getFileSuffix )
import CmdLineOpts	( dynFlag, restoreDynFlags,
			  saveDynFlags, setDynFlags, getDynFlags, dynFlag,
			  DynFlags(..), HscLang(..), v_Static_hsc_opts
			)
import Outputable
import Util
import Panic		( GhcException(..), panic )

-- Standard Haskell libraries
import IO
import Directory	( doesFileExist )
import IOExts		( readIORef, writeIORef )
import Exception	( throwDyn, Exception(..), 
			  AsyncException(StackOverflow) )
import System		( getArgs, exitWith, ExitCode(..) )
import Monad
import List
import Maybe

#ifndef mingw32_TARGET_OS
import Concurrent	( myThreadId )
#if __GLASGOW_HASKELL__ < 500
import Exception        ( raiseInThread )
#define throwTo  raiseInThread
#else
import Exception	( throwTo )
#endif

import Posix		( Handler(Catch), installHandler, sigINT, sigQUIT )
import Dynamic		( toDyn )
#endif

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

	-- install signal handlers
#ifndef mingw32_TARGET_OS
   main_thread <- myThreadId
   let sig_handler = Catch (throwTo main_thread 
				(DynException (toDyn Interrupted)))
   installHandler sigQUIT sig_handler Nothing 
   installHandler sigINT  sig_handler Nothing
#endif

   argv <- getArgs
   let (minusB_args, argv') = partition (prefixMatch "-B") argv
   top_dir <- initSysTools minusB_args

	-- Read the package configuration
   conf_file <- getPackageConfigPath
   readPackageConf conf_file

	-- process all the other arguments, and get the source files
   non_static <- processArgs static_flags argv' []
   mode <- readIORef v_GhcMode
   stop_flag <- readIORef v_GhcModeFlag

	-- -O and --interactive are not a good combination
	-- ditto with any kind of way selection
   orig_opt_level <- readIORef v_OptLevel
   when (orig_opt_level > 0 && mode == DoInteractive) $
      do putStr "warning: -O conflicts with --interactive; -O turned off.\n"
         writeIORef v_OptLevel 0
   orig_ways <- readIORef v_Ways
   when (not (null orig_ways) && mode == DoInteractive) $
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
   let lang = case mode of 
		 DoInteractive  -> HscInterpreted
		 _other         -> defaultHscLang

   setDynFlags (dyn_flags{ coreToDo = core_todo,
			   stgToDo  = stg_todo,
                  	   hscLang  = lang,
			   -- leave out hscOutName for now
	                   hscOutName = panic "Main.main:hscOutName not set",
		  	   verbosity = 1
			})

	-- the rest of the arguments are "dynamic"
   srcs <- processArgs dynamic_flags (extra_non_static ++ non_static) []

	-- save the "initial DynFlags" away
   saveDynFlags

    	-- complain about any unknown flags
   mapM unknownFlagErr [ f | f@('-':_) <- srcs ]

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

   pkg_details <- readIORef v_Package_details
   showPackages pkg_details

   when (verb >= 3) 
	(hPutStrLn stderr ("Hsc static flags: " ++ unwords static_opts))

	-- initialise the finder
   pkg_avails <- getPackageInfo
   initFinder pkg_avails

	-- mkdependHS is special
   when (mode == DoMkDependHS) beginMkDependHS

	-- -ohi sanity checking
   ohi    <- readIORef v_Output_hi
   if (isJust ohi && 
	(mode == DoMake || mode == DoInteractive || srcs `lengthExceeds` 1))
	then throwDyn (UsageError "-ohi can only be used when compiling a single source file")
	else do

	-- make/interactive require invoking the compilation manager
   if (mode == DoMake)        then beginMake srcs        else do
   if (mode == DoInteractive) then beginInteractive srcs else do

	-- -o sanity checking
   o_file <- readIORef v_Output_file
   if (srcs `lengthExceeds` 1 && isJust o_file && mode /= DoLink && mode /= DoMkDLL)
	then throwDyn (UsageError "can't apply -o to multiple source files")
	else do

   if null srcs then throwDyn (UsageError "no input files") else do

   let compileFile src = do
	  restoreDynFlags

	  exists <- doesFileExist src
          when (not exists) $ 
		throwDyn (CmdLineError ("file `" ++ src ++ "' does not exist"))

	  -- We compile in two stages, because the file may have an
	  -- OPTIONS pragma that affects the compilation pipeline (eg. -fvia-C)
	  let (basename, suffix) = splitFilename src

	  -- just preprocess (Haskell source only)
   	  let src_and_suff = (src, getFileSuffix src)
	  let not_hs_file  = not (haskellish_src_file src)
	  pp <- if not_hs_file || mode == StopBefore Hsc || mode == StopBefore HsPp
			then return src_and_suff else do
		phases <- genPipeline (StopBefore Hsc) stop_flag
				      False{-not persistent-} defaultHscLang
				      src_and_suff
	  	pipeLoop phases src_and_suff False{-no linking-} False{-no -o flag-}
			basename suffix

	  -- rest of compilation
	  hsc_lang <- dynFlag hscLang
	  phases   <- genPipeline mode stop_flag True hsc_lang pp
	  (r,_)    <- pipeLoop phases pp (mode==DoLink || mode==DoMkDLL)
	  			      True{-use -o flag-} basename suffix
	  return r

   o_files <- mapM compileFile srcs

   when (mode == DoMkDependHS) endMkDependHS
   when (mode == DoLink) (doLink o_files)
   when (mode == DoMkDLL) (doMkDLL o_files)



beginMake :: [String] -> IO ()
beginMake fileish_args
  = do let (objs, mods) = partition objish_file fileish_args
       mapM (add v_Ld_inputs) objs

       case mods of
	 []    -> throwDyn (UsageError "no input files")
	 _     -> do dflags <- getDynFlags 
		     state <- cmInit Batch
		     graph <- cmDepAnal state dflags mods
		     (_, ok, _) <- cmLoadModules state dflags graph
		     when (not ok) (exitWith (ExitFailure 1))
		     return ()


beginInteractive :: [String] -> IO ()
#ifndef GHCI
beginInteractive = throwDyn (CmdLineError "not built for interactive use")
#else
beginInteractive fileish_args
  = do minus_ls <- readIORef v_Cmdline_libraries

       let (objs, mods) = partition objish_file fileish_args
	   libs = map Left objs ++ map Right minus_ls

       state <- cmInit Interactive
       interactiveUI state mods libs
#endif
