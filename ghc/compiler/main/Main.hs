{-# OPTIONS -W -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.24 2000/11/16 15:57:06 simonmar Exp $
--
-- GHC Driver program
--
-- (c) Simon Marlow 2000
--
-----------------------------------------------------------------------------

-- with path so that ghc -M can find config.h
#include "../includes/config.h"

module Main (main) where

#include "HsVersions.h"

import CompManager
import Interpreter
#ifdef GHCI
import InteractiveUI
#endif
import DriverPipeline
import DriverState
import DriverFlags
import DriverMkDepend
import DriverUtil
import Panic
import DriverPhases	( Phase(..) )
import CmdLineOpts	( HscLang(..), DynFlags(..), v_Static_hsc_opts )
import Module		( mkModuleName )
import TmpFiles
import Finder		( initFinder )
import CmStaticInfo
import Config
import Util

import Concurrent
#ifndef mingw32_TARGET_OS
import Posix
#endif
import Directory
import IOExts
import Exception
import Dynamic

import IO
import Monad
import List
import System
import Maybe


-----------------------------------------------------------------------------
-- Changes:

-- * -fglasgow-exts NO LONGER IMPLIES -package lang!!!  (-fglasgow-exts is a
--   dynamic flag whereas -package is a static flag.)

-----------------------------------------------------------------------------
-- ToDo:

-- -nohi doesn't work
-- new mkdependHS doesn't support all the options that the old one did (-X et al.)
-- time commands when run with -v
-- split marker
-- mkDLL
-- java generation
-- user ways
-- Win32 support: proper signal handling
-- make sure OPTIONS in .hs file propogate to .hc file if -C or -keep-hc-file-too
-- reading the package configuration file is too slow
-- -H, -K, -Rghc-timing
-- hi-diffs

-----------------------------------------------------------------------------
-- Differences vs. old driver:

-- No more "Enter your Haskell program, end with ^D (on a line of its own):"
-- consistency checking removed (may do this properly later)
-- removed -noC
-- no hi diffs (could be added later)
-- no -Ofile

-----------------------------------------------------------------------------
-- Main loop

main =
  -- all error messages are propagated as exceptions
  handleDyn (\dyn -> case dyn of
			  PhaseFailed _phase code -> exitWith code
			  Interrupted -> exitWith (ExitFailure 1)
			  _ -> do hPutStrLn stderr (show (dyn :: BarfKind))
			          exitWith (ExitFailure 1)
	      ) $ do

   -- make sure we clean up after ourselves
   later (do  forget_it <- readIORef v_Keep_tmp_files
	      unless forget_it $ do
	      verb <- readIORef v_Verbose
	      cleanTempFiles verb
     ) $ do
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.

	-- install signal handlers
   main_thread <- myThreadId
#ifndef mingw32_TARGET_OS
   let sig_handler = Catch (throwTo main_thread 
				(DynException (toDyn Interrupted)))
   installHandler sigQUIT sig_handler Nothing 
   installHandler sigINT  sig_handler Nothing
#endif

   pgm    <- getProgName
   writeIORef v_Prog_name pgm

   argv   <- getArgs

	-- grab any -B options from the command line first
   argv'  <- setTopDir argv
   top_dir <- readIORef v_TopDir

   let installed s = top_dir ++ '/':s
       inplace s   = top_dir ++ '/':cCURRENT_DIR ++ '/':s

       installed_pkgconfig = installed ("package.conf")
       inplace_pkgconfig   = inplace (cGHC_DRIVER_DIR ++ "/package.conf.inplace")

	-- discover whether we're running in a build tree or in an installation,
	-- by looking for the package configuration file.
   am_installed <- doesFileExist installed_pkgconfig

   if am_installed
	then writeIORef v_Path_package_config installed_pkgconfig
	else do am_inplace <- doesFileExist inplace_pkgconfig
	        if am_inplace
		    then writeIORef v_Path_package_config inplace_pkgconfig
		    else throwDyn (OtherError "can't find package.conf")

	-- set the location of our various files
   if am_installed
	then do writeIORef v_Path_usage (installed "ghc-usage.txt")
		writeIORef v_Pgm_L (installed "unlit")
		writeIORef v_Pgm_m (installed "ghc-asm")
		writeIORef v_Pgm_s (installed "ghc-split")

	else do writeIORef v_Path_usage (inplace (cGHC_DRIVER_DIR ++ "/ghc-usage.txt"))
		writeIORef v_Pgm_L (inplace cGHC_UNLIT)
		writeIORef v_Pgm_m (inplace cGHC_MANGLER)
		writeIORef v_Pgm_s (inplace cGHC_SPLIT)

	-- read the package configuration
   conf_file <- readIORef v_Path_package_config
   contents <- readFile conf_file
   let pkg_details = read contents	-- ToDo: faster
   writeIORef v_Package_details pkg_details

	-- find the phase to stop after (i.e. -E, -C, -c, -S flags)
   (flags2, mode, stop_flag) <- getGhcMode argv'
   writeIORef v_GhcMode mode

	-- process all the other arguments, and get the source files
   non_static <- processArgs static_flags flags2 []

	-- find the build tag, and re-process the build-specific options
   more_opts <- findBuildTag
   way_non_static <- processArgs static_flags more_opts []

	-- give the static flags to hsc
   static_opts <- buildStaticHscOpts
   writeIORef v_Static_hsc_opts static_opts

	-- warnings
   warn_level <- readIORef v_Warning_opt

   let warn_opts =  case warn_level of
		  	W_default -> standardWarnings
		  	W_        -> minusWOpts
		  	W_all	  -> minusWallOpts
		  	W_not     -> []

	-- build the default DynFlags (these may be adjusted on a per
	-- module basis by OPTIONS pragmas and settings in the interpreter).

   core_todo <- buildCoreToDo
   stg_todo  <- buildStgToDo

   -- set the "global" HscLang.  The HscLang can be further adjusted on a module
   -- by module basis, using only the -fvia-C and -fasm flags.  If the global
   -- HscLang is not HscC or HscAsm, -fvia-C and -fasm have no effect.
   opt_level  <- readIORef v_OptLevel
   let lang = case mode of 
		 StopBefore HCc -> HscC
		 DoInteractive  -> HscInterpreted
		 _other        | opt_level >= 1  -> HscC  -- -O implies -fvia-C 
			       | otherwise       -> defaultHscLang

   writeIORef v_DynFlags 
	DynFlags{ coreToDo = core_todo,
		  stgToDo  = stg_todo,
                  hscLang  = lang,
		  -- leave out hscOutName for now
                  hscOutName = panic "Main.main:hscOutName not set",
		  flags = [] }

	-- the rest of the arguments are "dynamic"
   srcs <- processArgs dynamic_flags (way_non_static ++ 
					non_static ++ warn_opts) []
	-- save the "initial DynFlags" away
   init_dyn_flags <- readIORef v_DynFlags
   writeIORef v_InitDynFlags init_dyn_flags

    	-- complain about any unknown flags
   mapM unknownFlagErr [ f | f@('-':_) <- srcs ]

	-- get the -v flag
   verb <- readIORef v_Verbose

   when verb (do hPutStr stderr "Glasgow Haskell Compiler, Version "
 	         hPutStr stderr cProjectVersion
	         hPutStr stderr ", for Haskell 98, compiled by GHC version "
	         hPutStrLn stderr cBooterVersion)

   when verb (hPutStrLn stderr ("Using package config file: " ++ conf_file))

	-- initialise the finder
   initFinder pkg_details

	-- mkdependHS is special
   when (mode == DoMkDependHS) beginMkDependHS

	-- make/interactive require invoking the compilation manager
   if (mode == DoMake)        then beginMake pkg_details srcs        else do
   if (mode == DoInteractive) then beginInteractive pkg_details srcs else do

	-- for each source file, find which phases to run
   let lang = hscLang init_dyn_flags
   pipelines <- mapM (genPipeline mode stop_flag True lang) srcs
   let src_pipelines = zip srcs pipelines

	-- sanity checking
   o_file <- readIORef v_Output_file
   ohi    <- readIORef v_Output_hi
   if length srcs > 1 && (isJust ohi || (isJust o_file && mode /= DoLink))
	then throwDyn (UsageError "can't apply -o or -ohi options to multiple source files")
	else do

   if null srcs then throwDyn (UsageError "no input files") else do

	-- save the flag state, because this could be modified by OPTIONS 
	-- pragmas during the compilation, and we'll need to restore it
	-- before starting the next compilation.
   saved_driver_state <- readIORef v_Driver_state

   let compileFile (src, phases) = do
	  writeIORef v_Driver_state saved_driver_state
	  writeIORef v_DynFlags init_dyn_flags
	  r <- runPipeline phases src (mode==DoLink) True
	  return r

   o_files <- mapM compileFile src_pipelines

   when (mode == DoMkDependHS) endMkDependHS
   when (mode == DoLink) (doLink o_files)

	-- grab the last -B option on the command line, and
	-- set topDir to its value.
setTopDir :: [String] -> IO [String]
setTopDir args = do
  let (minusbs, others) = partition (prefixMatch "-B") args
  (case minusbs of
    []   -> writeIORef v_TopDir clibdir
    some -> writeIORef v_TopDir (drop 2 (last some)))
  return others

beginMake :: PackageConfigInfo -> [String] -> IO ()
beginMake pkg_details mods
  = do case mods of
	 []    -> throwDyn (UsageError "no input files")
	 [mod] -> do state <- cmInit pkg_details Batch
		     cmLoadModule state (mkModuleName mod)
		     return ()
	 _     -> throwDyn (UsageError "only one module allowed with --make")

#ifndef GHCI
beginInteractive = throwDyn (OtherError "not build for interactive use")
#else
beginInteractive pkg_details mods
  = do state <- cmInit pkg_details Interactive
       case mods of
	   []    -> return ()
	   [mod] -> do cmLoadModule state (mkModuleName mod); return ()
	   _     -> throwDyn (UsageError 
				"only one module allowed with --interactive")
       interactiveUI state
#endif
