{-# OPTIONS -fno-warn-incomplete-patterns #-}
-----------------------------------------------------------------------------
-- $Id: Main.hs,v 1.64 2001/05/08 10:58:48 simonmar Exp $
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


#ifdef GHCI
import InteractiveUI
#endif

#ifndef mingw32_TARGET_OS
import Dynamic
import Posix
#endif

import CompManager
import ParsePkgConf
import DriverPipeline
import DriverState
import DriverFlags
import DriverMkDepend
import DriverUtil
import Panic
import DriverPhases	( Phase(..), haskellish_src_file, objish_file )
import CmdLineOpts
import TmpFiles
import Finder		( initFinder )
import CmStaticInfo
import Config
import Outputable
import Util

import Concurrent
import Directory
import IOExts
import Exception

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
-- removed -noC
-- no -Ofile

-----------------------------------------------------------------------------
-- Main loop

main =
  -- top-level exception handler: any unrecognised exception is a compiler bug.
  handle (\exception -> do hPutStr stderr (show (Panic (show exception)))
			   exitWith (ExitFailure 1)
         ) $ do

  -- all error messages are propagated as exceptions
  handleDyn (\dyn -> case dyn of
			  PhaseFailed _phase code -> exitWith code
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
   main_thread <- myThreadId
#ifndef mingw32_TARGET_OS
   let sig_handler = Catch (throwTo main_thread 
				(DynException (toDyn Interrupted)))
   installHandler sigQUIT sig_handler Nothing 
   installHandler sigINT  sig_handler Nothing
#endif

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
		    else throwDyn (InstallationError 
			             ("Can't find package.conf in " ++ 
				      inplace_pkgconfig))

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
   r <- parsePkgConf conf_file
   case r of {
	Left err -> throwDyn (InstallationError (showSDoc err));
	Right pkg_details -> do

   writeIORef v_Package_details pkg_details

	-- find the phase to stop after (i.e. -E, -C, -c, -S flags)
   (flags2, mode, stop_flag) <- getGhcMode argv'
   writeIORef v_GhcMode mode

	-- Show the GHCi banner?
#  ifdef GHCI
   when (mode == DoInteractive) $
      hPutStrLn stdout ghciWelcomeMsg
#  endif

	-- process all the other arguments, and get the source files
   non_static <- processArgs static_flags flags2 []

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
   way_non_static <- processArgs static_flags (unreg_opts ++ way_opts) []

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
   opt_level  <- readIORef v_OptLevel


   let lang = case mode of 
		 StopBefore HCc -> HscC
		 DoInteractive  -> HscInterpreted
		 _other        | opt_level >= 1  -> HscC  -- -O implies -fvia-C 
			       | otherwise       -> defaultHscLang

   writeIORef v_DynFlags 
	defaultDynFlags{ coreToDo = core_todo,
		  	 stgToDo  = stg_todo,
                  	 hscLang  = lang,
		  	 -- leave out hscOutName for now
                  	 hscOutName = panic "Main.main:hscOutName not set",

		  	 verbosity = case mode of
					DoInteractive -> 1
					DoMake	      -> 1
					_other        -> 0,
			}

	-- the rest of the arguments are "dynamic"
   srcs <- processArgs dynamic_flags (way_non_static ++ non_static) []
	-- save the "initial DynFlags" away
   init_dyn_flags <- readIORef v_DynFlags
   writeIORef v_InitDynFlags init_dyn_flags

    	-- complain about any unknown flags
   mapM unknownFlagErr [ f | f@('-':_) <- srcs ]

   verb <- dynFlag verbosity

   when (verb >= 2) 
	(do hPutStr stderr "Glasgow Haskell Compiler, Version "
 	    hPutStr stderr cProjectVersion
	    hPutStr stderr ", for Haskell 98, compiled by GHC version "
	    hPutStrLn stderr cBooterVersion)

   when (verb >= 2) 
	(hPutStrLn stderr ("Using package config file: " ++ conf_file))

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
	(mode == DoMake || mode == DoInteractive || length srcs > 1))
	then throwDyn (UsageError "-ohi can only be used when compiling a single source file")
	else do

	-- make/interactive require invoking the compilation manager
   if (mode == DoMake)        then beginMake srcs        else do
   if (mode == DoInteractive) then beginInteractive srcs else do

	-- -o sanity checking
   o_file <- readIORef v_Output_file
   if (length srcs > 1 && isJust o_file && mode /= DoLink && mode /= DoMkDLL)
	then throwDyn (UsageError "can't apply -o to multiple source files")
	else do

   if null srcs then throwDyn (UsageError "no input files") else do

   let compileFile src = do
	  writeIORef v_DynFlags init_dyn_flags

	  exists <- doesFileExist src
          when (not exists) $ 
		throwDyn (CmdLineError ("file `" ++ src ++ "' does not exist"))

	  -- We compile in two stages, because the file may have an
	  -- OPTIONS pragma that affects the compilation pipeline (eg. -fvia-C)
	  let (basename, suffix) = splitFilename src

	  -- just preprocess (Haskell source only)
	  pp <- if not (haskellish_src_file src) || mode == StopBefore Hsc
			then return src else do
		phases <- genPipeline (StopBefore Hsc) stop_flag
			    False{-not persistent-} defaultHscLang src
	  	pipeLoop phases src False{-no linking-} False{-no -o flag-}
			basename suffix

	  -- rest of compilation
	  dyn_flags <- readIORef v_DynFlags
	  phases <- genPipeline mode stop_flag True (hscLang dyn_flags) pp
	  r <- pipeLoop phases pp (mode==DoLink || mode==DoMkDLL) True{-use -o flag-}
			basename suffix
	  return r

   o_files <- mapM compileFile srcs

   when (mode == DoMkDependHS) endMkDependHS
   when (mode == DoLink) (doLink o_files)
   when (mode == DoMkDLL) (doMkDLL o_files)
  }
	-- grab the last -B option on the command line, and
	-- set topDir to its value.
setTopDir :: [String] -> IO [String]
setTopDir args = do
  let (minusbs, others) = partition (prefixMatch "-B") args
  (case minusbs of
    []   -> throwDyn (InstallationError ("missing -B<dir> option"))
    some -> writeIORef v_TopDir (drop 2 (last some)))
  return others

beginMake :: [String] -> IO ()
beginMake fileish_args
  = do let (objs, mods) = partition objish_file fileish_args
       mapM (add v_Ld_inputs) objs

       case mods of
	 []    -> throwDyn (UsageError "no input files")
	 [mod] -> do state <- cmInit Batch
		     cmLoadModule state mod
		     return ()
	 _     -> throwDyn (UsageError "only one module allowed with --make")


beginInteractive :: [String] -> IO ()
#ifndef GHCI
beginInteractive = throwDyn (CmdLineError "not built for interactive use")
#else
beginInteractive fileish_args
  = do minus_ls <- readIORef v_Cmdline_libraries

       let (objs, mods) = partition objish_file fileish_args
	   libs = map Left objs ++ map Right minus_ls

       state <- cmInit Interactive
       case mods of
	  []    -> interactiveUI state Nothing    libs
	  [mod] -> interactiveUI state (Just mod) libs
	  _     -> throwDyn (UsageError 
                             "only one module allowed with --interactive")
#endif
