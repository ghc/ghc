{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}
-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main (main) where

#include "HsVersions.h"

#ifdef GHCI
import InteractiveUI	( ghciWelcomeMsg, interactiveUI )
#endif


import MkIface		( showIface )
import CompManager	( cmInit, cmLoadModules, cmDepAnal )
import Config
import SysTools
import Packages		( dumpPackages, initPackages, haskell98PackageId,
			  PackageIdH(..) )
import DriverPipeline	( runPipeline, staticLink, doMkDLL )

import DriverMkDepend	( doMkDependHS )
import DriverPhases	( Phase(..), isStopLn, isSourceFilename, anyHsc )

import DynFlags
import StaticFlags	( parseStaticFlags, staticFlags, v_Ld_inputs )
import CmdLineParser
import BasicTypes	( failed )
import Util
import Panic

-- Standard Haskell libraries
import EXCEPTION	( throwDyn, Exception(..), 
			  AsyncException(StackOverflow) )

import IO
import Directory	( doesFileExist, doesDirectoryExist )
import System		( getArgs, exitWith, ExitCode(..) )
import Monad
import List
import Maybe

-----------------------------------------------------------------------------
-- ToDo:

-- time commands when run with -v
-- user ways
-- Win32 support: proper signal handling
-- reading the package configuration file is too slow
-- -K<size>

-----------------------------------------------------------------------------
-- Main loop

main =
  ---------------------------------------
  -- exception handlers

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

   ----------------------------------------
   -- command-line parsing
   argv0 <- getArgs

   -- 1. we grab the -B option if there is one
   let (minusB_args, argv1) = partition (prefixMatch "-B") argv0
   dflags0 <- initSysTools minusB_args defaultDynFlags

   -- 2. Parse the "mode" flags (--make, --interactive etc.)
   (cli_mode, argv2) <- parseModeFlags argv1

   -- 3. Parse the static flags
   argv3 <- parseStaticFlags argv2

   -- 4. Parse the dynamic flags
   dflags1 <- initDynFlags dflags0

   -- set the default HscTarget.  The HscTarget can be further
   -- adjusted on a module by module basis, using only the -fvia-C and
   -- -fasm flags.  If the default HscTarget is not HscC or HscAsm,
   -- -fvia-C and -fasm have no effect.
   let lang = case cli_mode of 
		 DoInteractive  -> HscInterpreted
		 DoEval _	-> HscInterpreted
		 _other		-> hscTarget dflags1

   let mode = case cli_mode of
		DoInteractive	-> Interactive
		DoEval _	-> Interactive
		DoMake		-> BatchCompile
		DoMkDependHS	-> MkDepend
		_		-> OneShot

   let dflags2 = dflags1{ ghcMode = mode,
                  	  hscTarget  = lang,
			  -- leave out hscOutName for now
	                  hscOutName = panic "Main.main:hscOutName not set",
		  	  verbosity = case cli_mode of
				 	 DoEval _ -> 0
				 	 _other   -> 1
			}

	-- The rest of the arguments are "dynamic"
	-- Leftover ones are presumably files
   (dflags3, fileish_args) <- parseDynamicFlags dflags2 argv3

	-- make sure we clean up after ourselves
   later (unless (dopt Opt_KeepTmpFiles dflags3) $ 
	    cleanTempFiles dflags3) $ do
	-- exceptions will be blocked while we clean the temporary files,
	-- so there shouldn't be any difficulty if we receive further
	-- signals.

	-- Display banner
   showBanner cli_mode dflags3

	-- Read the package config(s), and process the package-related
	-- command-line flags
   dflags <- initPackages dflags3

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
   mapM_ (consIORef v_Ld_inputs) (reverse objs)

	---------------- Display configuration -----------
   when (verbosity dflags >= 4) $
	dumpPackages dflags

   when (verbosity dflags >= 3) $ do
	hPutStrLn stderr ("Hsc static flags: " ++ unwords staticFlags)

	---------------- Final sanity checking -----------
   checkOptions cli_mode dflags srcs objs

	---------------- Do the business -----------
   case cli_mode of
	ShowUsage       -> showGhcUsage cli_mode
	PrintLibdir     -> do d <- getTopDir; putStrLn d
	ShowVersion     -> showVersion
        ShowNumVersion  -> putStrLn cProjectVersion
        ShowInterface f -> showIface f
	DoMake 	        -> doMake dflags srcs
	DoMkDependHS    -> doMkDependHS dflags srcs 
	StopBefore p    -> oneShot dflags p srcs
	DoInteractive   -> interactiveUI dflags srcs Nothing
	DoEval expr     -> interactiveUI dflags srcs (Just expr)

   exitWith ExitSuccess

#ifndef GHCI
interactiveUI _ _ _ = 
  throwDyn (CmdLineError "not built for interactive use")
#endif


-- -----------------------------------------------------------------------------
-- Option sanity checks

checkOptions :: CmdLineMode -> DynFlags -> [String] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions cli_mode dflags srcs objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | f@('-':_) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

	-- -prof and --interactive are not a good combination
   when (notNull (wayNames dflags)  && isInterpretiveMode cli_mode) $
      do throwDyn (UsageError 
                   "--interactive can't be used with -prof, -ticky, -unreg or -smp.")
	-- -ohi sanity check
   if (isJust (outputHi dflags) && 
      (isCompManagerMode cli_mode || srcs `lengthExceeds` 1))
	then throwDyn (UsageError "-ohi can only be used when compiling a single source file")
	else do

	-- -o sanity checking
   if (srcs `lengthExceeds` 1 && isJust (outputFile dflags)
	 && not (isLinkMode cli_mode))
	then throwDyn (UsageError "can't apply -o to multiple source files")
	else do

	-- Check that there are some input files
	-- (except in the interactive case)
   if null srcs && null objs && not (isInterpretiveMode cli_mode)
	then throwDyn (UsageError "no input files")
	else do

     -- Verify that output files point somewhere sensible.
   verifyOutputFiles dflags


-- Compiler output options

-- called to verify that the output files & directories
-- point somewhere valid. 
--
-- The assumption is that the directory portion of these output
-- options will have to exist by the time 'verifyOutputFiles'
-- is invoked.
-- 
verifyOutputFiles :: DynFlags -> IO ()
verifyOutputFiles dflags = do
  let odir = outputDir dflags
  when (isJust odir) $ do
     let dir = fromJust odir
     flg <- doesDirectoryExist dir
     when (not flg) (nonExistentDir "-odir" dir)
  let ofile = outputFile dflags
  when (isJust ofile) $ do
     let fn = fromJust ofile
     flg <- doesDirNameExist fn
     when (not flg) (nonExistentDir "-o" fn)
  let ohi = outputHi dflags
  when (isJust ohi) $ do
     let hi = fromJust ohi
     flg <- doesDirNameExist hi
     when (not flg) (nonExistentDir "-ohi" hi)
 where
   nonExistentDir flg dir = 
     throwDyn (CmdLineError ("error: directory portion of " ++ 
                             show dir ++ " does not exist (used with " ++ 
			     show flg ++ " option.)"))

-----------------------------------------------------------------------------
-- GHC modes of operation

data CmdLineMode
  = ShowUsage			-- ghc -?
  | PrintLibdir			-- ghc --print-libdir
  | ShowVersion			-- ghc -V/--version
  | ShowNumVersion		-- ghc --numeric-version
  | ShowInterface String	-- ghc --show-iface
  | DoMkDependHS		-- ghc -M
  | StopBefore Phase		-- ghc -E | -C | -S
				-- StopBefore StopLn is the default
  | DoMake			-- ghc --make
  | DoInteractive		-- ghc --interactive
  | DoEval String		-- ghc -e
  deriving (Show)

isInteractiveMode, isInterpretiveMode     :: CmdLineMode -> Bool
isLinkMode, isCompManagerMode :: CmdLineMode -> Bool

isInteractiveMode DoInteractive = True
isInteractiveMode _		= False

-- isInterpretiveMode: byte-code compiler involved
isInterpretiveMode DoInteractive = True
isInterpretiveMode (DoEval _)    = True
isInterpretiveMode _             = False

-- True if we are going to attempt to link in this mode.
-- (we might not actually link, depending on the GhcLink flag)
isLinkMode (StopBefore StopLn) = True
isLinkMode DoMake	       = True
isLinkMode _   		       = False

isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False


-- -----------------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [String] -> IO (CmdLineMode, [String])
parseModeFlags args = do
  let ((leftover, errs), (mode, _, flags)) = 
	 runCmdLine (processArgs mode_flags args) (StopBefore StopLn, "", []) 
  when (not (null errs)) $ do
    throwDyn (UsageError (unlines errs))
  return (mode, flags ++ leftover)

type ModeM a = CmdLineP (CmdLineMode, String, [String]) a
  -- mode flags sometimes give rise to new DynFlags (eg. -C, see below)
  -- so we collect the new ones and return them.

mode_flags :: [(String, OptKind (CmdLineP (CmdLineMode, String, [String])))]
mode_flags =
  [  ------- help / version ----------------------------------------------
     ( "?"    		 , PassFlag (setMode ShowUsage))
  ,  ( "-help"	 	 , PassFlag (setMode ShowUsage))
  ,  ( "-print-libdir"   , PassFlag (setMode PrintLibdir))
  ,  ( "V"	 	 , PassFlag (setMode ShowVersion))
  ,  ( "-version"	 , PassFlag (setMode ShowVersion))
  ,  ( "-numeric-version", PassFlag (setMode ShowNumVersion))

      ------- interfaces ----------------------------------------------------
  ,  ( "-show-iface"     , HasArg (\f -> setMode (ShowInterface f)
					  "--show-iface"))

      ------- primary modes ------------------------------------------------
  ,  ( "M"		, PassFlag (setMode DoMkDependHS))
  ,  ( "E"		, PassFlag (setMode (StopBefore anyHsc)))
  ,  ( "C"		, PassFlag (\f -> do setMode (StopBefore HCc) f
					     addFlag "-fvia-C"))
  ,  ( "S"		, PassFlag (setMode (StopBefore As)))
  ,  ( "-make"		, PassFlag (setMode DoMake))
  ,  ( "-interactive"	, PassFlag (setMode DoInteractive))
  ,  ( "e"              , HasArg   (\s -> setMode (DoEval s) "-e"))

	-- -fno-code says to stop after Hsc but don't generate any code.
  ,  ( "fno-code"	, PassFlag (\f -> do setMode (StopBefore HCc) f
					     addFlag "-fno-code"
					     addFlag "-no-recomp"))
  ]

setMode :: CmdLineMode -> String -> ModeM ()
setMode m flag = do
  (old_mode, old_flag, flags) <- getCmdLineState
  when (notNull old_flag && flag /= old_flag) $
      throwDyn (UsageError 
          ("cannot use `" ++ old_flag ++ "' with `" ++ flag ++ "'"))
  putCmdLineState (m, flag, flags)

addFlag :: String -> ModeM ()
addFlag s = do
  (m, f, flags) <- getCmdLineState
  putCmdLineState (m, f, s:flags)


-- -----------------------------------------------------------------------------
-- Compile files in one-shot mode.

oneShot :: DynFlags -> Phase -> [String] -> IO ()
oneShot dflags stop_phase srcs = do
	o_files <- compileFiles stop_phase dflags srcs 
	doLink dflags stop_phase o_files

compileFiles :: Phase
	     -> DynFlags
	     -> [String]	-- Source files
	     -> IO [String]	-- Object files
compileFiles stop_phase dflags srcs 
  = mapM (compileFile stop_phase dflags) srcs

compileFile :: Phase -> DynFlags -> FilePath -> IO FilePath
compileFile stop_phase dflags src = do
   exists <- doesFileExist src
   when (not exists) $ 
   	throwDyn (CmdLineError ("does not exist: " ++ src))
   
   let
	split    = dopt Opt_SplitObjs dflags
	o_file   = outputFile dflags
	ghc_link = ghcLink dflags	-- Set by -c or -no-link

	-- When linking, the -o argument refers to the linker's output.	
	-- otherwise, we use it as the name for the pipeline's output.
        maybe_o_file
	 | StopLn <- stop_phase, not (isNoLink ghc_link) = Nothing
		-- -o foo applies to linker
	 | otherwise = o_file
		-- -o foo applies to the file we are compiling now

        stop_phase' = case stop_phase of 
			As | split -> SplitAs
			other      -> stop_phase

   (_, out_file) <- runPipeline stop_phase' dflags
			 True maybe_o_file src Nothing{-no ModLocation-}
   return out_file


doLink :: DynFlags -> Phase -> [FilePath] -> IO ()
doLink dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()		-- We stopped before the linking phase

  | otherwise
  = case ghcLink dflags of
	NoLink     -> return ()
	StaticLink -> staticLink dflags o_files link_pkgs
	MkDLL      -> doMkDLL dflags o_files link_pkgs
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
    state  <- cmInit dflags
    graph  <- cmDepAnal state srcs
    (_, ok_flag, _) <- cmLoadModules state graph
    when (failed ok_flag) (exitWith (ExitFailure 1))
    return ()


-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanner :: CmdLineMode -> DynFlags -> IO ()
showBanner cli_mode dflags = do
   let verb = verbosity dflags
	-- Show the GHCi banner
#  ifdef GHCI
   when (isInteractiveMode cli_mode && verb >= 1) $
      hPutStrLn stdout ghciWelcomeMsg
#  endif

	-- Display details of the configuration in verbose mode
   when (not (isInteractiveMode cli_mode) && verb >= 2) $
	do hPutStr stderr "Glasgow Haskell Compiler, Version "
 	   hPutStr stderr cProjectVersion
	   hPutStr stderr ", for Haskell 98, compiled by GHC version "
	   hPutStrLn stderr cBooterVersion

showVersion :: IO ()
showVersion = do
  putStrLn (cProjectName ++ ", version " ++ cProjectVersion)
  exitWith ExitSuccess

showGhcUsage cli_mode = do 
  (ghc_usage_path,ghci_usage_path) <- getUsageMsgPaths
  let usage_path 
	| DoInteractive <- cli_mode = ghci_usage_path
	| otherwise		    = ghc_usage_path
  usage <- readFile usage_path
  dump usage
  exitWith ExitSuccess
  where
     dump ""	      = return ()
     dump ('$':'$':s) = hPutStr stderr progName >> dump s
     dump (c:s)	      = hPutChar stderr c >> dump s

-- -----------------------------------------------------------------------------
-- Util

unknownFlagsErr :: [String] -> a
unknownFlagsErr fs = throwDyn (UsageError ("unrecognised flags: " ++ unwords fs))
