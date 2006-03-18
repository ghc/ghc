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

-- The official GHC API
import qualified GHC
import GHC		( Session, DynFlags(..), GhcMode(..), HscTarget(..),
			  LoadHowMuch(..), dopt, DynFlag(..) )
import CmdLineParser

-- Implementations of the various modes (--show-iface, mkdependHS. etc.)
import MkIface		( showIface )
import DriverPipeline	( oneShot, compileFile )
import DriverMkDepend	( doMkDependHS )
import SysTools		( getTopDir, getUsageMsgPaths )
#ifdef GHCI
import InteractiveUI	( ghciWelcomeMsg, interactiveUI )
#endif

-- Various other random stuff that we need
import Config		( cProjectVersion, cBooterVersion, cProjectName )
import Packages		( dumpPackages, initPackages )
import DriverPhases	( Phase(..), isSourceFilename, anyHsc,
			  startPhase, isHaskellSrcFilename )
import StaticFlags	( staticFlags, v_Ld_inputs, parseStaticFlags )
import DynFlags         ( defaultDynFlags )
import BasicTypes	( failed )
import ErrUtils		( Message, debugTraceMsg, putMsg )
import FastString	( getFastStringTable, isZEncoded, hasZEncoding )
import Outputable
import Util
import Panic

-- Standard Haskell libraries
import EXCEPTION	( throwDyn )
import IO
import Directory	( doesDirectoryExist )
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
-- GHC's command-line interface

main =
  GHC.defaultErrorHandler defaultDynFlags $ do
  
  argv0 <- getArgs
  argv1 <- parseStaticFlags =<< GHC.initFromArgs argv0

  -- 2. Parse the "mode" flags (--make, --interactive etc.)
  (cli_mode, argv2) <- parseModeFlags argv1

  let mode = case cli_mode of
		DoInteractive	-> Interactive
		DoEval _	-> Interactive
		DoMake		-> BatchCompile
		DoMkDependHS	-> MkDepend
		_		-> OneShot

  -- start our GHC session
  session <- GHC.newSession mode

  dflags0 <- GHC.getSessionDynFlags session

  -- set the default HscTarget.  The HscTarget can be further
  -- adjusted on a module by module basis, using only the -fvia-C and
  -- -fasm flags.  If the default HscTarget is not HscC or HscAsm,
  -- -fvia-C and -fasm have no effect.
  let lang = case cli_mode of 
		 DoInteractive  -> HscInterpreted
		 DoEval _	-> HscInterpreted
		 _other		-> hscTarget dflags0

  let dflags1 = dflags0{ ghcMode = mode,
                  	 hscTarget  = lang,
			 -- leave out hscOutName for now
	                 hscOutName = panic "Main.main:hscOutName not set",
		  	 verbosity = case cli_mode of
				 	 DoEval _ -> 0
				 	 _other   -> 1
			}

	-- The rest of the arguments are "dynamic"
	-- Leftover ones are presumably files
  (dflags2, fileish_args) <- GHC.parseDynamicFlags dflags1 argv2

	-- make sure we clean up after ourselves
  GHC.defaultCleanupHandler dflags2 $ do

	-- Display banner
  showBanner cli_mode dflags2

	-- Read the package config(s), and process the package-related
	-- command-line flags
  dflags <- initPackages dflags2

  -- we've finished manipulating the DynFlags, update the session
  GHC.setSessionDynFlags session dflags

  let
     -- To simplify the handling of filepaths, we normalise all filepaths right 
     -- away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    normal_fileish_paths = map normalisePath fileish_args
    (srcs, objs)         = partition_args normal_fileish_paths [] []

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
	DoMake 	        -> doMake session srcs
	DoMkDependHS    -> doMkDependHS session (map fst srcs)
	StopBefore p
            -- Stop after compiling Haskell if we aren't
            -- interested in any further results.
            | HscNothing <- hscTarget dflags
                        -> oneShot dflags HCc srcs
            | otherwise
                        -> oneShot dflags p srcs
	DoInteractive   -> interactiveUI session srcs Nothing
	DoEval expr     -> interactiveUI session srcs (Just expr)

  dumpFinalStats dflags
  exitWith ExitSuccess

#ifndef GHCI
interactiveUI _ _ _ = 
  throwDyn (CmdLineError "not built for interactive use")
#endif

-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff	= partition_args args srcs objs
  | StopLn <- phase	= partition_args args srcs (slurp ++ objs)
  | otherwise		= partition_args rest (these_srcs ++ srcs) objs
	where phase = startPhase suff
	      (slurp,rest) = break (== "-x") args 
	      these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

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

-- -----------------------------------------------------------------------------
-- Option sanity checks

checkOptions :: CmdLineMode -> DynFlags -> [(String,Maybe Phase)] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions cli_mode dflags srcs objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | (f@('-':_), _) <- srcs ]
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
   if null srcs && null objs && needsInputsMode cli_mode
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
  let odir = objectDir dflags
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

needsInputsMode DoMkDependHS	= True
needsInputsMode (StopBefore _)	= True
needsInputsMode DoMake		= True
needsInputsMode _		= False

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


-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: Session -> [(String,Maybe Phase)] -> IO ()
doMake sess []    = throwDyn (UsageError "no input files")
doMake sess srcs  = do 
    let (hs_srcs, non_hs_srcs) = partition haskellish srcs

	haskellish (f,Nothing) = 
	  looksLikeModuleName f || isHaskellSrcFilename f || '.' `notElem` f
	haskellish (f,Just phase) = 
	  phase `notElem` [As, Cc, CmmCpp, Cmm, StopLn]

    dflags <- GHC.getSessionDynFlags sess
    o_files <- mapM (compileFile dflags StopLn) non_hs_srcs
    mapM_ (consIORef v_Ld_inputs) (reverse o_files)

    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets sess targets
    ok_flag <- GHC.load sess LoadAllTargets
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
#ifdef GHCI
	   -- GHCI is only set when we are bootstrapping...
 	   hPutStrLn stderr cProjectVersion
#else
	   hPutStrLn stderr cBooterVersion
#endif

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
     dump ('$':'$':s) = putStr progName >> dump s
     dump (c:s)	      = putChar c >> dump s

dumpFinalStats :: DynFlags -> IO ()
dumpFinalStats dflags = 
  when (dopt Opt_D_faststring_stats dflags) $ dumpFastStringStats dflags

dumpFastStringStats :: DynFlags -> IO ()
dumpFastStringStats dflags = do
  buckets <- getFastStringTable
  let (entries, longest, is_z, has_z) = countFS 0 0 0 0 buckets
      msg = text "FastString stats:" $$
	    nest 4 (vcat [text "size:           " <+> int (length buckets),
			  text "entries:        " <+> int entries,
			  text "longest chain:  " <+> int longest,
			  text "z-encoded:      " <+> (is_z `pcntOf` entries),
			  text "has z-encoding: " <+> (has_z `pcntOf` entries)
			 ])
	-- we usually get more "has z-encoding" than "z-encoded", because
	-- when we z-encode a string it might hash to the exact same string,
	-- which will is not counted as "z-encoded".  Only strings whose
	-- Z-encoding is different from the original string are counted in
	-- the "z-encoded" total.
  putMsg dflags msg
  where
   x `pcntOf` y = int ((x * 100) `quot` y) <> char '%'
  
countFS entries longest is_z has_z [] = (entries, longest, is_z, has_z)
countFS entries longest is_z has_z (b:bs) = 
  let
	len = length b
	longest' = max len longest
	entries' = entries + len
	is_zs = length (filter isZEncoded b)
	has_zs = length (filter hasZEncoding b)
  in
	countFS entries' longest' (is_z + is_zs) (has_z + has_zs) bs

-- -----------------------------------------------------------------------------
-- Util

unknownFlagsErr :: [String] -> a
unknownFlagsErr fs = throwDyn (UsageError ("unrecognised flags: " ++ unwords fs))
