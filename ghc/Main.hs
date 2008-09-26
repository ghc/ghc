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
import GHC		( DynFlags(..), HscTarget(..),
                          GhcMode(..), GhcLink(..),
			  LoadHowMuch(..), dopt, DynFlag(..) )
import CmdLineParser

-- Implementations of the various modes (--show-iface, mkdependHS. etc.)
import LoadIface	( showIface )
import HscMain          ( newHscEnv )
import DriverPipeline	( oneShot, compileFile )
import DriverMkDepend	( doMkDependHS )
#ifdef GHCI
import InteractiveUI	( interactiveUI, ghciWelcomeMsg )
#endif

-- Various other random stuff that we need
import Config
import HscTypes
import Packages		( dumpPackages )
import DriverPhases	( Phase(..), isSourceFilename, anyHsc,
			  startPhase, isHaskellSrcFilename )
import BasicTypes       ( failed )
import StaticFlags
import StaticFlagParser
import DynFlags
import ErrUtils
import FastString
import Outputable
import SrcLoc
import Util
import Panic
import MonadUtils       ( liftIO )

-- Standard Haskell libraries
import System.IO
import System.Environment
import System.Exit
import System.FilePath
import Control.Monad
import Data.List
import Data.Maybe

-----------------------------------------------------------------------------
-- ToDo:

-- time commands when run with -v
-- user ways
-- Win32 support: proper signal handling
-- reading the package configuration file is too slow
-- -K<size>

-----------------------------------------------------------------------------
-- GHC's command-line interface

main :: IO ()
main =
  
  GHC.defaultErrorHandler defaultDynFlags $ do
  -- 1. extract the -B flag from the args
  argv0 <- getArgs

  let
        (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))

  let argv1' = map (mkGeneralLocated "on the commandline") argv1
  (argv2, staticFlagWarnings) <- parseStaticFlags argv1'

  -- 2. Parse the "mode" flags (--make, --interactive etc.)
  (m_uber_mode, cli_mode, argv3, modeFlagWarnings) <- parseModeFlags argv2

  -- If all we want to do is to show the version number then do it
  -- now, before we start a GHC session etc.
  -- If we do it later then bootstrapping gets confused as it tries
  -- to find out what version of GHC it's using before package.conf
  -- exists, so starting the session fails.
  case m_uber_mode of
    -- ShowUsage currently has to be handled specially, as it needs to
    -- actually start up GHC so that it can find the usage.txt files
    -- in the libdir. It would be nice to embed the text in the
    -- executable so that we don't have to do that, and things are more
    -- uniform here.
    Just ShowUsage -> return ()
    Just um ->
        do case um of
               ShowInfo                -> showInfo
               ShowSupportedLanguages  -> showSupportedLanguages
               ShowVersion             -> showVersion
               ShowNumVersion          -> putStrLn cProjectVersion
           exitWith ExitSuccess
    Nothing -> return ()

  -- start our GHC session
  GHC.runGhc mbMinusB $ do

  dflags0 <- GHC.getSessionDynFlags

  -- set the default GhcMode, HscTarget and GhcLink.  The HscTarget
  -- can be further adjusted on a module by module basis, using only
  -- the -fvia-C and -fasm flags.  If the default HscTarget is not
  -- HscC or HscAsm, -fvia-C and -fasm have no effect.
  let dflt_target = hscTarget dflags0
      (mode, lang, link)
         = case cli_mode of
        	DoInteractive	-> (CompManager, HscInterpreted, LinkInMemory)
        	DoEval _	-> (CompManager, HscInterpreted, LinkInMemory)
        	DoMake		-> (CompManager, dflt_target,    LinkBinary)
        	DoMkDependHS	-> (MkDepend,    dflt_target,    LinkBinary)
        	_		-> (OneShot,     dflt_target,    LinkBinary)

  let dflags1 = dflags0{ ghcMode   = mode,
                  	 hscTarget = lang,
                         ghcLink   = link,
        		 -- leave out hscOutName for now
                         hscOutName = panic "Main.main:hscOutName not set",
        	  	 verbosity = case cli_mode of
        			 	 DoEval _ -> 0
        			 	 _other   -> 1
        		}

      -- turn on -fimplicit-import-qualified for GHCi now, so that it
      -- can be overriden from the command-line
      dflags1a | DoInteractive <- cli_mode = imp_qual_enabled
               | DoEval _      <- cli_mode = imp_qual_enabled
               | otherwise                 = dflags1
        where imp_qual_enabled = dflags1 `dopt_set` Opt_ImplicitImportQualified

        -- The rest of the arguments are "dynamic"
        -- Leftover ones are presumably files
  (dflags2, fileish_args, dynamicFlagWarnings) <- GHC.parseDynamicFlags dflags1a argv3

  -- As noted earlier, currently we hvae to handle ShowUsage down here
  case m_uber_mode of
      Just ShowUsage -> liftIO $ showGhcUsage dflags2 cli_mode
      _              -> return ()

  let flagWarnings = staticFlagWarnings
                  ++ modeFlagWarnings
                  ++ dynamicFlagWarnings
  liftIO $ handleFlagWarnings dflags2 flagWarnings

        -- make sure we clean up after ourselves
  GHC.defaultCleanupHandler dflags2 $ do

  liftIO $ showBanner cli_mode dflags2

  -- we've finished manipulating the DynFlags, update the session
  GHC.setSessionDynFlags dflags2
  dflags3 <- GHC.getSessionDynFlags
  hsc_env <- GHC.getSession

  let
     -- To simplify the handling of filepaths, we normalise all filepaths right 
     -- away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    normal_fileish_paths = map (normalise . unLoc) fileish_args
    (srcs, objs)         = partition_args normal_fileish_paths [] []

  -- Note: have v_Ld_inputs maintain the order in which 'objs' occurred on 
  --       the command-line.
  liftIO $ mapM_ (consIORef v_Ld_inputs) (reverse objs)

        ---------------- Display configuration -----------
  when (verbosity dflags3 >= 4) $
        liftIO $ dumpPackages dflags3

  when (verbosity dflags3 >= 3) $ do
        liftIO $ hPutStrLn stderr ("Hsc static flags: " ++ unwords staticFlags)

        ---------------- Final sanity checking -----------
  liftIO $ checkOptions cli_mode dflags3 srcs objs

  ---------------- Do the business -----------
  handleSourceError (\e -> do
       GHC.printExceptionAndWarnings e
       liftIO $ exitWith (ExitFailure 1)) $ do
    case cli_mode of
       PrintLibdir            -> liftIO $ putStrLn (topDir dflags3)
       ShowInterface f        -> liftIO $ doShowIface dflags3 f
       DoMake                 -> doMake srcs
       DoMkDependHS           -> doMkDependHS (map fst srcs)
       StopBefore p           -> oneShot hsc_env p srcs >> GHC.printWarnings
       DoInteractive          -> interactiveUI srcs Nothing
       DoEval exprs           -> interactiveUI srcs $ Just $ reverse exprs

  liftIO $ dumpFinalStats dflags3
  liftIO $ exitWith ExitSuccess

#ifndef GHCI
interactiveUI :: b -> c -> Ghc ()
interactiveUI _ _ =
  ghcError (CmdLineError "not built for interactive use")
#endif

-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
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
looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m 
		      || looksLikeModuleName m
		      || '.' `notElem` m

-- -----------------------------------------------------------------------------
-- Option sanity checks

-- | Ensure sanity of options.
--
-- Throws 'UsageError' or 'CmdLineError' if not.
checkOptions :: CmdLineMode -> DynFlags -> [(String,Maybe Phase)] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions cli_mode dflags srcs objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | (f@('-':_), _) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

   when (notNull (filter isRTSWay (wayNames dflags))
         && isInterpretiveMode cli_mode) $
        hPutStrLn stderr ("Warning: -debug, -threaded and -ticky are ignored by GHCi")

	-- -prof and --interactive are not a good combination
   when (notNull (filter (not . isRTSWay) (wayNames dflags))
         && isInterpretiveMode cli_mode) $
      do ghcError (UsageError 
                   "--interactive can't be used with -prof or -unreg.")
	-- -ohi sanity check
   if (isJust (outputHi dflags) && 
      (isCompManagerMode cli_mode || srcs `lengthExceeds` 1))
	then ghcError (UsageError "-ohi can only be used when compiling a single source file")
	else do

	-- -o sanity checking
   if (srcs `lengthExceeds` 1 && isJust (outputFile dflags)
	 && not (isLinkMode cli_mode))
	then ghcError (UsageError "can't apply -o to multiple source files")
	else do

   let not_linking = not (isLinkMode cli_mode) || isNoLink (ghcLink dflags)

   when (not_linking && not (null objs)) $
        hPutStrLn stderr ("Warning: the following files would be used as linker inputs, but linking is not being done: " ++ unwords objs)

	-- Check that there are some input files
	-- (except in the interactive case)
   if null srcs && (null objs || not_linking) && needsInputsMode cli_mode
	then ghcError (UsageError "no input files")
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
  -- not -odir: we create the directory for -odir if it doesn't exist (#2278).
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
     ghcError (CmdLineError ("error: directory portion of " ++ 
                             show dir ++ " does not exist (used with " ++ 
			     show flg ++ " option.)"))

-----------------------------------------------------------------------------
-- GHC modes of operation

data UberMode
  = ShowUsage               -- ghc -?
  | ShowVersion             -- ghc -V/--version
  | ShowNumVersion          -- ghc --numeric-version
  | ShowSupportedLanguages  -- ghc --supported-languages
  | ShowInfo                -- ghc --info
  deriving (Show)

data CmdLineMode
  = PrintLibdir             -- ghc --print-libdir
  | ShowInterface String    -- ghc --show-iface
  | DoMkDependHS            -- ghc -M
  | StopBefore Phase        -- ghc -E | -C | -S
                            -- StopBefore StopLn is the default
  | DoMake                  -- ghc --make
  | DoInteractive           -- ghc --interactive
  | DoEval [String]         -- ghc -e foo -e bar => DoEval ["bar", "foo"]
  deriving (Show)

#ifdef GHCI
isInteractiveMode :: CmdLineMode -> Bool
isInteractiveMode DoInteractive = True
isInteractiveMode _		= False
#endif

-- isInterpretiveMode: byte-code compiler involved
isInterpretiveMode :: CmdLineMode -> Bool
isInterpretiveMode DoInteractive = True
isInterpretiveMode (DoEval _)    = True
isInterpretiveMode _             = False

needsInputsMode :: CmdLineMode -> Bool
needsInputsMode DoMkDependHS	= True
needsInputsMode (StopBefore _)	= True
needsInputsMode DoMake		= True
needsInputsMode _		= False

-- True if we are going to attempt to link in this mode.
-- (we might not actually link, depending on the GhcLink flag)
isLinkMode :: CmdLineMode -> Bool
isLinkMode (StopBefore StopLn) = True
isLinkMode DoMake	       = True
isLinkMode DoInteractive       = True
isLinkMode (DoEval _)          = True
isLinkMode _   		       = False

isCompManagerMode :: CmdLineMode -> Bool
isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False


-- -----------------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> IO (Maybe UberMode,
                      CmdLineMode,
                      [Located String],
                      [Located String])
parseModeFlags args = do
  let ((leftover, errs, warns), (mUberMode, mode, _, flags')) =
          runCmdLine (processArgs mode_flags args)
                     (Nothing, StopBefore StopLn, "", [])
  when (not (null errs)) $ ghcError $ errorsToGhcException errs
  return (mUberMode, mode, flags' ++ leftover, warns)

type ModeM = CmdLineP (Maybe UberMode, CmdLineMode, String, [Located String])
  -- mode flags sometimes give rise to new DynFlags (eg. -C, see below)
  -- so we collect the new ones and return them.

mode_flags :: [Flag ModeM]
mode_flags =
  [  ------- help / version ----------------------------------------------
    Flag "?"                    (NoArg (setUberMode ShowUsage))
         Supported
  , Flag "-help"                (NoArg (setUberMode ShowUsage))
         Supported
  , Flag "V"                    (NoArg (setUberMode ShowVersion))
         Supported
  , Flag "-version"             (NoArg (setUberMode ShowVersion))
         Supported
  , Flag "-numeric-version"     (NoArg (setUberMode ShowNumVersion))
         Supported
  , Flag "-info"                (NoArg (setUberMode ShowInfo))
         Supported
  , Flag "-supported-languages" (NoArg (setUberMode ShowSupportedLanguages))
         Supported
  , Flag "-print-libdir"        (PassFlag (setMode PrintLibdir))
         Supported

      ------- interfaces ----------------------------------------------------
  , Flag "-show-iface"  (HasArg (\f -> setMode (ShowInterface f)
                                               "--show-iface"))
         Supported

      ------- primary modes ------------------------------------------------
  , Flag "M"            (PassFlag (setMode DoMkDependHS))
         Supported
  , Flag "E"            (PassFlag (setMode (StopBefore anyHsc)))
         Supported
  , Flag "C"            (PassFlag (\f -> do setMode (StopBefore HCc) f
                                            addFlag "-fvia-C"))
         Supported
  , Flag "S"            (PassFlag (setMode (StopBefore As)))
         Supported
  , Flag "-make"        (PassFlag (setMode DoMake))
         Supported
  , Flag "-interactive" (PassFlag (setMode DoInteractive))
         Supported
  , Flag "e"            (HasArg   (\s -> updateMode (updateDoEval s) "-e"))
         Supported

       -- -fno-code says to stop after Hsc but don't generate any code.
  , Flag "fno-code"     (PassFlag (\f -> do setMode (StopBefore HCc) f
                                            addFlag "-fno-code"
                                            addFlag "-fforce-recomp"))
         Supported
  ]

setUberMode :: UberMode -> ModeM ()
setUberMode m = do
    (_, cmdLineMode, flag, flags') <- getCmdLineState
    putCmdLineState (Just m, cmdLineMode, flag, flags')

setMode :: CmdLineMode -> String -> ModeM ()
setMode m flag = updateMode (\_ -> m) flag

updateDoEval :: String -> CmdLineMode -> CmdLineMode
updateDoEval expr (DoEval exprs) = DoEval (expr : exprs)
updateDoEval expr _              = DoEval [expr]

updateMode :: (CmdLineMode -> CmdLineMode) -> String -> ModeM ()
updateMode f flag = do
  (m_uber_mode, old_mode, old_flag, flags') <- getCmdLineState
  if null old_flag || flag == old_flag
      then putCmdLineState (m_uber_mode, f old_mode, flag, flags')
      else ghcError (UsageError
               ("cannot use `" ++ old_flag ++ "' with `" ++ flag ++ "'"))

addFlag :: String -> ModeM ()
addFlag s = do
  (u, m, f, flags') <- getCmdLineState
  -- XXX Can we get a useful Loc?
  putCmdLineState (u, m, f, mkGeneralLocated "addFlag" s : flags')


-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: [(String,Maybe Phase)] -> Ghc ()
doMake []    = ghcError (UsageError "no input files")
doMake srcs  = do
    let (hs_srcs, non_hs_srcs) = partition haskellish srcs

	haskellish (f,Nothing) = 
	  looksLikeModuleName f || isHaskellSrcFilename f || '.' `notElem` f
	haskellish (_,Just phase) = 
	  phase `notElem` [As, Cc, CmmCpp, Cmm, StopLn]

    hsc_env <- GHC.getSession
    o_files <- mapM (\x -> do
                        f <- compileFile hsc_env StopLn x
                        GHC.printWarnings
                        return f)
                 non_hs_srcs
    liftIO $ mapM_ (consIORef v_Ld_inputs) (reverse o_files)

    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load LoadAllTargets

    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    return ()


-- ---------------------------------------------------------------------------
-- --show-iface mode

doShowIface :: DynFlags -> FilePath -> IO ()
doShowIface dflags file = do
  hsc_env <- newHscEnv dflags
  showIface hsc_env file

-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanner :: CmdLineMode -> DynFlags -> IO ()
showBanner _cli_mode dflags = do
   let verb = verbosity dflags

#ifdef GHCI
   -- Show the GHCi banner
   when (isInteractiveMode _cli_mode && verb >= 1) $ putStrLn ghciWelcomeMsg
#endif

   -- Display details of the configuration in verbose mode
   when (verb >= 2) $
    do hPutStr stderr "Glasgow Haskell Compiler, Version "
       hPutStr stderr cProjectVersion
       hPutStr stderr ", for Haskell 98, stage "
       hPutStr stderr cStage
       hPutStr stderr " booted by GHC version "
       hPutStrLn stderr cBooterVersion

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: IO ()
showInfo = do
    let sq x = " [" ++ x ++ "\n ]"
    putStrLn $ sq $ concat $ intersperse "\n ," $ map show compilerInfo
    exitWith ExitSuccess

showSupportedLanguages :: IO ()
showSupportedLanguages = do mapM_ putStrLn supportedLanguages
                            exitWith ExitSuccess

showVersion :: IO ()
showVersion = do
  putStrLn (cProjectName ++ ", version " ++ cProjectVersion)
  exitWith ExitSuccess

showGhcUsage :: DynFlags -> CmdLineMode -> IO ()
showGhcUsage dflags cli_mode = do 
  let usage_path 
	| DoInteractive <- cli_mode = ghciUsagePath dflags
	| otherwise		    = ghcUsagePath dflags
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

countFS :: Int -> Int -> Int -> Int -> [[FastString]] -> (Int, Int, Int, Int)
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
unknownFlagsErr fs = ghcError (UsageError ("unrecognised flags: " ++ unwords fs))
