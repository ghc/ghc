{-# LANGUAGE CPP, NondecreasingIndentation, TupleSections, LambdaCase #-}
-- GHC frontend ( ghc/Main.hs ) adapted for GHCJS
#undef GHCI
#undef HAVE_INTERNAL_INTERPRETER

{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main (main) where

import qualified Compiler.GhcjsProgram  as Ghcjs
import qualified Compiler.GhcjsPlatform as Ghcjs
import qualified Compiler.GhcjsHooks    as Ghcjs
import qualified Compiler.Info          as Ghcjs
import qualified Compiler.Settings      as Ghcjs
import qualified Compiler.Utils         as Ghcjs
import           Data.IORef
import Prelude

-- The official GHC API
import qualified GHC
import GHC              ( -- DynFlags(..), HscTarget(..),
                          -- GhcMode(..), GhcLink(..),
                          Ghc, GhcMonad(..),
                          LoadHowMuch(..) )
import CmdLineParser

-- Implementations of the various modes (--show-iface, mkdependHS. etc.)
import LoadIface        ( showIface )
import HscMain          ( newHscEnv )
import DriverPipeline   ( oneShot, compileFile )
import DriverMkDepend   ( doMkDependHS )
#if defined(HAVE_INTERNAL_INTERPRETER)
import GHCi.UI          ( interactiveUI, ghciWelcomeMsg, defaultGhciSettings )
#endif

-- Frontend plugins
import DynamicLoading   ( loadFrontendPlugin )
import Plugins
#if defined(HAVE_INTERNAL_INTERPRETER)
import DynamicLoading   ( initializePlugins )
#endif
import Module           ( ModuleName )


-- Various other random stuff that we need
import GHC.HandleEncoding
import GHC.Platform
import GHC.Platform.Host
import Config
import Constants
import HscTypes
import Packages         ( pprPackages, pprPackagesSimple )
import DriverPhases
import BasicTypes       ( failed )
import DynFlags hiding (WarnReason(..))
import ErrUtils
import FastString
import Outputable
import SysTools.BaseDir
import SrcLoc
import Util
import Panic
import UniqSupply
import MonadUtils       ( liftIO )
import SysTools.Settings

-- Imports for --abi-hash
import LoadIface           ( loadUserInterface )
import Module              ( mkModuleName )
import Finder              ( findImportedModule, cannotFindModule )
import TcRnMonad           ( initIfaceCheck )
import Binary              ( openBinMem, put_ )
import BinFingerprint      ( fingerprintBinMem )

-- Standard Haskell libraries
import System.IO
import System.Exit
import System.FilePath
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Char
import Data.List ( isPrefixOf, partition, intercalate )
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
main = do
   initGCStatistics -- See Note [-Bsymbolic and hooks]
   hSetBuffering stdout LineBuffering
   hSetBuffering stderr LineBuffering

   configureHandleEncoding

   Ghcjs.ghcjsErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- 1. extract the -B flag from the args
    (argv0, _booting, booting_stage1) <- Ghcjs.getWrappedArgs

    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))
        argv1' = map (mkGeneralLocated "on the commandline") argv1
    when (any (== "--run") argv1) (Ghcjs.runJsProgram mbMinusB argv1)

    (argv2, ghcjsSettings) <- Ghcjs.getGhcjsSettings argv1'

    -- fall back to native GHC if we're booting (we can't build Setup.hs with GHCJS yet)
    when (booting_stage1 && Ghcjs.gsBuildRunner ghcjsSettings)
      Ghcjs.bootstrapFallback

    -- 2. Parse the "mode" flags (--make, --interactive etc.)
    (mode, argv3, flagWarnings) <- parseModeFlags argv2

    -- If all we want to do is something like showing the version number
    -- then do it now, before we start a GHC session etc. This makes
    -- getting basic information much more resilient.

    -- In particular, if we wait until later before giving the version
    -- number then bootstrapping gets confused, as it tries to find out
    -- what version of GHC it's using before package.conf exists, so
    -- starting the session fails.
    case mode of
        Left preStartupMode ->
            do case preStartupMode of
                   ShowSupportedExtensions   -> showSupportedExtensions mbMinusB
                   ShowVersion               -> Ghcjs.printVersion
                   ShowNumVersion            -> Ghcjs.printNumericVersion
                   ShowNumGhcVersion         -> putStrLn cProjectVersion
                   ShowOptions isInteractive -> showOptions isInteractive

        Right postStartupMode -> do

            -- start our GHC session
            GHC.runGhc mbMinusB $ do

            dflags <- GHC.getSessionDynFlags

            case postStartupMode of
                Left preLoadMode ->
                    liftIO $ do
                        case preLoadMode of
                            ShowInfo               -> showInfo ghcjsSettings dflags
                            ShowGhcUsage           -> showGhcUsage  dflags
                            ShowGhciUsage          -> showGhciUsage dflags
                            PrintWithDynFlags f    -> putStrLn (f ghcjsSettings dflags)
                Right postLoadMode ->
                    main' postLoadMode dflags argv3 flagWarnings ghcjsSettings False

main' :: PostLoadMode -> DynFlags -> [Located String] -> [Warn]
      -> Ghcjs.GhcjsSettings
      -> Bool
      -> Ghc ()
main' postLoadMode dflags0 args flagWarnings ghcjsSettings native = do
  -- set the default GhcMode, HscTarget and GhcLink.  The HscTarget
  -- can be further adjusted on a module by module basis, using only
  -- the -fvia-C and -fasm flags.  If the default HscTarget is not
  -- HscC or HscAsm, -fvia-C and -fasm have no effect.
  let dflt_target = hscTarget dflags0
      (mode, lang, link)
         = case postLoadMode of
               DoInteractive   -> (CompManager, HscInterpreted, LinkInMemory)
               DoEval _        -> (CompManager, HscInterpreted, LinkInMemory)
               DoMake          -> (CompManager, dflt_target,    LinkBinary)
               DoMkDependHS    -> (MkDepend,    dflt_target,    LinkBinary)
               DoAbiHash       -> (OneShot,     dflt_target,    LinkBinary)
               _               -> (OneShot,     dflt_target,    LinkBinary)
  let dflags1 = case lang of
                HscInterpreted ->
                    let platform = targetPlatform dflags0
                        dflags0a = updateWays $ dflags0 { ways = interpWays }
                        dflags0b = foldl gopt_set dflags0a
                                 $ concatMap (wayGeneralFlags platform)
                                             interpWays
                        dflags0c = foldl gopt_unset dflags0b
                                 $ concatMap (wayUnsetGeneralFlags platform)
                                             interpWays
                    in dflags0c
                _ ->
                    dflags0
      dflags1a = dflags1{ ghcMode   = mode,
                         hscTarget = lang,
                         ghcLink   = link,
                         verbosity = case postLoadMode of
                                         DoEval _ -> 0
                                         _other   -> 1
                        }

      -- turn on -fimplicit-import-qualified for GHCi now, so that it
      -- can be overriden from the command-line
      -- XXX: this should really be in the interactive DynFlags, but
      -- we don't set that until later in interactiveUI
      -- We also set -fignore-optim-changes and -fignore-hpc-changes,
      -- which are program-level options. Again, this doesn't really
      -- feel like the right place to handle this, but we don't have
      -- a great story for the moment.
      dflags2  | DoInteractive <- postLoadMode = def_ghci_flags
               | DoEval _      <- postLoadMode = def_ghci_flags
               | otherwise                     = dflags1a
        where def_ghci_flags = dflags1 `gopt_set` Opt_ImplicitImportQualified
                                       `gopt_set` Opt_IgnoreOptimChanges
                                       `gopt_set` Opt_IgnoreHpcChanges

        -- The rest of the arguments are "dynamic"
        -- Leftover ones are presumably files
  (dflags3, fileish_args, dynamicFlagWarnings) <-
      GHC.parseDynamicFlags dflags2 args

  let dflags4 = case lang of
                HscInterpreted | not (gopt Opt_ExternalInterpreter dflags3) ->
                    let platform = targetPlatform dflags3
                        dflags3a = updateWays $ dflags3 { ways = interpWays }
                        dflags3b = foldl gopt_set dflags3a
                                 $ concatMap (wayGeneralFlags platform)
                                             interpWays
                        dflags3c = foldl gopt_unset dflags3b
                                 $ concatMap (wayUnsetGeneralFlags platform)
                                             interpWays
                    in dflags3c
                _ ->
                    dflags3

  GHC.prettyPrintGhcErrors dflags4 $ do

  let flagWarnings' = flagWarnings ++ dynamicFlagWarnings

  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
         liftIO $ handleFlagWarnings dflags4 flagWarnings'

  jsEnv <- liftIO Ghcjs.newGhcjsEnv

        -- make sure we clean up after ourselves
  Ghcjs.ghcjsCleanupHandler dflags4 jsEnv $ do

  liftIO $ showBanner postLoadMode dflags4

  let
     -- To simplify the handling of filepaths, we normalise all filepaths right
     -- away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    normal_fileish_paths = map (normalise . unLoc) fileish_args
    (srcs, js_objs, objs)         = partition_args_js normal_fileish_paths [] []


  -- add GHCJS configuration
  let baseDir = Ghcjs.getLibDir dflags4
  dflags4b <- if native
                then return (Ghcjs.setNativePlatform jsEnv ghcjsSettings baseDir dflags4)
                else return $
                       Ghcjs.setGhcjsPlatform ghcjsSettings jsEnv js_objs baseDir $
                       -- updateWays $ addWay' (WayCustom "js") $
                       -- Ghcjs.setGhcjsSuffixes {- oneshot -} False
                       dflags4 -- fixme value of oneshot?

  let dflags5 = dflags4b { ldInputs = map (FileOption "") objs
                                   ++ ldInputs dflags4b }
  -- we've finished manipulating the DynFlags, update the session
  _ <- GHC.setSessionDynFlags dflags5
  dflags6 <- GHC.getSessionDynFlags
  hsc_env <- GHC.getSession

  liftIO (writeIORef (canGenerateDynamicToo dflags6) True) -- fixme is this still necessary for GHCJS?

        ---------------- Display configuration -----------
  case verbosity dflags6 of
    v | v == 4 -> liftIO $ dumpPackagesSimple dflags6
      | v >= 5 -> liftIO $ dumpPackages dflags6
      | otherwise -> return ()

  liftIO $ initUniqSupply (initialUnique dflags6) (uniqueIncrement dflags6)
        ---------------- Final sanity checking -----------
  liftIO $ checkOptions ghcjsSettings postLoadMode dflags6 srcs objs (js_objs ++ Ghcjs.gsJsLibSrcs ghcjsSettings)

  ---------------- Do the business -----------
  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
    case postLoadMode of
       ShowInterface f        -> liftIO (doShowIface dflags6 f)
       DoMake                 -> doMake jsEnv ghcjsSettings native srcs
       DoMkDependHS           -> doMkDependHS (map fst srcs)
       StopBefore p           -> liftIO (Ghcjs.ghcjsOneShot jsEnv ghcjsSettings native hsc_env p srcs)
       DoInteractive          -> ghciUI srcs Nothing
       DoEval exprs           -> (ghciUI srcs $ Just $ reverse exprs)
       DoAbiHash              -> abiHash (map fst srcs)
       ShowPackages           -> liftIO $ showPackages dflags6
       DoGenerateLib          -> Ghcjs.generateLib ghcjsSettings
       DoPrintRts             -> liftIO (Ghcjs.printRts dflags6)
       DoInstallExecutable    -> liftIO (Ghcjs.installExecutable dflags6 ghcjsSettings normal_fileish_paths)
       DoPrintObj obj         -> liftIO (Ghcjs.printObj obj)
       DoPrintDeps obj        -> liftIO (Ghcjs.printDeps obj)
       DoBuildJsLibrary       -> liftIO (Ghcjs.buildJsLibrary dflags6 (map fst srcs) js_objs objs)

  liftIO $ dumpFinalStats dflags6
  return ()

ghciUI :: [(FilePath, Maybe Phase)] -> Maybe [String] -> Ghc ()
#if !defined(HAVE_INTERNAL_INTERPRETER)
ghciUI _ _ = throwGhcException (CmdLineError "not built for interactive use")
#else
ghciUI     = interactiveUI defaultGhciSettings
#endif

-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

partition_args_js :: [String] -> [(String, Maybe Phase)] -> [String]
                  -> ([(String, Maybe Phase)], [String], [String])
partition_args_js args srcs objs =
  let (srcs', objs') = partition_args args srcs objs
      (js_objs, nonjs_objs) = partition Ghcjs.isJsFile objs'
  in (srcs', js_objs, nonjs_objs)

partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)

       - and finally we consider everything not containing a '.' to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || '.' `notElem` m

-- -----------------------------------------------------------------------------
-- Option sanity checks

-- | Ensure sanity of options.
--
-- Throws 'UsageError' or 'CmdLineError' if not.
checkOptions :: Ghcjs.GhcjsSettings -> PostLoadMode -> DynFlags -> [(String,Maybe Phase)] -> [String] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions settings mode dflags srcs objs js_objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | (f@('-':_), _) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

   when (notNull (filter wayRTSOnly (ways dflags))
         && isInterpretiveMode mode) $
        hPutStrLn stderr ("Warning: -debug, -threaded and -ticky are ignored by GHCi")

   when (isInterpretiveMode mode) $
      do throwGhcException (UsageError
                   "--interactive is not yet supported.")

        -- -prof and --interactive are not a good combination
   when ((filter (not . wayRTSOnly) (ways dflags) /= interpWays)
         && isInterpretiveMode mode) $
      do throwGhcException (UsageError
                   "--interactive can't be used with -prof or -static.")
        -- -ohi sanity check
   if (isJust (outputHi dflags) &&
      (isCompManagerMode mode || srcs `lengthExceeds` 1))
        then throwGhcException (UsageError "-ohi can only be used when compiling a single source file")
        else do

        -- -o sanity checking
   if (srcs `lengthExceeds` 1 && isJust (outputFile dflags)
         && not (isLinkMode mode))
        then throwGhcException (UsageError "can't apply -o to multiple source files")
        else do

   let not_linking = not (isLinkMode mode) || isNoLink (ghcLink dflags)

   when (not_linking && not (null objs)) $
        hPutStrLn stderr ("Warning: the following files would be used as linker inputs, but linking is not being done: " ++ unwords objs)

        -- Check that there are some input files
        -- (except in the interactive case)
   if null srcs && isNothing (Ghcjs.gsLinkJsLib settings) && (null (objs++js_objs) || not_linking) && needsInputsMode mode
        then throwGhcException (UsageError "no input files")
        else do

   case mode of
      StopBefore HCc | hscTarget dflags /= HscC
        -> throwGhcException $ UsageError $
           "the option -C is only available with an unregisterised GHC"
      StopBefore (As False) | ghcLink dflags == NoLink
        -> throwGhcException $ UsageError $
           "the options -S and -fno-code are incompatible. Please omit -S"

      _ -> return ()

     -- Verify that output files point somewhere sensible.
   verifyOutputFiles dflags

-- Compiler output options

-- called to verify that the output files & directories
-- point somewhere valid.
-- Called to verify that the output files point somewhere valid.
--
-- The assumption is that the directory portion of these output
-- options will have to exist by the time 'verifyOutputFiles'
-- is invoked.
--
-- We create the directories for -odir, -hidir, -outputdir etc. ourselves if
-- they don't exist, so don't check for those here (#2278).
verifyOutputFiles :: DynFlags -> IO ()
verifyOutputFiles dflags = do
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
     throwGhcException (CmdLineError ("error: directory portion of " ++
                             show dir ++ " does not exist (used with " ++
                             show flg ++ " option.)"))

-----------------------------------------------------------------------------
-- GHC modes of operation

type Mode = Either PreStartupMode PostStartupMode
type PostStartupMode = Either PreLoadMode PostLoadMode

data PreStartupMode
  = ShowVersion                          -- ghc -V/--version
  | ShowNumVersion                       -- ghc --numeric-version
  | ShowSupportedExtensions              -- ghc --supported-extensions
  | ShowOptions Bool {- isInteractive -} -- ghc --show-options
-- GHCJS pre-startup modes
  | ShowNumGhcVersion       -- ghcjs --numeric-ghc-version

showNumGhcVersionMode :: Mode
showNumGhcVersionMode = mkPreStartupMode ShowNumGhcVersion
-- end GHCJS pre-startup modes

showVersionMode, showNumVersionMode, showSupportedExtensionsMode, showOptionsMode :: Mode
showVersionMode             = mkPreStartupMode ShowVersion
showNumVersionMode          = mkPreStartupMode ShowNumVersion
showSupportedExtensionsMode = mkPreStartupMode ShowSupportedExtensions
showOptionsMode             = mkPreStartupMode (ShowOptions False)

mkPreStartupMode :: PreStartupMode -> Mode
mkPreStartupMode = Left

isShowVersionMode :: Mode -> Bool
isShowVersionMode (Left ShowVersion) = True
isShowVersionMode _ = False

isShowNumVersionMode :: Mode -> Bool
isShowNumVersionMode (Left ShowNumVersion) = True
isShowNumVersionMode _ = False

isShowNumGhcVersionMode :: Mode -> Bool
isShowNumGhcVersionMode (Left ShowNumGhcVersion) = True
isShowNumGhcVersionMode _ = False

data PreLoadMode
  = ShowGhcUsage                                                  -- ghcjs -?
  | ShowGhciUsage                                                 -- ghci -?
  | ShowInfo                                                      -- ghcjs --info
  | PrintWithDynFlags (Ghcjs.GhcjsSettings -> DynFlags -> String) -- ghcjs --print-foo

showGhcUsageMode, showGhciUsageMode, showInfoMode :: Mode
showGhcUsageMode = mkPreLoadMode ShowGhcUsage
showGhciUsageMode = mkPreLoadMode ShowGhciUsage
showInfoMode = mkPreLoadMode ShowInfo

printSetting :: String -> Mode
printSetting k = mkPreLoadMode (PrintWithDynFlags f)
    where f _settings dflags = fromMaybe (panic ("Setting not found: " ++ show k))
                             $ lookup k (Ghcjs.compilerInfo dflags)

mkPreLoadMode :: PreLoadMode -> Mode
mkPreLoadMode = Right . Left

isShowGhcUsageMode :: Mode -> Bool
isShowGhcUsageMode (Right (Left ShowGhcUsage)) = True
isShowGhcUsageMode _ = False

isShowGhciUsageMode :: Mode -> Bool
isShowGhciUsageMode (Right (Left ShowGhciUsage)) = True
isShowGhciUsageMode _ = False

data PostLoadMode
  = ShowInterface FilePath  -- ghc --show-iface
  | DoMkDependHS            -- ghc -M
  | StopBefore Phase        -- ghc -E | -C | -S
                            -- StopBefore StopLn is the default
  | DoMake                  -- ghc --make
  | DoInteractive           -- ghc --interactive
  | DoEval [String]         -- ghc -e foo -e bar => DoEval ["bar", "foo"]
  | DoAbiHash               -- ghc --abi-hash
  | ShowPackages            -- ghc --show-packages

-- GHCJS modes
  | DoGenerateLib                         -- ghcjs --generate-lib
  | DoPrintRts                            -- ghcjs --print-rts
  | DoInstallExecutable                   -- ghcjs --install-executable ? -o ?
  | DoPrintObj FilePath                   -- ghcjs --print-obj file
  | DoPrintDeps FilePath                  -- ghcjs --print-deps file
  | DoBuildJsLibrary                      -- ghcjs --build-js-library

doGenerateLib, doPrintRts, doBuildJsLibrary :: Mode
doGenerateLib = mkPostLoadMode DoGenerateLib
doPrintRts = mkPostLoadMode DoPrintRts
doBuildJsLibrary = mkPostLoadMode DoBuildJsLibrary

doInstallExecutable :: Mode
doInstallExecutable = mkPostLoadMode DoInstallExecutable

doPrintObj :: FilePath -> Mode
doPrintObj file = mkPostLoadMode (DoPrintObj file)

doPrintDeps :: FilePath -> Mode
doPrintDeps file = mkPostLoadMode (DoPrintDeps file)
---- end GHCJS modes

doMkDependHSMode, doMakeMode, doInteractiveMode, doAbiHashMode :: Mode
doMkDependHSMode = mkPostLoadMode DoMkDependHS
doMakeMode = mkPostLoadMode DoMake
doInteractiveMode = mkPostLoadMode DoInteractive
doAbiHashMode = mkPostLoadMode DoAbiHash

showPackagesMode :: Mode
showPackagesMode = mkPostLoadMode ShowPackages

showInterfaceMode :: FilePath -> Mode
showInterfaceMode fp = mkPostLoadMode (ShowInterface fp)

stopBeforeMode :: Phase -> Mode
stopBeforeMode phase = mkPostLoadMode (StopBefore phase)

doEvalMode :: String -> Mode
doEvalMode str = mkPostLoadMode (DoEval [str])

mkPostLoadMode :: PostLoadMode -> Mode
mkPostLoadMode = Right . Right

isDoInteractiveMode :: Mode -> Bool
isDoInteractiveMode (Right (Right DoInteractive)) = True
isDoInteractiveMode _ = False

isStopLnMode :: Mode -> Bool
isStopLnMode (Right (Right (StopBefore StopLn))) = True
isStopLnMode _ = False

isDoMakeMode :: Mode -> Bool
isDoMakeMode (Right (Right DoMake)) = True
isDoMakeMode _ = False

isDoEvalMode :: Mode -> Bool
isDoEvalMode (Right (Right (DoEval _))) = True
isDoEvalMode _ = False

#if defined(HAVE_INTERNAL_INTERPRETER)
isInteractiveMode :: PostLoadMode -> Bool
isInteractiveMode DoInteractive = True
isInteractiveMode _             = False
#endif

-- isInterpretiveMode: byte-code compiler involved
isInterpretiveMode :: PostLoadMode -> Bool
isInterpretiveMode DoInteractive = True
isInterpretiveMode (DoEval _)    = True
isInterpretiveMode _             = False

needsInputsMode :: PostLoadMode -> Bool
needsInputsMode DoMkDependHS    = True
needsInputsMode (StopBefore _)  = True
needsInputsMode DoMake          = True
needsInputsMode _               = False

-- True if we are going to attempt to link in this mode.
-- (we might not actually link, depending on the GhcLink flag)
isLinkMode :: PostLoadMode -> Bool
isLinkMode (StopBefore StopLn) = True
isLinkMode DoMake              = True
isLinkMode DoInteractive       = True
isLinkMode (DoEval _)          = True
isLinkMode _                   = False

isCompManagerMode :: PostLoadMode -> Bool
isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False

-- -----------------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> IO (Mode,
                      [Located String],
                      [Warn])
parseModeFlags args = do
  let ((leftover, errs1, warns), (mModeFlag, errs2, flags')) =
          runCmdLine (processArgs mode_flags args)
                     (Nothing, [], [])
      mode = case mModeFlag of
             Nothing     -> doMakeMode
             Just (m, _) -> m

  -- See Note [Handling errors when parsing commandline flags]
  unless (null errs1 && null errs2) $ throwGhcException $ errorsToGhcException $
      map (("on the commandline", )) $ map (unLoc . errMsg) errs1 ++ errs2

  return (mode, flags' ++ leftover, warns)

type ModeM = CmdLineP (Maybe (Mode, String), [String], [Located String])
  -- mode flags sometimes give rise to new DynFlags (eg. -C, see below)
  -- so we collect the new ones and return them.

mode_flags :: [Flag ModeM]
mode_flags =
  [  ------- help / version ----------------------------------------------
    defFlag "?"                     (PassFlag (setMode showGhcUsageMode))
  , defFlag "-help"                 (PassFlag (setMode showGhcUsageMode))
  , defFlag "V"                     (PassFlag (setMode showVersionMode))
  , defFlag "-version"              (PassFlag (setMode showVersionMode))
  , defFlag "-numeric-version"      (PassFlag (setMode showNumVersionMode))
  , defFlag "-info"                 (PassFlag (setMode showInfoMode))
  , defFlag "-show-options"         (PassFlag (setMode showOptionsMode))
  , defFlag "-supported-languages"  (PassFlag (setMode showSupportedExtensionsMode))
  , defFlag "-supported-extensions" (PassFlag (setMode showSupportedExtensionsMode))
  , defFlag "-show-packages"        (PassFlag (setMode showPackagesMode))
  ] ++
  [ defFlag k'                      (PassFlag (setMode (printSetting k)))
  | k <- ["Project version",
          "Booter version",
          "Stage",
          "Build platform",
          "Host platform",
          "Target platform",
          "Have interpreter",
          "Object splitting supported",
          "Have native code generator",
          "Support SMP",
          "Unregisterised",
          "Tables next to code",
          "RTS ways",
          "Leading underscore",
          "Debug on",
          "LibDir",
          "Global Package DB",
          "C compiler flags",
          "Gcc Linker flags",
          "Ld Linker flags",
          "Native Too"],
    let k' = "-print-" ++ map (replaceSpace . toLower) k
        replaceSpace ' ' = '-'
        replaceSpace c   = c
  ] ++
      ------- interfaces ----------------------------------------------------
  [ defFlag "-show-iface"  (HasArg (\f -> setMode (showInterfaceMode f)
                                               "--show-iface"))

      ------- primary modes ------------------------------------------------
  , defFlag "c"            (PassFlag (\f -> do setMode (stopBeforeMode StopLn) f
                                               addFlag "-no-link" f))
  , defFlag "M"            (PassFlag (setMode doMkDependHSMode))
  , defFlag "E"            (PassFlag (setMode (stopBeforeMode anyHsc)))
  , defFlag "C"            (PassFlag (setMode (stopBeforeMode HCc)))
  , defFlag "S"            (PassFlag (setMode (stopBeforeMode (As False))))
  , defFlag "-make"        (PassFlag (setMode doMakeMode))
  , defFlag "-interactive" (PassFlag (setMode doInteractiveMode))
  , defFlag "-abi-hash"    (PassFlag (setMode doAbiHashMode))
  , defFlag "e"            (SepArg   (\s -> setMode (doEvalMode s) "-e"))

      ------- GHCJS modes ------------------------------------------------
  , defFlag "-generate-lib"           (PassFlag (setMode doGenerateLib))
  , defFlag "-install-executable"     (PassFlag (setMode doInstallExecutable))
  , defFlag "-print-obj"              (HasArg (\f -> setMode (doPrintObj f) "--print-obj"))
  , defFlag "-print-deps"             (HasArg (\f -> setMode (doPrintDeps f) "--print-deps"))
  , defFlag "-print-rts"              (PassFlag (setMode doPrintRts))
  , defFlag "-numeric-ghc-version"    (PassFlag (setMode (showNumGhcVersionMode)))
  , defFlag "-numeric-ghcjs-version"  (PassFlag (setMode (showNumVersionMode)))
  , defFlag "-build-js-library"       (PassFlag (setMode doBuildJsLibrary))
  ]

setMode :: Mode -> String -> EwM ModeM ()
setMode newMode newFlag = liftEwM $ do
    (mModeFlag, errs, flags') <- getCmdLineState
    let (modeFlag', errs') =
            case mModeFlag of
            Nothing -> ((newMode, newFlag), errs)
            Just (oldMode, oldFlag) ->
                case (oldMode, newMode) of
                    -- -c/--make are allowed together, and mean --make -no-link
                    _ |  isStopLnMode oldMode && isDoMakeMode newMode
                      || isStopLnMode newMode && isDoMakeMode oldMode ->
                      ((doMakeMode, "--make"), [])

                    -- If we have both --help and --interactive then we
                    -- want showGhciUsage
                    _ | isShowGhcUsageMode oldMode &&
                        isDoInteractiveMode newMode ->
                            ((showGhciUsageMode, oldFlag), [])
                      | isShowGhcUsageMode newMode &&
                        isDoInteractiveMode oldMode ->
                            ((showGhciUsageMode, newFlag), [])

                    -- If we have both -e and --interactive then -e always wins
                    _ | isDoEvalMode oldMode &&
                        isDoInteractiveMode newMode ->
                            ((oldMode, oldFlag), [])
                      | isDoEvalMode newMode &&
                        isDoInteractiveMode oldMode ->
                            ((newMode, newFlag), [])

                    -- Otherwise, --help/--version/--numeric-version always win
                      | isDominantFlag oldMode -> ((oldMode, oldFlag), [])
                      | isDominantFlag newMode -> ((newMode, newFlag), [])
                    -- We need to accumulate eval flags like "-e foo -e bar"
                    (Right (Right (DoEval esOld)),
                     Right (Right (DoEval [eNew]))) ->
                        ((Right (Right (DoEval (eNew : esOld))), oldFlag),
                         errs)
                    -- Saying e.g. --interactive --interactive is OK
                    _ | oldFlag == newFlag -> ((oldMode, oldFlag), errs)

                    -- --interactive and --show-options are used together
                    (Right (Right DoInteractive), Left (ShowOptions _)) ->
                      ((Left (ShowOptions True),
                        "--interactive --show-options"), errs)
                    (Left (ShowOptions _), (Right (Right DoInteractive))) ->
                      ((Left (ShowOptions True),
                        "--show-options --interactive"), errs)
                    -- Otherwise, complain
                    _ -> let err = flagMismatchErr oldFlag newFlag
                         in ((oldMode, oldFlag), err : errs)
    putCmdLineState (Just modeFlag', errs', flags')
  where isDominantFlag f = isShowGhcUsageMode   f ||
                           isShowGhciUsageMode  f ||
                           isShowVersionMode    f ||
                           isShowNumVersionMode f ||
                           isShowNumGhcVersionMode f

flagMismatchErr :: String -> String -> String
flagMismatchErr oldFlag newFlag
    = "cannot use `" ++ oldFlag ++  "' with `" ++ newFlag ++ "'"

addFlag :: String -> String -> EwM ModeM ()
addFlag s flag = liftEwM $ do
  (m, e, flags') <- getCmdLineState
  putCmdLineState (m, e, mkGeneralLocated loc s : flags')
    where loc = "addFlag by " ++ flag ++ " on the commandline"

-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: Ghcjs.GhcjsEnv -> Ghcjs.GhcjsSettings -> Bool -> [(String,Maybe Phase)] -> Ghc ()
doMake jsEnv settings native srcs  = do
    let (hs_srcs, non_hs_srcs) = partition haskellish srcs

        haskellish (f,Nothing) =
          looksLikeModuleName f || isHaskellUserSrcFilename f || '.' `notElem` f
        haskellish (_,Just phase) =
          phase `notElem` [As False, As True, Cc, Cobjc, Cobjcxx, CmmCpp, Cmm, StopLn]

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (Ghcjs.ghcjsOneShot jsEnv settings native hsc_env StopLn srcs)
       else do

    o_files <- mapM (\x -> liftIO $ compileFile hsc_env StopLn x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'

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

showBanner :: PostLoadMode -> DynFlags -> IO ()
showBanner _postLoadMode dflags = do
   let verb = verbosity dflags

#if defined(HAVE_INTERNAL_INTERPRETER)
   -- Show the GHCi banner
   when (isInteractiveMode _postLoadMode && verb >= 1) $ putStrLn ghciWelcomeMsg
#endif

   -- Display details of the configuration in verbose mode
   when (verb >= 2) $
    do hPutStr stderr "Glasgow Haskell Compiler for JavaScript, Version "
       hPutStr stderr cProjectVersion
       hPutStr stderr ", stage "
       hPutStr stderr cStage
       hPutStr stderr " booted by GHC version "
       hPutStrLn stderr cBooterVersion

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: Ghcjs.GhcjsSettings -> DynFlags -> IO ()
showInfo _settings dflags = do
        let sq x = " [" ++ x ++ "\n ]"
        putStrLn $ sq $ intercalate "\n ," $ map show $ Ghcjs.compilerInfo dflags

showSupportedExtensions :: Maybe String -> IO ()
showSupportedExtensions m_top_dir = do
  res <- runExceptT $ do
    top_dir <- lift (tryFindTopDir m_top_dir) >>= \case
      Nothing -> throwE $ SettingsError_MissingData "Could not find the top directory, missing -B flag"
      Just dir -> pure dir
    initSettings top_dir
  targetPlatformMini <- case res of
    Right s -> pure $ platformMini $ sTargetPlatform s
    Left (SettingsError_MissingData msg) -> do
      hPutStrLn stderr $ "WARNING: " ++ show msg
      hPutStrLn stderr $ "cannot know target platform so guessing target == host (native compiler)."
      pure cHostPlatformMini
    Left (SettingsError_BadData msg) -> do
      hPutStrLn stderr msg
      exitWith $ ExitFailure 1
  mapM_ putStrLn $ supportedLanguagesAndExtensions targetPlatformMini

showVersion :: IO ()
showVersion = putStrLn (cProjectName ++ ", version " ++ cProjectVersion)

showOptions :: Bool -> IO ()
showOptions isInteractive = putStr (unlines availableOptions)
    where
      availableOptions = concat [
        flagsForCompletion isInteractive,
        map ('-':) (getFlagNames mode_flags)
        ]
      getFlagNames opts         = map flagName opts

showGhcUsage :: DynFlags -> IO ()
showGhcUsage = showUsage False

showGhciUsage :: DynFlags -> IO ()
showGhciUsage = showUsage True

showUsage :: Bool -> DynFlags -> IO ()
showUsage ghci dflags = do
  let usage_path = if ghci then ghciUsagePath dflags
                           else ghcUsagePath dflags
  usage <- readFile usage_path
  dump usage
  where
     dump ""          = return ()
     dump ('$':'$':s) = putStr progName >> dump s
     dump (c:s)       = putChar c >> dump s

dumpFinalStats :: DynFlags -> IO ()
dumpFinalStats dflags =
  when (gopt Opt_D_faststring_stats dflags) $ dumpFastStringStats dflags

dumpFastStringStats :: DynFlags -> IO ()
dumpFastStringStats dflags = do
  segments <- getFastStringTable
  hasZ <- getFastStringZEncCounter
  let buckets = concat segments
      bucketsPerSegment = map length segments
      entriesPerBucket = map length buckets
      entries = sum entriesPerBucket
      msg = text "FastString stats:" $$ nest 4 (vcat
        [ text "segments:         " <+> int (length segments)
        , text "buckets:          " <+> int (sum bucketsPerSegment)
        , text "entries:          " <+> int entries
        , text "largest segment:  " <+> int (maximum bucketsPerSegment)
        , text "smallest segment: " <+> int (minimum bucketsPerSegment)
        , text "longest bucket:   " <+> int (maximum entriesPerBucket)
        , text "has z-encoding:   " <+> (hasZ `pcntOf` entries)
        ])
        -- we usually get more "has z-encoding" than "z-encoded", because
        -- when we z-encode a string it might hash to the exact same string,
        -- which is not counted as "z-encoded".  Only strings whose
        -- Z-encoding is different from the original string are counted in
        -- the "z-encoded" total.
  putMsg dflags msg
  where
   x `pcntOf` y = int ((x * 100) `quot` y) Outputable.<> char '%'

showPackages, dumpPackages, dumpPackagesSimple :: DynFlags -> IO ()
showPackages       dflags = putStrLn (showSDoc dflags (pprPackages dflags))
dumpPackages       dflags = putMsg dflags (pprPackages dflags)
dumpPackagesSimple dflags = putMsg dflags (pprPackagesSimple dflags)

-- -----------------------------------------------------------------------------
-- Frontend plugin support

doFrontend :: ModuleName -> [(String, Maybe Phase)] -> Ghc ()
doFrontend modname srcs = do
    hsc_env <- getSession
    frontend_plugin <- liftIO $ loadFrontendPlugin hsc_env modname
    frontend frontend_plugin
      (reverse $ frontendPluginOpts (hsc_dflags hsc_env)) srcs

-- -----------------------------------------------------------------------------
-- ABI hash support

{-
        ghc --abi-hash Data.Foo System.Bar

Generates a combined hash of the ABI for modules Data.Foo and
System.Bar.  The modules must already be compiled, and appropriate -i
options may be necessary in order to find the .hi files.

This is used by Cabal for generating the ComponentId for a
package.  The ComponentId must change when the visible ABI of
the package chagnes, so during registration Cabal calls ghc --abi-hash
to get a hash of the package's ABI.
-}

-- | Print ABI hash of input modules.
--
-- The resulting hash is the MD5 of the GHC version used (#5328,
-- see 'hiVersion') and of the existing ABI hash from each module (see
-- 'mi_mod_hash').
abiHash :: [String] -- ^ List of module names
        -> Ghc ()
abiHash strs = do
  hsc_env <- getSession
  let dflags = hsc_dflags hsc_env

  liftIO $ do

  let find_it str = do
         let modname = mkModuleName str
         r <- findImportedModule hsc_env modname Nothing
         case r of
           Found _ m -> return m
           _error    -> throwGhcException $ CmdLineError $ showSDoc dflags $
                          cannotFindModule dflags modname r

  mods <- mapM find_it strs

  let get_iface modl = loadUserInterface False (text "abiHash") modl
  ifaces <- initIfaceCheck (text "abiHash") hsc_env $ mapM get_iface mods

  bh <- openBinMem (3*1024) -- just less than a block
  put_ bh hiVersion
    -- package hashes change when the compiler version changes (for now)
    -- see #5328
  mapM_ (put_ bh . mi_mod_hash . mi_final_exts) ifaces
  f <- fingerprintBinMem bh

  putStrLn (showPpr dflags f)

-- -----------------------------------------------------------------------------
-- Util

unknownFlagsErr :: [String] -> a
unknownFlagsErr fs = throwGhcException $ UsageError $ concatMap oneError fs
  where
    oneError f =
        "unrecognised flag: " ++ f ++ "\n" ++
        (case match f (nubSort allNonDeprecatedFlags) of
            [] -> ""
            suggs -> "did you mean one of:\n" ++ unlines (map ("  " ++) suggs))
    -- fixes #11789
    -- If the flag contains '=',
    -- this uses both the whole and the left side of '=' for comparing.
    match f allFlags
        | elem '=' f =
              let (flagsWithEq, flagsWithoutEq) = partition (elem '=') allFlags
                  fName = takeWhile (/= '=') f
              in (fuzzyMatch f flagsWithEq) ++ (fuzzyMatch fName flagsWithoutEq)
        | otherwise = fuzzyMatch f allFlags

{- Note [-Bsymbolic and hooks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Bsymbolic is a flag that prevents the binding of references to global
symbols to symbols outside the shared library being compiled (see `man
ld`). When dynamically linking, we don't use -Bsymbolic on the RTS
package: that is because we want hooks to be overridden by the user,
we don't want to constrain them to the RTS package.

Unfortunately this seems to have broken somehow on OS X: as a result,
defaultHooks (in hschooks.c) is not called, which does not initialize
the GC stats. As a result, this breaks things like `:set +s` in GHCi
(#8754). As a hacky workaround, we instead call 'defaultHooks'
directly to initalize the flags in the RTS.

A byproduct of this, I believe, is that hooks are likely broken on OS
X when dynamically linking. But this probably doesn't affect most
people since we're linking GHC dynamically, but most things themselves
link statically.
-}

-- foreign import ccall safe "initGCStatistics"
--  initGCStatistics :: IO ()

initGCStatistics :: IO ()
initGCStatistics = pure ()
