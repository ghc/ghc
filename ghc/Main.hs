{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main (main) where

-- The official GHC API
import qualified GHC
import GHC              (parseTargetFiles,  Ghc, GhcMonad(..),
                          LoadHowMuch(..) )

import GHC.Driver.Backend
import GHC.Driver.CmdLine
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Phases
import GHC.Driver.Session
import GHC.Driver.Ppr
import GHC.Driver.Pipeline  ( oneShot, compileFile )
import GHC.Driver.MakeFile  ( doMkDependHS )
import GHC.Driver.Backpack  ( doBackpack )
import GHC.Driver.Plugins
import GHC.Driver.Config.Logger (initLogFlags)
import GHC.Driver.Config.Diagnostic

import GHC.Platform
import GHC.Platform.Ways
import GHC.Platform.Host

#if defined(HAVE_INTERNAL_INTERPRETER)
import GHCi.UI              ( interactiveUI, ghciWelcomeMsg, defaultGhciSettings )
#endif

import GHC.Runtime.Loader   ( loadFrontendPlugin, initializeSessionPlugins )

import GHC.Unit.Env
import GHC.Unit (UnitId, homeUnitDepends)
import GHC.Unit.Home.ModInfo (emptyHomePackageTable)
import GHC.Unit.Module ( ModuleName, mkModuleName )
import GHC.Unit.Module.ModIface
import GHC.Unit.State  ( pprUnits, pprUnitsSimple )
import GHC.Unit.Finder ( findImportedModule, FindResult(..) )
import qualified GHC.Unit.State as State
import GHC.Unit.Types  ( IsBootInterface(..) )

import GHC.Types.Basic     ( failed )
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.Unique.Supply
import GHC.Types.PkgQual

import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Monad       ( liftIO, mapMaybeM )
import GHC.Utils.Binary        ( openBinMem, put_ )
import GHC.Utils.Logger

import GHC.Settings.Config
import GHC.Settings.Constants
import GHC.Settings.IO

import GHC.HandleEncoding
import GHC.Data.FastString
import GHC.SysTools.BaseDir

import GHC.Iface.Load
import GHC.Iface.Recomp.Binary ( fingerprintBinMem )

import GHC.Tc.Utils.Monad      ( initIfaceCheck )
import System.FilePath

-- Standard Haskell libraries
import System.IO
import System.Environment
import System.Exit
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (throwE, runExceptT)
import Data.Char
import Data.List ( isPrefixOf, partition, intercalate, (\\) )
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Prelude
import GHC.ResponseFile (expandResponse)
import Data.Bifunctor
import GHC.Data.Graph.Directed
import qualified Data.List.NonEmpty as NE

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
   hSetBuffering stdout LineBuffering
   hSetBuffering stderr LineBuffering

   configureHandleEncoding
   GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- 1. extract the -B flag from the args
    argv0 <- getArgs

    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))

    let argv2 = map (mkGeneralLocated "on the commandline") argv1

    -- 2. Parse the "mode" flags (--make, --interactive etc.)
    (mode, units, argv3, flagWarnings) <- parseModeFlags argv2

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
                   ShowVersion               -> showVersion
                   ShowNumVersion            -> putStrLn cProjectVersion
                   ShowOptions isInteractive -> showOptions isInteractive
        Right postStartupMode ->
            -- start our GHC session
            GHC.runGhc mbMinusB $ do

            dflags <- GHC.getSessionDynFlags

            case postStartupMode of
                Left preLoadMode ->
                    liftIO $ do
                        case preLoadMode of
                            ShowInfo               -> showInfo dflags
                            ShowGhcUsage           -> showGhcUsage  dflags
                            ShowGhciUsage          -> showGhciUsage dflags
                            PrintWithDynFlags f    -> putStrLn (f dflags)
                Right postLoadMode ->
                    main' postLoadMode units dflags argv3 flagWarnings

main' :: PostLoadMode -> [String] -> DynFlags -> [Located String] -> [Warn]
      -> Ghc ()
main' postLoadMode units dflags0 args flagWarnings = do
  let args' = case postLoadMode of
                DoRun -> takeWhile (\arg -> unLoc arg /= "--") args
                _     -> args

  -- set the default GhcMode, backend and GhcLink.  The backend
  -- can be further adjusted on a module by module basis, using only
  -- the -fllvm and -fasm flags.  If the default backend is not
  -- LLVM or NCG, -fllvm and -fasm have no effect.
  let dflt_backend = backend dflags0
      (mode, bcknd, link)
         = case postLoadMode of
               DoInteractive   -> (CompManager, interpreterBackend,  LinkInMemory)
               DoEval _        -> (CompManager, interpreterBackend,  LinkInMemory)
               DoRun           -> (CompManager, interpreterBackend,  LinkInMemory)
               DoMake          -> (CompManager, dflt_backend, LinkBinary)
               DoBackpack      -> (CompManager, dflt_backend, LinkBinary)
               DoMkDependHS    -> (MkDepend,    dflt_backend, LinkBinary)
               DoAbiHash       -> (OneShot,     dflt_backend, LinkBinary)
               _               -> (OneShot,     dflt_backend, LinkBinary)

  let dflags1 = dflags0{ ghcMode   = mode,
                         backend   = bcknd,
                         ghcLink   = link,
                         verbosity = case postLoadMode of
                                         DoEval _ -> 0
                                         DoRun    -> 0
                                         _other   -> 1
                        }

      -- turn on -fimplicit-import-qualified for GHCi now, so that it
      -- can be overridden from the command-line
      -- XXX: this should really be in the interactive DynFlags, but
      -- we don't set that until later in interactiveUI
      -- We also set -fignore-optim-changes and -fignore-hpc-changes,
      -- which are program-level options. Again, this doesn't really
      -- feel like the right place to handle this, but we don't have
      -- a great story for the moment.
      dflags2  | DoInteractive <- postLoadMode = def_ghci_flags
               | DoEval _      <- postLoadMode = def_ghci_flags
               | DoRun         <- postLoadMode = def_ghci_flags
               | otherwise                     = dflags1
        where def_ghci_flags = dflags1 `gopt_set` Opt_ImplicitImportQualified
                                       `gopt_set` Opt_IgnoreOptimChanges
                                       `gopt_set` Opt_IgnoreHpcChanges
                                       -- Setting this by default has the nice effect that
                                       -- -fno-code and --interactive falls back to interpreter rather than
                                       -- object code but has little other effect unless you are also using
                                       -- fat interface files.
                                       `gopt_set` Opt_UseBytecodeRatherThanObjects

  logger1 <- getLogger
  let logger2 = setLogFlags logger1 (initLogFlags dflags2)

        -- The rest of the arguments are "dynamic"
        -- Leftover ones are presumably files
  (dflags3, fileish_args, dynamicFlagWarnings) <-
      GHC.parseDynamicFlags logger2 dflags2 args'

  let dflags4 = if backendNeedsFullWays bcknd &&
                   not (gopt Opt_ExternalInterpreter dflags3)
                then
                    let platform = targetPlatform dflags3
                        dflags3a = dflags3 { targetWays_ = hostFullWays }
                        dflags3b = foldl gopt_set dflags3a
                                 $ concatMap (wayGeneralFlags platform)
                                             hostFullWays
                        dflags3c = foldl gopt_unset dflags3b
                                 $ concatMap (wayUnsetGeneralFlags platform)
                                             hostFullWays
                    in dflags3c
                else
                    dflags3

  let logger4 = setLogFlags logger2 (initLogFlags dflags4)

  GHC.prettyPrintGhcErrors logger4 $ do

  let flagWarnings' = flagWarnings ++ dynamicFlagWarnings

  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
         liftIO $ handleFlagWarnings logger4 (initPrintConfig dflags4) (initDiagOpts dflags4) flagWarnings'

  liftIO $ showBanner postLoadMode dflags4

  let (dflags5, srcs, objs) = parseTargetFiles dflags4 (map unLoc fileish_args)

  -- we've finished manipulating the DynFlags, update the session
  _ <- GHC.setSessionDynFlags dflags5
  dflags6 <- GHC.getSessionDynFlags

  -- Must do this before loading plugins
  liftIO $ initUniqSupply (initialUnique dflags6) (uniqueIncrement dflags6)

  -- Initialise plugins here because the plugin author might already expect this
  -- subsequent call to `getLogger` to be affected by a plugin.
  initializeSessionPlugins
  hsc_env <- getSession
  logger <- getLogger


        ---------------- Display configuration -----------
  case verbosity dflags6 of
    v | v == 4 -> liftIO $ dumpUnitsSimple hsc_env
      | v >= 5 -> liftIO $ dumpUnits       hsc_env
      | otherwise -> return ()

        ---------------- Final sanity checking -----------
  liftIO $ checkOptions postLoadMode dflags6 srcs objs units

  ---------------- Do the business -----------
  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
    case postLoadMode of
       ShowInterface f        -> liftIO $ showIface logger
                                                    (hsc_dflags hsc_env)
                                                    (hsc_units  hsc_env)
                                                    (hsc_NC     hsc_env)
                                                    f
       DoMake                 -> doMake units srcs
       DoMkDependHS           -> doMkDependHS (map fst srcs)
       StopBefore p           -> liftIO (oneShot hsc_env p srcs)
       DoInteractive          -> ghciUI units srcs Nothing
       DoEval exprs           -> ghciUI units srcs $ Just $ reverse exprs
       DoRun                  -> doRun units srcs args
       DoAbiHash              -> abiHash (map fst srcs)
       ShowPackages           -> liftIO $ showUnits hsc_env
       DoFrontend f           -> doFrontend f srcs
       DoBackpack             -> doBackpack (map fst srcs)

  liftIO $ dumpFinalStats logger

doRun :: [String] -> [(FilePath, Maybe Phase)] -> [Located String] -> Ghc ()
doRun units srcs args = do
    dflags <- getDynFlags
    let mainFun = fromMaybe "main" (mainFunIs dflags)
    ghciUI units srcs (Just ["System.Environment.withArgs " ++ show args' ++ " (Control.Monad.void " ++ mainFun ++ ")"])
  where
    args' = drop 1 $ dropWhile (/= "--") $ map unLoc args

ghciUI :: [String] -> [(FilePath, Maybe Phase)] -> Maybe [String] -> Ghc ()
#if !defined(HAVE_INTERNAL_INTERPRETER)
ghciUI _ _ _ =
  throwGhcException (CmdLineError "not built for interactive use")
#else
ghciUI units srcs maybe_expr = do
  hs_srcs <- case NE.nonEmpty units of
    Just ne_units -> do
      initMulti ne_units
    Nothing -> do
      case srcs of
        [] -> return []
        _  -> do
          s <- initMake srcs
          return $ map (uncurry (,Nothing,)) s
  interactiveUI defaultGhciSettings hs_srcs maybe_expr
#endif


-- -----------------------------------------------------------------------------
-- Option sanity checks

-- | Ensure sanity of options.
--
-- Throws 'UsageError' or 'CmdLineError' if not.
checkOptions :: PostLoadMode -> DynFlags -> [(String,Maybe Phase)] -> [String] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions mode dflags srcs objs units = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | (f@('-':_), _) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

   when (not (Set.null (rtsWays (ways dflags)))
         && isInterpretiveMode mode) $
        hPutStrLn stderr ("Warning: -debug, -threaded and -ticky are ignored by GHCi")

        -- -prof and --interactive are not a good combination
   when ((fullWays (ways dflags) /= hostFullWays)
         && isInterpretiveMode mode
         && not (gopt Opt_ExternalInterpreter dflags)) $
      do throwGhcException (UsageError
              "-fexternal-interpreter is required when using --interactive with a non-standard way (-prof, -static, or -dynamic).")
        -- -ohi sanity check
   if (isJust (outputHi dflags) &&
      (isCompManagerMode mode || srcs `lengthExceeds` 1))
        then throwGhcException (UsageError "-ohi can only be used when compiling a single source file")
        else do

   if (isJust (dynOutputHi dflags) &&
      (isCompManagerMode mode || srcs `lengthExceeds` 1))
     then throwGhcException (UsageError "-dynohi can only be used when compiling a single source file")
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
   if null srcs && (null objs || not_linking) && needsInputsMode mode && null units
        then throwGhcException (UsageError "no input files" )
        else do

   case mode of
      StopBefore StopC | not (backendGeneratesHc (backend dflags))
        -> throwGhcException $ UsageError $
           "the option -C is only available with an unregisterised GHC"
      StopBefore StopAs | ghcLink dflags == NoLink
        -> throwGhcException $ UsageError $
           "the options -S and -fno-code are incompatible. Please omit -S"

      _ -> return ()

     -- Verify that output files point somewhere sensible.
   verifyOutputFiles dflags

-- Compiler output options

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

data PreLoadMode
  = ShowGhcUsage                           -- ghc -?
  | ShowGhciUsage                          -- ghci -?
  | ShowInfo                               -- ghc --info
  | PrintWithDynFlags (DynFlags -> String) -- ghc --print-foo

showGhcUsageMode, showGhciUsageMode, showInfoMode :: Mode
showGhcUsageMode = mkPreLoadMode ShowGhcUsage
showGhciUsageMode = mkPreLoadMode ShowGhciUsage
showInfoMode = mkPreLoadMode ShowInfo

printSetting :: String -> Mode
printSetting k = mkPreLoadMode (PrintWithDynFlags f)
    where f dflags = fromMaybe (panic ("Setting not found: " ++ show k))
                   $ lookup k (compilerInfo dflags)

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
  | StopBefore StopPhase    -- ghc -E | -C | -S
                            -- StopBefore StopLn is the default
  | DoMake                  -- ghc --make
  | DoBackpack              -- ghc --backpack foo.bkp
  | DoInteractive           -- ghc --interactive
  | DoEval [String]         -- ghc -e foo -e bar => DoEval ["bar", "foo"]
  | DoRun                   -- ghc --run
  | DoAbiHash               -- ghc --abi-hash
  | ShowPackages            -- ghc --show-packages
  | DoFrontend ModuleName   -- ghc --frontend Plugin.Module

doMkDependHSMode, doMakeMode, doInteractiveMode, doRunMode,
  doAbiHashMode, showUnitsMode :: Mode
doMkDependHSMode = mkPostLoadMode DoMkDependHS
doMakeMode = mkPostLoadMode DoMake
doInteractiveMode = mkPostLoadMode DoInteractive
doRunMode = mkPostLoadMode DoRun
doAbiHashMode = mkPostLoadMode DoAbiHash
showUnitsMode = mkPostLoadMode ShowPackages

showInterfaceMode :: FilePath -> Mode
showInterfaceMode fp = mkPostLoadMode (ShowInterface fp)

stopBeforeMode :: StopPhase -> Mode
stopBeforeMode phase = mkPostLoadMode (StopBefore phase)

doEvalMode :: String -> Mode
doEvalMode str = mkPostLoadMode (DoEval [str])

doFrontendMode :: String -> Mode
doFrontendMode str = mkPostLoadMode (DoFrontend (mkModuleName str))

doBackpackMode :: Mode
doBackpackMode = mkPostLoadMode DoBackpack

mkPostLoadMode :: PostLoadMode -> Mode
mkPostLoadMode = Right . Right

isDoInteractiveMode :: Mode -> Bool
isDoInteractiveMode (Right (Right DoInteractive)) = True
isDoInteractiveMode _ = False

isStopLnMode :: Mode -> Bool
isStopLnMode (Right (Right (StopBefore NoStop))) = True
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
isLinkMode (StopBefore NoStop) = True
isLinkMode DoMake              = True
isLinkMode DoRun               = True
isLinkMode DoInteractive       = True
isLinkMode (DoEval _)          = True
isLinkMode _                   = False

isCompManagerMode :: PostLoadMode -> Bool
isCompManagerMode DoRun         = True
isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False

-- -----------------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> IO (Mode, [String],
                      [Located String],
                      [Warn])
parseModeFlags args = do
  ((leftover, errs1, warns), (mModeFlag, units, errs2, flags')) <-
        processCmdLineP mode_flags (Nothing, [], [], []) args
  let mode = case mModeFlag of
             Nothing     -> doMakeMode
             Just (m, _) -> m

  -- See Note [Handling errors when parsing command-line flags]
  unless (null errs1 && null errs2) $ throwGhcException $ errorsToGhcException $
      map (("on the commandline", )) $ map (unLoc . errMsg) errs1 ++ errs2

  return (mode, units, flags' ++ leftover, warns)

type ModeM = CmdLineP (Maybe (Mode, String), [String], [String], [Located String])
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
  , defFlag "-show-packages"        (PassFlag (setMode showUnitsMode))
  ] ++
  [ defFlag k'                      (PassFlag (setMode (printSetting k)))
  | k <- ["Project version",
          "Project Git commit id",
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
          "C compiler link flags",
          "ld flags"],
    let k' = "-print-" ++ map (replaceSpace . toLower) k
        replaceSpace ' ' = '-'
        replaceSpace c   = c
  ] ++
      ------- interfaces ----------------------------------------------------
  [ defFlag "-show-iface"  (HasArg (\f -> setMode (showInterfaceMode f)
                                               "--show-iface"))

      ------- primary modes ------------------------------------------------
  , defFlag "c"            (PassFlag (\f -> do setMode (stopBeforeMode NoStop) f
                                               addFlag "-no-link" f))
  , defFlag "M"            (PassFlag (setMode doMkDependHSMode))
  , defFlag "E"            (PassFlag (setMode (stopBeforeMode StopPreprocess )))
  , defFlag "C"            (PassFlag (setMode (stopBeforeMode StopC)))
  , defFlag "S"            (PassFlag (setMode (stopBeforeMode StopAs)))
  , defFlag "-run"         (PassFlag (setMode doRunMode))
  , defFlag "-make"        (PassFlag (setMode doMakeMode))
  , defFlag "unit"         (SepArg   (\s -> addUnit s "-unit"))
  , defFlag "-backpack"    (PassFlag (setMode doBackpackMode))
  , defFlag "-interactive" (PassFlag (setMode doInteractiveMode))
  , defFlag "-abi-hash"    (PassFlag (setMode doAbiHashMode))
  , defFlag "e"            (SepArg   (\s -> setMode (doEvalMode s) "-e"))
  , defFlag "-frontend"    (SepArg   (\s -> setMode (doFrontendMode s) "-frontend"))
  ]

addUnit :: String -> String -> EwM ModeM ()
addUnit unit_str _arg = liftEwM $ do
  (mModeFlag, units, errs, flags') <- getCmdLineState
  putCmdLineState (mModeFlag, unit_str:units, errs, flags')

setMode :: Mode -> String -> EwM ModeM ()
setMode newMode newFlag = liftEwM $ do
    (mModeFlag, units, errs, flags') <- getCmdLineState
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
    putCmdLineState (Just modeFlag', units, errs', flags')
  where isDominantFlag f = isShowGhcUsageMode   f ||
                           isShowGhciUsageMode  f ||
                           isShowVersionMode    f ||
                           isShowNumVersionMode f

flagMismatchErr :: String -> String -> String
flagMismatchErr oldFlag newFlag
    = "cannot use `" ++ oldFlag ++  "' with `" ++ newFlag ++ "'"

addFlag :: String -> String -> EwM ModeM ()
addFlag s flag = liftEwM $ do
  (m, units, e, flags') <- getCmdLineState
  putCmdLineState (m, units, e, mkGeneralLocated loc s : flags')
    where loc = "addFlag by " ++ flag ++ " on the commandline"

-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: [String] -> [(String, Maybe Phase)] -> Ghc ()
doMake units targets = do
  hs_srcs <- case NE.nonEmpty units of
    Just ne_units -> do
      initMulti ne_units
    Nothing -> do
      s <- initMake targets
      return $ map (uncurry (,Nothing,)) s
  case hs_srcs of
    [] -> return ()
    _  -> do
      targets' <- mapM (\(src, uid, phase) -> GHC.guessTarget src uid phase) hs_srcs
      GHC.setTargets targets'
      ok_flag <- GHC.load LoadAllTargets
      when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))

initMake :: [(String,Maybe Phase)] -> Ghc [(String, Maybe Phase)]
initMake srcs  = do
    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot hsc_env NoStop srcs) >> return []
       else do

    o_files <- mapMaybeM (\x -> liftIO $ compileFile hsc_env NoStop x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'
    return hs_srcs

-- Strip out any ["+RTS", ..., "-RTS"] sequences in the command string list.
removeRTS :: [String] -> [String]
removeRTS ("+RTS" : xs)  =
  case dropWhile (/= "-RTS") xs of
    [] -> []
    (_ : ys) -> removeRTS ys
removeRTS (y:ys)         = y : removeRTS ys
removeRTS []             = []

initMulti :: NE.NonEmpty String -> Ghc ([(String, Maybe UnitId, Maybe Phase)])
initMulti unitArgsFiles  = do
  hsc_env <- GHC.getSession
  let logger = hsc_logger hsc_env
  initial_dflags <- GHC.getSessionDynFlags

  dynFlagsAndSrcs <- forM unitArgsFiles $ \f -> do
    when (verbosity initial_dflags > 2) (liftIO $ print f)
    args <- liftIO $ expandResponse [f]
    (dflags2, fileish_args, warns) <- parseDynamicFlagsCmdLine initial_dflags (map (mkGeneralLocated f) (removeRTS args))
    handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
         liftIO $ handleFlagWarnings logger (initPrintConfig dflags2) (initDiagOpts dflags2) warns

    let (dflags3, srcs, objs) = parseTargetFiles dflags2 (map unLoc fileish_args)
        dflags4 = offsetDynFlags dflags3

    let (hs_srcs, non_hs_srcs) = partition isHaskellishTarget srcs

    -- This is dubious as the whole unit environment won't be set-up correctly, but
    -- that doesn't matter for what we use it for (linking and oneShot)
    let dubious_hsc_env = hscSetFlags dflags4 hsc_env
    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot dubious_hsc_env NoStop srcs) >> return (dflags4, [])
       else do

    o_files <- mapMaybeM (\x -> liftIO $ compileFile dubious_hsc_env NoStop x)
                 non_hs_srcs
    let dflags5 = dflags4 { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags4 }

    liftIO $ checkOptions DoMake dflags5 srcs objs []

    pure (dflags5, hs_srcs)

  let
    unitDflags = NE.map fst dynFlagsAndSrcs
    srcs = NE.map (\(dflags, lsrcs) -> map (uncurry (,Just $ homeUnitId_ dflags,)) lsrcs) dynFlagsAndSrcs
    (hs_srcs, _non_hs_srcs) = unzip (map (partition (\(file, _uid, phase) -> isHaskellishTarget (file, phase))) (NE.toList srcs))

  checkDuplicateUnits initial_dflags (NE.toList (NE.zip unitArgsFiles unitDflags))

  let (initial_home_graph, mainUnitId) = createUnitEnvFromFlags unitDflags
      home_units = unitEnv_keys initial_home_graph

  home_unit_graph <- forM initial_home_graph $ \homeUnitEnv -> do
    let cached_unit_dbs = homeUnitEnv_unit_dbs homeUnitEnv
        hue_flags = homeUnitEnv_dflags homeUnitEnv
        dflags = homeUnitEnv_dflags homeUnitEnv
    (dbs,unit_state,home_unit,mconstants) <- liftIO $ State.initUnits logger hue_flags cached_unit_dbs home_units

    updated_dflags <- liftIO $ updatePlatformConstants dflags mconstants
    pure $ HomeUnitEnv
      { homeUnitEnv_units = unit_state
      , homeUnitEnv_unit_dbs = Just dbs
      , homeUnitEnv_dflags = updated_dflags
      , homeUnitEnv_hpt = emptyHomePackageTable
      , homeUnitEnv_home_unit = Just home_unit
      }

  checkUnitCycles initial_dflags home_unit_graph

  let dflags = homeUnitEnv_dflags $ unitEnv_lookup mainUnitId home_unit_graph
  unitEnv <- assertUnitEnvInvariant <$> (liftIO $ initUnitEnv mainUnitId home_unit_graph (ghcNameVersion dflags) (targetPlatform dflags))
  let final_hsc_env = hsc_env { hsc_unit_env = unitEnv }

  GHC.setSession final_hsc_env

  -- if we have no haskell sources from which to do a dependency
  -- analysis, then just do one-shot compilation and/or linking.
  -- This means that "ghc Foo.o Bar.o -o baz" links the program as
  -- we expect.
  if (null hs_srcs)
      then do
        liftIO $ hPutStrLn stderr $ "Multi Mode can not be used for one-shot mode."
        liftIO $ exitWith (ExitFailure 1)
      else do

{-
  o_files <- liftIO $ mapMaybeM
                (\(src, uid, mphase) ->
                  compileFile (hscSetActiveHomeUnit (ue_unitHomeUnit (fromJust uid) unitEnv) final_hsc_env) NoStop (src, mphase)
                )
                (concat non_hs_srcs)
                -}

  -- MP: This should probably modify dflags for each unit?
  --let dflags' = dflags { ldInputs = map (FileOption "") o_files
  --                                  ++ ldInputs dflags }
  return $ concat hs_srcs

-- | Check that we don't have multiple units with the same UnitId.

checkUnitCycles :: DynFlags -> UnitEnvGraph HomeUnitEnv -> Ghc ()
checkUnitCycles dflags graph = processSCCs sccs
  where
    mkNode :: (UnitId, HomeUnitEnv) -> Node UnitId UnitId
    mkNode (uid, hue) = DigraphNode uid uid (homeUnitDepends (homeUnitEnv_units hue))
    nodes = map mkNode (unitEnv_elts graph)

    sccs = stronglyConnCompFromEdgedVerticesOrd nodes

    processSCCs [] = return ()
    processSCCs (AcyclicSCC _: other_sccs) = processSCCs other_sccs
    processSCCs (CyclicSCC uids: _) = throwGhcException $ CmdLineError $ showSDoc dflags (cycle_err uids)


    cycle_err uids =
      hang (text "Units form a dependency cycle:")
           2
           (one_err uids)

    one_err uids = vcat $
                    (map (\uid -> text "-" <+> ppr uid <+> text "depends on") start)
                    ++ [text "-" <+> ppr final]
      where
        start = init uids
        final = last uids

checkDuplicateUnits :: DynFlags -> [(FilePath, DynFlags)] -> Ghc ()
checkDuplicateUnits dflags flags =
  unless (null duplicate_ids)
         (throwGhcException $ CmdLineError $ showSDoc dflags multi_err)

  where
    uids = map (second homeUnitId_) flags
    deduplicated_uids = ordNubOn snd uids
    duplicate_ids = Set.fromList (map snd uids \\ map snd deduplicated_uids)

    duplicate_flags = filter (flip Set.member duplicate_ids . snd) uids

    one_err (fp, home_uid) = text "-" <+> ppr home_uid <+> text "defined in" <+> text fp

    multi_err =
      hang (text "Multiple units with the same unit-id:")
           2
           (vcat (map one_err duplicate_flags))


offsetDynFlags :: DynFlags -> DynFlags
offsetDynFlags dflags =
  dflags { hiDir = c hiDir
         , objectDir  = c objectDir
         , stubDir = c stubDir
         , hieDir  = c hieDir
         , dumpDir = c dumpDir  }

  where
    c f = augment_maybe (f dflags)

    augment_maybe Nothing = Nothing
    augment_maybe (Just f) = Just (augment f)
    augment f | isRelative f, Just offset <- workingDirectory dflags = offset </> f
              | otherwise = f


createUnitEnvFromFlags :: NE.NonEmpty DynFlags -> (HomeUnitGraph, UnitId)
createUnitEnvFromFlags unitDflags =
  let
    newInternalUnitEnv dflags = mkHomeUnitEnv dflags emptyHomePackageTable Nothing
    unitEnvList = NE.map (\dflags -> (homeUnitId_ dflags, newInternalUnitEnv dflags)) unitDflags
    activeUnit = fst $ NE.head unitEnvList
  in
    (unitEnv_new (Map.fromList (NE.toList (unitEnvList))), activeUnit)

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
    do hPutStr stderr "Glasgow Haskell Compiler, Version "
       hPutStr stderr cProjectVersion
       hPutStr stderr ", stage "
       hPutStr stderr cStage
       hPutStr stderr " booted by GHC version "
       hPutStrLn stderr cBooterVersion

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: DynFlags -> IO ()
showInfo dflags = do
        let sq x = " [" ++ x ++ "\n ]"
        putStrLn $ sq $ intercalate "\n ," $ map show $ compilerInfo dflags

-- TODO use GHC.Utils.Error once that is disentangled from all the other GhcMonad stuff?
showSupportedExtensions :: Maybe String -> IO ()
showSupportedExtensions m_top_dir = do
  res <- runExceptT $ do
    top_dir <- lift (tryFindTopDir m_top_dir) >>= \case
      Nothing -> throwE $ SettingsError_MissingData "Could not find the top directory, missing -B flag"
      Just dir -> pure dir
    initSettings top_dir
  arch_os <- case res of
    Right s -> pure $ platformArchOS $ sTargetPlatform s
    Left (SettingsError_MissingData msg) -> do
      hPutStrLn stderr $ "WARNING: " ++ show msg
      hPutStrLn stderr $ "cannot know target platform so guessing target == host (native compiler)."
      pure hostPlatformArchOS
    Left (SettingsError_BadData msg) -> do
      hPutStrLn stderr msg
      exitWith $ ExitFailure 1
  mapM_ putStrLn $ supportedLanguagesAndExtensions arch_os

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
  progName <- getProgName
  dump progName usage
  where
    dump progName xs = case xs of
      ""        -> return ()
      '$':'$':s -> putStr progName >> dump progName s
      c:s       -> putChar c >> dump progName s

dumpFinalStats :: Logger -> IO ()
dumpFinalStats logger = do
  when (logHasDumpFlag logger Opt_D_faststring_stats) $ dumpFastStringStats logger

  when (logHasDumpFlag logger Opt_D_dump_faststrings) $ do
    fss <- getFastStringTable
    let ppr_table         = fmap ppr_segment (fss `zip` [0..])
        ppr_segment (s,n) = hang (text "Segment" <+> int n) 2 (vcat (fmap ppr_bucket (s `zip` [0..])))
        ppr_bucket  (b,n) = hang (text "Bucket" <+> int n) 2 (vcat (fmap ftext b))
    putDumpFileMaybe logger Opt_D_dump_faststrings "FastStrings" FormatText (vcat ppr_table)

dumpFastStringStats :: Logger -> IO ()
dumpFastStringStats logger = do
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
  putMsg logger msg
  where
   x `pcntOf` y = int ((x * 100) `quot` y) Outputable.<> char '%'

showUnits, dumpUnits, dumpUnitsSimple :: HscEnv -> IO ()
showUnits       hsc_env = putStrLn (showSDoc (hsc_dflags hsc_env) (pprUnits (hsc_units hsc_env)))
dumpUnits       hsc_env = putMsg (hsc_logger hsc_env) (pprUnits (hsc_units hsc_env))
dumpUnitsSimple hsc_env = putMsg (hsc_logger hsc_env) (pprUnitsSimple (hsc_units hsc_env))

-- -----------------------------------------------------------------------------
-- Frontend plugin support

doFrontend :: ModuleName -> [(String, Maybe Phase)] -> Ghc ()
doFrontend modname srcs = do
    hsc_env <- getSession
    (frontend_plugin, _pkgs, _deps) <- liftIO $ loadFrontendPlugin hsc_env modname -- TODO do these need to recorded?
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
the package changes, so during registration Cabal calls ghc --abi-hash
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
  let dflags    = hsc_dflags hsc_env

  liftIO $ do

  let find_it str = do
         let modname = mkModuleName str
         r <- findImportedModule hsc_env modname NoPkgQual
         case r of
           Found _ m -> return m
           _error    -> throwGhcException $ CmdLineError $ showSDoc dflags $
                          cannotFindModule hsc_env modname r

  mods <- mapM find_it strs

  let get_iface modl = loadUserInterface NotBoot (text "abiHash") modl
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
