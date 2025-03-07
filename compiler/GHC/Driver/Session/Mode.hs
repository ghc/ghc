{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE TupleSections #-}
module GHC.Driver.Session.Mode where

import GHC.Driver.CmdLine
import GHC.Driver.Phases
import GHC.Driver.Session
import GHC.Unit.Module ( ModuleName, mkModuleName )

import GHC.Types.SrcLoc

import GHC.Utils.Panic

import GHC.Data.Maybe

-- Standard Haskell libraries
import System.IO
import Control.Monad
import Data.Char
import Prelude

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
          "C compiler link flags"
          ],
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
