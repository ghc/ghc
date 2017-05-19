{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

-----------------------------------------------------------------------------
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2005-2006
--
-----------------------------------------------------------------------------

module GHCi.UI (
        interactiveUI,
        GhciSettings(..),
        defaultGhciSettings,
        ghciCommands,
        ghciWelcomeMsg
    ) where

#include "HsVersions.h"

-- GHCi
import qualified GHCi.UI.Monad as GhciMonad ( args, runStmt, runDecls )
import GHCi.UI.Monad hiding ( args, runStmt, runDecls )
import GHCi.UI.Tags
import GHCi.UI.Info
import Debugger

-- The GHC interface
import GHCi
import GHCi.RemoteTypes
import GHCi.BreakArray
import DynFlags
import ErrUtils
import GhcMonad ( modifySession )
import qualified GHC
import GHC ( LoadHowMuch(..), Target(..),  TargetId(..), InteractiveImport(..),
             TyThing(..), Phase, BreakIndex, Resume, SingleStep, Ghc,
             getModuleGraph, handleSourceError )
import HsImpExp
import HsSyn
import HscTypes ( tyThingParent_maybe, handleFlagWarnings, getSafeMode, hsc_IC,
                  setInteractivePrintName, hsc_dflags, msObjFilePath )
import Module
import Name
import Packages ( trusted, getPackageDetails, getInstalledPackageDetails,
                  listVisibleModuleNames, pprFlag )
import IfaceSyn ( showToHeader )
import PprTyThing
import PrelNames
import RdrName ( getGRE_NameQualifier_maybes, getRdrName )
import SrcLoc
import qualified Lexer

import StringBuffer
import Outputable hiding ( printForUser, printForUserPartWay )

-- Other random utilities
import BasicTypes hiding ( isTopLevel )
import Config
import Digraph
import Encoding
import FastString
import Linker
import Maybes ( orElse, expectJust )
import NameSet
import Panic hiding ( showException )
import Util
import qualified GHC.LanguageExtensions as LangExt

-- Haskell Libraries
import System.Console.Haskeline as Haskeline

import Control.Applicative hiding (empty)
import Control.DeepSeq (deepseq)
import Control.Monad as Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Function
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef, writeIORef )
import Data.List ( find, group, intercalate, intersperse, isPrefixOf, nub,
                   partition, sort, sortBy )
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Map as M
import Data.Time.LocalTime ( getZonedTime )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Data.Version ( showVersion )

import Exception hiding (catch)
import Foreign hiding (void)
import GHC.Stack hiding (SrcLoc(..))

import System.Directory
import System.Environment
import System.Exit ( exitWith, ExitCode(..) )
import System.FilePath
import System.Info
import System.IO
import System.IO.Error
import System.IO.Unsafe ( unsafePerformIO )
import System.Process
import Text.Printf
import Text.Read ( readMaybe )
import Text.Read.Lex (isSymbolChar)

import Unsafe.Coerce

#if !defined(mingw32_HOST_OS)
import System.Posix hiding ( getEnv )
#else
import qualified System.Win32
#endif

import GHC.IO.Exception ( IOErrorType(InvalidArgument) )
import GHC.IO.Handle ( hFlushAll )
import GHC.TopHandler ( topHandler )

-----------------------------------------------------------------------------

data GhciSettings = GhciSettings {
        availableCommands :: [Command],
        shortHelpText     :: String,
        fullHelpText      :: String,
        defPrompt         :: PromptFunction,
        defPromptCont     :: PromptFunction
    }

defaultGhciSettings :: GhciSettings
defaultGhciSettings =
    GhciSettings {
        availableCommands = ghciCommands,
        shortHelpText     = defShortHelpText,
        defPrompt         = default_prompt,
        defPromptCont     = default_prompt_cont,
        fullHelpText      = defFullHelpText
    }

ghciWelcomeMsg :: String
ghciWelcomeMsg = "GHCi, version " ++ cProjectVersion ++
                 ": http://www.haskell.org/ghc/  :? for help"

ghciCommands :: [Command]
ghciCommands = map mkCmd [
  -- Hugs users are accustomed to :e, so make sure it doesn't overlap
  ("?",         keepGoing help,                 noCompletion),
  ("add",       keepGoingPaths addModule,       completeFilename),
  ("abandon",   keepGoing abandonCmd,           noCompletion),
  ("break",     keepGoing breakCmd,             completeIdentifier),
  ("back",      keepGoing backCmd,              noCompletion),
  ("browse",    keepGoing' (browseCmd False),   completeModule),
  ("browse!",   keepGoing' (browseCmd True),    completeModule),
  ("cd",        keepGoing' changeDirectory,     completeFilename),
  ("check",     keepGoing' checkModule,         completeHomeModule),
  ("continue",  keepGoing continueCmd,          noCompletion),
  ("cmd",       keepGoing cmdCmd,               completeExpression),
  ("ctags",     keepGoing createCTagsWithLineNumbersCmd, completeFilename),
  ("ctags!",    keepGoing createCTagsWithRegExesCmd, completeFilename),
  ("def",       keepGoing (defineMacro False),  completeExpression),
  ("def!",      keepGoing (defineMacro True),   completeExpression),
  ("delete",    keepGoing deleteCmd,            noCompletion),
  ("edit",      keepGoing' editFile,            completeFilename),
  ("etags",     keepGoing createETagsFileCmd,   completeFilename),
  ("force",     keepGoing forceCmd,             completeExpression),
  ("forward",   keepGoing forwardCmd,           noCompletion),
  ("help",      keepGoing help,                 noCompletion),
  ("history",   keepGoing historyCmd,           noCompletion),
  ("info",      keepGoing' (info False),        completeIdentifier),
  ("info!",     keepGoing' (info True),         completeIdentifier),
  ("issafe",    keepGoing' isSafeCmd,           completeModule),
  ("kind",      keepGoing' (kindOfType False),  completeIdentifier),
  ("kind!",     keepGoing' (kindOfType True),   completeIdentifier),
  ("load",      keepGoingPaths loadModule_,     completeHomeModuleOrFile),
  ("load!",     keepGoingPaths loadModuleDefer, completeHomeModuleOrFile),
  ("list",      keepGoing' listCmd,             noCompletion),
  ("module",    keepGoing moduleCmd,            completeSetModule),
  ("main",      keepGoing runMain,              completeFilename),
  ("print",     keepGoing printCmd,             completeExpression),
  ("quit",      quit,                           noCompletion),
  ("reload",    keepGoing' reloadModule,        noCompletion),
  ("reload!",   keepGoing' reloadModuleDefer,   noCompletion),
  ("run",       keepGoing runRun,               completeFilename),
  ("script",    keepGoing' scriptCmd,           completeFilename),
  ("set",       keepGoing setCmd,               completeSetOptions),
  ("seti",      keepGoing setiCmd,              completeSeti),
  ("show",      keepGoing showCmd,              completeShowOptions),
  ("showi",     keepGoing showiCmd,             completeShowiOptions),
  ("sprint",    keepGoing sprintCmd,            completeExpression),
  ("step",      keepGoing stepCmd,              completeIdentifier),
  ("steplocal", keepGoing stepLocalCmd,         completeIdentifier),
  ("stepmodule",keepGoing stepModuleCmd,        completeIdentifier),
  ("type",      keepGoing' typeOfExpr,          completeExpression),
  ("trace",     keepGoing traceCmd,             completeExpression),
  ("undef",     keepGoing undefineMacro,        completeMacro),
  ("unset",     keepGoing unsetOptions,         completeSetOptions),
  ("where",     keepGoing whereCmd,             noCompletion)
  ] ++ map mkCmdHidden [ -- hidden commands
  ("all-types", keepGoing' allTypesCmd),
  ("complete",  keepGoing completeCmd),
  ("loc-at",    keepGoing' locAtCmd),
  ("type-at",   keepGoing' typeAtCmd),
  ("uses",      keepGoing' usesCmd)
  ]
 where
  mkCmd (n,a,c) = Command { cmdName = n
                          , cmdAction = a
                          , cmdHidden = False
                          , cmdCompletionFunc = c
                          }

  mkCmdHidden (n,a) = Command { cmdName = n
                              , cmdAction = a
                              , cmdHidden = True
                              , cmdCompletionFunc = noCompletion
                              }

-- We initialize readline (in the interactiveUI function) to use
-- word_break_chars as the default set of completion word break characters.
-- This can be overridden for a particular command (for example, filename
-- expansion shouldn't consider '/' to be a word break) by setting the third
-- entry in the Command tuple above.
--
-- NOTE: in order for us to override the default correctly, any custom entry
-- must be a SUBSET of word_break_chars.
word_break_chars :: String
word_break_chars = spaces ++ specials ++ symbols

symbols, specials, spaces :: String
symbols = "!#$%&*+/<=>?@\\^|-~"
specials = "(),;[]`{}"
spaces = " \t\n"

flagWordBreakChars :: String
flagWordBreakChars = " \t\n"


keepGoing :: (String -> GHCi ()) -> (String -> InputT GHCi Bool)
keepGoing a str = keepGoing' (lift . a) str

keepGoing' :: Monad m => (String -> m ()) -> String -> m Bool
keepGoing' a str = a str >> return False

keepGoingPaths :: ([FilePath] -> InputT GHCi ()) -> (String -> InputT GHCi Bool)
keepGoingPaths a str
 = do case toArgs str of
          Left err -> liftIO $ hPutStrLn stderr err
          Right args -> a args
      return False

defShortHelpText :: String
defShortHelpText = "use :? for help.\n"

defFullHelpText :: String
defFullHelpText =
  " Commands available from the prompt:\n" ++
  "\n" ++
  "   <statement>                 evaluate/run <statement>\n" ++
  "   :                           repeat last command\n" ++
  "   :{\\n ..lines.. \\n:}\\n       multiline command\n" ++
  "   :add [*]<module> ...        add module(s) to the current target set\n" ++
  "   :browse[!] [[*]<mod>]       display the names defined by module <mod>\n" ++
  "                               (!: more details; *: all top-level names)\n" ++
  "   :cd <dir>                   change directory to <dir>\n" ++
  "   :cmd <expr>                 run the commands returned by <expr>::IO String\n" ++
  "   :complete <dom> [<rng>] <s> list completions for partial input string\n" ++
  "   :ctags[!] [<file>]          create tags file <file> for Vi (default: \"tags\")\n" ++
  "                               (!: use regex instead of line number)\n" ++
  "   :def <cmd> <expr>           define command :<cmd> (later defined command has\n" ++
  "                               precedence, ::<cmd> is always a builtin command)\n" ++
  "   :edit <file>                edit file\n" ++
  "   :edit                       edit last module\n" ++
  "   :etags [<file>]             create tags file <file> for Emacs (default: \"TAGS\")\n" ++
  "   :help, :?                   display this list of commands\n" ++
  "   :info[!] [<name> ...]       display information about the given names\n" ++
  "                               (!: do not filter instances)\n" ++
  "   :issafe [<mod>]             display safe haskell information of module <mod>\n" ++
  "   :kind[!] <type>             show the kind of <type>\n" ++
  "                               (!: also print the normalised type)\n" ++
  "   :load[!] [*]<module> ...    load module(s) and their dependents\n" ++
  "                               (!: defer type errors)\n" ++
  "   :main [<arguments> ...]     run the main function with the given arguments\n" ++
  "   :module [+/-] [*]<mod> ...  set the context for expression evaluation\n" ++
  "   :quit                       exit GHCi\n" ++
  "   :reload[!]                  reload the current module set\n" ++
  "                               (!: defer type errors)\n" ++
  "   :run function [<arguments> ...] run the function with the given arguments\n" ++
  "   :script <file>              run the script <file>\n" ++
  "   :type <expr>                show the type of <expr>\n" ++
  "   :type +d <expr>             show the type of <expr>, defaulting type variables\n" ++
  "   :type +v <expr>             show the type of <expr>, with its specified tyvars\n" ++
  "   :undef <cmd>                undefine user-defined command :<cmd>\n" ++
  "   :!<command>                 run the shell command <command>\n" ++
  "\n" ++
  " -- Commands for debugging:\n" ++
  "\n" ++
  "   :abandon                    at a breakpoint, abandon current computation\n" ++
  "   :back [<n>]                 go back in the history N steps (after :trace)\n" ++
  "   :break [<mod>] <l> [<col>]  set a breakpoint at the specified location\n" ++
  "   :break <name>               set a breakpoint on the specified function\n" ++
  "   :continue                   resume after a breakpoint\n" ++
  "   :delete <number>            delete the specified breakpoint\n" ++
  "   :delete *                   delete all breakpoints\n" ++
  "   :force <expr>               print <expr>, forcing unevaluated parts\n" ++
  "   :forward [<n>]              go forward in the history N step s(after :back)\n" ++
  "   :history [<n>]              after :trace, show the execution history\n" ++
  "   :list                       show the source code around current breakpoint\n" ++
  "   :list <identifier>          show the source code for <identifier>\n" ++
  "   :list [<module>] <line>     show the source code around line number <line>\n" ++
  "   :print [<name> ...]         show a value without forcing its computation\n" ++
  "   :sprint [<name> ...]        simplified version of :print\n" ++
  "   :step                       single-step after stopping at a breakpoint\n"++
  "   :step <expr>                single-step into <expr>\n"++
  "   :steplocal                  single-step within the current top-level binding\n"++
  "   :stepmodule                 single-step restricted to the current module\n"++
  "   :trace                      trace after stopping at a breakpoint\n"++
  "   :trace <expr>               evaluate <expr> with tracing on (see :history)\n"++

  "\n" ++
  " -- Commands for changing settings:\n" ++
  "\n" ++
  "   :set <option> ...           set options\n" ++
  "   :seti <option> ...          set options for interactive evaluation only\n" ++
  "   :set args <arg> ...         set the arguments returned by System.getArgs\n" ++
  "   :set prog <progname>        set the value returned by System.getProgName\n" ++
  "   :set prompt <prompt>        set the prompt used in GHCi\n" ++
  "   :set prompt-cont <prompt>   set the continuation prompt used in GHCi\n" ++
  "   :set prompt-function <expr> set the function to handle the prompt\n" ++
  "   :set prompt-cont-function <expr>" ++
                     "set the function to handle the continuation prompt\n" ++
  "   :set editor <cmd>           set the command used for :edit\n" ++
  "   :set stop [<n>] <cmd>       set the command to run when a breakpoint is hit\n" ++
  "   :unset <option> ...         unset options\n" ++
  "\n" ++
  "  Options for ':set' and ':unset':\n" ++
  "\n" ++
  "    +m            allow multiline commands\n" ++
  "    +r            revert top-level expressions after each evaluation\n" ++
  "    +s            print timing/memory stats after each evaluation\n" ++
  "    +t            print type after evaluation\n" ++
  "    +c            collect type/location info after loading modules\n" ++
  "    -<flags>      most GHC command line flags can also be set here\n" ++
  "                         (eg. -v2, -XFlexibleInstances, etc.)\n" ++
  "                    for GHCi-specific flags, see User's Guide,\n"++
  "                    Flag reference, Interactive-mode options\n" ++
  "\n" ++
  " -- Commands for displaying information:\n" ++
  "\n" ++
  "   :show bindings              show the current bindings made at the prompt\n" ++
  "   :show breaks                show the active breakpoints\n" ++
  "   :show context               show the breakpoint context\n" ++
  "   :show imports               show the current imports\n" ++
  "   :show linker                show current linker state\n" ++
  "   :show modules               show the currently loaded modules\n" ++
  "   :show packages              show the currently active package flags\n" ++
  "   :show paths                 show the currently active search paths\n" ++
  "   :show language              show the currently active language flags\n" ++
  "   :show <setting>             show value of <setting>, which is one of\n" ++
  "                                  [args, prog, editor, stop]\n" ++
  "   :showi language             show language flags for interactive evaluation\n" ++
  "\n"

findEditor :: IO String
findEditor = do
  getEnv "EDITOR"
    `catchIO` \_ -> do
#if defined(mingw32_HOST_OS)
        win <- System.Win32.getWindowsDirectory
        return (win </> "notepad.exe")
#else
        return ""
#endif

default_progname, default_stop :: String
default_progname = "<interactive>"
default_stop = ""

default_prompt, default_prompt_cont :: PromptFunction
default_prompt = generatePromptFunctionFromString "%s> "
default_prompt_cont = generatePromptFunctionFromString "%s| "

default_args :: [String]
default_args = []

interactiveUI :: GhciSettings -> [(FilePath, Maybe Phase)] -> Maybe [String]
              -> Ghc ()
interactiveUI config srcs maybe_exprs = do
   -- HACK! If we happen to get into an infinite loop (eg the user
   -- types 'let x=x in x' at the prompt), then the thread will block
   -- on a blackhole, and become unreachable during GC.  The GC will
   -- detect that it is unreachable and send it the NonTermination
   -- exception.  However, since the thread is unreachable, everything
   -- it refers to might be finalized, including the standard Handles.
   -- This sounds like a bug, but we don't have a good solution right
   -- now.
   _ <- liftIO $ newStablePtr stdin
   _ <- liftIO $ newStablePtr stdout
   _ <- liftIO $ newStablePtr stderr

    -- Initialise buffering for the *interpreted* I/O system
   (nobuffering, flush) <- initInterpBuffering

   -- The initial set of DynFlags used for interactive evaluation is the same
   -- as the global DynFlags, plus -XExtendedDefaultRules and
   -- -XNoMonomorphismRestriction.
   dflags <- getDynFlags
   let dflags' = (`xopt_set` LangExt.ExtendedDefaultRules)
               . (`xopt_unset` LangExt.MonomorphismRestriction)
               $ dflags
   GHC.setInteractiveDynFlags dflags'

   lastErrLocationsRef <- liftIO $ newIORef []
   progDynFlags <- GHC.getProgramDynFlags
   _ <- GHC.setProgramDynFlags $
      progDynFlags { log_action = ghciLogAction lastErrLocationsRef }

   when (isNothing maybe_exprs) $ do
        -- Only for GHCi (not runghc and ghc -e):

        -- Turn buffering off for the compiled program's stdout/stderr
        turnOffBuffering_ nobuffering
        -- Turn buffering off for GHCi's stdout
        liftIO $ hFlush stdout
        liftIO $ hSetBuffering stdout NoBuffering
        -- We don't want the cmd line to buffer any input that might be
        -- intended for the program, so unbuffer stdin.
        liftIO $ hSetBuffering stdin NoBuffering
        liftIO $ hSetBuffering stderr NoBuffering
#if defined(mingw32_HOST_OS)
        -- On Unix, stdin will use the locale encoding.  The IO library
        -- doesn't do this on Windows (yet), so for now we use UTF-8,
        -- for consistency with GHC 6.10 and to make the tests work.
        liftIO $ hSetEncoding stdin utf8
#endif

   default_editor <- liftIO $ findEditor
   eval_wrapper <- mkEvalWrapper default_progname default_args
   let prelude_import = simpleImportDecl preludeModuleName
   startGHCi (runGHCi srcs maybe_exprs)
        GHCiState{ progname           = default_progname,
                   args               = default_args,
                   evalWrapper        = eval_wrapper,
                   prompt             = default_prompt,
                   prompt_cont        = default_prompt_cont,
                   stop               = default_stop,
                   editor             = default_editor,
                   options            = [],
                   -- We initialize line number as 0, not 1, because we use
                   -- current line number while reporting errors which is
                   -- incremented after reading a line.
                   line_number        = 0,
                   break_ctr          = 0,
                   breaks             = [],
                   tickarrays         = emptyModuleEnv,
                   ghci_commands      = availableCommands config,
                   ghci_macros        = [],
                   last_command       = Nothing,
                   cmdqueue           = [],
                   remembered_ctx     = [],
                   transient_ctx      = [],
                   extra_imports      = [],
                   prelude_imports    = [prelude_import],
                   ghc_e              = isJust maybe_exprs,
                   short_help         = shortHelpText config,
                   long_help          = fullHelpText config,
                   lastErrorLocations = lastErrLocationsRef,
                   mod_infos          = M.empty,
                   flushStdHandles    = flush,
                   noBuffering        = nobuffering
                 }

   return ()

resetLastErrorLocations :: GHCi ()
resetLastErrorLocations = do
    st <- getGHCiState
    liftIO $ writeIORef (lastErrorLocations st) []

ghciLogAction :: IORef [(FastString, Int)] ->  LogAction
ghciLogAction lastErrLocations dflags flag severity srcSpan style msg = do
    defaultLogAction dflags flag severity srcSpan style msg
    case severity of
        SevError -> case srcSpan of
            RealSrcSpan rsp -> modifyIORef lastErrLocations
                (++ [(srcLocFile (realSrcSpanStart rsp), srcLocLine (realSrcSpanStart rsp))])
            _ -> return ()
        _ -> return ()

withGhcAppData :: (FilePath -> IO a) -> IO a -> IO a
withGhcAppData right left = do
    either_dir <- tryIO (getAppUserDataDirectory "ghc")
    case either_dir of
        Right dir ->
            do createDirectoryIfMissing False dir `catchIO` \_ -> return ()
               right dir
        _ -> left

runGHCi :: [(FilePath, Maybe Phase)] -> Maybe [String] -> GHCi ()
runGHCi paths maybe_exprs = do
  dflags <- getDynFlags
  let
   ignore_dot_ghci = gopt Opt_IgnoreDotGhci dflags

   current_dir = return (Just ".ghci")

   app_user_dir = liftIO $ withGhcAppData
                    (\dir -> return (Just (dir </> "ghci.conf")))
                    (return Nothing)

   home_dir = do
    either_dir <- liftIO $ tryIO (getEnv "HOME")
    case either_dir of
      Right home -> return (Just (home </> ".ghci"))
      _ -> return Nothing

   canonicalizePath' :: FilePath -> IO (Maybe FilePath)
   canonicalizePath' fp = liftM Just (canonicalizePath fp)
                `catchIO` \_ -> return Nothing

   sourceConfigFile :: FilePath -> GHCi ()
   sourceConfigFile file = do
     exists <- liftIO $ doesFileExist file
     when exists $ do
       either_hdl <- liftIO $ tryIO (openFile file ReadMode)
       case either_hdl of
         Left _e   -> return ()
         -- NOTE: this assumes that runInputT won't affect the terminal;
         -- can we assume this will always be the case?
         -- This would be a good place for runFileInputT.
         Right hdl ->
             do runInputTWithPrefs defaultPrefs defaultSettings $
                          runCommands $ fileLoop hdl
                liftIO (hClose hdl `catchIO` \_ -> return ())
                -- Don't print a message if this is really ghc -e (#11478).
                -- Also, let the user silence the message with -v0
                -- (the default verbosity in GHCi is 1).
                when (isNothing maybe_exprs && verbosity dflags > 0) $
                  liftIO $ putStrLn ("Loaded GHCi configuration from " ++ file)

  --

  setGHCContextFromGHCiState

  dot_cfgs <- if ignore_dot_ghci then return [] else do
    dot_files <- catMaybes <$> sequence [ current_dir, app_user_dir, home_dir ]
    liftIO $ filterM checkFileAndDirPerms dot_files
  mdot_cfgs <- liftIO $ mapM canonicalizePath' dot_cfgs

  let arg_cfgs = reverse $ ghciScripts dflags
    -- -ghci-script are collected in reverse order
    -- We don't require that a script explicitly added by -ghci-script
    -- is owned by the current user. (#6017)
  mapM_ sourceConfigFile $ nub $ (catMaybes mdot_cfgs) ++ arg_cfgs
    -- nub, because we don't want to read .ghci twice if the CWD is $HOME.

  -- Perform a :load for files given on the GHCi command line
  -- When in -e mode, if the load fails then we want to stop
  -- immediately rather than going on to evaluate the expression.
  when (not (null paths)) $ do
     ok <- ghciHandle (\e -> do showException e; return Failed) $
                -- TODO: this is a hack.
                runInputTWithPrefs defaultPrefs defaultSettings $
                    loadModule paths
     when (isJust maybe_exprs && failed ok) $
        liftIO (exitWith (ExitFailure 1))

  installInteractivePrint (interactivePrint dflags) (isJust maybe_exprs)

  -- if verbosity is greater than 0, or we are connected to a
  -- terminal, display the prompt in the interactive loop.
  is_tty <- liftIO (hIsTerminalDevice stdin)
  let show_prompt = verbosity dflags > 0 || is_tty

  -- reset line number
  modifyGHCiState $ \st -> st{line_number=0}

  case maybe_exprs of
        Nothing ->
          do
            -- enter the interactive loop
            runGHCiInput $ runCommands $ nextInputLine show_prompt is_tty
        Just exprs -> do
            -- just evaluate the expression we were given
            enqueueCommands exprs
            let hdle e = do st <- getGHCiState
                            -- flush the interpreter's stdout/stderr on exit (#3890)
                            flushInterpBuffers
                            -- Jump through some hoops to get the
                            -- current progname in the exception text:
                            -- <progname>: <exception>
                            liftIO $ withProgName (progname st)
                                   $ topHandler e
                                   -- this used to be topHandlerFastExit, see #2228
            runInputTWithPrefs defaultPrefs defaultSettings $ do
                -- make `ghc -e` exit nonzero on invalid input, see Trac #7962
                _ <- runCommands' hdle
                     (Just $ hdle (toException $ ExitFailure 1) >> return ())
                     (return Nothing)
                return ()

  -- and finally, exit
  liftIO $ when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."

runGHCiInput :: InputT GHCi a -> GHCi a
runGHCiInput f = do
    dflags <- getDynFlags
    let ghciHistory = gopt Opt_GhciHistory dflags
    let localGhciHistory = gopt Opt_LocalGhciHistory dflags
    currentDirectory <- liftIO $ getCurrentDirectory

    histFile <- case (ghciHistory, localGhciHistory) of
      (True, True) -> return (Just (currentDirectory </> ".ghci_history"))
      (True, _) -> liftIO $ withGhcAppData
        (\dir -> return (Just (dir </> "ghci_history"))) (return Nothing)
      _ -> return Nothing

    runInputT
        (setComplete ghciCompleteWord $ defaultSettings {historyFile = histFile})
        f

-- | How to get the next input line from the user
nextInputLine :: Bool -> Bool -> InputT GHCi (Maybe String)
nextInputLine show_prompt is_tty
  | is_tty = do
    prmpt <- if show_prompt then lift mkPrompt else return ""
    r <- getInputLine prmpt
    incrementLineNo
    return r
  | otherwise = do
    when show_prompt $ lift mkPrompt >>= liftIO . putStr
    fileLoop stdin

-- NOTE: We only read .ghci files if they are owned by the current user,
-- and aren't world writable (files owned by root are ok, see #9324).
-- Otherwise, we could be accidentally running code planted by
-- a malicious third party.

-- Furthermore, We only read ./.ghci if . is owned by the current user
-- and isn't writable by anyone else.  I think this is sufficient: we
-- don't need to check .. and ../.. etc. because "."  always refers to
-- the same directory while a process is running.

checkFileAndDirPerms :: FilePath -> IO Bool
checkFileAndDirPerms file = do
  file_ok <- checkPerms file
  -- Do not check dir perms when .ghci doesn't exist, otherwise GHCi will
  -- print some confusing and useless warnings in some cases (e.g. in
  -- travis). Note that we can't add a test for this, as all ghci tests should
  -- run with -ignore-dot-ghci, which means we never get here.
  if file_ok then checkPerms (getDirectory file) else return False
  where
  getDirectory f = case takeDirectory f of
    "" -> "."
    d -> d

checkPerms :: FilePath -> IO Bool
#if defined(mingw32_HOST_OS)
checkPerms _ = return True
#else
checkPerms file =
  handleIO (\_ -> return False) $ do
    st <- getFileStatus file
    me <- getRealUserID
    let mode = System.Posix.fileMode st
        ok = (fileOwner st == me || fileOwner st == 0) &&
             groupWriteMode /= mode `intersectFileModes` groupWriteMode &&
             otherWriteMode /= mode `intersectFileModes` otherWriteMode
    unless ok $
      -- #8248: Improving warning to include a possible fix.
      putStrLn $ "*** WARNING: " ++ file ++
                 " is writable by someone else, IGNORING!" ++
                 "\nSuggested fix: execute 'chmod go-w " ++ file ++ "'"
    return ok
#endif

incrementLineNo :: InputT GHCi ()
incrementLineNo = modifyGHCiState incLineNo
  where
    incLineNo st = st { line_number = line_number st + 1 }

fileLoop :: Handle -> InputT GHCi (Maybe String)
fileLoop hdl = do
   l <- liftIO $ tryIO $ hGetLine hdl
   case l of
        Left e | isEOFError e              -> return Nothing
               | -- as we share stdin with the program, the program
                 -- might have already closed it, so we might get a
                 -- handle-closed exception. We therefore catch that
                 -- too.
                 isIllegalOperation e      -> return Nothing
               | InvalidArgument <- etype  -> return Nothing
               | otherwise                 -> liftIO $ ioError e
                where etype = ioeGetErrorType e
                -- treat InvalidArgument in the same way as EOF:
                -- this can happen if the user closed stdin, or
                -- perhaps did getContents which closes stdin at
                -- EOF.
        Right l' -> do
           incrementLineNo
           return (Just l')

formatCurrentTime :: String -> IO String
formatCurrentTime format =
  getZonedTime >>= return . (formatTime defaultTimeLocale format)

getUserName :: IO String
getUserName = do
#if defined(mingw32_HOST_OS)
  getEnv "USERNAME"
    `catchIO` \e -> do
      putStrLn $ show e
      return ""
#else
  getLoginName
#endif

getInfoForPrompt :: GHCi (SDoc, [String], Int)
getInfoForPrompt = do
  st <- getGHCiState
  imports <- GHC.getContext
  resumes <- GHC.getResumeContext

  context_bit <-
        case resumes of
            [] -> return empty
            r:_ -> do
                let ix = GHC.resumeHistoryIx r
                if ix == 0
                   then return (brackets (ppr (GHC.resumeSpan r)) <> space)
                   else do
                        let hist = GHC.resumeHistory r !! (ix-1)
                        pan <- GHC.getHistorySpan hist
                        return (brackets (ppr (negate ix) <> char ':'
                                          <+> ppr pan) <> space)

  let
        dots | _:rs <- resumes, not (null rs) = text "... "
             | otherwise = empty

        rev_imports = reverse imports -- rightmost are the most recent

        myIdeclName d | Just m <- ideclAs d = unLoc m
                      | otherwise           = unLoc (ideclName d)

        modules_names =
             ['*':(moduleNameString m) | IIModule m <- rev_imports] ++
             [moduleNameString (myIdeclName d) | IIDecl d <- rev_imports]
        line = 1 + line_number st

  return (dots <> context_bit, modules_names, line)

parseCallEscape :: String -> (String, String)
parseCallEscape s
  | not (all isSpace beforeOpen) = ("", "")
  | null sinceOpen               = ("", "")
  | null sinceClosed             = ("", "")
  | null cmd                     = ("", "")
  | otherwise                    = (cmd, tail sinceClosed)
  where
    (beforeOpen, sinceOpen) = span (/='(') s
    (cmd, sinceClosed) = span (/=')') (tail sinceOpen)

checkPromptStringForErrors :: String -> Maybe String
checkPromptStringForErrors ('%':'c':'a':'l':'l':xs) =
  case parseCallEscape xs of
    ("", "") -> Just ("Incorrect %call syntax. " ++
                      "Should be %call(a command and arguments).")
    (_, afterClosed) -> checkPromptStringForErrors afterClosed
checkPromptStringForErrors ('%':'%':xs) = checkPromptStringForErrors xs
checkPromptStringForErrors (_:xs) = checkPromptStringForErrors xs
checkPromptStringForErrors "" = Nothing

generatePromptFunctionFromString :: String -> PromptFunction
generatePromptFunctionFromString promptS = \_ _ -> do
    (context, modules_names, line) <- getInfoForPrompt

    let
        processString :: String -> GHCi SDoc
        processString ('%':'s':xs) =
            liftM2 (<>) (return modules_list) (processString xs)
            where
              modules_list = context <> modules_bit
              modules_bit = hsep $ map text modules_names
        processString ('%':'l':xs) =
            liftM2 (<>) (return $ ppr line) (processString xs)
        processString ('%':'d':xs) =
            liftM2 (<>) (liftM text formatted_time) (processString xs)
            where
              formatted_time = liftIO $ formatCurrentTime "%a %b %d"
        processString ('%':'t':xs) =
            liftM2 (<>) (liftM text formatted_time) (processString xs)
            where
              formatted_time = liftIO $ formatCurrentTime "%H:%M:%S"
        processString ('%':'T':xs) = do
            liftM2 (<>) (liftM text formatted_time) (processString xs)
            where
              formatted_time = liftIO $ formatCurrentTime "%I:%M:%S"
        processString ('%':'@':xs) = do
            liftM2 (<>) (liftM text formatted_time) (processString xs)
            where
              formatted_time = liftIO $ formatCurrentTime "%I:%M %P"
        processString ('%':'A':xs) = do
            liftM2 (<>) (liftM text formatted_time) (processString xs)
            where
              formatted_time = liftIO $ formatCurrentTime "%H:%M"
        processString ('%':'u':xs) =
            liftM2 (<>) (liftM text user_name) (processString xs)
            where
              user_name = liftIO $ getUserName
        processString ('%':'w':xs) =
            liftM2 (<>) (liftM text current_directory) (processString xs)
            where
              current_directory = liftIO $ getCurrentDirectory
        processString ('%':'o':xs) =
            liftM ((text os) <>) (processString xs)
        processString ('%':'a':xs) =
            liftM ((text arch) <>) (processString xs)
        processString ('%':'N':xs) =
            liftM ((text compilerName) <>) (processString xs)
        processString ('%':'V':xs) =
            liftM ((text $ showVersion compilerVersion) <>) (processString xs)
        processString ('%':'c':'a':'l':'l':xs) = do
            respond <- liftIO $ do
                (code, out, err) <-
                    readProcessWithExitCode
                    (head list_words) (tail list_words) ""
                    `catchIO` \e -> return (ExitFailure 1, "", show e)
                case code of
                    ExitSuccess -> return out
                    _ -> do
                        hPutStrLn stderr err
                        return ""
            liftM ((text respond) <>) (processString afterClosed)
            where
              (cmd, afterClosed) = parseCallEscape xs
              list_words = words cmd
        processString ('%':'%':xs) =
            liftM ((char '%') <>) (processString xs)
        processString (x:xs) =
            liftM (char x <>) (processString xs)
        processString "" =
            return empty

    processString promptS

mkPrompt :: GHCi String
mkPrompt = do
  st <- getGHCiState
  dflags <- getDynFlags
  (context, modules_names, line) <- getInfoForPrompt

  prompt_string <- (prompt st) modules_names line
  let prompt_doc = context <> prompt_string

  return (showSDoc dflags prompt_doc)

queryQueue :: GHCi (Maybe String)
queryQueue = do
  st <- getGHCiState
  case cmdqueue st of
    []   -> return Nothing
    c:cs -> do setGHCiState st{ cmdqueue = cs }
               return (Just c)

-- Reconfigurable pretty-printing Ticket #5461
installInteractivePrint :: Maybe String -> Bool -> GHCi ()
installInteractivePrint Nothing _  = return ()
installInteractivePrint (Just ipFun) exprmode = do
  ok <- trySuccess $ do
                (name:_) <- GHC.parseName ipFun
                modifySession (\he -> let new_ic = setInteractivePrintName (hsc_IC he) name
                                      in he{hsc_IC = new_ic})
                return Succeeded

  when (failed ok && exprmode) $ liftIO (exitWith (ExitFailure 1))

-- | The main read-eval-print loop
runCommands :: InputT GHCi (Maybe String) -> InputT GHCi ()
runCommands gCmd = runCommands' handler Nothing gCmd >> return ()

runCommands' :: (SomeException -> GHCi Bool) -- ^ Exception handler
             -> Maybe (GHCi ()) -- ^ Source error handler
             -> InputT GHCi (Maybe String)
             -> InputT GHCi (Maybe Bool)
         -- We want to return () here, but have to return (Maybe Bool)
         -- because gmask is not polymorphic enough: we want to use
         -- unmask at two different types.
runCommands' eh sourceErrorHandler gCmd = gmask $ \unmask -> do
    b <- ghandle (\e -> case fromException e of
                          Just UserInterrupt -> return $ Just False
                          _ -> case fromException e of
                                 Just ghce ->
                                   do liftIO (print (ghce :: GhcException))
                                      return Nothing
                                 _other ->
                                   liftIO (Exception.throwIO e))
            (unmask $ runOneCommand eh gCmd)
    case b of
      Nothing -> return Nothing
      Just success -> do
        unless success $ maybe (return ()) lift sourceErrorHandler
        unmask $ runCommands' eh sourceErrorHandler gCmd

-- | Evaluate a single line of user input (either :<command> or Haskell code).
-- A result of Nothing means there was no more input to process.
-- Otherwise the result is Just b where b is True if the command succeeded;
-- this is relevant only to ghc -e, which will exit with status 1
-- if the command was unsuccessful. GHCi will continue in either case.
runOneCommand :: (SomeException -> GHCi Bool) -> InputT GHCi (Maybe String)
            -> InputT GHCi (Maybe Bool)
runOneCommand eh gCmd = do
  -- run a previously queued command if there is one, otherwise get new
  -- input from user
  mb_cmd0 <- noSpace (lift queryQueue)
  mb_cmd1 <- maybe (noSpace gCmd) (return . Just) mb_cmd0
  case mb_cmd1 of
    Nothing -> return Nothing
    Just c  -> ghciHandle (\e -> lift $ eh e >>= return . Just) $
             handleSourceError printErrorAndFail
               (doCommand c)
               -- source error's are handled by runStmt
               -- is the handler necessary here?
  where
    printErrorAndFail err = do
        GHC.printException err
        return $ Just False     -- Exit ghc -e, but not GHCi

    noSpace q = q >>= maybe (return Nothing)
                            (\c -> case removeSpaces c of
                                     ""   -> noSpace q
                                     ":{" -> multiLineCmd q
                                     _    -> return (Just c) )
    multiLineCmd q = do
      st <- getGHCiState
      let p = prompt st
      setGHCiState st{ prompt = prompt_cont st }
      mb_cmd <- collectCommand q "" `GHC.gfinally`
                modifyGHCiState (\st' -> st' { prompt = p })
      return mb_cmd
    -- we can't use removeSpaces for the sublines here, so
    -- multiline commands are somewhat more brittle against
    -- fileformat errors (such as \r in dos input on unix),
    -- we get rid of any extra spaces for the ":}" test;
    -- we also avoid silent failure if ":}" is not found;
    -- and since there is no (?) valid occurrence of \r (as
    -- opposed to its String representation, "\r") inside a
    -- ghci command, we replace any such with ' ' (argh:-(
    collectCommand q c = q >>=
      maybe (liftIO (ioError collectError))
            (\l->if removeSpaces l == ":}"
                 then return (Just c)
                 else collectCommand q (c ++ "\n" ++ map normSpace l))
      where normSpace '\r' = ' '
            normSpace   x  = x
    -- SDM (2007-11-07): is userError the one to use here?
    collectError = userError "unterminated multiline command :{ .. :}"

    -- | Handle a line of input
    doCommand :: String -> InputT GHCi (Maybe Bool)

    -- command
    doCommand stmt | (':' : cmd) <- removeSpaces stmt = do
      result <- specialCommand cmd
      case result of
        True -> return Nothing
        _    -> return $ Just True

    -- haskell
    doCommand stmt = do
      -- if 'stmt' was entered via ':{' it will contain '\n's
      let stmt_nl_cnt = length [ () | '\n' <- stmt ]
      ml <- lift $ isOptionSet Multiline
      if ml && stmt_nl_cnt == 0 -- don't trigger automatic multi-line mode for ':{'-multiline input
        then do
          fst_line_num <- line_number <$> getGHCiState
          mb_stmt <- checkInputForLayout stmt gCmd
          case mb_stmt of
            Nothing      -> return $ Just True
            Just ml_stmt -> do
              -- temporarily compensate line-number for multi-line input
              result <- timeIt runAllocs $ lift $
                runStmtWithLineNum fst_line_num ml_stmt GHC.RunToCompletion
              return $ Just (runSuccess result)
        else do -- single line input and :{ - multiline input
          last_line_num <- line_number <$> getGHCiState
          -- reconstruct first line num from last line num and stmt
          let fst_line_num | stmt_nl_cnt > 0 = last_line_num - (stmt_nl_cnt2 + 1)
                           | otherwise = last_line_num -- single line input
              stmt_nl_cnt2 = length [ () | '\n' <- stmt' ]
              stmt' = dropLeadingWhiteLines stmt -- runStmt doesn't like leading empty lines
          -- temporarily compensate line-number for multi-line input
          result <- timeIt runAllocs $ lift $
            runStmtWithLineNum fst_line_num stmt' GHC.RunToCompletion
          return $ Just (runSuccess result)

    -- runStmt wrapper for temporarily overridden line-number
    runStmtWithLineNum :: Int -> String -> SingleStep
                       -> GHCi (Maybe GHC.ExecResult)
    runStmtWithLineNum lnum stmt step = do
        st0 <- getGHCiState
        setGHCiState st0 { line_number = lnum }
        result <- runStmt stmt step
        -- restore original line_number
        getGHCiState >>= \st -> setGHCiState st { line_number = line_number st0 }
        return result

    -- note: this is subtly different from 'unlines . dropWhile (all isSpace) . lines'
    dropLeadingWhiteLines s | (l0,'\n':r) <- break (=='\n') s
                            , all isSpace l0 = dropLeadingWhiteLines r
                            | otherwise = s


-- #4316
-- lex the input.  If there is an unclosed layout context, request input
checkInputForLayout :: String -> InputT GHCi (Maybe String)
                    -> InputT GHCi (Maybe String)
checkInputForLayout stmt getStmt = do
   dflags' <- getDynFlags
   let dflags = xopt_set dflags' LangExt.AlternativeLayoutRule
   st0 <- getGHCiState
   let buf'   =  stringToStringBuffer stmt
       loc    = mkRealSrcLoc (fsLit (progname st0)) (line_number st0) 1
       pstate = Lexer.mkPState dflags buf' loc
   case Lexer.unP goToEnd pstate of
     (Lexer.POk _ False) -> return $ Just stmt
     _other              -> do
       st1 <- getGHCiState
       let p = prompt st1
       setGHCiState st1{ prompt = prompt_cont st1 }
       mb_stmt <- ghciHandle (\ex -> case fromException ex of
                            Just UserInterrupt -> return Nothing
                            _ -> case fromException ex of
                                 Just ghce ->
                                   do liftIO (print (ghce :: GhcException))
                                      return Nothing
                                 _other -> liftIO (Exception.throwIO ex))
                     getStmt
       modifyGHCiState (\st' -> st' { prompt = p })
       -- the recursive call does not recycle parser state
       -- as we use a new string buffer
       case mb_stmt of
         Nothing  -> return Nothing
         Just str -> if str == ""
           then return $ Just stmt
           else do
             checkInputForLayout (stmt++"\n"++str) getStmt
     where goToEnd = do
             eof <- Lexer.nextIsEOF
             if eof
               then Lexer.activeContext
               else Lexer.lexer False return >> goToEnd

enqueueCommands :: [String] -> GHCi ()
enqueueCommands cmds = do
  -- make sure we force any exceptions in the commands while we're
  -- still inside the exception handler, otherwise bad things will
  -- happen (see #10501)
  cmds `deepseq` return ()
  modifyGHCiState $ \st -> st{ cmdqueue = cmds ++ cmdqueue st }

-- | Entry point to execute some haskell code from user.
-- The return value True indicates success, as in `runOneCommand`.
runStmt :: String -> SingleStep -> GHCi (Maybe GHC.ExecResult)
runStmt stmt step = do
  dflags <- GHC.getInteractiveDynFlags
  if | GHC.isStmt dflags stmt    -> run_stmt
     | GHC.isImport dflags stmt  -> run_import
     -- Every import declaration should be handled by `run_import`. As GHCi
     -- in general only accepts one command at a time, we simply throw an
     -- exception when the input contains multiple commands of which at least
     -- one is an import command (see #10663).
     | GHC.hasImport dflags stmt -> throwGhcException
       (CmdLineError "error: expecting a single import declaration")
     -- Note: `GHC.isDecl` returns False on input like
     -- `data Infix a b = a :@: b; infixl 4 :@:`
     -- and should therefore not be used here.
     | otherwise                 -> run_decl

  where
    run_import = do
      addImportToContext stmt
      return (Just (GHC.ExecComplete (Right []) 0))

    run_decl =
        do _ <- liftIO $ tryIO $ hFlushAll stdin
           m_result <- GhciMonad.runDecls stmt
           case m_result of
               Nothing     -> return Nothing
               Just result ->
                 Just <$> afterRunStmt (const True)
                            (GHC.ExecComplete (Right result) 0)

    run_stmt =
        do -- In the new IO library, read handles buffer data even if the Handle
           -- is set to NoBuffering.  This causes problems for GHCi where there
           -- are really two stdin Handles.  So we flush any bufferred data in
           -- GHCi's stdin Handle here (only relevant if stdin is attached to
           -- a file, otherwise the read buffer can't be flushed).
           _ <- liftIO $ tryIO $ hFlushAll stdin
           m_result <- GhciMonad.runStmt stmt step
           case m_result of
               Nothing     -> return Nothing
               Just result -> Just <$> afterRunStmt (const True) result

-- | Clean up the GHCi environment after a statement has run
afterRunStmt :: (SrcSpan -> Bool) -> GHC.ExecResult -> GHCi GHC.ExecResult
afterRunStmt step_here run_result = do
  resumes <- GHC.getResumeContext
  case run_result of
     GHC.ExecComplete{..} ->
       case execResult of
          Left ex -> liftIO $ Exception.throwIO ex
          Right names -> do
            show_types <- isOptionSet ShowType
            when show_types $ printTypeOfNames names
     GHC.ExecBreak names mb_info
         | isNothing  mb_info ||
           step_here (GHC.resumeSpan $ head resumes) -> do
               mb_id_loc <- toBreakIdAndLocation mb_info
               let bCmd = maybe "" ( \(_,l) -> onBreakCmd l ) mb_id_loc
               if (null bCmd)
                 then printStoppedAtBreakInfo (head resumes) names
                 else enqueueCommands [bCmd]
               -- run the command set with ":set stop <cmd>"
               st <- getGHCiState
               enqueueCommands [stop st]
               return ()
         | otherwise -> resume step_here GHC.SingleStep >>=
                        afterRunStmt step_here >> return ()

  flushInterpBuffers
  withSignalHandlers $ do
     b <- isOptionSet RevertCAFs
     when b revertCAFs

  return run_result

runSuccess :: Maybe GHC.ExecResult -> Bool
runSuccess run_result
  | Just (GHC.ExecComplete { execResult = Right _ }) <- run_result = True
  | otherwise = False

runAllocs :: Maybe GHC.ExecResult -> Maybe Integer
runAllocs m = do
  res <- m
  case res of
    GHC.ExecComplete{..} -> Just (fromIntegral execAllocation)
    _ -> Nothing

toBreakIdAndLocation ::
  Maybe GHC.BreakInfo -> GHCi (Maybe (Int, BreakLocation))
toBreakIdAndLocation Nothing = return Nothing
toBreakIdAndLocation (Just inf) = do
  let md = GHC.breakInfo_module inf
      nm = GHC.breakInfo_number inf
  st <- getGHCiState
  return $ listToMaybe [ id_loc | id_loc@(_,loc) <- breaks st,
                                  breakModule loc == md,
                                  breakTick loc == nm ]

printStoppedAtBreakInfo :: Resume -> [Name] -> GHCi ()
printStoppedAtBreakInfo res names = do
  printForUser $ pprStopped res
  --  printTypeOfNames session names
  let namesSorted = sortBy compareNames names
  tythings <- catMaybes `liftM` mapM GHC.lookupName namesSorted
  docs <- mapM pprTypeAndContents [i | AnId i <- tythings]
  printForUserPartWay $ vcat docs

printTypeOfNames :: [Name] -> GHCi ()
printTypeOfNames names
 = mapM_ (printTypeOfName ) $ sortBy compareNames names

compareNames :: Name -> Name -> Ordering
n1 `compareNames` n2 = compareWith n1 `compare` compareWith n2
    where compareWith n = (getOccString n, getSrcSpan n)

printTypeOfName :: Name -> GHCi ()
printTypeOfName n
   = do maybe_tything <- GHC.lookupName n
        case maybe_tything of
            Nothing    -> return ()
            Just thing -> printTyThing thing


data MaybeCommand = GotCommand Command | BadCommand | NoLastCommand

-- | Entry point for execution a ':<command>' input from user
specialCommand :: String -> InputT GHCi Bool
specialCommand ('!':str) = lift $ shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  maybe_cmd <- lift $ lookupCommand cmd
  htxt <- short_help <$> getGHCiState
  case maybe_cmd of
    GotCommand cmd -> (cmdAction cmd) (dropWhile isSpace rest)
    BadCommand ->
      do liftIO $ hPutStr stdout ("unknown command ':" ++ cmd ++ "'\n"
                           ++ htxt)
         return False
    NoLastCommand ->
      do liftIO $ hPutStr stdout ("there is no last command to perform\n"
                           ++ htxt)
         return False

shellEscape :: String -> GHCi Bool
shellEscape str = liftIO (system str >> return False)

lookupCommand :: String -> GHCi (MaybeCommand)
lookupCommand "" = do
  st <- getGHCiState
  case last_command st of
      Just c -> return $ GotCommand c
      Nothing -> return NoLastCommand
lookupCommand str = do
  mc <- lookupCommand' str
  modifyGHCiState (\st -> st { last_command = mc })
  return $ case mc of
           Just c -> GotCommand c
           Nothing -> BadCommand

lookupCommand' :: String -> GHCi (Maybe Command)
lookupCommand' ":" = return Nothing
lookupCommand' str' = do
  macros    <- ghci_macros <$> getGHCiState
  ghci_cmds <- ghci_commands <$> getGHCiState

  let ghci_cmds_nohide = filter (not . cmdHidden) ghci_cmds

  let (str, xcmds) = case str' of
          ':' : rest -> (rest, [])     -- "::" selects a builtin command
          _          -> (str', macros) -- otherwise include macros in lookup

      lookupExact  s = find $ (s ==)           . cmdName
      lookupPrefix s = find $ (s `isPrefixOf`) . cmdName

      -- hidden commands can only be matched exact
      builtinPfxMatch = lookupPrefix str ghci_cmds_nohide

  -- first, look for exact match (while preferring macros); then, look
  -- for first prefix match (preferring builtins), *unless* a macro
  -- overrides the builtin; see #8305 for motivation
  return $ lookupExact str xcmds <|>
           lookupExact str ghci_cmds <|>
           (builtinPfxMatch >>= \c -> lookupExact (cmdName c) xcmds) <|>
           builtinPfxMatch <|>
           lookupPrefix str xcmds

getCurrentBreakSpan :: GHCi (Maybe SrcSpan)
getCurrentBreakSpan = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = GHC.resumeHistoryIx r
        if ix == 0
           then return (Just (GHC.resumeSpan r))
           else do
                let hist = GHC.resumeHistory r !! (ix-1)
                pan <- GHC.getHistorySpan hist
                return (Just pan)

getCallStackAtCurrentBreakpoint :: GHCi (Maybe [String])
getCallStackAtCurrentBreakpoint = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
       hsc_env <- GHC.getSession
       Just <$> liftIO (costCentreStackInfo hsc_env (GHC.resumeCCS r))

getCurrentBreakModule :: GHCi (Maybe Module)
getCurrentBreakModule = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = GHC.resumeHistoryIx r
        if ix == 0
           then return (GHC.breakInfo_module `liftM` GHC.resumeBreakInfo r)
           else do
                let hist = GHC.resumeHistory r !! (ix-1)
                return $ Just $ GHC.getHistoryModule  hist

-----------------------------------------------------------------------------
--
-- Commands
--
-----------------------------------------------------------------------------

noArgs :: GHCi () -> String -> GHCi ()
noArgs m "" = m
noArgs _ _  = liftIO $ putStrLn "This command takes no arguments"

withSandboxOnly :: String -> GHCi () -> GHCi ()
withSandboxOnly cmd this = do
   dflags <- getDynFlags
   if not (gopt Opt_GhciSandbox dflags)
      then printForUser (text cmd <+>
                         ptext (sLit "is not supported with -fno-ghci-sandbox"))
      else this

-----------------------------------------------------------------------------
-- :help

help :: String -> GHCi ()
help _ = do
    txt <- long_help `fmap` getGHCiState
    liftIO $ putStr txt

-----------------------------------------------------------------------------
-- :info

info :: Bool -> String -> InputT GHCi ()
info _ "" = throwGhcException (CmdLineError "syntax: ':i <thing-you-want-info-about>'")
info allInfo s  = handleSourceError GHC.printException $ do
    unqual <- GHC.getPrintUnqual
    dflags <- getDynFlags
    sdocs  <- mapM (infoThing allInfo) (words s)
    mapM_ (liftIO . putStrLn . showSDocForUser dflags unqual) sdocs

infoThing :: GHC.GhcMonad m => Bool -> String -> m SDoc
infoThing allInfo str = do
    names     <- GHC.parseName str
    mb_stuffs <- mapM (GHC.getInfo allInfo) names
    let filtered = filterOutChildren (\(t,_f,_ci,_fi) -> t) (catMaybes mb_stuffs)
    return $ vcat (intersperse (text "") $ map pprInfo filtered)

  -- Filter out names whose parent is also there Good
  -- example is '[]', which is both a type and data
  -- constructor in the same type
filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs
  = filterOut has_parent xs
  where
    all_names = mkNameSet (map (getName . get_thing) xs)
    has_parent x = case tyThingParent_maybe (get_thing x) of
                     Just p  -> getName p `elemNameSet` all_names
                     Nothing -> False

pprInfo :: (TyThing, Fixity, [GHC.ClsInst], [GHC.FamInst]) -> SDoc
pprInfo (thing, fixity, cls_insts, fam_insts)
  =  pprTyThingInContextLoc thing
  $$ show_fixity
  $$ vcat (map GHC.pprInstance cls_insts)
  $$ vcat (map GHC.pprFamInst  fam_insts)
  where
    show_fixity
        | fixity == GHC.defaultFixity = empty
        | otherwise                   = ppr fixity <+> pprInfixName (GHC.getName thing)

-----------------------------------------------------------------------------
-- :main

runMain :: String -> GHCi ()
runMain s = case toArgs s of
            Left err   -> liftIO (hPutStrLn stderr err)
            Right args ->
                do dflags <- getDynFlags
                   let main = fromMaybe "main" (mainFunIs dflags)
                   -- Wrap the main function in 'void' to discard its value instead
                   -- of printing it (#9086). See Haskell 2010 report Chapter 5.
                   doWithArgs args $ "Control.Monad.void (" ++ main ++ ")"

-----------------------------------------------------------------------------
-- :run

runRun :: String -> GHCi ()
runRun s = case toCmdArgs s of
           Left err          -> liftIO (hPutStrLn stderr err)
           Right (cmd, args) -> doWithArgs args cmd

doWithArgs :: [String] -> String -> GHCi ()
doWithArgs args cmd = enqueueCommands ["System.Environment.withArgs " ++
                                       show args ++ " (" ++ cmd ++ ")"]

-----------------------------------------------------------------------------
-- :cd

changeDirectory :: String -> InputT GHCi ()
changeDirectory "" = do
  -- :cd on its own changes to the user's home directory
  either_dir <- liftIO $ tryIO getHomeDirectory
  case either_dir of
     Left _e -> return ()
     Right dir -> changeDirectory dir
changeDirectory dir = do
  graph <- GHC.getModuleGraph
  when (not (null graph)) $
        liftIO $ putStrLn "Warning: changing directory causes all loaded modules to be unloaded,\nbecause the search path has changed."
  GHC.setTargets []
  _ <- GHC.load LoadAllTargets
  lift $ setContextAfterLoad False []
  GHC.workingDirectoryChanged
  dir' <- expandPath dir
  liftIO $ setCurrentDirectory dir'
  dflags <- getDynFlags
  -- With -fexternal-interpreter, we have to change the directory of the subprocess too.
  -- (this gives consistent behaviour with and without -fexternal-interpreter)
  when (gopt Opt_ExternalInterpreter dflags) $
    lift $ enqueueCommands ["System.Directory.setCurrentDirectory " ++ show dir']

trySuccess :: GHC.GhcMonad m => m SuccessFlag -> m SuccessFlag
trySuccess act =
    handleSourceError (\e -> do GHC.printException e
                                return Failed) $ do
      act

-----------------------------------------------------------------------------
-- :edit

editFile :: String -> InputT GHCi ()
editFile str =
  do file <- if null str then lift chooseEditFile else expandPath str
     st <- getGHCiState
     errs <- liftIO $ readIORef $ lastErrorLocations st
     let cmd = editor st
     when (null cmd)
       $ throwGhcException (CmdLineError "editor not set, use :set editor")
     lineOpt <- liftIO $ do
         let sameFile p1 p2 = liftA2 (==) (canonicalizePath p1) (canonicalizePath p2)
              `catchIO` (\_ -> return False)

         curFileErrs <- filterM (\(f, _) -> unpackFS f `sameFile` file) errs
         return $ case curFileErrs of
             (_, line):_ -> " +" ++ show line
             _ -> ""
     let cmdArgs = ' ':(file ++ lineOpt)
     code <- liftIO $ system (cmd ++ cmdArgs)

     when (code == ExitSuccess)
       $ reloadModule ""

-- The user didn't specify a file so we pick one for them.
-- Our strategy is to pick the first module that failed to load,
-- or otherwise the first target.
--
-- XXX: Can we figure out what happened if the depndecy analysis fails
--      (e.g., because the porgrammeer mistyped the name of a module)?
-- XXX: Can we figure out the location of an error to pass to the editor?
-- XXX: if we could figure out the list of errors that occured during the
-- last load/reaload, then we could start the editor focused on the first
-- of those.
chooseEditFile :: GHCi String
chooseEditFile =
  do let hasFailed x = fmap not $ GHC.isLoaded $ GHC.ms_mod_name x

     graph <- GHC.getModuleGraph
     failed_graph <- filterM hasFailed graph
     let order g  = flattenSCCs $ GHC.topSortModuleGraph True g Nothing
         pick xs  = case xs of
                      x : _ -> GHC.ml_hs_file (GHC.ms_location x)
                      _     -> Nothing

     case pick (order failed_graph) of
       Just file -> return file
       Nothing   ->
         do targets <- GHC.getTargets
            case msum (map fromTarget targets) of
              Just file -> return file
              Nothing   -> throwGhcException (CmdLineError "No files to edit.")

  where fromTarget (GHC.Target (GHC.TargetFile f _) _ _) = Just f
        fromTarget _ = Nothing -- when would we get a module target?


-----------------------------------------------------------------------------
-- :def

defineMacro :: Bool{-overwrite-} -> String -> GHCi ()
defineMacro _ (':':_) =
  liftIO $ putStrLn "macro name cannot start with a colon"
defineMacro overwrite s = do
  let (macro_name, definition) = break isSpace s
  macros <- ghci_macros <$> getGHCiState
  let defined = map cmdName macros
  if null macro_name
        then if null defined
                then liftIO $ putStrLn "no macros defined"
                else liftIO $ putStr ("the following macros are defined:\n" ++
                                      unlines defined)
        else do
  if (not overwrite && macro_name `elem` defined)
        then throwGhcException (CmdLineError
                ("macro '" ++ macro_name ++ "' is already defined"))
        else do

  -- compile the expression
  handleSourceError GHC.printException $ do
    step <- getGhciStepIO
    expr <- GHC.parseExpr definition
    -- > ghciStepIO . definition :: String -> IO String
    let stringTy = nlHsTyVar stringTy_RDR
        ioM = nlHsTyVar (getRdrName ioTyConName) `nlHsAppTy` stringTy
        body = nlHsVar compose_RDR `mkHsApp` (nlHsPar step)
                                   `mkHsApp` (nlHsPar expr)
        tySig = mkLHsSigWcType (stringTy `nlHsFunTy` ioM)
        new_expr = L (getLoc expr) $ ExprWithTySig body tySig
    hv <- GHC.compileParsedExprRemote new_expr

    let newCmd = Command { cmdName = macro_name
                         , cmdAction = lift . runMacro hv
                         , cmdHidden = False
                         , cmdCompletionFunc = noCompletion
                         }

    -- later defined macros have precedence
    modifyGHCiState $ \s ->
        let filtered = [ cmd | cmd <- macros, cmdName cmd /= macro_name ]
        in s { ghci_macros = newCmd : filtered }

runMacro :: GHC.ForeignHValue{-String -> IO String-} -> String -> GHCi Bool
runMacro fun s = do
  hsc_env <- GHC.getSession
  str <- liftIO $ evalStringToIOString hsc_env fun s
  enqueueCommands (lines str)
  return False


-----------------------------------------------------------------------------
-- :undef

undefineMacro :: String -> GHCi ()
undefineMacro str = mapM_ undef (words str)
 where undef macro_name = do
        cmds <- ghci_macros <$> getGHCiState
        if (macro_name `notElem` map cmdName cmds)
           then throwGhcException (CmdLineError
                ("macro '" ++ macro_name ++ "' is not defined"))
           else do
            -- This is a tad racy but really, it's a shell
            modifyGHCiState $ \s ->
                s { ghci_macros = filter ((/= macro_name) . cmdName)
                                         (ghci_macros s) }


-----------------------------------------------------------------------------
-- :cmd

cmdCmd :: String -> GHCi ()
cmdCmd str = handleSourceError GHC.printException $ do
    step <- getGhciStepIO
    expr <- GHC.parseExpr str
    -- > ghciStepIO str :: IO String
    let new_expr = step `mkHsApp` expr
    hv <- GHC.compileParsedExprRemote new_expr

    hsc_env <- GHC.getSession
    cmds <- liftIO $ evalString hsc_env hv
    enqueueCommands (lines cmds)

-- | Generate a typed ghciStepIO expression
-- @ghciStepIO :: Ty String -> IO String@.
getGhciStepIO :: GHCi (LHsExpr GhcPs)
getGhciStepIO = do
  ghciTyConName <- GHC.getGHCiMonad
  let stringTy = nlHsTyVar stringTy_RDR
      ghciM = nlHsTyVar (getRdrName ghciTyConName) `nlHsAppTy` stringTy
      ioM = nlHsTyVar (getRdrName ioTyConName) `nlHsAppTy` stringTy
      body = nlHsVar (getRdrName ghciStepIoMName)
      tySig = mkLHsSigWcType (ghciM `nlHsFunTy` ioM)
  return $ noLoc $ ExprWithTySig body tySig

-----------------------------------------------------------------------------
-- :check

checkModule :: String -> InputT GHCi ()
checkModule m = do
  let modl = GHC.mkModuleName m
  ok <- handleSourceError (\e -> GHC.printException e >> return False) $ do
          r <- GHC.typecheckModule =<< GHC.parseModule =<< GHC.getModSummary modl
          dflags <- getDynFlags
          liftIO $ putStrLn $ showSDoc dflags $
           case GHC.moduleInfo r of
             cm | Just scope <- GHC.modInfoTopLevelScope cm ->
                let
                    (loc, glob) = ASSERT( all isExternalName scope )
                                  partition ((== modl) . GHC.moduleName . GHC.nameModule) scope
                in
                        (text "global names: " <+> ppr glob) $$
                        (text "local  names: " <+> ppr loc)
             _ -> empty
          return True
  afterLoad (successIf ok) False


-----------------------------------------------------------------------------
-- :load, :add, :reload

-- | Sets '-fdefer-type-errors' if 'defer' is true, executes 'load' and unsets
-- '-fdefer-type-errors' again if it has not been set before.
wrapDeferTypeErrors :: InputT GHCi a -> InputT GHCi a
wrapDeferTypeErrors load =
  gbracket
    (do
      -- Force originalFlags to avoid leaking the associated HscEnv
      !originalFlags <- getDynFlags
      void $ GHC.setProgramDynFlags $
         setGeneralFlag' Opt_DeferTypeErrors originalFlags
      return originalFlags)
    (\originalFlags -> void $ GHC.setProgramDynFlags originalFlags)
    (\_ -> load)

loadModule :: [(FilePath, Maybe Phase)] -> InputT GHCi SuccessFlag
loadModule fs = timeIt (const Nothing) (loadModule' fs)

-- | @:load@ command
loadModule_ :: [FilePath] -> InputT GHCi ()
loadModule_ fs = void $ loadModule (zip fs (repeat Nothing))

loadModuleDefer :: [FilePath] -> InputT GHCi ()
loadModuleDefer = wrapDeferTypeErrors . loadModule_

loadModule' :: [(FilePath, Maybe Phase)] -> InputT GHCi SuccessFlag
loadModule' files = do
  let (filenames, phases) = unzip files
  exp_filenames <- mapM expandPath filenames
  let files' = zip exp_filenames phases
  targets <- mapM (uncurry GHC.guessTarget) files'

  -- NOTE: we used to do the dependency anal first, so that if it
  -- fails we didn't throw away the current set of modules.  This would
  -- require some re-working of the GHC interface, so we'll leave it
  -- as a ToDo for now.

  -- unload first
  _ <- GHC.abandonAll
  lift discardActiveBreakPoints
  GHC.setTargets []
  _ <- GHC.load LoadAllTargets

  GHC.setTargets targets
  doLoadAndCollectInfo False LoadAllTargets

-- | @:add@ command
addModule :: [FilePath] -> InputT GHCi ()
addModule files = do
  lift revertCAFs -- always revert CAFs on load/add.
  files' <- mapM expandPath files
  targets <- mapM (\m -> GHC.guessTarget m Nothing) files'
  -- remove old targets with the same id; e.g. for :add *M
  mapM_ GHC.removeTarget [ tid | Target tid _ _ <- targets ]
  mapM_ GHC.addTarget targets
  _ <- doLoadAndCollectInfo False LoadAllTargets
  return ()

-- | @:reload@ command
reloadModule :: String -> InputT GHCi ()
reloadModule m = void $ doLoadAndCollectInfo True loadTargets
  where
    loadTargets | null m    = LoadAllTargets
                | otherwise = LoadUpTo (GHC.mkModuleName m)

reloadModuleDefer :: String -> InputT GHCi ()
reloadModuleDefer = wrapDeferTypeErrors . reloadModule

-- | Load/compile targets and (optionally) collect module-info
--
-- This collects the necessary SrcSpan annotated type information (via
-- 'collectInfo') required by the @:all-types@, @:loc-at@, @:type-at@,
-- and @:uses@ commands.
--
-- Meta-info collection is not enabled by default and needs to be
-- enabled explicitly via @:set +c@.  The reason is that collecting
-- the type-information for all sub-spans can be quite expensive, and
-- since those commands are designed to be used by editors and
-- tooling, it's useless to collect this data for normal GHCi
-- sessions.
doLoadAndCollectInfo :: Bool -> LoadHowMuch -> InputT GHCi SuccessFlag
doLoadAndCollectInfo retain_context howmuch = do
  doCollectInfo <- lift (isOptionSet CollectInfo)

  doLoad retain_context howmuch >>= \case
    Succeeded | doCollectInfo -> do
      loaded <- getModuleGraph >>= filterM GHC.isLoaded . map GHC.ms_mod_name
      v <- mod_infos <$> getGHCiState
      !newInfos <- collectInfo v loaded
      modifyGHCiState (\st -> st { mod_infos = newInfos })
      return Succeeded
    flag -> return flag

doLoad :: Bool -> LoadHowMuch -> InputT GHCi SuccessFlag
doLoad retain_context howmuch = do
  -- turn off breakpoints before we load: we can't turn them off later, because
  -- the ModBreaks will have gone away.
  lift discardActiveBreakPoints

  lift resetLastErrorLocations
  -- Enable buffering stdout and stderr as we're compiling. Keeping these
  -- handles unbuffered will just slow the compilation down, especially when
  -- compiling in parallel.
  gbracket (liftIO $ do hSetBuffering stdout LineBuffering
                        hSetBuffering stderr LineBuffering)
           (\_ ->
            liftIO $ do hSetBuffering stdout NoBuffering
                        hSetBuffering stderr NoBuffering) $ \_ -> do
      ok <- trySuccess $ GHC.load howmuch
      afterLoad ok retain_context
      return ok


afterLoad :: SuccessFlag
          -> Bool   -- keep the remembered_ctx, as far as possible (:reload)
          -> InputT GHCi ()
afterLoad ok retain_context = do
  lift revertCAFs  -- always revert CAFs on load.
  lift discardTickArrays
  loaded_mods <- getLoadedModules
  modulesLoadedMsg ok loaded_mods
  lift $ setContextAfterLoad retain_context loaded_mods

setContextAfterLoad :: Bool -> [GHC.ModSummary] -> GHCi ()
setContextAfterLoad keep_ctxt [] = do
  setContextKeepingPackageModules keep_ctxt []
setContextAfterLoad keep_ctxt ms = do
  -- load a target if one is available, otherwise load the topmost module.
  targets <- GHC.getTargets
  case [ m | Just m <- map (findTarget ms) targets ] of
        []    ->
          let graph' = flattenSCCs (GHC.topSortModuleGraph True ms Nothing) in
          load_this (last graph')
        (m:_) ->
          load_this m
 where
   findTarget mds t
    = case filter (`matches` t) mds of
        []    -> Nothing
        (m:_) -> Just m

   summary `matches` Target (TargetModule m) _ _
        = GHC.ms_mod_name summary == m
   summary `matches` Target (TargetFile f _) _ _
        | Just f' <- GHC.ml_hs_file (GHC.ms_location summary)   = f == f'
   _ `matches` _
        = False

   load_this summary | m <- GHC.ms_mod summary = do
        is_interp <- GHC.moduleIsInterpreted m
        dflags <- getDynFlags
        let star_ok = is_interp && not (safeLanguageOn dflags)
              -- We import the module with a * iff
              --   - it is interpreted, and
              --   - -XSafe is off (it doesn't allow *-imports)
        let new_ctx | star_ok   = [mkIIModule (GHC.moduleName m)]
                    | otherwise = [mkIIDecl   (GHC.moduleName m)]
        setContextKeepingPackageModules keep_ctxt new_ctx


-- | Keep any package modules (except Prelude) when changing the context.
setContextKeepingPackageModules
        :: Bool                 -- True  <=> keep all of remembered_ctx
                                -- False <=> just keep package imports
        -> [InteractiveImport]  -- new context
        -> GHCi ()

setContextKeepingPackageModules keep_ctx trans_ctx = do

  st <- getGHCiState
  let rem_ctx = remembered_ctx st
  new_rem_ctx <- if keep_ctx then return rem_ctx
                             else keepPackageImports rem_ctx
  setGHCiState st{ remembered_ctx = new_rem_ctx,
                   transient_ctx  = filterSubsumed new_rem_ctx trans_ctx }
  setGHCContextFromGHCiState

-- | Filters a list of 'InteractiveImport', clearing out any home package
-- imports so only imports from external packages are preserved.  ('IIModule'
-- counts as a home package import, because we are only able to bring a
-- full top-level into scope when the source is available.)
keepPackageImports :: [InteractiveImport] -> GHCi [InteractiveImport]
keepPackageImports = filterM is_pkg_import
  where
     is_pkg_import :: InteractiveImport -> GHCi Bool
     is_pkg_import (IIModule _) = return False
     is_pkg_import (IIDecl d)
         = do e <- gtry $ GHC.findModule mod_name (fmap sl_fs $ ideclPkgQual d)
              case e :: Either SomeException Module of
                Left _  -> return False
                Right m -> return (not (isHomeModule m))
        where
          mod_name = unLoc (ideclName d)


modulesLoadedMsg :: SuccessFlag -> [GHC.ModSummary] -> InputT GHCi ()
modulesLoadedMsg ok mods = do
  dflags <- getDynFlags
  unqual <- GHC.getPrintUnqual
  let mod_name mod = do
        is_interpreted <- GHC.moduleIsBootOrNotObjectLinkable mod
        return $ if is_interpreted
                  then ppr (GHC.ms_mod mod)
                  else ppr (GHC.ms_mod mod)
                       <> text " ("
                       <> text (normalise $ msObjFilePath mod)
                       <> text ")" -- fix #9887
  mod_names <- mapM mod_name mods
  let mod_commas
        | null mods = text "none."
        | otherwise = hsep (punctuate comma mod_names) <> text "."
      status = case ok of
                   Failed    -> text "Failed"
                   Succeeded -> text "Ok"

      msg = status <> text ", modules loaded:" <+> mod_commas

  when (verbosity dflags > 0) $
     liftIO $ putStrLn $ showSDocForUser dflags unqual msg


-- | Run an 'ExceptT' wrapped 'GhcMonad' while handling source errors
-- and printing 'throwE' strings to 'stderr'
runExceptGhcMonad :: GHC.GhcMonad m => ExceptT SDoc m () -> m ()
runExceptGhcMonad act = handleSourceError GHC.printException $
                        either handleErr pure =<<
                        runExceptT act
  where
    handleErr sdoc = do
        dflags <- getDynFlags
        liftIO . hPutStrLn stderr . showSDocForUser dflags alwaysQualify $ sdoc

-- | Inverse of 'runExceptT' for \"pure\" computations
-- (c.f. 'except' for 'Except')
exceptT :: Applicative m => Either e a -> ExceptT e m a
exceptT = ExceptT . pure

-----------------------------------------------------------------------------
-- | @:type@ command. See also Note [TcRnExprMode] in TcRnDriver.

typeOfExpr :: String -> InputT GHCi ()
typeOfExpr str = handleSourceError GHC.printException $ do
    let (mode, expr_str) = case break isSpace str of
          ("+d", rest) -> (GHC.TM_Default, dropWhile isSpace rest)
          ("+v", rest) -> (GHC.TM_NoInst,  dropWhile isSpace rest)
          _            -> (GHC.TM_Inst,    str)
    ty <- GHC.exprType mode expr_str
    printForUser $ sep [text expr_str, nest 2 (dcolon <+> pprTypeForUser ty)]

-----------------------------------------------------------------------------
-- | @:type-at@ command

typeAtCmd :: String -> InputT GHCi ()
typeAtCmd str = runExceptGhcMonad $ do
    (span',sample) <- exceptT $ parseSpanArg str
    infos      <- mod_infos <$> getGHCiState
    (info, ty) <- findType infos span' sample
    lift $ printForUserModInfo (modinfoInfo info)
                               (sep [text sample,nest 2 (dcolon <+> ppr ty)])

-----------------------------------------------------------------------------
-- | @:uses@ command

usesCmd :: String -> InputT GHCi ()
usesCmd str = runExceptGhcMonad $ do
    (span',sample) <- exceptT $ parseSpanArg str
    infos  <- mod_infos <$> getGHCiState
    uses   <- findNameUses infos span' sample
    forM_ uses (liftIO . putStrLn . showSrcSpan)

-----------------------------------------------------------------------------
-- | @:loc-at@ command

locAtCmd :: String -> InputT GHCi ()
locAtCmd str = runExceptGhcMonad $ do
    (span',sample) <- exceptT $ parseSpanArg str
    infos    <- mod_infos <$> getGHCiState
    (_,_,sp) <- findLoc infos span' sample
    liftIO . putStrLn . showSrcSpan $ sp

-----------------------------------------------------------------------------
-- | @:all-types@ command

allTypesCmd :: String -> InputT GHCi ()
allTypesCmd _ = runExceptGhcMonad $ do
    infos <- mod_infos <$> getGHCiState
    forM_ (M.elems infos) $ \mi ->
        forM_ (modinfoSpans mi) (lift . printSpan)
  where
    printSpan span'
      | Just ty <- spaninfoType span' = do
        df <- getDynFlags
        let tyInfo = unwords . words $
                     showSDocForUser df alwaysQualify (pprTypeForUser ty)
        liftIO . putStrLn $
            showRealSrcSpan (spaninfoSrcSpan span') ++ ": " ++ tyInfo
      | otherwise = return ()

-----------------------------------------------------------------------------
-- Helpers for locAtCmd/typeAtCmd/usesCmd

-- | Parse a span: <module-name/filepath> <sl> <sc> <el> <ec> <string>
parseSpanArg :: String -> Either SDoc (RealSrcSpan,String)
parseSpanArg s = do
    (fp,s0) <- readAsString (skipWs s)
    s0'     <- skipWs1 s0
    (sl,s1) <- readAsInt s0'
    s1'     <- skipWs1 s1
    (sc,s2) <- readAsInt s1'
    s2'     <- skipWs1 s2
    (el,s3) <- readAsInt s2'
    s3'     <- skipWs1 s3
    (ec,s4) <- readAsInt s3'

    trailer <- case s4 of
        [] -> Right ""
        _  -> skipWs1 s4

    let fs    = mkFastString fp
        span' = mkRealSrcSpan (mkRealSrcLoc fs sl sc)
                              (mkRealSrcLoc fs el ec)

    return (span',trailer)
  where
    readAsInt :: String -> Either SDoc (Int,String)
    readAsInt "" = Left "Premature end of string while expecting Int"
    readAsInt s0 = case reads s0 of
        [s_rest] -> Right s_rest
        _        -> Left ("Couldn't read" <+> text (show s0) <+> "as Int")

    readAsString :: String -> Either SDoc (String,String)
    readAsString s0
      | '"':_ <- s0 = case reads s0 of
          [s_rest] -> Right s_rest
          _        -> leftRes
      | s_rest@(_:_,_) <- breakWs s0 = Right s_rest
      | otherwise = leftRes
      where
        leftRes = Left ("Couldn't read" <+> text (show s0) <+> "as String")

    skipWs1 :: String -> Either SDoc String
    skipWs1 (c:cs) | isWs c = Right (skipWs cs)
    skipWs1 s0 = Left ("Expected whitespace in" <+> text (show s0))

    isWs    = (`elem` [' ','\t'])
    skipWs  = dropWhile isWs
    breakWs = break isWs


-- | Pretty-print \"real\" 'SrcSpan's as
-- @<filename>:(<line>,<col>)-(<line-end>,<col-end>)@
-- while simply unpacking 'UnhelpfulSpan's
showSrcSpan :: SrcSpan -> String
showSrcSpan (UnhelpfulSpan s)  = unpackFS s
showSrcSpan (RealSrcSpan spn)  = showRealSrcSpan spn

-- | Variant of 'showSrcSpan' for 'RealSrcSpan's
showRealSrcSpan :: RealSrcSpan -> String
showRealSrcSpan spn = concat [ fp, ":(", show sl, ",", show sc
                             , ")-(", show el, ",", show ec, ")"
                             ]
  where
    fp = unpackFS (srcSpanFile spn)
    sl = srcSpanStartLine spn
    sc = srcSpanStartCol  spn
    el = srcSpanEndLine   spn
    ec = srcSpanEndCol    spn

-----------------------------------------------------------------------------
-- | @:kind@ command

kindOfType :: Bool -> String -> InputT GHCi ()
kindOfType norm str = handleSourceError GHC.printException $ do
    (ty, kind) <- GHC.typeKind norm str
    printForUser $ vcat [ text str <+> dcolon <+> pprTypeForUser kind
                        , ppWhen norm $ equals <+> pprTypeForUser ty ]

-----------------------------------------------------------------------------
-- :quit

quit :: String -> InputT GHCi Bool
quit _ = return True


-----------------------------------------------------------------------------
-- :script

-- running a script file #1363

scriptCmd :: String -> InputT GHCi ()
scriptCmd ws = do
  case words ws of
    [s]    -> runScript s
    _      -> throwGhcException (CmdLineError "syntax:  :script <filename>")

runScript :: String    -- ^ filename
           -> InputT GHCi ()
runScript filename = do
  filename' <- expandPath filename
  either_script <- liftIO $ tryIO (openFile filename' ReadMode)
  case either_script of
    Left _err    -> throwGhcException (CmdLineError $ "IO error:  \""++filename++"\" "
                      ++(ioeGetErrorString _err))
    Right script -> do
      st <- getGHCiState
      let prog = progname st
          line = line_number st
      setGHCiState st{progname=filename',line_number=0}
      scriptLoop script
      liftIO $ hClose script
      new_st <- getGHCiState
      setGHCiState new_st{progname=prog,line_number=line}
  where scriptLoop script = do
          res <- runOneCommand handler $ fileLoop script
          case res of
            Nothing -> return ()
            Just s  -> if s
              then scriptLoop script
              else return ()

-----------------------------------------------------------------------------
-- :issafe

-- Displaying Safe Haskell properties of a module

isSafeCmd :: String -> InputT GHCi ()
isSafeCmd m =
    case words m of
        [s] | looksLikeModuleName s -> do
            md <- lift $ lookupModule s
            isSafeModule md
        [] -> do md <- guessCurrentModule "issafe"
                 isSafeModule md
        _ -> throwGhcException (CmdLineError "syntax:  :issafe <module>")

isSafeModule :: Module -> InputT GHCi ()
isSafeModule m = do
    mb_mod_info <- GHC.getModuleInfo m
    when (isNothing mb_mod_info)
         (throwGhcException $ CmdLineError $ "unknown module: " ++ mname)

    dflags <- getDynFlags
    let iface = GHC.modInfoIface $ fromJust mb_mod_info
    when (isNothing iface)
         (throwGhcException $ CmdLineError $ "can't load interface file for module: " ++
                                    (GHC.moduleNameString $ GHC.moduleName m))

    (msafe, pkgs) <- GHC.moduleTrustReqs m
    let trust  = showPpr dflags $ getSafeMode $ GHC.mi_trust $ fromJust iface
        pkg    = if packageTrusted dflags m then "trusted" else "untrusted"
        (good, bad) = tallyPkgs dflags pkgs

    -- print info to user...
    liftIO $ putStrLn $ "Trust type is (Module: " ++ trust ++ ", Package: " ++ pkg ++ ")"
    liftIO $ putStrLn $ "Package Trust: " ++ (if packageTrustOn dflags then "On" else "Off")
    when (not $ S.null good)
         (liftIO $ putStrLn $ "Trusted package dependencies (trusted): " ++
                        (intercalate ", " $ map (showPpr dflags) (S.toList good)))
    case msafe && S.null bad of
        True -> liftIO $ putStrLn $ mname ++ " is trusted!"
        False -> do
            when (not $ null bad)
                 (liftIO $ putStrLn $ "Trusted package dependencies (untrusted): "
                            ++ (intercalate ", " $ map (showPpr dflags) (S.toList bad)))
            liftIO $ putStrLn $ mname ++ " is NOT trusted!"

  where
    mname = GHC.moduleNameString $ GHC.moduleName m

    packageTrusted dflags md
        | thisPackage dflags == moduleUnitId md = True
        | otherwise = trusted $ getPackageDetails dflags (moduleUnitId md)

    tallyPkgs dflags deps | not (packageTrustOn dflags) = (S.empty, S.empty)
                          | otherwise = S.partition part deps
        where part pkg = trusted $ getInstalledPackageDetails dflags pkg

-----------------------------------------------------------------------------
-- :browse

-- Browsing a module's contents

browseCmd :: Bool -> String -> InputT GHCi ()
browseCmd bang m =
  case words m of
    ['*':s] | looksLikeModuleName s -> do
        md <- lift $ wantInterpretedModule s
        browseModule bang md False
    [s] | looksLikeModuleName s -> do
        md <- lift $ lookupModule s
        browseModule bang md True
    [] -> do md <- guessCurrentModule ("browse" ++ if bang then "!" else "")
             browseModule bang md True
    _ -> throwGhcException (CmdLineError "syntax:  :browse <module>")

guessCurrentModule :: String -> InputT GHCi Module
-- Guess which module the user wants to browse.  Pick
-- modules that are interpreted first.  The most
-- recently-added module occurs last, it seems.
guessCurrentModule cmd
  = do imports <- GHC.getContext
       when (null imports) $ throwGhcException $
          CmdLineError (':' : cmd ++ ": no current module")
       case (head imports) of
          IIModule m -> GHC.findModule m Nothing
          IIDecl d   -> GHC.findModule (unLoc (ideclName d))
                                       (fmap sl_fs $ ideclPkgQual d)

-- without bang, show items in context of their parents and omit children
-- with bang, show class methods and data constructors separately, and
--            indicate import modules, to aid qualifying unqualified names
-- with sorted, sort items alphabetically
browseModule :: Bool -> Module -> Bool -> InputT GHCi ()
browseModule bang modl exports_only = do
  -- :browse reports qualifiers wrt current context
  unqual <- GHC.getPrintUnqual

  mb_mod_info <- GHC.getModuleInfo modl
  case mb_mod_info of
    Nothing -> throwGhcException (CmdLineError ("unknown module: " ++
                                GHC.moduleNameString (GHC.moduleName modl)))
    Just mod_info -> do
        dflags <- getDynFlags
        let names
               | exports_only = GHC.modInfoExports mod_info
               | otherwise    = GHC.modInfoTopLevelScope mod_info
                                `orElse` []

                -- sort alphabetically name, but putting locally-defined
                -- identifiers first. We would like to improve this; see #1799.
            sorted_names = loc_sort local ++ occ_sort external
                where
                (local,external) = ASSERT( all isExternalName names )
                                   partition ((==modl) . nameModule) names
                occ_sort = sortBy (compare `on` nameOccName)
                -- try to sort by src location. If the first name in our list
                -- has a good source location, then they all should.
                loc_sort ns
                      | n:_ <- ns, isGoodSrcSpan (nameSrcSpan n)
                      = sortBy (compare `on` nameSrcSpan) ns
                      | otherwise
                      = occ_sort ns

        mb_things <- mapM GHC.lookupName sorted_names
        let filtered_things = filterOutChildren (\t -> t) (catMaybes mb_things)

        rdr_env <- GHC.getGRE

        let things | bang      = catMaybes mb_things
                   | otherwise = filtered_things
            pretty | bang      = pprTyThing showToHeader
                   | otherwise = pprTyThingInContext showToHeader

            labels  [] = text "-- not currently imported"
            labels  l  = text $ intercalate "\n" $ map qualifier l

            qualifier :: Maybe [ModuleName] -> String
            qualifier  = maybe "-- defined locally"
                             (("-- imported via "++) . intercalate ", "
                               . map GHC.moduleNameString)
            importInfo = RdrName.getGRE_NameQualifier_maybes rdr_env

            modNames :: [[Maybe [ModuleName]]]
            modNames   = map (importInfo . GHC.getName) things

            -- annotate groups of imports with their import modules
            -- the default ordering is somewhat arbitrary, so we group
            -- by header and sort groups; the names themselves should
            -- really come in order of source appearance.. (trac #1799)
            annotate mts = concatMap (\(m,ts)->labels m:ts)
                         $ sortBy cmpQualifiers $ grp mts
              where cmpQualifiers =
                      compare `on` (map (fmap (map moduleNameFS)) . fst)
            grp []            = []
            grp mts@((m,_):_) = (m,map snd g) : grp ng
              where (g,ng) = partition ((==m).fst) mts

        let prettyThings, prettyThings' :: [SDoc]
            prettyThings = map pretty things
            prettyThings' | bang      = annotate $ zip modNames prettyThings
                          | otherwise = prettyThings
        liftIO $ putStrLn $ showSDocForUser dflags unqual (vcat prettyThings')
        -- ToDo: modInfoInstances currently throws an exception for
        -- package modules.  When it works, we can do this:
        --        $$ vcat (map GHC.pprInstance (GHC.modInfoInstances mod_info))


-----------------------------------------------------------------------------
-- :module

-- Setting the module context.  For details on context handling see
-- "remembered_ctx" and "transient_ctx" in GhciMonad.

moduleCmd :: String -> GHCi ()
moduleCmd str
  | all sensible strs = cmd
  | otherwise = throwGhcException (CmdLineError "syntax:  :module [+/-] [*]M1 ... [*]Mn")
  where
    (cmd, strs) =
        case str of
          '+':stuff -> rest addModulesToContext   stuff
          '-':stuff -> rest remModulesFromContext stuff
          stuff     -> rest setContext            stuff

    rest op stuff = (op as bs, stuffs)
       where (as,bs) = partitionWith starred stuffs
             stuffs  = words stuff

    sensible ('*':m) = looksLikeModuleName m
    sensible m       = looksLikeModuleName m

    starred ('*':m) = Left  (GHC.mkModuleName m)
    starred m       = Right (GHC.mkModuleName m)


-- -----------------------------------------------------------------------------
-- Four ways to manipulate the context:
--   (a) :module +<stuff>:     addModulesToContext
--   (b) :module -<stuff>:     remModulesFromContext
--   (c) :module <stuff>:      setContext
--   (d) import <module>...:   addImportToContext

addModulesToContext :: [ModuleName] -> [ModuleName] -> GHCi ()
addModulesToContext starred unstarred = restoreContextOnFailure $ do
   addModulesToContext_ starred unstarred

addModulesToContext_ :: [ModuleName] -> [ModuleName] -> GHCi ()
addModulesToContext_ starred unstarred = do
   mapM_ addII (map mkIIModule starred ++ map mkIIDecl unstarred)
   setGHCContextFromGHCiState

remModulesFromContext :: [ModuleName] -> [ModuleName] -> GHCi ()
remModulesFromContext  starred unstarred = do
   -- we do *not* call restoreContextOnFailure here.  If the user
   -- is trying to fix up a context that contains errors by removing
   -- modules, we don't want GHC to silently put them back in again.
   mapM_ rm (starred ++ unstarred)
   setGHCContextFromGHCiState
 where
   rm :: ModuleName -> GHCi ()
   rm str = do
     m <- moduleName <$> lookupModuleName str
     let filt = filter ((/=) m . iiModuleName)
     modifyGHCiState $ \st ->
        st { remembered_ctx = filt (remembered_ctx st)
           , transient_ctx  = filt (transient_ctx st) }

setContext :: [ModuleName] -> [ModuleName] -> GHCi ()
setContext starred unstarred = restoreContextOnFailure $ do
  modifyGHCiState $ \st -> st { remembered_ctx = [], transient_ctx = [] }
                                -- delete the transient context
  addModulesToContext_ starred unstarred

addImportToContext :: String -> GHCi ()
addImportToContext str = restoreContextOnFailure $ do
  idecl <- GHC.parseImportDecl str
  addII (IIDecl idecl)   -- #5836
  setGHCContextFromGHCiState

-- Util used by addImportToContext and addModulesToContext
addII :: InteractiveImport -> GHCi ()
addII iidecl = do
  checkAdd iidecl
  modifyGHCiState $ \st ->
     st { remembered_ctx = addNotSubsumed iidecl (remembered_ctx st)
        , transient_ctx = filter (not . (iidecl `iiSubsumes`))
                                 (transient_ctx st)
        }

-- Sometimes we can't tell whether an import is valid or not until
-- we finally call 'GHC.setContext'.  e.g.
--
--   import System.IO (foo)
--
-- will fail because System.IO does not export foo.  In this case we
-- don't want to store the import in the context permanently, so we
-- catch the failure from 'setGHCContextFromGHCiState' and set the
-- context back to what it was.
--
-- See #6007
--
restoreContextOnFailure :: GHCi a -> GHCi a
restoreContextOnFailure do_this = do
  st <- getGHCiState
  let rc = remembered_ctx st; tc = transient_ctx st
  do_this `gonException` (modifyGHCiState $ \st' ->
     st' { remembered_ctx = rc, transient_ctx = tc })

-- -----------------------------------------------------------------------------
-- Validate a module that we want to add to the context

checkAdd :: InteractiveImport -> GHCi ()
checkAdd ii = do
  dflags <- getDynFlags
  let safe = safeLanguageOn dflags
  case ii of
    IIModule modname
       | safe -> throwGhcException $ CmdLineError "can't use * imports with Safe Haskell"
       | otherwise -> wantInterpretedModuleName modname >> return ()

    IIDecl d -> do
       let modname = unLoc (ideclName d)
           pkgqual = ideclPkgQual d
       m <- GHC.lookupModule modname (fmap sl_fs pkgqual)
       when safe $ do
           t <- GHC.isModuleTrusted m
           when (not t) $ throwGhcException $ ProgramError $ ""

-- -----------------------------------------------------------------------------
-- Update the GHC API's view of the context

-- | Sets the GHC context from the GHCi state.  The GHC context is
-- always set this way, we never modify it incrementally.
--
-- We ignore any imports for which the ModuleName does not currently
-- exist.  This is so that the remembered_ctx can contain imports for
-- modules that are not currently loaded, perhaps because we just did
-- a :reload and encountered errors.
--
-- Prelude is added if not already present in the list.  Therefore to
-- override the implicit Prelude import you can say 'import Prelude ()'
-- at the prompt, just as in Haskell source.
--
setGHCContextFromGHCiState :: GHCi ()
setGHCContextFromGHCiState = do
  st <- getGHCiState
      -- re-use checkAdd to check whether the module is valid.  If the
      -- module does not exist, we do *not* want to print an error
      -- here, we just want to silently keep the module in the context
      -- until such time as the module reappears again.  So we ignore
      -- the actual exception thrown by checkAdd, using tryBool to
      -- turn it into a Bool.
  iidecls <- filterM (tryBool.checkAdd) (transient_ctx st ++ remembered_ctx st)

  prel_iidecls <- getImplicitPreludeImports iidecls
  valid_prel_iidecls <- filterM (tryBool . checkAdd) prel_iidecls

  extra_imports <- filterM (tryBool . checkAdd) (map IIDecl (extra_imports st))

  GHC.setContext $ iidecls ++ extra_imports ++ valid_prel_iidecls


getImplicitPreludeImports :: [InteractiveImport] -> GHCi [InteractiveImport]
getImplicitPreludeImports iidecls = do
  dflags <- GHC.getInteractiveDynFlags
     -- allow :seti to override -XNoImplicitPrelude
  st <- getGHCiState

  -- We add the prelude imports if there are no *-imports, and we also
  -- allow each prelude import to be subsumed by another explicit import
  -- of the same module.  This means that you can override the prelude import
  -- with "import Prelude hiding (map)", for example.
  let prel_iidecls =
         if xopt LangExt.ImplicitPrelude dflags && not (any isIIModule iidecls)
            then [ IIDecl imp
                 | imp <- prelude_imports st
                 , not (any (sameImpModule imp) iidecls) ]
            else []

  return prel_iidecls

-- -----------------------------------------------------------------------------
-- Utils on InteractiveImport

mkIIModule :: ModuleName -> InteractiveImport
mkIIModule = IIModule

mkIIDecl :: ModuleName -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl

iiModules :: [InteractiveImport] -> [ModuleName]
iiModules is = [m | IIModule m <- is]

isIIModule :: InteractiveImport -> Bool
isIIModule (IIModule _) = True
isIIModule _ = False

iiModuleName :: InteractiveImport -> ModuleName
iiModuleName (IIModule m) = m
iiModuleName (IIDecl d)   = unLoc (ideclName d)

preludeModuleName :: ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"

sameImpModule :: ImportDecl GhcPs -> InteractiveImport -> Bool
sameImpModule _ (IIModule _) = False -- we only care about imports here
sameImpModule imp (IIDecl d) = unLoc (ideclName d) == unLoc (ideclName imp)

addNotSubsumed :: InteractiveImport
               -> [InteractiveImport] -> [InteractiveImport]
addNotSubsumed i is
  | any (`iiSubsumes` i) is = is
  | otherwise               = i : filter (not . (i `iiSubsumes`)) is

-- | @filterSubsumed is js@ returns the elements of @js@ not subsumed
-- by any of @is@.
filterSubsumed :: [InteractiveImport] -> [InteractiveImport]
               -> [InteractiveImport]
filterSubsumed is js = filter (\j -> not (any (`iiSubsumes` j) is)) js

-- | Returns True if the left import subsumes the right one.  Doesn't
-- need to be 100% accurate, conservatively returning False is fine.
-- (EXCEPT: (IIModule m) *must* subsume itself, otherwise a panic in
-- plusProv will ensue (#5904))
--
-- Note that an IIModule does not necessarily subsume an IIDecl,
-- because e.g. a module might export a name that is only available
-- qualified within the module itself.
--
-- Note that 'import M' does not necessarily subsume 'import M(foo)',
-- because M might not export foo and we want an error to be produced
-- in that case.
--
iiSubsumes :: InteractiveImport -> InteractiveImport -> Bool
iiSubsumes (IIModule m1) (IIModule m2) = m1==m2
iiSubsumes (IIDecl d1) (IIDecl d2)      -- A bit crude
  =  unLoc (ideclName d1) == unLoc (ideclName d2)
     && ideclAs d1 == ideclAs d2
     && (not (ideclQualified d1) || ideclQualified d2)
     && (ideclHiding d1 `hidingSubsumes` ideclHiding d2)
  where
     _                    `hidingSubsumes` Just (False,L _ []) = True
     Just (False, L _ xs) `hidingSubsumes` Just (False,L _ ys)
                                                           = all (`elem` xs) ys
     h1                   `hidingSubsumes` h2              = h1 == h2
iiSubsumes _ _ = False


----------------------------------------------------------------------------
-- :set

-- set options in the interpreter.  Syntax is exactly the same as the
-- ghc command line, except that certain options aren't available (-C,
-- -E etc.)
--
-- This is pretty fragile: most options won't work as expected.  ToDo:
-- figure out which ones & disallow them.

setCmd :: String -> GHCi ()
setCmd ""   = showOptions False
setCmd "-a" = showOptions True
setCmd str
  = case getCmd str of
    Right ("args",    rest) ->
        case toArgs rest of
            Left err -> liftIO (hPutStrLn stderr err)
            Right args -> setArgs args
    Right ("prog",    rest) ->
        case toArgs rest of
            Right [prog] -> setProg prog
            _ -> liftIO (hPutStrLn stderr "syntax: :set prog <progname>")

    Right ("prompt",           rest) ->
        setPromptString setPrompt (dropWhile isSpace rest)
                        "syntax: set prompt <string>"
    Right ("prompt-function",  rest) ->
        setPromptFunc setPrompt $ dropWhile isSpace rest
    Right ("prompt-cont",          rest) ->
        setPromptString setPromptCont (dropWhile isSpace rest)
                        "syntax: :set prompt-cont <string>"
    Right ("prompt-cont-function", rest) ->
        setPromptFunc setPromptCont $ dropWhile isSpace rest

    Right ("editor",  rest) -> setEditor  $ dropWhile isSpace rest
    Right ("stop",    rest) -> setStop    $ dropWhile isSpace rest
    _ -> case toArgs str of
         Left err -> liftIO (hPutStrLn stderr err)
         Right wds -> setOptions wds

setiCmd :: String -> GHCi ()
setiCmd ""   = GHC.getInteractiveDynFlags >>= liftIO . showDynFlags False
setiCmd "-a" = GHC.getInteractiveDynFlags >>= liftIO . showDynFlags True
setiCmd str  =
  case toArgs str of
    Left err -> liftIO (hPutStrLn stderr err)
    Right wds -> newDynFlags True wds

showOptions :: Bool -> GHCi ()
showOptions show_all
  = do st <- getGHCiState
       dflags <- getDynFlags
       let opts = options st
       liftIO $ putStrLn (showSDoc dflags (
              text "options currently set: " <>
              if null opts
                   then text "none."
                   else hsep (map (\o -> char '+' <> text (optToStr o)) opts)
           ))
       getDynFlags >>= liftIO . showDynFlags show_all


showDynFlags :: Bool -> DynFlags -> IO ()
showDynFlags show_all dflags = do
  showLanguages' show_all dflags
  putStrLn $ showSDoc dflags $
     text "GHCi-specific dynamic flag settings:" $$
         nest 2 (vcat (map (setting "-f" "-fno-" gopt) ghciFlags))
  putStrLn $ showSDoc dflags $
     text "other dynamic, non-language, flag settings:" $$
         nest 2 (vcat (map (setting "-f" "-fno-" gopt) others))
  putStrLn $ showSDoc dflags $
     text "warning settings:" $$
         nest 2 (vcat (map (setting "-W" "-Wno-" wopt) DynFlags.wWarningFlags))
  where
        setting prefix noPrefix test flag
          | quiet     = empty
          | is_on     = text prefix <> text name
          | otherwise = text noPrefix <> text name
          where name = flagSpecName flag
                f = flagSpecFlag flag
                is_on = test f dflags
                quiet = not show_all && test f default_dflags == is_on

        default_dflags = defaultDynFlags (settings dflags)

        (ghciFlags,others)  = partition (\f -> flagSpecFlag f `elem` flgs)
                                        DynFlags.fFlags
        flgs = [ Opt_PrintExplicitForalls
               , Opt_PrintExplicitKinds
               , Opt_PrintUnicodeSyntax
               , Opt_PrintBindResult
               , Opt_BreakOnException
               , Opt_BreakOnError
               , Opt_PrintEvldWithShow
               ]

setArgs, setOptions :: [String] -> GHCi ()
setProg, setEditor, setStop :: String -> GHCi ()

setArgs args = do
  st <- getGHCiState
  wrapper <- mkEvalWrapper (progname st) args
  setGHCiState st { GhciMonad.args = args, evalWrapper = wrapper }

setProg prog = do
  st <- getGHCiState
  wrapper <- mkEvalWrapper prog (GhciMonad.args st)
  setGHCiState st { progname = prog, evalWrapper = wrapper }

setEditor cmd = modifyGHCiState (\st -> st { editor = cmd })

setStop str@(c:_) | isDigit c
  = do let (nm_str,rest) = break (not.isDigit) str
           nm = read nm_str
       st <- getGHCiState
       let old_breaks = breaks st
       if all ((/= nm) . fst) old_breaks
              then printForUser (text "Breakpoint" <+> ppr nm <+>
                                 text "does not exist")
              else do
       let new_breaks = map fn old_breaks
           fn (i,loc) | i == nm   = (i,loc { onBreakCmd = dropWhile isSpace rest })
                      | otherwise = (i,loc)
       setGHCiState st{ breaks = new_breaks }
setStop cmd = modifyGHCiState (\st -> st { stop = cmd })

setPrompt :: PromptFunction -> GHCi ()
setPrompt v = modifyGHCiState (\st -> st {prompt = v})

setPromptCont :: PromptFunction -> GHCi ()
setPromptCont v = modifyGHCiState (\st -> st {prompt_cont = v})

setPromptFunc :: (PromptFunction -> GHCi ()) -> String -> GHCi ()
setPromptFunc fSetPrompt s = do
    -- We explicitly annotate the type of the expression to ensure
    -- that unsafeCoerce# is passed the exact type necessary rather
    -- than a more general one
    let exprStr = "(" ++ s ++ ") :: [String] -> Int -> IO String"
    (HValue funValue) <- GHC.compileExpr exprStr
    fSetPrompt (convertToPromptFunction $ unsafeCoerce funValue)
    where
      convertToPromptFunction :: ([String] -> Int -> IO String)
                              -> PromptFunction
      convertToPromptFunction func = (\mods line -> liftIO $
                                       liftM text (func mods line))

setPromptString :: (PromptFunction -> GHCi ()) -> String -> String -> GHCi ()
setPromptString fSetPrompt value err = do
  if null value
    then liftIO $ hPutStrLn stderr $ err
    else case value of
           ('\"':_) ->
             case reads value of
               [(value', xs)] | all isSpace xs ->
                 setParsedPromptString fSetPrompt value'
               _ -> liftIO $ hPutStrLn stderr
                             "Can't parse prompt string. Use Haskell syntax."
           _ ->
             setParsedPromptString fSetPrompt value

setParsedPromptString :: (PromptFunction -> GHCi ()) ->  String -> GHCi ()
setParsedPromptString fSetPrompt s = do
  case (checkPromptStringForErrors s) of
    Just err ->
      liftIO $ hPutStrLn stderr err
    Nothing ->
      fSetPrompt $ generatePromptFunctionFromString s

setOptions wds =
   do -- first, deal with the GHCi opts (+s, +t, etc.)
      let (plus_opts, minus_opts)  = partitionWith isPlus wds
      mapM_ setOpt plus_opts
      -- then, dynamic flags
      when (not (null minus_opts)) $ newDynFlags False minus_opts

newDynFlags :: Bool -> [String] -> GHCi ()
newDynFlags interactive_only minus_opts = do
      let lopts = map noLoc minus_opts

      idflags0 <- GHC.getInteractiveDynFlags
      (idflags1, leftovers, warns) <- GHC.parseDynamicFlags idflags0 lopts

      liftIO $ handleFlagWarnings idflags1 warns
      when (not $ null leftovers)
           (throwGhcException . CmdLineError
            $ "Some flags have not been recognized: "
            ++ (concat . intersperse ", " $ map unLoc leftovers))

      when (interactive_only && packageFlagsChanged idflags1 idflags0) $ do
          liftIO $ hPutStrLn stderr "cannot set package flags with :seti; use :set"
      GHC.setInteractiveDynFlags idflags1
      installInteractivePrint (interactivePrint idflags1) False

      dflags0 <- getDynFlags
      when (not interactive_only) $ do
        (dflags1, _, _) <- liftIO $ GHC.parseDynamicFlags dflags0 lopts
        new_pkgs <- GHC.setProgramDynFlags dflags1

        -- if the package flags changed, reset the context and link
        -- the new packages.
        hsc_env <- GHC.getSession
        let dflags2 = hsc_dflags hsc_env
        when (packageFlagsChanged dflags2 dflags0) $ do
          when (verbosity dflags2 > 0) $
            liftIO . putStrLn $
              "package flags have changed, resetting and loading new packages..."
          GHC.setTargets []
          _ <- GHC.load LoadAllTargets
          liftIO $ linkPackages hsc_env new_pkgs
          -- package flags changed, we can't re-use any of the old context
          setContextAfterLoad False []
          -- and copy the package state to the interactive DynFlags
          idflags <- GHC.getInteractiveDynFlags
          GHC.setInteractiveDynFlags
              idflags{ pkgState = pkgState dflags2
                     , pkgDatabase = pkgDatabase dflags2
                     , packageFlags = packageFlags dflags2 }

        let ld0length   = length $ ldInputs dflags0
            fmrk0length = length $ cmdlineFrameworks dflags0

            newLdInputs     = drop ld0length (ldInputs dflags2)
            newCLFrameworks = drop fmrk0length (cmdlineFrameworks dflags2)

            hsc_env' = hsc_env { hsc_dflags =
                         dflags2 { ldInputs = newLdInputs
                                 , cmdlineFrameworks = newCLFrameworks } }

        when (not (null newLdInputs && null newCLFrameworks)) $
          liftIO $ linkCmdLineLibs hsc_env'

      return ()


unsetOptions :: String -> GHCi ()
unsetOptions str
  =   -- first, deal with the GHCi opts (+s, +t, etc.)
     let opts = words str
         (minus_opts, rest1) = partition isMinus opts
         (plus_opts, rest2)  = partitionWith isPlus rest1
         (other_opts, rest3) = partition (`elem` map fst defaulters) rest2

         defaulters =
           [ ("args"   , setArgs default_args)
           , ("prog"   , setProg default_progname)
           , ("prompt"     , setPrompt default_prompt)
           , ("prompt-cont", setPromptCont default_prompt_cont)
           , ("editor" , liftIO findEditor >>= setEditor)
           , ("stop"   , setStop default_stop)
           ]

         no_flag ('-':'f':rest) = return ("-fno-" ++ rest)
         no_flag ('-':'X':rest) = return ("-XNo" ++ rest)
         no_flag f = throwGhcException (ProgramError ("don't know how to reverse " ++ f))

     in if (not (null rest3))
           then liftIO (putStrLn ("unknown option: '" ++ head rest3 ++ "'"))
           else do
             mapM_ (fromJust.flip lookup defaulters) other_opts

             mapM_ unsetOpt plus_opts

             no_flags <- mapM no_flag minus_opts
             when (not (null no_flags)) $ newDynFlags False no_flags

isMinus :: String -> Bool
isMinus ('-':_) = True
isMinus _ = False

isPlus :: String -> Either String String
isPlus ('+':opt) = Left opt
isPlus other     = Right other

setOpt, unsetOpt :: String -> GHCi ()

setOpt str
  = case strToGHCiOpt str of
        Nothing -> liftIO (putStrLn ("unknown option: '" ++ str ++ "'"))
        Just o  -> setOption o

unsetOpt str
  = case strToGHCiOpt str of
        Nothing -> liftIO (putStrLn ("unknown option: '" ++ str ++ "'"))
        Just o  -> unsetOption o

strToGHCiOpt :: String -> (Maybe GHCiOption)
strToGHCiOpt "m" = Just Multiline
strToGHCiOpt "s" = Just ShowTiming
strToGHCiOpt "t" = Just ShowType
strToGHCiOpt "r" = Just RevertCAFs
strToGHCiOpt "c" = Just CollectInfo
strToGHCiOpt _   = Nothing

optToStr :: GHCiOption -> String
optToStr Multiline  = "m"
optToStr ShowTiming = "s"
optToStr ShowType   = "t"
optToStr RevertCAFs = "r"
optToStr CollectInfo = "c"


-- ---------------------------------------------------------------------------
-- :show

showCmd :: String -> GHCi ()
showCmd ""   = showOptions False
showCmd "-a" = showOptions True
showCmd str = do
    st <- getGHCiState
    dflags <- getDynFlags

    let lookupCmd :: String -> Maybe (GHCi ())
        lookupCmd name = lookup name $ map (\(_,b,c) -> (b,c)) cmds

        -- (show in help?, command name, action)
        action :: String -> GHCi () -> (Bool, String, GHCi ())
        action name m = (True, name, m)

        hidden :: String -> GHCi () -> (Bool, String, GHCi ())
        hidden name m = (False, name, m)

        cmds =
            [ action "args"       $ liftIO $ putStrLn (show (GhciMonad.args st))
            , action "prog"       $ liftIO $ putStrLn (show (progname st))
            , action "editor"     $ liftIO $ putStrLn (show (editor st))
            , action "stop"       $ liftIO $ putStrLn (show (stop st))
            , action "imports"    $ showImports
            , action "modules"    $ showModules
            , action "bindings"   $ showBindings
            , action "linker"     $ getDynFlags >>= liftIO . showLinkerState
            , action "breaks"     $ showBkptTable
            , action "context"    $ showContext
            , action "packages"   $ showPackages
            , action "paths"      $ showPaths
            , action "language"   $ showLanguages
            , hidden "languages"  $ showLanguages -- backwards compat
            , hidden "lang"       $ showLanguages -- useful abbreviation
            ]

    case words str of
      [w] | Just action <- lookupCmd w -> action

      _ -> let helpCmds = [ text name | (True, name, _) <- cmds ]
           in throwGhcException $ CmdLineError $ showSDoc dflags
              $ hang (text "syntax:") 4
              $ hang (text ":show") 6
              $ brackets (fsep $ punctuate (text " |") helpCmds)

showiCmd :: String -> GHCi ()
showiCmd str = do
  case words str of
        ["languages"]  -> showiLanguages -- backwards compat
        ["language"]   -> showiLanguages
        ["lang"]       -> showiLanguages -- useful abbreviation
        _ -> throwGhcException (CmdLineError ("syntax:  :showi language"))

showImports :: GHCi ()
showImports = do
  st <- getGHCiState
  dflags <- getDynFlags
  let rem_ctx   = reverse (remembered_ctx st)
      trans_ctx = transient_ctx st

      show_one (IIModule star_m)
          = ":module +*" ++ moduleNameString star_m
      show_one (IIDecl imp) = showPpr dflags imp

  prel_iidecls <- getImplicitPreludeImports (rem_ctx ++ trans_ctx)

  let show_prel p = show_one p ++ " -- implicit"
      show_extra p = show_one (IIDecl p) ++ " -- fixed"

      trans_comment s = s ++ " -- added automatically" :: String
  --
  liftIO $ mapM_ putStrLn (map show_one rem_ctx ++
                           map (trans_comment . show_one) trans_ctx ++
                           map show_prel prel_iidecls ++
                           map show_extra (extra_imports st))

showModules :: GHCi ()
showModules = do
  loaded_mods <- getLoadedModules
        -- we want *loaded* modules only, see #1734
  let show_one ms = do m <- GHC.showModule ms; liftIO (putStrLn m)
  mapM_ show_one loaded_mods

getLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getLoadedModules = do
  graph <- GHC.getModuleGraph
  filterM (GHC.isLoaded . GHC.ms_mod_name) graph

showBindings :: GHCi ()
showBindings = do
    bindings <- GHC.getBindings
    (insts, finsts) <- GHC.getInsts
    docs     <- mapM makeDoc (reverse bindings)
                  -- reverse so the new ones come last
    let idocs  = map GHC.pprInstanceHdr insts
        fidocs = map GHC.pprFamInst finsts
    mapM_ printForUserPartWay (docs ++ idocs ++ fidocs)
  where
    makeDoc (AnId i) = pprTypeAndContents i
    makeDoc tt = do
        mb_stuff <- GHC.getInfo False (getName tt)
        return $ maybe (text "") pprTT mb_stuff

    pprTT :: (TyThing, Fixity, [GHC.ClsInst], [GHC.FamInst]) -> SDoc
    pprTT (thing, fixity, _cls_insts, _fam_insts)
      = pprTyThing showToHeader thing
        $$ show_fixity
      where
        show_fixity
            | fixity == GHC.defaultFixity  = empty
            | otherwise                    = ppr fixity <+> ppr (GHC.getName thing)


printTyThing :: TyThing -> GHCi ()
printTyThing tyth = printForUser (pprTyThing showToHeader tyth)

showBkptTable :: GHCi ()
showBkptTable = do
  st <- getGHCiState
  printForUser $ prettyLocations (breaks st)

showContext :: GHCi ()
showContext = do
   resumes <- GHC.getResumeContext
   printForUser $ vcat (map pp_resume (reverse resumes))
  where
   pp_resume res =
        ptext (sLit "--> ") <> text (GHC.resumeStmt res)
        $$ nest 2 (pprStopped res)

pprStopped :: GHC.Resume -> SDoc
pprStopped res =
  ptext (sLit "Stopped in")
    <+> ((case mb_mod_name of
           Nothing -> empty
           Just mod_name -> text (moduleNameString mod_name) <> char '.')
         <> text (GHC.resumeDecl res))
    <> char ',' <+> ppr (GHC.resumeSpan res)
 where
  mb_mod_name = moduleName <$> GHC.breakInfo_module <$> GHC.resumeBreakInfo res

showPackages :: GHCi ()
showPackages = do
  dflags <- getDynFlags
  let pkg_flags = packageFlags dflags
  liftIO $ putStrLn $ showSDoc dflags $
    text ("active package flags:"++if null pkg_flags then " none" else "") $$
      nest 2 (vcat (map pprFlag pkg_flags))

showPaths :: GHCi ()
showPaths = do
  dflags <- getDynFlags
  liftIO $ do
    cwd <- getCurrentDirectory
    putStrLn $ showSDoc dflags $
      text "current working directory: " $$
        nest 2 (text cwd)
    let ipaths = importPaths dflags
    putStrLn $ showSDoc dflags $
      text ("module import search paths:"++if null ipaths then " none" else "") $$
        nest 2 (vcat (map text ipaths))

showLanguages :: GHCi ()
showLanguages = getDynFlags >>= liftIO . showLanguages' False

showiLanguages :: GHCi ()
showiLanguages = GHC.getInteractiveDynFlags >>= liftIO . showLanguages' False

showLanguages' :: Bool -> DynFlags -> IO ()
showLanguages' show_all dflags =
  putStrLn $ showSDoc dflags $ vcat
     [ text "base language is: " <>
         case language dflags of
           Nothing          -> text "Haskell2010"
           Just Haskell98   -> text "Haskell98"
           Just Haskell2010 -> text "Haskell2010"
     , (if show_all then text "all active language options:"
                    else text "with the following modifiers:") $$
          nest 2 (vcat (map (setting xopt) DynFlags.xFlags))
     ]
  where
   setting test flag
          | quiet     = empty
          | is_on     = text "-X" <> text name
          | otherwise = text "-XNo" <> text name
          where name = flagSpecName flag
                f = flagSpecFlag flag
                is_on = test f dflags
                quiet = not show_all && test f default_dflags == is_on

   default_dflags =
       defaultDynFlags (settings dflags) `lang_set`
         case language dflags of
           Nothing -> Just Haskell2010
           other   -> other

-- -----------------------------------------------------------------------------
-- Completion

completeCmd :: String -> GHCi ()
completeCmd argLine0 = case parseLine argLine0 of
    Just ("repl", resultRange, left) -> do
        (unusedLine,compls) <- ghciCompleteWord (reverse left,"")
        let compls' = takeRange resultRange compls
        liftIO . putStrLn $ unwords [ show (length compls'), show (length compls), show (reverse unusedLine) ]
        forM_ (takeRange resultRange compls) $ \(Completion r _ _) -> do
            liftIO $ print r
    _ -> throwGhcException (CmdLineError "Syntax: :complete repl [<range>] <quoted-string-to-complete>")
  where
    parseLine argLine
        | null argLine = Nothing
        | null rest1   = Nothing
        | otherwise    = (,,) dom <$> resRange <*> s
      where
        (dom, rest1) = breakSpace argLine
        (rng, rest2) = breakSpace rest1
        resRange | head rest1 == '"' = parseRange ""
                 | otherwise         = parseRange rng
        s | head rest1 == '"' = readMaybe rest1 :: Maybe String
          | otherwise         = readMaybe rest2
        breakSpace = fmap (dropWhile isSpace) . break isSpace

    takeRange (lb,ub) = maybe id (drop . pred) lb . maybe id take ub

    -- syntax: [n-][m] with semantics "drop (n-1) . take m"
    parseRange :: String -> Maybe (Maybe Int,Maybe Int)
    parseRange s = case span isDigit s of
                   (_, "") ->
                       -- upper limit only
                       Just (Nothing, bndRead s)
                   (s1, '-' : s2)
                    | all isDigit s2 ->
                       Just (bndRead s1, bndRead s2)
                   _ ->
                       Nothing
      where
        bndRead x = if null x then Nothing else Just (read x)



completeGhciCommand, completeMacro, completeIdentifier, completeModule,
    completeSetModule, completeSeti, completeShowiOptions,
    completeHomeModule, completeSetOptions, completeShowOptions,
    completeHomeModuleOrFile, completeExpression
    :: CompletionFunc GHCi

-- | Provide completions for last word in a given string.
--
-- Takes a tuple of two strings.  First string is a reversed line to be
-- completed.  Second string is likely unused, 'completeCmd' always passes an
-- empty string as second item in tuple.
ghciCompleteWord :: CompletionFunc GHCi
ghciCompleteWord line@(left,_) = case firstWord of
    -- If given string starts with `:` colon, and there is only one following
    -- word then provide REPL command completions.  If there is more than one
    -- word complete either filename or builtin ghci commands or macros.
    ':':cmd     | null rest     -> completeGhciCommand line
                | otherwise     -> do
                        completion <- lookupCompletion cmd
                        completion line
    -- If given string starts with `import` keyword provide module name
    -- completions
    "import"    -> completeModule line
    -- otherwise provide identifier completions
    _           -> completeExpression line
  where
    (firstWord,rest) = break isSpace $ dropWhile isSpace $ reverse left
    lookupCompletion ('!':_) = return completeFilename
    lookupCompletion c = do
        maybe_cmd <- lookupCommand' c
        case maybe_cmd of
            Just cmd -> return (cmdCompletionFunc cmd)
            Nothing  -> return completeFilename

completeGhciCommand = wrapCompleter " " $ \w -> do
  macros <- ghci_macros <$> getGHCiState
  cmds   <- ghci_commands `fmap` getGHCiState
  let macro_names = map (':':) . map cmdName $ macros
  let command_names = map (':':) . map cmdName $ filter (not . cmdHidden) cmds
  let{ candidates = case w of
      ':' : ':' : _ -> map (':':) command_names
      _ -> nub $ macro_names ++ command_names }
  return $ filter (w `isPrefixOf`) candidates

completeMacro = wrapIdentCompleter $ \w -> do
  cmds <- ghci_macros <$> getGHCiState
  return (filter (w `isPrefixOf`) (map cmdName cmds))

completeIdentifier line@(left, _) =
  -- Note: `left` is a reversed input
  case left of
    (x:_) | isSymbolChar x -> wrapCompleter (specials ++ spaces) complete line
    _                      -> wrapIdentCompleter complete line
  where
    complete w = do
      rdrs <- GHC.getRdrNamesInScope
      dflags <- GHC.getSessionDynFlags
      return (filter (w `isPrefixOf`) (map (showPpr dflags) rdrs))

completeModule = wrapIdentCompleter $ \w -> do
  dflags <- GHC.getSessionDynFlags
  let pkg_mods = allVisibleModules dflags
  loaded_mods <- liftM (map GHC.ms_mod_name) getLoadedModules
  return $ filter (w `isPrefixOf`)
        $ map (showPpr dflags) $ loaded_mods ++ pkg_mods

completeSetModule = wrapIdentCompleterWithModifier "+-" $ \m w -> do
  dflags <- GHC.getSessionDynFlags
  modules <- case m of
    Just '-' -> do
      imports <- GHC.getContext
      return $ map iiModuleName imports
    _ -> do
      let pkg_mods = allVisibleModules dflags
      loaded_mods <- liftM (map GHC.ms_mod_name) getLoadedModules
      return $ loaded_mods ++ pkg_mods
  return $ filter (w `isPrefixOf`) $ map (showPpr dflags) modules

completeHomeModule = wrapIdentCompleter listHomeModules

listHomeModules :: String -> GHCi [String]
listHomeModules w = do
    g <- GHC.getModuleGraph
    let home_mods = map GHC.ms_mod_name g
    dflags <- getDynFlags
    return $ sort $ filter (w `isPrefixOf`)
            $ map (showPpr dflags) home_mods

completeSetOptions = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) opts)
    where opts = "args":"prog":"prompt":"prompt-cont":"prompt-function":
                 "prompt-cont-function":"editor":"stop":flagList
          flagList = map head $ group $ sort allNonDeprecatedFlags

completeSeti = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) flagList)
    where flagList = map head $ group $ sort allNonDeprecatedFlags

completeShowOptions = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) opts)
    where opts = ["args", "prog", "editor", "stop",
                     "modules", "bindings", "linker", "breaks",
                     "context", "packages", "paths", "language", "imports"]

completeShowiOptions = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) ["language"])

completeHomeModuleOrFile = completeWord Nothing filenameWordBreakChars
                $ unionComplete (fmap (map simpleCompletion) . listHomeModules)
                            listFiles

unionComplete :: Monad m => (a -> m [b]) -> (a -> m [b]) -> a -> m [b]
unionComplete f1 f2 line = do
  cs1 <- f1 line
  cs2 <- f2 line
  return (cs1 ++ cs2)

wrapCompleter :: String -> (String -> GHCi [String]) -> CompletionFunc GHCi
wrapCompleter breakChars fun = completeWord Nothing breakChars
    $ fmap (map simpleCompletion . nubSort) . fun

wrapIdentCompleter :: (String -> GHCi [String]) -> CompletionFunc GHCi
wrapIdentCompleter = wrapCompleter word_break_chars

wrapIdentCompleterWithModifier :: String -> (Maybe Char -> String -> GHCi [String]) -> CompletionFunc GHCi
wrapIdentCompleterWithModifier modifChars fun = completeWordWithPrev Nothing word_break_chars
    $ \rest -> fmap (map simpleCompletion . nubSort) . fun (getModifier rest)
 where
  getModifier = find (`elem` modifChars)

-- | Return a list of visible module names for autocompletion.
-- (NB: exposed != visible)
allVisibleModules :: DynFlags -> [ModuleName]
allVisibleModules dflags = listVisibleModuleNames dflags

completeExpression = completeQuotedWord (Just '\\') "\"" listFiles
                        completeIdentifier


-- -----------------------------------------------------------------------------
-- commands for debugger

sprintCmd, printCmd, forceCmd :: String -> GHCi ()
sprintCmd = pprintCommand False False
printCmd  = pprintCommand True False
forceCmd  = pprintCommand False True

pprintCommand :: Bool -> Bool -> String -> GHCi ()
pprintCommand bind force str = do
  pprintClosureCommand bind force str

stepCmd :: String -> GHCi ()
stepCmd arg = withSandboxOnly ":step" $ step arg
  where
  step []         = doContinue (const True) GHC.SingleStep
  step expression = runStmt expression GHC.SingleStep >> return ()

stepLocalCmd :: String -> GHCi ()
stepLocalCmd arg = withSandboxOnly ":steplocal" $ step arg
  where
  step expr
   | not (null expr) = stepCmd expr
   | otherwise = do
      mb_span <- getCurrentBreakSpan
      case mb_span of
        Nothing  -> stepCmd []
        Just loc -> do
           Just md <- getCurrentBreakModule
           current_toplevel_decl <- enclosingTickSpan md loc
           doContinue (`isSubspanOf` RealSrcSpan current_toplevel_decl) GHC.SingleStep

stepModuleCmd :: String -> GHCi ()
stepModuleCmd arg = withSandboxOnly ":stepmodule" $ step arg
  where
  step expr
   | not (null expr) = stepCmd expr
   | otherwise = do
      mb_span <- getCurrentBreakSpan
      case mb_span of
        Nothing  -> stepCmd []
        Just pan -> do
           let f some_span = srcSpanFileName_maybe pan == srcSpanFileName_maybe some_span
           doContinue f GHC.SingleStep

-- | Returns the span of the largest tick containing the srcspan given
enclosingTickSpan :: Module -> SrcSpan -> GHCi RealSrcSpan
enclosingTickSpan _ (UnhelpfulSpan _) = panic "enclosingTickSpan UnhelpfulSpan"
enclosingTickSpan md (RealSrcSpan src) = do
  ticks <- getTickArray md
  let line = srcSpanStartLine src
  ASSERT(inRange (bounds ticks) line) do
  let enclosing_spans = [ pan | (_,pan) <- ticks ! line
                               , realSrcSpanEnd pan >= realSrcSpanEnd src]
  return . head . sortBy leftmostLargestRealSrcSpan $ enclosing_spans
 where

leftmostLargestRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> Ordering
leftmostLargestRealSrcSpan a b =
  (realSrcSpanStart a `compare` realSrcSpanStart b)
     `thenCmp`
  (realSrcSpanEnd b `compare` realSrcSpanEnd a)

traceCmd :: String -> GHCi ()
traceCmd arg
  = withSandboxOnly ":trace" $ tr arg
  where
  tr []         = doContinue (const True) GHC.RunAndLogSteps
  tr expression = runStmt expression GHC.RunAndLogSteps >> return ()

continueCmd :: String -> GHCi ()
continueCmd = noArgs $ withSandboxOnly ":continue" $ doContinue (const True) GHC.RunToCompletion

-- doContinue :: SingleStep -> GHCi ()
doContinue :: (SrcSpan -> Bool) -> SingleStep -> GHCi ()
doContinue pre step = do
  runResult <- resume pre step
  _ <- afterRunStmt pre runResult
  return ()

abandonCmd :: String -> GHCi ()
abandonCmd = noArgs $ withSandboxOnly ":abandon" $ do
  b <- GHC.abandon -- the prompt will change to indicate the new context
  when (not b) $ liftIO $ putStrLn "There is no computation running."

deleteCmd :: String -> GHCi ()
deleteCmd argLine = withSandboxOnly ":delete" $ do
   deleteSwitch $ words argLine
   where
   deleteSwitch :: [String] -> GHCi ()
   deleteSwitch [] =
      liftIO $ putStrLn "The delete command requires at least one argument."
   -- delete all break points
   deleteSwitch ("*":_rest) = discardActiveBreakPoints
   deleteSwitch idents = do
      mapM_ deleteOneBreak idents
      where
      deleteOneBreak :: String -> GHCi ()
      deleteOneBreak str
         | all isDigit str = deleteBreak (read str)
         | otherwise = return ()

historyCmd :: String -> GHCi ()
historyCmd arg
  | null arg        = history 20
  | all isDigit arg = history (read arg)
  | otherwise       = liftIO $ putStrLn "Syntax:  :history [num]"
  where
  history num = do
    resumes <- GHC.getResumeContext
    case resumes of
      [] -> liftIO $ putStrLn "Not stopped at a breakpoint"
      (r:_) -> do
        let hist = GHC.resumeHistory r
            (took,rest) = splitAt num hist
        case hist of
          [] -> liftIO $ putStrLn $
                   "Empty history. Perhaps you forgot to use :trace?"
          _  -> do
                 pans <- mapM GHC.getHistorySpan took
                 let nums  = map (printf "-%-3d:") [(1::Int)..]
                     names = map GHC.historyEnclosingDecls took
                 printForUser (vcat(zipWith3
                                 (\x y z -> x <+> y <+> z)
                                 (map text nums)
                                 (map (bold . hcat . punctuate colon . map text) names)
                                 (map (parens . ppr) pans)))
                 liftIO $ putStrLn $ if null rest then "<end of history>" else "..."

bold :: SDoc -> SDoc
bold c | do_bold   = text start_bold <> c <> text end_bold
       | otherwise = c

backCmd :: String -> GHCi ()
backCmd arg
  | null arg        = back 1
  | all isDigit arg = back (read arg)
  | otherwise       = liftIO $ putStrLn "Syntax:  :back [num]"
  where
  back num = withSandboxOnly ":back" $ do
      (names, _, pan, _) <- GHC.back num
      printForUser $ ptext (sLit "Logged breakpoint at") <+> ppr pan
      printTypeOfNames names
       -- run the command set with ":set stop <cmd>"
      st <- getGHCiState
      enqueueCommands [stop st]

forwardCmd :: String -> GHCi ()
forwardCmd arg
  | null arg        = forward 1
  | all isDigit arg = forward (read arg)
  | otherwise       = liftIO $ putStrLn "Syntax:  :back [num]"
  where
  forward num = withSandboxOnly ":forward" $ do
      (names, ix, pan, _) <- GHC.forward num
      printForUser $ (if (ix == 0)
                        then ptext (sLit "Stopped at")
                        else ptext (sLit "Logged breakpoint at")) <+> ppr pan
      printTypeOfNames names
       -- run the command set with ":set stop <cmd>"
      st <- getGHCiState
      enqueueCommands [stop st]

-- handle the "break" command
breakCmd :: String -> GHCi ()
breakCmd argLine = withSandboxOnly ":break" $ breakSwitch $ words argLine

breakSwitch :: [String] -> GHCi ()
breakSwitch [] = do
   liftIO $ putStrLn "The break command requires at least one argument."
breakSwitch (arg1:rest)
   | looksLikeModuleName arg1 && not (null rest) = do
        md <- wantInterpretedModule arg1
        breakByModule md rest
   | all isDigit arg1 = do
        imports <- GHC.getContext
        case iiModules imports of
           (mn : _) -> do
              md <- lookupModuleName mn
              breakByModuleLine md (read arg1) rest
           [] -> do
              liftIO $ putStrLn "No modules are loaded with debugging support."
   | otherwise = do -- try parsing it as an identifier
        wantNameFromInterpretedModule noCanDo arg1 $ \name -> do
        maybe_info <- GHC.getModuleInfo (GHC.nameModule name)
        case maybe_info of
          Nothing -> noCanDo name (ptext (sLit "cannot get module info"))
          Just minf ->
               ASSERT( isExternalName name )
                    findBreakAndSet (GHC.nameModule name) $
                       findBreakForBind name (GHC.modInfoModBreaks minf)
       where
          noCanDo n why = printForUser $
                text "cannot set breakpoint on " <> ppr n <> text ": " <> why

breakByModule :: Module -> [String] -> GHCi ()
breakByModule md (arg1:rest)
   | all isDigit arg1 = do  -- looks like a line number
        breakByModuleLine md (read arg1) rest
breakByModule _ _
   = breakSyntax

breakByModuleLine :: Module -> Int -> [String] -> GHCi ()
breakByModuleLine md line args
   | [] <- args = findBreakAndSet md $ maybeToList . findBreakByLine line
   | [col] <- args, all isDigit col =
        findBreakAndSet md $ maybeToList . findBreakByCoord Nothing (line, read col)
   | otherwise = breakSyntax

breakSyntax :: a
breakSyntax = throwGhcException (CmdLineError "Syntax: :break [<mod>] <line> [<column>]")

findBreakAndSet :: Module -> (TickArray -> [(Int, RealSrcSpan)]) -> GHCi ()
findBreakAndSet md lookupTickTree = do
   tickArray <- getTickArray md
   (breakArray, _) <- getModBreak md
   case lookupTickTree tickArray of
      []  -> liftIO $ putStrLn $ "No breakpoints found at that location."
      some -> mapM_ (breakAt breakArray) some
 where
   breakAt breakArray (tick, pan) = do
         setBreakFlag True breakArray tick
         (alreadySet, nm) <-
               recordBreak $ BreakLocation
                       { breakModule = md
                       , breakLoc = RealSrcSpan pan
                       , breakTick = tick
                       , onBreakCmd = ""
                       }
         printForUser $
            text "Breakpoint " <> ppr nm <>
            if alreadySet
               then text " was already set at " <> ppr pan
               else text " activated at " <> ppr pan

-- When a line number is specified, the current policy for choosing
-- the best breakpoint is this:
--    - the leftmost complete subexpression on the specified line, or
--    - the leftmost subexpression starting on the specified line, or
--    - the rightmost subexpression enclosing the specified line
--
findBreakByLine :: Int -> TickArray -> Maybe (BreakIndex,RealSrcSpan)
findBreakByLine line arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (leftmostLargestRealSrcSpan `on` snd)  comp)   `mplus`
    listToMaybe (sortBy (compare `on` snd) incomp) `mplus`
    listToMaybe (sortBy (flip compare `on` snd) ticks)
  where
        ticks = arr ! line

        starts_here = [ (ix,pan) | (ix, pan) <- ticks,
                        GHC.srcSpanStartLine pan == line ]

        (comp, incomp) = partition ends_here starts_here
            where ends_here (_,pan) = GHC.srcSpanEndLine pan == line

-- The aim is to find the breakpoints for all the RHSs of the
-- equations corresponding to a binding.  So we find all breakpoints
-- for
--   (a) this binder only (not a nested declaration)
--   (b) that do not have an enclosing breakpoint
findBreakForBind :: Name -> GHC.ModBreaks -> TickArray
                 -> [(BreakIndex,RealSrcSpan)]
findBreakForBind name modbreaks _ = filter (not . enclosed) ticks
  where
    ticks = [ (index, span)
            | (index, [n]) <- assocs (GHC.modBreaks_decls modbreaks),
              n == occNameString (nameOccName name),
              RealSrcSpan span <- [GHC.modBreaks_locs modbreaks ! index] ]
    enclosed (_,sp0) = any subspan ticks
      where subspan (_,sp) = sp /= sp0 &&
                         realSrcSpanStart sp <= realSrcSpanStart sp0 &&
                         realSrcSpanEnd sp0 <= realSrcSpanEnd sp

findBreakByCoord :: Maybe FastString -> (Int,Int) -> TickArray
                 -> Maybe (BreakIndex,RealSrcSpan)
findBreakByCoord mb_file (line, col) arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (flip compare `on` snd) contains ++
                 sortBy (compare `on` snd) after_here)
  where
        ticks = arr ! line

        -- the ticks that span this coordinate
        contains = [ tick | tick@(_,pan) <- ticks, RealSrcSpan pan `spans` (line,col),
                            is_correct_file pan ]

        is_correct_file pan
                 | Just f <- mb_file = GHC.srcSpanFile pan == f
                 | otherwise         = True

        after_here = [ tick | tick@(_,pan) <- ticks,
                              GHC.srcSpanStartLine pan == line,
                              GHC.srcSpanStartCol pan >= col ]

-- For now, use ANSI bold on terminals that we know support it.
-- Otherwise, we add a line of carets under the active expression instead.
-- In particular, on Windows and when running the testsuite (which sets
-- TERM to vt100 for other reasons) we get carets.
-- We really ought to use a proper termcap/terminfo library.
do_bold :: Bool
do_bold = (`isPrefixOf` unsafePerformIO mTerm) `any` ["xterm", "linux"]
    where mTerm = System.Environment.getEnv "TERM"
                  `catchIO` \_ -> return "TERM not set"

start_bold :: String
start_bold = "\ESC[1m"
end_bold :: String
end_bold   = "\ESC[0m"

-----------------------------------------------------------------------------
-- :where

whereCmd :: String -> GHCi ()
whereCmd = noArgs $ do
  mstrs <- getCallStackAtCurrentBreakpoint
  case mstrs of
    Nothing -> return ()
    Just strs -> liftIO $ putStrLn (renderStack strs)

-----------------------------------------------------------------------------
-- :list

listCmd :: String -> InputT GHCi ()
listCmd c = listCmd' c

listCmd' :: String -> InputT GHCi ()
listCmd' "" = do
   mb_span <- lift getCurrentBreakSpan
   case mb_span of
      Nothing ->
          printForUser $ text "Not stopped at a breakpoint; nothing to list"
      Just (RealSrcSpan pan) ->
          listAround pan True
      Just pan@(UnhelpfulSpan _) ->
          do resumes <- GHC.getResumeContext
             case resumes of
                 [] -> panic "No resumes"
                 (r:_) ->
                     do let traceIt = case GHC.resumeHistory r of
                                      [] -> text "rerunning with :trace,"
                                      _ -> empty
                            doWhat = traceIt <+> text ":back then :list"
                        printForUser (text "Unable to list source for" <+>
                                      ppr pan
                                   $$ text "Try" <+> doWhat)
listCmd' str = list2 (words str)

list2 :: [String] -> InputT GHCi ()
list2 [arg] | all isDigit arg = do
    imports <- GHC.getContext
    case iiModules imports of
        [] -> liftIO $ putStrLn "No module to list"
        (mn : _) -> do
          md <- lift $ lookupModuleName mn
          listModuleLine md (read arg)
list2 [arg1,arg2] | looksLikeModuleName arg1, all isDigit arg2 = do
        md <- wantInterpretedModule arg1
        listModuleLine md (read arg2)
list2 [arg] = do
        wantNameFromInterpretedModule noCanDo arg $ \name -> do
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        case loc of
            RealSrcLoc l ->
               do tickArray <- ASSERT( isExternalName name )
                               lift $ getTickArray (GHC.nameModule name)
                  let mb_span = findBreakByCoord (Just (GHC.srcLocFile l))
                                        (GHC.srcLocLine l, GHC.srcLocCol l)
                                        tickArray
                  case mb_span of
                    Nothing       -> listAround (realSrcLocSpan l) False
                    Just (_, pan) -> listAround pan False
            UnhelpfulLoc _ ->
                  noCanDo name $ text "can't find its location: " <>
                                 ppr loc
    where
        noCanDo n why = printForUser $
            text "cannot list source code for " <> ppr n <> text ": " <> why
list2  _other =
        liftIO $ putStrLn "syntax:  :list [<line> | <module> <line> | <identifier>]"

listModuleLine :: Module -> Int -> InputT GHCi ()
listModuleLine modl line = do
   graph <- GHC.getModuleGraph
   let this = filter ((== modl) . GHC.ms_mod) graph
   case this of
     [] -> panic "listModuleLine"
     summ:_ -> do
           let filename = expectJust "listModuleLine" (ml_hs_file (GHC.ms_location summ))
               loc = mkRealSrcLoc (mkFastString (filename)) line 0
           listAround (realSrcLocSpan loc) False

-- | list a section of a source file around a particular SrcSpan.
-- If the highlight flag is True, also highlight the span using
-- start_bold\/end_bold.

-- GHC files are UTF-8, so we can implement this by:
-- 1) read the file in as a BS and syntax highlight it as before
-- 2) convert the BS to String using utf-string, and write it out.
-- It would be better if we could convert directly between UTF-8 and the
-- console encoding, of course.
listAround :: MonadIO m => RealSrcSpan -> Bool -> InputT m ()
listAround pan do_highlight = do
      contents <- liftIO $ BS.readFile (unpackFS file)
      -- Drop carriage returns to avoid duplicates, see #9367.
      let ls  = BS.split '\n' $ BS.filter (/= '\r') contents
          ls' = take (line2 - line1 + 1 + pad_before + pad_after) $
                        drop (line1 - 1 - pad_before) $ ls
          fst_line = max 1 (line1 - pad_before)
          line_nos = [ fst_line .. ]

          highlighted | do_highlight = zipWith highlight line_nos ls'
                      | otherwise    = [\p -> BS.concat[p,l] | l <- ls']

          bs_line_nos = [ BS.pack (show l ++ "  ") | l <- line_nos ]
          prefixed = zipWith ($) highlighted bs_line_nos
          output   = BS.intercalate (BS.pack "\n") prefixed

      let utf8Decoded = utf8DecodeByteString output
      liftIO $ putStrLn utf8Decoded
  where
        file  = GHC.srcSpanFile pan
        line1 = GHC.srcSpanStartLine pan
        col1  = GHC.srcSpanStartCol pan - 1
        line2 = GHC.srcSpanEndLine pan
        col2  = GHC.srcSpanEndCol pan - 1

        pad_before | line1 == 1 = 0
                   | otherwise  = 1
        pad_after = 1

        highlight | do_bold   = highlight_bold
                  | otherwise = highlight_carets

        highlight_bold no line prefix
          | no == line1 && no == line2
          = let (a,r) = BS.splitAt col1 line
                (b,c) = BS.splitAt (col2-col1) r
            in
            BS.concat [prefix, a,BS.pack start_bold,b,BS.pack end_bold,c]
          | no == line1
          = let (a,b) = BS.splitAt col1 line in
            BS.concat [prefix, a, BS.pack start_bold, b]
          | no == line2
          = let (a,b) = BS.splitAt col2 line in
            BS.concat [prefix, a, BS.pack end_bold, b]
          | otherwise   = BS.concat [prefix, line]

        highlight_carets no line prefix
          | no == line1 && no == line2
          = BS.concat [prefix, line, nl, indent, BS.replicate col1 ' ',
                                         BS.replicate (col2-col1) '^']
          | no == line1
          = BS.concat [indent, BS.replicate (col1 - 2) ' ', BS.pack "vv", nl,
                                         prefix, line]
          | no == line2
          = BS.concat [prefix, line, nl, indent, BS.replicate col2 ' ',
                                         BS.pack "^^"]
          | otherwise   = BS.concat [prefix, line]
         where
           indent = BS.pack ("  " ++ replicate (length (show no)) ' ')
           nl = BS.singleton '\n'


-- --------------------------------------------------------------------------
-- Tick arrays

getTickArray :: Module -> GHCi TickArray
getTickArray modl = do
   st <- getGHCiState
   let arrmap = tickarrays st
   case lookupModuleEnv arrmap modl of
      Just arr -> return arr
      Nothing  -> do
        (_breakArray, ticks) <- getModBreak modl
        let arr = mkTickArray (assocs ticks)
        setGHCiState st{tickarrays = extendModuleEnv arrmap modl arr}
        return arr

discardTickArrays :: GHCi ()
discardTickArrays = modifyGHCiState (\st -> st {tickarrays = emptyModuleEnv})

mkTickArray :: [(BreakIndex,SrcSpan)] -> TickArray
mkTickArray ticks
  = accumArray (flip (:)) [] (1, max_line)
        [ (line, (nm,pan)) | (nm,RealSrcSpan pan) <- ticks, line <- srcSpanLines pan ]
    where
        max_line = foldr max 0 [ GHC.srcSpanEndLine sp | (_, RealSrcSpan sp) <- ticks ]
        srcSpanLines pan = [ GHC.srcSpanStartLine pan ..  GHC.srcSpanEndLine pan ]

-- don't reset the counter back to zero?
discardActiveBreakPoints :: GHCi ()
discardActiveBreakPoints = do
   st <- getGHCiState
   mapM_ (turnOffBreak.snd) (breaks st)
   setGHCiState $ st { breaks = [] }

deleteBreak :: Int -> GHCi ()
deleteBreak identity = do
   st <- getGHCiState
   let oldLocations    = breaks st
       (this,rest)     = partition (\loc -> fst loc == identity) oldLocations
   if null this
      then printForUser (text "Breakpoint" <+> ppr identity <+>
                         text "does not exist")
      else do
           mapM_ (turnOffBreak.snd) this
           setGHCiState $ st { breaks = rest }

turnOffBreak :: BreakLocation -> GHCi ()
turnOffBreak loc = do
  (arr, _) <- getModBreak (breakModule loc)
  hsc_env <- GHC.getSession
  liftIO $ enableBreakpoint hsc_env arr (breakTick loc) False

getModBreak :: Module -> GHCi (ForeignRef BreakArray, Array Int SrcSpan)
getModBreak m = do
   Just mod_info <- GHC.getModuleInfo m
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let arr        = GHC.modBreaks_flags modBreaks
   let ticks      = GHC.modBreaks_locs  modBreaks
   return (arr, ticks)

setBreakFlag :: Bool -> ForeignRef BreakArray -> Int -> GHCi ()
setBreakFlag toggle arr i = do
  hsc_env <- GHC.getSession
  liftIO $ enableBreakpoint hsc_env arr i toggle

-- ---------------------------------------------------------------------------
-- User code exception handling

-- This is the exception handler for exceptions generated by the
-- user's code and exceptions coming from children sessions;
-- it normally just prints out the exception.  The
-- handler must be recursive, in case showing the exception causes
-- more exceptions to be raised.
--
-- Bugfix: if the user closed stdout or stderr, the flushing will fail,
-- raising another exception.  We therefore don't put the recursive
-- handler arond the flushing operation, so if stderr is closed
-- GHCi will just die gracefully rather than going into an infinite loop.
handler :: SomeException -> GHCi Bool

handler exception = do
  flushInterpBuffers
  withSignalHandlers $
     ghciHandle handler (showException exception >> return False)

showException :: SomeException -> GHCi ()
showException se =
  liftIO $ case fromException se of
           -- omit the location for CmdLineError:
           Just (CmdLineError s)    -> putException s
           -- ditto:
           Just other_ghc_ex        -> putException (show other_ghc_ex)
           Nothing                  ->
               case fromException se of
               Just UserInterrupt -> putException "Interrupted."
               _                  -> putException ("*** Exception: " ++ show se)
  where
    putException = hPutStrLn stderr


-----------------------------------------------------------------------------
-- recursive exception handlers

-- Don't forget to unblock async exceptions in the handler, or if we're
-- in an exception loop (eg. let a = error a in a) the ^C exception
-- may never be delivered.  Thanks to Marcin for pointing out the bug.

ghciHandle :: (HasDynFlags m, ExceptionMonad m) => (SomeException -> m a) -> m a -> m a
ghciHandle h m = gmask $ \restore -> do
                 -- Force dflags to avoid leaking the associated HscEnv
                 !dflags <- getDynFlags
                 gcatch (restore (GHC.prettyPrintGhcErrors dflags m)) $ \e -> restore (h e)

ghciTry :: GHCi a -> GHCi (Either SomeException a)
ghciTry (GHCi m) = GHCi $ \s -> gtry (m s)

tryBool :: GHCi a -> GHCi Bool
tryBool m = do
    r <- ghciTry m
    case r of
      Left _  -> return False
      Right _ -> return True

-- ----------------------------------------------------------------------------
-- Utils

lookupModule :: GHC.GhcMonad m => String -> m Module
lookupModule mName = lookupModuleName (GHC.mkModuleName mName)

lookupModuleName :: GHC.GhcMonad m => ModuleName -> m Module
lookupModuleName mName = GHC.lookupModule mName Nothing

isHomeModule :: Module -> Bool
isHomeModule m = GHC.moduleUnitId m == mainUnitId

-- TODO: won't work if home dir is encoded.
-- (changeDirectory may not work either in that case.)
expandPath :: MonadIO m => String -> InputT m String
expandPath = liftIO . expandPathIO

expandPathIO :: String -> IO String
expandPathIO p =
  case dropWhile isSpace p of
   ('~':d) -> do
        tilde <- getHomeDirectory -- will fail if HOME not defined
        return (tilde ++ '/':d)
   other ->
        return other

wantInterpretedModule :: GHC.GhcMonad m => String -> m Module
wantInterpretedModule str = wantInterpretedModuleName (GHC.mkModuleName str)

wantInterpretedModuleName :: GHC.GhcMonad m => ModuleName -> m Module
wantInterpretedModuleName modname = do
   modl <- lookupModuleName modname
   let str = moduleNameString modname
   dflags <- getDynFlags
   when (GHC.moduleUnitId modl /= thisPackage dflags) $
      throwGhcException (CmdLineError ("module '" ++ str ++ "' is from another package;\nthis command requires an interpreted module"))
   is_interpreted <- GHC.moduleIsInterpreted modl
   when (not is_interpreted) $
       throwGhcException (CmdLineError ("module '" ++ str ++ "' is not interpreted; try \':add *" ++ str ++ "' first"))
   return modl

wantNameFromInterpretedModule :: GHC.GhcMonad m
                              => (Name -> SDoc -> m ())
                              -> String
                              -> (Name -> m ())
                              -> m ()
wantNameFromInterpretedModule noCanDo str and_then =
  handleSourceError GHC.printException $ do
   names <- GHC.parseName str
   case names of
      []    -> return ()
      (n:_) -> do
            let modl = ASSERT( isExternalName n ) GHC.nameModule n
            if not (GHC.isExternalName n)
               then noCanDo n $ ppr n <>
                                text " is not defined in an interpreted module"
               else do
            is_interpreted <- GHC.moduleIsInterpreted modl
            if not is_interpreted
               then noCanDo n $ text "module " <> ppr modl <>
                                text " is not interpreted"
               else and_then n
