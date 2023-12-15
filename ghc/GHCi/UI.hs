{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-name-shadowing #-}
-- This module does a lot of it

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

-- GHCi
import qualified GHCi.UI.Monad as GhciMonad ( args, runStmt, runDecls' )
import GHCi.UI.Monad hiding ( args, runStmt )
import GHCi.UI.Info
import GHCi.UI.Exception
import GHC.Runtime.Debugger
import GHC.Runtime.Eval (mkTopLevEnv)

-- The GHC interface
import GHC.Runtime.Interpreter
import GHCi.RemoteTypes
import GHCi.BreakArray( breakOn, breakOff )
import GHC.ByteCode.Types
import GHC.Core.DataCon
import GHC.Core.ConLike
import GHC.Core.PatSyn
import GHC.Driver.Flags
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Phases
import GHC.Driver.Session as DynFlags
import GHC.Driver.Ppr hiding (printForUser)
import GHC.Utils.Error hiding (traceCmd)
import GHC.Driver.Monad ( modifySession )
import GHC.Driver.Make ( newIfaceCache, ModIfaceCache(..) )
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Config.Diagnostic
import qualified GHC
import GHC ( LoadHowMuch(..), Target(..),  TargetId(..),
             Resume, SingleStep, Ghc,
             GetDocsFailure(..), pushLogHookM,
             getModuleGraph, handleSourceError, ms_mod )
import GHC.Driver.Main (hscParseModuleWithLocation, hscParseStmtWithLocation)
import GHC.Hs.ImpExp
import GHC.Hs
import GHC.Driver.Env
import GHC.Runtime.Context
import GHC.Types.TyThing
import GHC.Types.TyThing.Ppr
import GHC.Core.TyCo.Ppr
import GHC.Types.SafeHaskell ( getSafeMode )
import GHC.Types.SourceError ( SourceError )
import GHC.Types.Name
import GHC.Types.Breakpoint
import GHC.Types.Var ( varType )
import GHC.Iface.Syntax ( showToHeader )
import GHC.Builtin.Names
import GHC.Builtin.Types( stringTyCon_RDR )
import GHC.Types.Name.Reader as RdrName ( getGRE_NameQualifier_maybes, getRdrName, greName, globalRdrEnvElts)
import GHC.Types.SrcLoc as SrcLoc
import qualified GHC.Parser.Lexer as Lexer
import GHC.Parser.Header ( toArgs )
import qualified GHC.Parser.Header as Header
import GHC.Types.PkgQual

import GHC.Unit
import GHC.Unit.Finder as Finder
import GHC.Unit.Module.Graph (filterToposortToModules)
import GHC.Unit.Module.ModSummary

import GHC.Data.StringBuffer
import GHC.Utils.Outputable
import GHC.Utils.Logger

-- Other random utilities
import GHC.Types.Basic hiding ( isTopLevel )
import GHC.Settings.Config
import GHC.Data.Graph.Directed
import GHC.Utils.Encoding
import GHC.Data.FastString
import qualified GHC.Linker.Loader as Loader
import GHC.Data.Maybe ( expectJust )
import GHC.Types.Name.Set
import GHC.Utils.Panic hiding ( showException, try )
import GHC.Utils.Misc
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Data.Strict as Strict
import GHC.Types.Error

-- Haskell Libraries
import System.Console.Haskeline as Haskeline

import Control.Applicative hiding (empty)
import Control.DeepSeq (deepseq)
import Control.Monad as Monad
import Control.Monad.Catch as MC
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Function
import Data.IORef ( IORef, modifyIORef, newIORef, readIORef, writeIORef )
import Data.List ( elemIndices, find, intercalate, intersperse, minimumBy,
                   isPrefixOf, isSuffixOf, nub, partition, sort, sortBy, (\\) )
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Map as M
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Time.LocalTime ( getZonedTime )
import Data.Time.Format ( formatTime, defaultTimeLocale )
import Data.Version ( showVersion )
import qualified Data.Semigroup as S
import Prelude hiding ((<>))

import GHC.Utils.Exception as Exception hiding (catch, mask, handle)
import Foreign hiding (void)
import GHC.Stack hiding (SrcLoc(..))
import GHC.Unit.Env
import GHC.Unit.Home.ModInfo

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

import GHCi.Leak
import qualified GHC.Unit.Module.Graph as GHC

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
                 ": https://www.haskell.org/ghc/  :? for help"

ghciCommands :: [Command]
ghciCommands = map mkCmd [
  -- Hugs users are accustomed to :e, so make sure it doesn't overlap
  ("?",         keepGoing help,                 noCompletion),
  ("add",       keepGoingPaths addModule,       completeFilename),
  ("abandon",   keepGoing abandonCmd,           noCompletion),
  ("break",     keepGoing breakCmd,             completeBreakpoint),
  ("back",      keepGoing backCmd,              noCompletion),
  ("browse",    keepGoing' (browseCmd False),   completeModule),
  ("browse!",   keepGoing' (browseCmd True),    completeModule),
  ("cd",        keepGoingMulti' changeDirectory,     completeFilename),
  ("continue",  keepGoing continueCmd,          noCompletion),
  ("cmd",       keepGoing cmdCmd,               completeExpression),
  ("def",       keepGoing (defineMacro False),  completeExpression),
  ("def!",      keepGoing (defineMacro True),   completeExpression),
  ("delete",    keepGoing deleteCmd,            noCompletion),
  ("disable",   keepGoing disableCmd,           noCompletion),
  ("doc",       keepGoing' docCmd,              completeIdentifier),
  ("edit",      keepGoingMulti' editFile,            completeFilename),
  ("enable",    keepGoing enableCmd,            noCompletion),
  ("force",     keepGoing forceCmd,             completeExpression),
  ("forward",   keepGoing forwardCmd,           noCompletion),
  ("help",      keepGoingMulti help,                 noCompletion),
  ("history",   keepGoingMulti historyCmd,           noCompletion),
  ("info",      keepGoingMulti' (info False),        completeIdentifier),
  ("info!",     keepGoingMulti' (info True),         completeIdentifier),
  ("issafe",    keepGoing' isSafeCmd,           completeModule),
  ("ignore",    keepGoing ignoreCmd,            noCompletion),
  ("kind",      keepGoingMulti' (kindOfType False),  completeIdentifier),
  ("kind!",     keepGoingMulti' (kindOfType True),   completeIdentifier),
  ("load",      keepGoingPaths loadModule_,     completeHomeModuleOrFile),
  ("load!",     keepGoingPaths loadModuleDefer, completeHomeModuleOrFile),
  ("list",      keepGoing' listCmd,             noCompletion),
  ("module",    keepGoing moduleCmd,            completeSetModule),
  ("main",      keepGoing runMain,              completeFilename),
  ("print",     keepGoing printCmd,             completeExpression),
  ("quit",      quit,                           noCompletion),
  ("reload",    keepGoingMulti' reloadModule,   noCompletion),
  ("reload!",   keepGoingMulti' reloadModuleDefer,   noCompletion),
  ("run",       keepGoing runRun,               completeFilename),
  ("script",    keepGoing' scriptCmd,           completeFilename),
  ("set",       keepGoingMulti setCmd,          completeSetOptions),
  ("seti",      keepGoingMulti setiCmd,         completeSeti),
  ("show",      keepGoingMulti' showCmd,        completeShowOptions),
  ("showi",     keepGoing showiCmd,             completeShowiOptions),
  ("sprint",    keepGoing sprintCmd,            completeExpression),
  ("step",      keepGoing stepCmd,              completeIdentifier),
  ("steplocal", keepGoing stepLocalCmd,         completeIdentifier),
  ("stepmodule",keepGoing stepModuleCmd,        completeIdentifier),
  ("type",      keepGoingMulti' typeOfExpr,          completeExpression),
  ("trace",     keepGoing traceCmd,             completeExpression),
  ("unadd",     keepGoingPaths unAddModule,     completeFilename),
  ("undef",     keepGoing undefineMacro,        completeMacro),
  ("unset",     keepGoing unsetOptions,         completeSetOptions),
  ("where",     keepGoing whereCmd,             noCompletion),
  ("instances", keepGoing' instancesCmd,        completeExpression)
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

word_break_chars_pred :: Char -> Bool
word_break_chars_pred '.' = False
word_break_chars_pred c = c `elem` (spaces ++ specials) || isSymbolChar c

symbols, specials, spaces :: String
symbols = "!#$%&*+/<=>?@\\^|-~"
specials = "(),;[]`{}"
spaces = " \t\n"

flagWordBreakChars :: String
flagWordBreakChars = " \t\n"


showSDocForUser' :: GHC.GhcMonad m => SDoc -> m String
showSDocForUser' doc = do
    dflags <- getDynFlags
    unit_state <- hsc_units <$> GHC.getSession
    name_ppr_ctx <- GHC.getNamePprCtx
    pure $ showSDocForUser dflags unit_state name_ppr_ctx doc

showSDocForUserQualify :: GHC.GhcMonad m => SDoc -> m String
showSDocForUserQualify doc = do
    dflags <- getDynFlags
    unit_state <- hsc_units <$> GHC.getSession
    pure $ showSDocForUser dflags unit_state alwaysQualify doc


keepGoing :: (String -> GHCi ()) -> (String -> InputT GHCi CmdExecOutcome)
keepGoing a str = keepGoing' (lift . a) str

keepGoingMulti :: (String -> GHCi ()) -> (String -> InputT GHCi CmdExecOutcome)
keepGoingMulti a str = keepGoingMulti' (lift . a) str

keepGoing' :: GhciMonad m => (a -> m ()) -> a -> m CmdExecOutcome
keepGoing' a str = do
  in_multi <- inMultiMode
  if in_multi
    then
      liftIO $ hPutStrLn stderr "Command is not supported (yet) in multi-mode"
    else
      a str
  return CmdSuccess

-- For commands which are actually support in multi-mode, initially just :reload
keepGoingMulti' :: GhciMonad m => (String -> m ()) -> String -> m CmdExecOutcome
keepGoingMulti' a str = a str >> return CmdSuccess

inMultiMode :: GhciMonad m => m Bool
inMultiMode = multiMode <$> getGHCiState

keepGoingPaths :: ([FilePath] -> InputT GHCi ()) -> (String -> InputT GHCi CmdExecOutcome)
keepGoingPaths a str
 = do case toArgsNoLoc str of
          Left err -> liftIO $ hPutStrLn stderr err >> return CmdSuccess
          Right args -> keepGoing' a args

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
  "   :def[!] <cmd> <expr>        define command :<cmd> (later defined command has\n" ++
  "                               precedence, ::<cmd> is always a builtin command)\n" ++
  "                               (!: redefine an existing command name)\n" ++
  "   :doc <name>                 display docs for the given name (experimental)\n" ++
  "   :edit <file>                edit file\n" ++
  "   :edit                       edit last module\n" ++
  "   :help, :?                   display this list of commands\n" ++
  "   :info[!] [<name> ...]       display information about the given names\n" ++
  "                               (!: do not filter instances)\n" ++
  "   :instances <type>           display the class instances available for <type>\n" ++
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
  "   :unadd <module> ...         remove module(s) from the current target set\n" ++
  "   :undef <cmd>                undefine user-defined command :<cmd>\n" ++
  "   ::<cmd>                     run the builtin command\n" ++
  "   :!<command>                 run the shell command <command>\n" ++
  "\n" ++
  " -- Commands for debugging:\n" ++
  "\n" ++
  "   :abandon                    at a breakpoint, abandon current computation\n" ++
  "   :back [<n>]                 go back in the history N steps (after :trace)\n" ++
  "   :break [<mod>] <l> [<col>]  set a breakpoint at the specified location\n" ++
  "   :break <name>               set a breakpoint on the specified function\n" ++
  "   :continue [<count>]         resume after a breakpoint [and set break ignore count]\n" ++
  "   :delete <number> ...        delete the specified breakpoints\n" ++
  "   :delete *                   delete all breakpoints\n" ++
  "   :disable <number> ...       disable the specified breakpoints\n" ++
  "   :disable *                  disable all breakpoints\n" ++
  "   :enable <number> ...        enable the specified breakpoints\n" ++
  "   :enable *                   enable all breakpoints\n" ++
  "   :force <expr>               print <expr>, forcing unevaluated parts\n" ++
  "   :forward [<n>]              go forward in the history N step s(after :back)\n" ++
  "   :history [<n>]              after :trace, show the execution history\n" ++
  "   :ignore <breaknum> <count>  for break <breaknum> set break ignore <count>\n" ++
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
  "   :set local-config { source | ignore }\n" ++
  "                               set whether to source .ghci in current dir\n" ++
  "                               (loading untrusted config is a security issue)\n" ++
  "   :set args <arg> ...         set the arguments returned by System.Environment.getArgs\n" ++
  "   :set prog <progname>        set the value returned by System.Environment.getProgName\n" ++
  "   :set prompt <prompt>        set the prompt used in GHCi\n" ++
  "   :set prompt-cont <prompt>   set the continuation prompt used in GHCi\n" ++
  "   :set prompt-function <expr> set the function to handle the prompt\n" ++
  "   :set prompt-cont-function <expr>\n" ++
  "                               set the function to handle the continuation prompt\n" ++
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
  "   :show targets               show the current set of targets\n" ++
  "   :show <setting>             show value of <setting>, which is one of\n" ++
  "                                  [args, prog, editor, stop]\n" ++
  "   :showi language             show language flags for interactive evaluation\n" ++
  "\n" ++
  " The User's Guide has more information. An online copy can be found here:\n" ++
  "\n" ++
  "   https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html\n" ++
  "\n"

findEditor :: IO String
findEditor = do
    getEnv "VISUAL" <|> getEnv "EDITOR" <|> defaultEditor
  where
    defaultEditor = do
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
default_prompt = generatePromptFunctionFromString "ghci> "
default_prompt_cont = generatePromptFunctionFromString "ghci| "

default_args :: [String]
default_args = []

interactiveUI :: GhciSettings -> [(FilePath, Maybe UnitId, Maybe Phase)] -> Maybe [String]
              -> Ghc ()
interactiveUI config srcs maybe_exprs = do
   -- This is a HACK to make sure dynflags are not overwritten when setting
   -- options. When GHCi is made properly multi component it should be removed.
   modifySession (\env -> hscSetActiveUnitId (hscActiveUnitId env) env)
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
   (nobuffering, flush) <- runInternal initInterpBuffering

   -- The initial set of DynFlags used for interactive evaluation is the same
   -- as the global DynFlags, plus -XExtendedDefaultRules and
   -- -XNoMonomorphismRestriction.
   -- See Note [Changing language extensions for interactive evaluation] #10857
   dflags <- getDynFlags
   let dflags' = (xopt_set_unlessExplSpec
                      LangExt.ExtendedDefaultRules xopt_set)
               . (xopt_set_unlessExplSpec
                      LangExt.MonomorphismRestriction xopt_unset)
               $ dflags
   GHC.setInteractiveDynFlags dflags'
   _ <- GHC.setProgramDynFlags
               -- Set Opt_KeepGoing so that :reload loads as much as
               -- possible
               (gopt_set dflags Opt_KeepGoing)

   -- Update the LogAction. Ensure we don't override the user's log action lest
   -- we break -ddump-json (#14078)
   lastErrLocationsRef <- liftIO $ newIORef []
   pushLogHookM (ghciLogAction lastErrLocationsRef)

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
   let prelude_import =
         case simpleImportDecl preludeModuleName of
           -- Set to True because Prelude is implicitly imported.
           impDecl@ImportDecl{ideclExt=ext} -> impDecl{ideclExt = ext{ideclImplicit=True}}
   hsc_env <- GHC.getSession
   let !in_multi = length (hsc_all_home_unit_ids hsc_env) > 1
        -- We force this to make sure we don't retain the hsc_env when reloading
   empty_cache <- liftIO newIfaceCache
   startGHCi (runGHCi srcs maybe_exprs)
        GHCiState{ progname           = default_progname,
                   args               = default_args,
                   evalWrapper        = eval_wrapper,
                   prompt             = defPrompt config,
                   prompt_cont        = defPromptCont config,
                   stop               = default_stop,
                   editor             = default_editor,
                   options            = [],
                   multiMode          = in_multi,
                   localConfig        = SourceLocalConfig,
                   -- We initialize line number as 0, not 1, because we use
                   -- current line number while reporting errors which is
                   -- incremented after reading a line.
                   line_number        = 0,
                   break_ctr          = 0,
                   breaks             = IntMap.empty,
                   tickarrays         = emptyModuleEnv,
                   ghci_commands      = availableCommands config,
                   ghci_macros        = [],
                   last_command       = Nothing,
                   cmd_wrapper        = (cmdSuccess =<<),
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
                   noBuffering        = nobuffering,
                   ifaceCache = empty_cache
                 }

   return ()

{-
Note [Changing language extensions for interactive evaluation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHCi maintains two sets of options:

- The "loading options" apply when loading modules
- The "interactive options" apply when evaluating expressions and commands
    typed at the GHCi prompt.

The loading options are mostly created in ghc/Main.hs:main' from the command
line flags. In the function ghc/GHCi/UI.hs:interactiveUI the loading options
are copied to the interactive options.

These interactive options (but not the loading options!) are supplemented
unconditionally by setting ExtendedDefaultRules ON and
MonomorphismRestriction OFF. The unconditional setting of these options
eventually overwrite settings already specified at the command line.

Therefore instead of unconditionally setting ExtendedDefaultRules and
NoMonomorphismRestriction for the interactive options, we use the function
'xopt_set_unlessExplSpec' to first check whether the extension has already
specified at the command line.

The ghci config file has not yet been processed.
-}

resetLastErrorLocations :: GhciMonad m => m ()
resetLastErrorLocations = do
    st <- getGHCiState
    liftIO $ writeIORef (lastErrorLocations st) []

ghciLogAction :: IORef [(FastString, Int)] -> LogAction -> LogAction
ghciLogAction lastErrLocations old_log_action
              dflags msg_class srcSpan msg = do
    old_log_action dflags msg_class srcSpan msg
    case msg_class of
        MCDiagnostic SevError _reason _code -> case srcSpan of
            RealSrcSpan rsp _ -> modifyIORef lastErrLocations
                (++ [(srcLocFile (realSrcSpanStart rsp), srcLocLine (realSrcSpanStart rsp))])
            _ -> return ()
        _ -> return ()

-- | Takes a file name and prefixes it with the appropriate GHC appdir.
-- ~/.ghc (getAppUserDataDirectory) is used if it exists, or XDG directories
-- are used to respect the XDG specification.
-- As a migration strategy, currently we will only create new directories in
-- the appropriate XDG location.
getAppDataFile :: XdgDirectory -> FilePath -> IO (Maybe FilePath)
getAppDataFile xdgDir file = do
  xdgAppDir <-
    tryIO (getXdgDirectory xdgDir "ghc") >>= \case
      Left _ -> pure Nothing
      Right dir -> flip catchIO (const $ pure Nothing) $ do
        createDirectoryIfMissing False dir
        pure $ Just dir
  appDir <-
    tryIO (getAppUserDataDirectory "ghc") >>= \case
      Right dir ->
        doesDirectoryExist dir >>= \case
          True -> pure $ Just dir
          False -> pure xdgAppDir
      Left _ -> pure xdgAppDir
  pure $ appDir >>= \dir -> Just $ dir </> file

runGHCi :: [(FilePath, Maybe UnitId, Maybe Phase)] -> Maybe [String] -> GHCi ()
runGHCi paths maybe_exprs = do
  dflags <- getDynFlags
  let
   ignore_dot_ghci = gopt Opt_IgnoreDotGhci dflags

   appDataCfg = liftIO $ getAppDataFile XdgConfig "ghci.conf"

   homeCfg = do
    liftIO $ tryIO (getEnv "HOME") >>= \case
      Right home -> pure $ Just $ home </> ".ghci"
      _ -> pure Nothing

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

  processedCfgs <- if ignore_dot_ghci
    then pure []
    else do
      userCfgs <- do
        paths <- catMaybes <$> sequence [ appDataCfg, homeCfg ]
        checkedPaths <- liftIO $ filterM checkFileAndDirPerms paths
        liftIO . fmap (nub . catMaybes) $ mapM canonicalizePath' checkedPaths

      localCfg <- do
        let path = ".ghci"
        ok <- liftIO $ checkFileAndDirPerms path
        if ok then liftIO $ canonicalizePath' path else pure Nothing

      mapM_ sourceConfigFile userCfgs
        -- Process the global and user .ghci
        -- (but not $CWD/.ghci or CLI args, yet)

      behaviour <- localConfig <$> getGHCiState

      processedLocalCfg <- case localCfg of
        Just path | path `notElem` userCfgs ->
          -- don't read .ghci twice if CWD is $HOME
          case behaviour of
            SourceLocalConfig -> localCfg <$ sourceConfigFile path
            IgnoreLocalConfig -> pure Nothing
        _ -> pure Nothing

      pure $ maybe id (:) processedLocalCfg userCfgs

  let arg_cfgs = reverse $ ghciScripts dflags
    -- -ghci-script are collected in reverse order
    -- We don't require that a script explicitly added by -ghci-script
    -- is owned by the current user. (#6017)

  mapM_ sourceConfigFile $ nub arg_cfgs \\ processedCfgs
    -- Dedup, and remove any configs we already processed.
    -- Importantly, if $PWD/.ghci was ignored due to configuration,
    -- explicitly specifying it does cause it to be processed.

  -- Perform a :reload for files given on the GHCi command line
  -- The appropriate targets will already be set
  -- When in -e mode, if the load fails then we want to stop
  -- immediately rather than going on to evaluate the expression.
  when (not (null paths)) $ do
     ok <- ghciHandle (\e -> do showException e; return Failed) $
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
                -- make `ghc -e` exit nonzero on failure, see #7962, #9916, #17560, #18441
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
    currentDirectory <- liftIO getCurrentDirectory

    histFile <- case (ghciHistory, localGhciHistory) of
      (True, True) -> pure $ Just $ currentDirectory </> ".ghci_history"
      (True, _) -> liftIO $ getAppDataFile XdgData "ghci_history"
      _ -> pure Nothing

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

incrementLineNo :: GhciMonad m => m ()
incrementLineNo = modifyGHCiState incLineNo
  where
    incLineNo st = st { line_number = line_number st + 1 }

fileLoop :: GhciMonad m => Handle -> m (Maybe String)
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

getInfoForPrompt :: GhciMonad m => m (SDoc, [String], Int)
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

-- | Takes a string, presumably following "%call", and tries to parse
-- a command and arguments in parentheses:
--
-- > parseCallEscape "  (cmd arg1 arg2)rest" = Just ("cmd" :| ["arg1", "arg2"], "rest")
-- > parseCallEscape "( )rest" = Nothing
--
parseCallEscape :: String -> Maybe (NE.NonEmpty String, String)
parseCallEscape s = case dropWhile isSpace s of
  '(' : sinceOpen -> case span (/= ')') sinceOpen of
    (call, ')' : sinceClosed)
      | cmd : args <- words call -> Just (cmd NE.:| args, sinceClosed)
    _ -> Nothing
  _ -> Nothing

checkPromptStringForErrors :: String -> Maybe String
checkPromptStringForErrors ('%':'c':'a':'l':'l':xs) =
  case parseCallEscape xs of
    Nothing  -> Just ("Incorrect %call syntax. " ++
                      "Should be %call(a command and arguments).")
    Just (_, afterClosed) -> checkPromptStringForErrors afterClosed
checkPromptStringForErrors ('%':'%':xs) = checkPromptStringForErrors xs
checkPromptStringForErrors (_:xs) = checkPromptStringForErrors xs
checkPromptStringForErrors "" = Nothing

generatePromptFunctionFromString :: String -> PromptFunction
generatePromptFunctionFromString promptS modules_names line =
        processString promptS
  where
        processString :: String -> GHCi SDoc
        processString ('%':'s':xs) =
            liftM2 (<>) (return modules_list) (processString xs)
            where
              modules_list = hsep . map text . ordNub $ modules_names
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
            -- Input has just been validated by parseCallEscape
            let (cmd NE.:| args, afterClosed) = fromJust $ parseCallEscape xs
            respond <- liftIO $ do
                (code, out, err) <-
                    readProcessWithExitCode
                    cmd args ""
                    `catchIO` \e -> return (ExitFailure 1, "", show e)
                case code of
                    ExitSuccess -> return out
                    _ -> do
                        hPutStrLn stderr err
                        return ""
            liftM ((text respond) <>) (processString afterClosed)
        processString ('%':'%':xs) =
            liftM ((char '%') <>) (processString xs)
        processString (x:xs) =
            liftM (char x <>) (processString xs)
        processString "" =
            return empty

mkPrompt :: GHCi String
mkPrompt = do
  st <- getGHCiState
  dflags <- getDynFlags
  (context, modules_names, line) <- getInfoForPrompt

  prompt_string <- (prompt st) modules_names line
  let prompt_doc = context <> prompt_string

  return (showSDoc dflags prompt_doc)

queryQueue :: GhciMonad m => m (Maybe String)
queryQueue = do
  st <- getGHCiState
  case cmdqueue st of
    []   -> return Nothing
    c:cs -> do setGHCiState st{ cmdqueue = cs }
               return (Just c)

-- Reconfigurable pretty-printing Ticket #5461
installInteractivePrint :: GhciMonad m => Maybe String -> Bool -> m ()
installInteractivePrint Nothing _  = return ()
installInteractivePrint (Just ipFun) exprmode = do
  ok <- trySuccess $ do
                name NE.:| _ <- GHC.parseName ipFun
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
             -> InputT GHCi ()
runCommands' eh sourceErrorHandler gCmd = mask $ \unmask -> do
    b <- handle (\e -> case fromException e of
                          Just UserInterrupt -> return $ Just False
                          _ -> case fromException e of
                                 Just ghce ->
                                   do liftIO (print (ghce :: GhcException))
                                      return Nothing
                                 _other ->
                                   liftIO (Exception.throwIO e))
            (unmask $ runOneCommand eh gCmd)
    case b of
      Nothing -> return ()
      Just success -> do
        unless success $ maybe (return ()) lift sourceErrorHandler
        unmask $ runCommands' eh sourceErrorHandler gCmd

-- | Evaluate a single line of user input (either :<command> or Haskell code).
-- A result of Nothing means there was no more input to process.
-- Otherwise the result is Just b where b is True if the command succeeded;
-- this is relevant only to ghc -e, which will exit with status 1
-- if the command was unsuccessful. GHCi will continue in either case.
-- TODO: replace Bool with CmdExecOutcome
runOneCommand :: (SomeException -> GHCi Bool) -> InputT GHCi (Maybe String)
            -> InputT GHCi (Maybe Bool)
runOneCommand eh gCmd = do
  -- run a previously queued command if there is one, otherwise get new
  -- input from user
  mb_cmd0 <- noSpace (lift queryQueue)
  mb_cmd1 <- maybe (noSpace gCmd) (return . Just) mb_cmd0
  case mb_cmd1 of
    Nothing -> return Nothing
    Just c  -> do
      st <- getGHCiState
      ghciHandle (\e -> lift $ eh e >>= return . Just) $
        handleSourceError printErrorAndFail $
          cmd_wrapper st $ doCommand c
               -- source error's are handled by runStmt
               -- is the handler necessary here?
  where
    printErrorAndFail err = do
        printGhciException err
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
      mb_cmd <- collectCommand q "" `MC.finally`
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

    cmdOutcome :: CmdExecOutcome -> Maybe Bool
    cmdOutcome CleanExit = Nothing
    cmdOutcome CmdSuccess = Just True
    cmdOutcome CmdFailure = Just False

    -- | Handle a line of input
    doCommand :: String -> InputT GHCi CommandResult

    -- command
    doCommand stmt | stmt'@(':' : cmd) <- removeSpaces stmt = do
      (stats, result) <- runWithStats (const Nothing) $ specialCommand cmd
      return $ CommandComplete stmt' (cmdOutcome <$> result) stats

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
            Nothing -> return CommandIncomplete
            Just ml_stmt -> do
              -- temporarily compensate line-number for multi-line input
              (stats, result) <- runAndPrintStats runAllocs $ lift $
                runStmtWithLineNum fst_line_num ml_stmt GHC.RunToCompletion
              return $
                CommandComplete ml_stmt (Just . runSuccess <$> result) stats
        else do -- single line input and :{ - multiline input
          last_line_num <- line_number <$> getGHCiState
          -- reconstruct first line num from last line num and stmt
          let fst_line_num | stmt_nl_cnt > 0 = last_line_num - (stmt_nl_cnt2 + 1)
                           | otherwise = last_line_num -- single line input
              stmt_nl_cnt2 = length [ () | '\n' <- stmt' ]
              stmt' = dropLeadingWhiteLines stmt -- runStmt doesn't like leading empty lines
          -- temporarily compensate line-number for multi-line input
          (stats, result) <- runAndPrintStats runAllocs $ lift $
            runStmtWithLineNum fst_line_num stmt' GHC.RunToCompletion
          return $ CommandComplete stmt' (Just . runSuccess <$> result) stats

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
checkInputForLayout
  :: GhciMonad m => String -> m (Maybe String) -> m (Maybe String)
checkInputForLayout stmt getStmt = do
   dflags' <- getDynFlags
   let dflags = xopt_set dflags' LangExt.AlternativeLayoutRule
   st0 <- getGHCiState
   let buf'   =  stringToStringBuffer stmt
       loc    = mkRealSrcLoc (fsLit (progname st0)) (line_number st0) 1
       pstate = Lexer.initParserState (initParserOpts dflags) buf' loc
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

enqueueCommands :: GhciMonad m => [String] -> m ()
enqueueCommands cmds = do
  -- make sure we force any exceptions in the commands while we're
  -- still inside the exception handler, otherwise bad things will
  -- happen (see #10501)
  cmds `deepseq` return ()
  modifyGHCiState $ \st -> st{ cmdqueue = cmds ++ cmdqueue st }

-- | Entry point to execute some haskell code from user.
-- The return value True indicates success, as in `runOneCommand`.
runStmt :: GhciMonad m => String -> SingleStep -> m (Maybe GHC.ExecResult)
runStmt input step = do
  pflags <- initParserOpts <$> GHC.getInteractiveDynFlags
  -- In GHCi, we disable `-fdefer-type-errors`, as well as `-fdefer-type-holes`
  -- and `-fdefer-out-of-scope-variables` for **naked expressions**. The
  -- declarations and statements are not affected.
  -- See Note [Deferred type errors in GHCi] in GHC.Tc.Module
  st <- getGHCiState
  let source = progname st
  let line = line_number st

  -- Add any LANGUAGE/OPTIONS_GHC pragmas we find find.
  set_pragmas pflags

  if | GHC.isStmt pflags input -> do
         hsc_env <- GHC.getSession
         mb_stmt <- liftIO (runInteractiveHsc hsc_env (hscParseStmtWithLocation source line input))
         case mb_stmt of
           Nothing ->
             -- empty statement / comment
             return (Just exec_complete)
           Just stmt ->
             run_stmt stmt

     -- Otherwise assume a declaration (or a list of declarations)
     -- and/or import(s) (#20473).
     -- Note: `GHC.isDecl` returns False on input like
     -- `data Infix a b = a :@: b; infixl 4 :@:`
     -- and should therefore not be used here.
     | otherwise -> do
         hsc_env <- GHC.getSession
         let !ic = hsc_IC hsc_env  -- Bang-pattern to avoid space leaks
         setDumpFilePrefix ic
           -- `-ddump-to-file` must work for normal GHCi compilations /
           --     evaluations. (#17500)
         -- Use >>= \case instead of MonadFail desugaring to take into
         -- consideration `instance XXModule p = DataConCantHappen`.
         -- Tracked in #15681
         liftIO (hscParseModuleWithLocation hsc_env source line input) >>= \case
           HsModule { hsmodDecls = decls, hsmodImports = imports } -> do
             run_imports imports
             run_decls decls
  where
    exec_complete = GHC.ExecComplete (Right []) 0

    run_imports imports = mapM_ (addImportToContext . unLoc) imports

    set_pragmas pflags =
      let stringbuf = stringToStringBuffer input
          (_msgs, loc_opts) = Header.getOptions pflags stringbuf "<interactive>"
          opts = unLoc <$> loc_opts
      in setOptions opts

    run_stmt :: GhciMonad m => GhciLStmt GhcPs -> m (Maybe GHC.ExecResult)
    run_stmt stmt = do
           m_result <- GhciMonad.runStmt stmt input step
           case m_result of
               Nothing     -> return Nothing
               Just result -> Just <$> afterRunStmt (const True) result

    -- `x = y` (a declaration) should be treated as `let x = y` (a statement).
    -- The reason is because GHCi wasn't designed to support `x = y`, but then
    -- b98ff3 (#7253) added support for it, except it did not do a good job and
    -- caused problems like:
    --
    --  - not adding the binders defined this way in the necessary places caused
    --    `x = y` to not work in some cases (#12091).
    --  - some GHCi command crashed after `x = y` (#15721)
    --  - warning generation did not work for `x = y` (#11606)
    --  - because `x = y` is a declaration (instead of a statement) differences
    --    in generated code caused confusion (#16089)
    --
    -- Instead of dealing with all these problems individually here we fix this
    -- mess by just treating `x = y` as `let x = y`.
    run_decls :: GhciMonad m => [LHsDecl GhcPs] -> m (Maybe GHC.ExecResult)
    -- Only turn `FunBind` and `VarBind` into statements, other bindings
    -- (e.g. `PatBind`) need to stay as decls.
    run_decls [L l (ValD _ bind@FunBind{})] = run_stmt (mk_stmt (locA l) bind)
    run_decls [L l (ValD _ bind@VarBind{})] = run_stmt (mk_stmt (locA l) bind)
    -- Note that any `x = y` declarations below will be run as declarations
    -- instead of statements (e.g. `...; x = y; ...`)
    run_decls decls = do
      -- In the new IO library, read handles buffer data even if the Handle
      -- is set to NoBuffering.  This causes problems for GHCi where there
      -- are really two stdin Handles.  So we flush any bufferred data in
      -- GHCi's stdin Handle here (only relevant if stdin is attached to
      -- a file, otherwise the read buffer can't be flushed).
      _ <- liftIO $ tryIO $ hFlushAll stdin
      m_result <- GhciMonad.runDecls' decls
      forM m_result $ \result ->
        afterRunStmt (const True) (GHC.ExecComplete (Right result) 0)

    mk_stmt :: SrcSpan -> HsBind GhcPs -> GhciLStmt GhcPs
    mk_stmt loc bind =
      let
        la  = L (noAnnSrcSpan loc)
        la' = L (noAnnSrcSpan loc)
      in la (LetStmt noAnn (HsValBinds noAnn (ValBinds NoAnnSortKey [la' bind] [])))

    setDumpFilePrefix :: GHC.GhcMonad m => InteractiveContext -> m () -- #17500
    setDumpFilePrefix ic = do
        dflags <- GHC.getInteractiveDynFlags
        GHC.setInteractiveDynFlags dflags { dumpPrefix = modStr ++ "." }
      where
        modStr = moduleNameString $ moduleName $ icInteractiveModule $ ic

-- | Clean up the GHCi environment after a statement has run
afterRunStmt :: GhciMonad m
             => (SrcSpan -> Bool) -> GHC.ExecResult -> m GHC.ExecResult
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
         | first_resume : _ <- resumes
         , isNothing  mb_info ||
           step_here (GHC.resumeSpan first_resume) -> do
               mb_id_loc <- toBreakIdAndLocation mb_info
               let bCmd = maybe "" ( \(_,l) -> onBreakCmd l ) mb_id_loc
               if (null bCmd)
                 then printStoppedAtBreakInfo first_resume names
                 else enqueueCommands [bCmd]
               -- run the command set with ":set stop <cmd>"
               st <- getGHCiState
               enqueueCommands [stop st]
               return ()
         | otherwise -> resume step_here GHC.SingleStep Nothing >>=
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

toBreakIdAndLocation :: GhciMonad m
                     => Maybe GHC.InternalBreakpointId -> m (Maybe (Int, BreakLocation))
toBreakIdAndLocation Nothing = return Nothing
toBreakIdAndLocation (Just inf) = do
  st <- getGHCiState
  return $ listToMaybe [ id_loc | id_loc@(_,loc) <- IntMap.assocs (breaks st),
                                  breakModule loc == ibi_tick_mod inf,
                                  breakTick loc == ibi_tick_index inf ]

printStoppedAtBreakInfo :: GHC.GhcMonad m => Resume -> [Name] -> m ()
printStoppedAtBreakInfo res names = do
  printForUser $ pprStopped res
  --  printTypeOfNames session names
  let namesSorted = sortBy compareNames names
  tythings <- catMaybes `liftM` mapM GHC.lookupName namesSorted
  docs <- mapM pprTypeAndContents [i | AnId i <- tythings]
  printForUserPartWay $ vcat docs

printTypeOfNames :: GHC.GhcMonad m => [Name] -> m ()
printTypeOfNames names
 = mapM_ (printTypeOfName ) $ sortBy compareNames names

compareNames :: Name -> Name -> Ordering
compareNames = on compare getOccString S.<> on SrcLoc.leftmost_smallest getSrcSpan

printTypeOfName :: GHC.GhcMonad m => Name -> m ()
printTypeOfName n
   = do maybe_tything <- GHC.lookupName n
        case maybe_tything of
            Nothing    -> return ()
            Just thing -> printTyThing thing


data MaybeCommand = GotCommand Command | BadCommand | NoLastCommand

-- | Entry point for execution a ':<command>' input from user
specialCommand :: String -> InputT GHCi CmdExecOutcome
specialCommand ('!':str) = lift $ shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  maybe_cmd <- lookupCommand cmd
  htxt <- short_help <$> getGHCiState
  case maybe_cmd of
    GotCommand cmd -> (cmdAction cmd) (dropWhile isSpace rest)
    BadCommand ->
      do liftIO $ hPutStr stderr ("unknown command ':" ++ cmd ++ "'\n"
                           ++ htxt)
         return CmdFailure
    NoLastCommand ->
      do liftIO $ hPutStr stderr ("there is no last command to perform\n"
                           ++ htxt)
         return CmdFailure

shellEscape :: MonadIO m => String -> m CmdExecOutcome
shellEscape str = liftIO $ do
  exitCode <- system str
  case exitCode of
    ExitSuccess -> return CmdSuccess
    ExitFailure _ -> return CmdFailure

lookupCommand :: GhciMonad m => String -> m (MaybeCommand)
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

lookupCommand' :: GhciMonad m => String -> m (Maybe Command)
lookupCommand' ":" = return Nothing
lookupCommand' str' = do
  macros    <- ghci_macros <$> getGHCiState
  ghci_cmds <- ghci_commands <$> getGHCiState

  let ghci_cmds_nohide = filter (not . cmdHidden) ghci_cmds

  let (str, xcmds) = case str' of
          ':' : rest -> (rest, [])     -- "::" selects a builtin command
          _          -> (str', macros) -- otherwise include macros in lookup

      lookupExact  s = find $ (s ==)              . cmdName
      lookupPrefix s = find $ (s `isPrefixOptOf`) . cmdName

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

-- This predicate is for prefix match with a command-body and
-- suffix match with an option, such as `!`.
-- The current implementation assumes only the `!` character
-- as the option delimiter.
-- See also #17345
isPrefixOptOf :: String -> String -> Bool
isPrefixOptOf s x = let (body, opt) = break (== '!') s
                    in  (body `isPrefixOf` x) && (opt `isSuffixOf` x)

getCurrentBreakSpan :: GHC.GhcMonad m => m (Maybe SrcSpan)
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

getCallStackAtCurrentBreakpoint :: GHC.GhcMonad m => m (Maybe [String])
getCallStackAtCurrentBreakpoint = do
  resumes <- GHC.getResumeContext
  case resumes of
    [] -> return Nothing
    (r:_) -> do
       interp <- hscInterp <$> GHC.getSession
       Just <$> liftIO (costCentreStackInfo interp (GHC.resumeCCS r))

getCurrentBreakModule :: GHC.GhcMonad m => m (Maybe Module)
getCurrentBreakModule = do
  resumes <- GHC.getResumeContext
  return $ case resumes of
    [] -> Nothing
    (r:_) -> case GHC.resumeHistoryIx r of
      0  -> ibi_tick_mod <$> GHC.resumeBreakpointId r
      ix -> Just $ GHC.getHistoryModule $ GHC.resumeHistory r !! (ix-1)

-----------------------------------------------------------------------------
--
-- Commands
--
-----------------------------------------------------------------------------

noArgs :: MonadIO m => m () -> String -> m ()
noArgs m "" = m
noArgs _ _  = liftIO $ putStrLn "This command takes no arguments"

withSandboxOnly :: GHC.GhcMonad m => String -> m () -> m ()
withSandboxOnly cmd this = do
   dflags <- getDynFlags
   if not (gopt Opt_GhciSandbox dflags)
      then printForUser (text cmd <+>
                         text "is not supported with -fno-ghci-sandbox")
      else this

-----------------------------------------------------------------------------
-- :help

help :: GhciMonad m => String -> m ()
help _ = do
    txt <- long_help `fmap` getGHCiState
    liftIO $ putStr txt

-----------------------------------------------------------------------------
-- :info

info :: GHC.GhcMonad m => Bool -> String -> m ()
info _ "" = throwGhcException (CmdLineError "syntax: ':i <thing-you-want-info-about>'")
info allInfo s  = handleSourceError printGhciException $ do
    forM_ (words s) $ \thing -> do
      sdoc <- infoThing allInfo thing
      rendered <- showSDocForUser' sdoc
      liftIO (putStrLn rendered)

infoThing :: GHC.GhcMonad m => Bool -> String -> m SDoc
infoThing allInfo str = do
    names     <- GHC.parseName str
    mb_stuffs <- mapM (GHC.getInfo allInfo) names
    let filtered = filterOutChildren (\(t,_f,_ci,_fi,_sd) -> t)
                                     (catMaybes (NE.toList mb_stuffs))
    return $ vcat (intersperse (text "") $ map pprInfo filtered)

  -- Filter out names whose parent is also there. Good
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

pprInfo :: (TyThing, Fixity, [GHC.ClsInst], [GHC.FamInst], SDoc) -> SDoc
pprInfo (thing, fixity, cls_insts, fam_insts, docs)
  =  docs
  $$ pprTyThingInContextLoc thing
  $$ showFixity thing fixity
  $$ vcat (map GHC.pprInstance cls_insts)
  $$ vcat (map GHC.pprFamInst  fam_insts)

-----------------------------------------------------------------------------
-- :main

runMain :: GhciMonad m => String -> m ()
runMain s = case toArgsNoLoc s of
              Left err   -> liftIO (hPutStrLn stderr err)
              Right args -> doWithMain (doWithArgs args)
  where
    doWithMain fun = do
      dflags  <- getDynFlags
      let main = fromMaybe "main" (mainFunIs dflags)
      handleSourceError printErrAndMaybeExit $ do
        -- doing this will prevent the main to run when it is not in scope
        -- this might seem useless, but it doesn't suggest other functions
        -- to be used, which is exactly what we want here. See #23996.
        _ <- GHC.parseName main

        -- Wrap the main function in 'void' to discard its value instead
        -- of printing it (#9086). See Haskell 2010 report Chapter 5.
        fun $ "Control.Monad.void (" ++ main ++ ")"




-----------------------------------------------------------------------------
-- :run

runRun :: GhciMonad m => String -> m ()
runRun s = case toCmdArgs s of
           Left err          -> liftIO (hPutStrLn stderr err)
           Right (cmd, args) -> doWithArgs args cmd

doWithArgs :: GhciMonad m => [String] -> String -> m ()
doWithArgs args cmd = enqueueCommands ["System.Environment.withArgs " ++
                                       show args ++ " (" ++ cmd ++ ")"]

{-
Akin to @Prelude.words@, but acts like the Bourne shell, treating
quoted strings as Haskell Strings, and also parses Haskell [String]
syntax.
-}

getCmd :: String -> Either String             -- Error
                           (String, String) -- (Cmd, Rest)
getCmd s = case break isSpace $ dropWhile isSpace s of
           ([], _) -> Left ("Couldn't find command in " ++ show s)
           res -> Right res

toCmdArgs :: String -> Either String             -- Error
                              (String, [String]) -- (Cmd, Args)
toCmdArgs s = case getCmd s of
              Left err -> Left err
              Right (cmd, s') -> case toArgsNoLoc s' of
                                 Left err -> Left err
                                 Right args -> Right (cmd, args)

-- wrapper around GHC.Parser.Header.toArgs, but without locations
toArgsNoLoc :: String -> Either String [String]
toArgsNoLoc str = map unLoc <$> toArgs fake_loc str
  where
    fake_loc = mkRealSrcLoc (fsLit "<interactive>") 1 1
    -- this should never be seen, because it's discarded with the `map unLoc`

-----------------------------------------------------------------------------
-- :cd

changeDirectory :: GhciMonad m => String -> m ()
changeDirectory "" = do
  -- :cd on its own changes to the user's home directory
  either_dir <- liftIO $ tryIO getHomeDirectory
  case either_dir of
     Left _e -> return ()
     Right dir -> changeDirectory dir
changeDirectory dir = do
  graph <- GHC.getModuleGraph
  when (not (null $ GHC.mgModSummaries graph)) $
        liftIO $ putStrLn "Warning: changing directory causes all loaded modules to be unloaded,\nbecause the search path has changed."
  -- delete defined breakpoints and clear the interface file cache (#1620)
  clearCaches
  setContextAfterLoad False Nothing
  GHC.workingDirectoryChanged
  dir' <- expandPath dir
  liftIO $ setCurrentDirectory dir'
  -- With -fexternal-interpreter, we have to change the directory of the subprocess too.
  -- (this gives consistent behaviour with and without -fexternal-interpreter)
  interp <- hscInterp <$> GHC.getSession
  case interpInstance interp of
    ExternalInterp {} -> do
      fhv <- compileGHCiExpr $
        "System.Directory.setCurrentDirectory " ++ show dir'
      liftIO $ evalIO interp fhv
    _ -> pure ()

trySuccess :: GhciMonad m => m SuccessFlag -> m SuccessFlag
trySuccess act =
    handleSourceError (\e -> do printErrAndMaybeExit e -- immediately exit fith failure if in ghc -e
                                pure Failed) act

-----------------------------------------------------------------------------
-- :edit

editFile :: GhciMonad m => String -> m ()
editFile str =
  do file <- if null str then chooseEditFile else expandPath str
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
-- XXX: Can we figure out what happened if the dependency analysis fails
--      (e.g., because the porgrammeer mistyped the name of a module)?
-- XXX: Can we figure out the location of an error to pass to the editor?
-- XXX: if we could figure out the list of errors that occurred during the
-- last load/reaload, then we could start the editor focused on the first
-- of those.
chooseEditFile :: GHC.GhcMonad m => m String
chooseEditFile =
  do let hasFailed (GHC.ModuleNode _deps x) = fmap not $ isLoadedModSummary x
         hasFailed _ = return False

     graph <- GHC.getModuleGraph
     failed_graph <-
       GHC.mkModuleGraph <$> filterM hasFailed (GHC.mgModSummaries' graph)
     let order g  = flattenSCCs $ filterToposortToModules $
           GHC.topSortModuleGraph True g Nothing
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

  where fromTarget GHC.Target { targetId = GHC.TargetFile f _ } = Just f
        fromTarget _ = Nothing -- when would we get a module target?


-----------------------------------------------------------------------------
-- :def

defineMacro :: GhciMonad m => Bool{-overwrite-} -> String -> m ()
defineMacro _ (':':_) = (liftIO $ hPutStrLn stderr
                          "macro name cannot start with a colon")
                            >> failIfExprEvalMode
defineMacro _ ('!':_) = (liftIO $ hPutStrLn stderr
                          "macro name cannot start with an exclamation mark")
                            >> failIfExprEvalMode
                          -- little code duplication allows to grep error msg
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
    isCommand <- isJust <$> lookupCommand' macro_name
    let check_newname
          | macro_name `elem` defined = throwGhcException (CmdLineError
            ("macro '" ++ macro_name ++ "' is already defined. " ++ hint))
          | isCommand = throwGhcException (CmdLineError
            ("macro '" ++ macro_name ++ "' overwrites builtin command. " ++ hint))
          | otherwise = return ()
        hint = " Use ':def!' to overwrite."

    unless overwrite check_newname
    -- compile the expression
    handleSourceError printErrAndMaybeExit $ do
      step <- getGhciStepIO
      expr <- GHC.parseExpr definition
      -- > ghciStepIO . definition :: String -> IO String
      let stringTy :: LHsType GhcPs
          stringTy = nlHsTyVar NotPromoted stringTyCon_RDR
          ioM :: LHsType GhcPs -- AZ
          ioM = nlHsTyVar NotPromoted (getRdrName ioTyConName) `nlHsAppTy` stringTy
          body = nlHsVar compose_RDR `mkHsApp` (nlHsPar step)
                                     `mkHsApp` (nlHsPar expr)
          tySig = mkHsWildCardBndrs $ noLocA $ mkHsImplicitSigType $
                  nlHsFunTy stringTy ioM
          new_expr = L (getLoc expr) $ ExprWithTySig noAnn body tySig
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

runMacro
  :: GhciMonad m
  => GHC.ForeignHValue  -- String -> IO String
  -> String
  -> m CmdExecOutcome
runMacro fun s = do
  interp <- hscInterp <$> GHC.getSession
  str <- liftIO $ evalStringToIOString interp fun s
  enqueueCommands (lines str)
  return CmdSuccess


-----------------------------------------------------------------------------
-- :undef

undefineMacro :: GhciMonad m => String -> m ()
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

cmdCmd :: GhciMonad m => String -> m ()
cmdCmd str = handleSourceError printErrAndMaybeExit $ do
    step <- getGhciStepIO
    expr <- GHC.parseExpr str
    -- > ghciStepIO str :: IO String
    let new_expr = step `mkHsApp` expr
    hv <- GHC.compileParsedExprRemote new_expr

    interp <- hscInterp <$> GHC.getSession
    cmds <- liftIO $ evalString interp hv
    enqueueCommands (lines cmds)

-- | Generate a typed ghciStepIO expression
-- @ghciStepIO :: Ty String -> IO String@.
getGhciStepIO :: GHC.GhcMonad m => m (LHsExpr GhcPs)
getGhciStepIO = do
  ghciTyConName <- GHC.getGHCiMonad
  let stringTy = nlHsTyVar NotPromoted stringTyCon_RDR
      ghciM = nlHsTyVar NotPromoted (getRdrName ghciTyConName) `nlHsAppTy` stringTy
      ioM = nlHsTyVar NotPromoted (getRdrName ioTyConName) `nlHsAppTy` stringTy
      body = nlHsVar (getRdrName ghciStepIoMName)
      tySig = mkHsWildCardBndrs $ noLocA $ mkHsImplicitSigType $
              nlHsFunTy ghciM ioM
  return $ noLocA $ ExprWithTySig noAnn body tySig

-----------------------------------------------------------------------------
-- :doc

docCmd :: GHC.GhcMonad m => String -> m ()
docCmd "" =
  throwGhcException (CmdLineError "syntax: ':doc <thing-you-want-docs-for>'")
docCmd s  = do
  -- TODO: Maybe also get module headers for module names
  names <- GHC.parseName s

  docs <- traverse (buildDocComponents s) names

  let sdocs = pprDocs (NE.toList docs)
      sdocs' = vcat (intersperse (text "") sdocs)
  sdoc <- showSDocForUser' sdocs'
  liftIO (putStrLn sdoc)

data DocComponents =
  DocComponents
    { docs      :: Maybe [HsDoc GhcRn]   -- ^ subject's haddocks
    , sigAndLoc :: Maybe SDoc          -- ^ type signature + category + location
    , argDocs   :: IntMap (HsDoc GhcRn) -- ^ haddocks for arguments
    }

buildDocComponents :: GHC.GhcMonad m => String -> Name -> m DocComponents
buildDocComponents str name = do
  mbThing <- GHC.lookupName name
  let sigAndLoc = sigAndLocDoc str <$> mbThing
  (docs, argDocs)
    <- either handleGetDocsFailure pure
         =<< GHC.getDocs name

  pure DocComponents{..}

-- | Produce output containing the type/kind signature, category, and definition
-- location of a TyThing.
sigAndLocDoc :: String -> TyThing -> SDoc
sigAndLocDoc str tyThing =
  let tyThingTyDoc :: TyThing -> SDoc
      tyThingTyDoc = \case
        AnId i                      -> pprSigmaType $ varType i
        AConLike (RealDataCon dc)   -> pprSigmaType $ dataConDisplayType False dc
        AConLike (PatSynCon patSyn) -> pprPatSynType patSyn
        ATyCon tyCon                -> pprSigmaType $ GHC.tyConKind tyCon
        ACoAxiom _                  -> empty

      tyDoc = tyThingTyDoc tyThing
      sigDoc = text str <+> nest 2 (dcolon <+> tyDoc)
      comment =
        hsep [ char '\t' <> text "--"
             , pprTyThingCategory tyThing
             , text "defined" <+> pprNameDefnLoc (getName tyThing)
             ]
   in hang sigDoc 2 comment

pprDocs :: [DocComponents] -> [SDoc]
pprDocs docs
  | null nonEmptyDocs = pprDoc <$> take 1 docs
  -- elide <has no documentation> if there's at least one non-empty doc (#15784)
  | otherwise = pprDoc <$> nonEmptyDocs
  where
    empty DocComponents{docs = mb_decl_docs, argDocs = arg_docs}
      = maybe True null mb_decl_docs && null arg_docs
    nonEmptyDocs = filter (not . empty) docs

-- TODO: also print arg docs.
pprDoc :: DocComponents -> SDoc
pprDoc DocComponents{sigAndLoc = mb_sig_loc, docs = mb_decl_docs} =
  maybe
    (text "<has no documentation>")
    formatDoc
    mb_decl_docs
  where
    formatDoc doc =
      vcat [ fromMaybe empty mb_sig_loc -- print contextual info (#19055)
           , pprHsDocStrings $ map hsDocString doc
           ]

handleGetDocsFailure :: GHC.GhcMonad m => GetDocsFailure -> m a
handleGetDocsFailure no_docs = do
  dflags <- getDynFlags
  let msg = showPpr dflags no_docs
  throwGhcException $ case no_docs of
    NameHasNoModule {} -> Sorry msg
    NoDocsInIface {} -> InstallationError msg
    InteractiveName -> ProgramError msg

-----------------------------------------------------------------------------
-- :instances

instancesCmd :: String -> InputT GHCi ()
instancesCmd "" =
  throwGhcException (CmdLineError "syntax: ':instances <type-you-want-instances-for>'")
instancesCmd s = do
  handleSourceError printGhciException $ do
    ty <- GHC.parseInstanceHead s
    res <- GHC.getInstancesForType ty

    printForUser $ vcat $ map ppr res

-----------------------------------------------------------------------------
-- :load, :add, :unadd, :reload

-- these are mainly used for displaying a more informative response
data LoadType = Add !Int | Unadd !Int | Load | Reload | Check

isReload :: LoadType -> Bool
isReload Reload = True
isReload _      = False

-- | Sets '-fdefer-type-errors' if 'defer' is true, executes 'load' and unsets
-- '-fdefer-type-errors' again if it has not been set before.
wrapDeferTypeErrors :: GHC.GhcMonad m => m a -> m a
wrapDeferTypeErrors load =
  MC.bracket
    (do
      -- Force originalFlags to avoid leaking the associated HscEnv
      !originalFlags <- getDynFlags
      void $ GHC.setProgramDynFlags $
         setGeneralFlag' Opt_DeferTypeErrors originalFlags
      return originalFlags)
    (\originalFlags -> void $ GHC.setProgramDynFlags originalFlags)
    (\_ -> load)

loadModule :: GhciMonad m => [(FilePath, Maybe UnitId, Maybe Phase)] -> m SuccessFlag
loadModule fs = do
  (_, result) <- runAndPrintStats (const Nothing) (loadModule' fs)
  either (liftIO . Exception.throwIO) return result

-- | @:load@ command
loadModule_ :: GhciMonad m => [FilePath] -> m ()
loadModule_ fs = void $ loadModule (zip3 fs (repeat Nothing) (repeat Nothing))

loadModuleDefer :: GhciMonad m => [FilePath] -> m ()
loadModuleDefer = wrapDeferTypeErrors . loadModule_

loadModule' :: GhciMonad m => [(FilePath, Maybe UnitId, Maybe Phase)] -> m SuccessFlag
loadModule' files = do
  let (filenames, uids, phases) = unzip3 files
  exp_filenames <- mapM expandPath filenames
  let files' = zip3 exp_filenames uids phases
  targets <- mapM (\(file, uid, phase) -> GHC.guessTarget file uid phase) files'

  -- NOTE: we used to do the dependency anal first, so that if it
  -- fails we didn't throw away the current set of modules.  This would
  -- require some re-working of the GHC interface, so we'll leave it
  -- as a ToDo for now.

  hsc_env <- GHC.getSession
  let !dflags = hsc_dflags hsc_env

  let load_module = do
        -- unload first
        _ <- GHC.abandonAll
        clearCaches

        GHC.setTargets targets
        doLoadAndCollectInfo Load LoadAllTargets

  if gopt Opt_GhciLeakCheck dflags
    then do
      -- Grab references to the currently loaded modules so that we can see if
      -- they leak.
      leak_indicators <- liftIO $ getLeakIndicators hsc_env
      success <- load_module
      liftIO $ checkLeakIndicators dflags leak_indicators
      return success
    else
      load_module

-- | @:add@ command
addModule :: GhciMonad m => [FilePath] -> m ()
addModule files = do
  revertCAFs -- always revert CAFs on load/add.
  files' <- mapM expandPath files
  targets <- mapM (\m -> GHC.guessTarget m Nothing Nothing) files'
  targets' <- filterM checkTarget targets
  -- remove old targets with the same id; e.g. for :add *M
  mapM_ GHC.removeTarget [ tid | Target { targetId = tid } <- targets' ]
  mapM_ GHC.addTarget targets'
  _ <- doLoadAndCollectInfo (Add $ length targets') LoadAllTargets
  return ()
  where
    checkTarget :: GhciMonad m => Target -> m Bool
    checkTarget Target { targetId = TargetModule m } = checkTargetModule m
    checkTarget Target { targetId = TargetFile f _ } = checkTargetFile f

    checkTargetModule :: GhciMonad m => ModuleName -> m Bool
    checkTargetModule m = do
      hsc_env <- GHC.getSession
      let home_unit = hsc_home_unit hsc_env
      result <- liftIO $
        Finder.findImportedModule hsc_env m (ThisPkg (homeUnitId home_unit))
      case result of
        Found _ _ -> return True
        _ -> do liftIO $ hPutStrLn stderr ("Module " ++ moduleNameString m ++ " not found")
                failIfExprEvalMode
                return False

    checkTargetFile :: GhciMonad m => String -> m Bool
    checkTargetFile f = do
      exists <- liftIO (doesFileExist f)
      unless exists $ do
        liftIO $ hPutStrLn stderr $ "File " ++ f ++ " not found"
        failIfExprEvalMode
      return exists

-- | @:unadd@ command
unAddModule :: GhciMonad m => [FilePath] -> m ()
unAddModule files = do
  files' <- mapM expandPath files
  targets <- mapM (\m -> GHC.guessTarget m Nothing Nothing) files'
  let removals = [ tid | Target { targetId = tid } <- targets ]
  mapM_ GHC.removeTarget removals
  _ <- doLoadAndCollectInfo (Unadd $ length removals) LoadAllTargets
  return ()

-- | @:reload@ command
reloadModule :: GhciMonad m => String -> m ()
reloadModule m = do
  session <- GHC.getSession
  let home_unit = homeUnitId (hsc_home_unit session)
  ok <- doLoadAndCollectInfo Reload (loadTargets home_unit)
  when (failed ok) failIfExprEvalMode
  where
    loadTargets hu | null m    = LoadAllTargets
                   | otherwise = LoadUpTo (mkModule hu (GHC.mkModuleName m))

reloadModuleDefer :: GhciMonad m => String -> m ()
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
doLoadAndCollectInfo :: GhciMonad m => LoadType -> LoadHowMuch -> m SuccessFlag
doLoadAndCollectInfo load_type howmuch = do
  doCollectInfo <- isOptionSet CollectInfo

  doLoad load_type howmuch >>= \case
    Succeeded | doCollectInfo -> do
      mod_summaries <- GHC.mgModSummaries <$> getModuleGraph
      -- MP: :set +c code path only works in single package mode atm, hence
      -- this call to isLoaded is ok. collectInfo needs to be modified further to
      -- work with :set +c so I have punted on that for now.
      loaded <- filterM GHC.isLoaded (map ms_mod_name mod_summaries)
      v <- mod_infos <$> getGHCiState
      !newInfos <- collectInfo v loaded
      modifyGHCiState (\st -> st { mod_infos = newInfos })
      pure Succeeded
    flag -> pure flag

doLoad :: GhciMonad m => LoadType -> LoadHowMuch -> m SuccessFlag
doLoad load_type howmuch = do
  -- turn off breakpoints before we load: we can't turn them off later, because
  -- the ModBreaks will have gone away.
  discardActiveBreakPoints

  resetLastErrorLocations
  -- Enable buffering stdout and stderr as we're compiling. Keeping these
  -- handles unbuffered will just slow the compilation down, especially when
  -- compiling in parallel.
  let setBuffering t = liftIO $ do
        hSetBuffering stdout t
        hSetBuffering stderr t
  MC.bracket_ (setBuffering LineBuffering) (setBuffering NoBuffering) $ do
      hmis <- ifaceCache <$> getGHCiState
      -- If GHCi message gets its own configuration at some stage then this will need to be
      -- modified to 'embedUnknownDiagnostic'.
      ok <- trySuccess $ GHC.loadWithCache (Just hmis) (mkUnknownDiagnostic . GHCiMessage) howmuch
      afterLoad ok load_type
      pure ok



afterLoad
  :: GhciMonad m
  => SuccessFlag
  -> LoadType
  -> m ()
afterLoad ok load_type = do
  revertCAFs  -- always revert CAFs on load.
  discardTickArrays
  loaded_mods <- getLoadedModules
  modulesLoadedMsg ok loaded_mods load_type
  graph <- GHC.getModuleGraph
  setContextAfterLoad (isReload load_type) (Just graph)

setContextAfterLoad :: GhciMonad m => Bool -> Maybe GHC.ModuleGraph -> m ()
setContextAfterLoad keep_ctxt Nothing = do
  setContextKeepingPackageModules keep_ctxt []
setContextAfterLoad keep_ctxt (Just graph) = do
  -- load a target if one is available, otherwise load the topmost module.
  targets <- GHC.getTargets
  loaded_graph <- filterM is_loaded $ GHC.mgModSummaries' graph
  case [ m | Just m <- map (findTarget loaded_graph) targets ] of
        []    ->
          let graph' = flattenSCCs $ filterToposortToModules $
                GHC.topSortModuleGraph True (GHC.mkModuleGraph loaded_graph) Nothing
          in case graph' of
              [] -> setContextKeepingPackageModules keep_ctxt []
              xs -> load_this (last xs)
        (m:_) ->
          load_this m
 where
   is_loaded (GHC.ModuleNode _ ms) = isLoadedModSummary ms
   is_loaded _ = return False

   findTarget mds t
    = case mapMaybe (`matches` t) mds of
        []    -> Nothing
        (m:_) -> Just m

   (GHC.ModuleNode _ summary) `matches` Target { targetId = TargetModule m }
        = if GHC.ms_mod_name summary == m then Just summary else Nothing
   (GHC.ModuleNode _ summary) `matches` Target { targetId = TargetFile f _ }
        | Just f' <- GHC.ml_hs_file (GHC.ms_location summary)   =
          if f == f' then Just summary else Nothing
   _ `matches` _ = Nothing

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
  :: GhciMonad m
  => Bool                 -- True  <=> keep all of remembered_ctx
                          -- False <=> just keep package imports
  -> [InteractiveImport]  -- new context
  -> m ()
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
keepPackageImports
  :: GHC.GhcMonad m => [InteractiveImport] -> m [InteractiveImport]
keepPackageImports = filterM is_pkg_import
  where
     is_pkg_import :: GHC.GhcMonad m => InteractiveImport -> m Bool
     is_pkg_import (IIModule _) = return False
     is_pkg_import (IIDecl d)
         = do pkgqual <- GHC.renameRawPkgQualM mod_name (ideclPkgQual d)
              e <- MC.try $ GHC.findQualifiedModule pkgqual mod_name
              case e :: Either SomeException Module of
                Left _  -> return False
                Right m -> return (not (isMainUnitModule m))
        where
          mod_name = unLoc (ideclName d)

{- Note [GHCi and local Preludes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC's compilation manager has no issues when the package being compiled
defines its own local Prelude module. It'll just shadow the Prelude from base.

GHCi however must check this condition, when it calls setContext ["Prelude"]
to prepopulate the interactive session's scope. This is because of two facts.

1. setContext must use previously compiled .hi interfaces only; it cannot
recurse into compiling .hs modules (even with LinkIntoMemory), simply because
it's not the right phase to do it. Import resolution happens way before GHC
properly "loads" modules (GHC.Linker.Loader.loadModule, GHC.load & siblings);
or in other words, at time of setContext the linker isn't even initialized yet.

2. The local Prelude.hs (or .lhs) may've never been compiled before, so its
interface file Prelude.hi can be outdated or altogether missing.

Thankfully, there's a simple solution: just let CM load the local Prelude normally
(either as a :load target, or as a dependency of another target) later. To do that,
detect if the implicit `import Prelude` resolves to the "home unit" (i.e. not base),
and if so, omit it from the early setContext call.

If we don't, a HomeModError will be (correctly) thrown. See #10920.
-}

modulesLoadedMsg :: GHC.GhcMonad m => SuccessFlag -> [GHC.ModSummary] -> LoadType -> m ()
modulesLoadedMsg ok mods load_type = do
  dflags <- getDynFlags
  when (verbosity dflags > 0) $ do
     mod_names <- mapM mod_name mods
     rendered_msg <- showSDocForUser' $
       if gopt Opt_ShowLoadedModules dflags
         then loaded_msg mod_names
         else msg
     liftIO $ putStrLn rendered_msg
  where
    num_mods = length mods
    none_loaded = num_mods == 0

    loaded_msg names =
      let mod_commas
           | null mods = text "none."
           | otherwise = hsep (punctuate comma names) <> text "."
      in status <> text ", modules loaded:" <+> mod_commas

    msg  = status <> comma <+> msg' <> dot
    msg' = case load_type of
      Reload  -> if none_loaded
                   then "no modules to be reloaded"
                   else n_mods num_mods "reloaded"
      Load    -> if none_loaded
                   then "unloaded all modules"
                   else n_mods num_mods "loaded"
      Check   -> n_mods 1 "checked"
      Add   n -> n_mods n "added"
      Unadd n -> n_mods n "unadded"
    n_mods amount action = speakNOf amount "module" <+> action

    status | Succeeded <- ok = "Ok"
           | otherwise       = "Failed"

    mod_name mod = do
        is_interpreted <- GHC.moduleIsBootOrNotObjectLinkable mod
        pure $ if is_interpreted
               then ppr (GHC.ms_mod mod)
               else ppr (GHC.ms_mod mod)
                    <+> parens (text $ normalise $ msObjFilePath mod)
                    -- Fix #9887

-- | Run an 'ExceptT' wrapped 'GhcMonad' while handling source errors
-- and printing 'throwE' strings to 'stderr'. If in expression
-- evaluation mode - throw GhcException and exit.
runExceptGhciMonad :: GhciMonad m => ExceptT SDoc m () -> m ()
runExceptGhciMonad act = handleSourceError printGhciException $
                         either handleErr pure =<<
                         runExceptT act
  where
    handleErr sdoc = do
        rendered <- showSDocForUserQualify sdoc
        liftIO $ hPutStrLn stderr rendered
        failIfExprEvalMode

-- | Inverse of 'runExceptT' for \"pure\" computations
-- (c.f. 'except' for 'Except')
exceptT :: Applicative m => Either e a -> ExceptT e m a
exceptT = ExceptT . pure

-----------------------------------------------------------------------------
-- | @:type@ command. See also Note [TcRnExprMode] in GHC.Tc.Module.

typeOfExpr :: GhciMonad m => String -> m ()
typeOfExpr str = handleSourceError printErrAndMaybeExit $
    case break isSpace str of
      ("+v", _)    -> printForUser (text "`:type +v' has gone; use `:type' instead")
      ("+d", rest) -> do_it GHC.TM_Default (dropWhile isSpace rest)
      _            -> do_it GHC.TM_Inst    str
  where
    do_it mode expr_str
      = do { ty <- GHC.exprType mode expr_str
           ;    printForUser $ sep [ text expr_str
                                   , nest 2 (dcolon <+> pprSigmaType ty)] }

-----------------------------------------------------------------------------
-- | @:type-at@ command

typeAtCmd :: GhciMonad m => String -> m ()
typeAtCmd str = runExceptGhciMonad $ do
    (span',sample) <- exceptT $ parseSpanArg str
    infos      <- lift $ mod_infos <$> getGHCiState
    (info, ty) <- findType infos span' sample
    let mb_rdr_env = Just (modinfoRdrEnv info)
    lift $ printForUserGlobalRdrEnv
              mb_rdr_env
              (sep [text sample,nest 2 (dcolon <+> ppr ty)])

-----------------------------------------------------------------------------
-- | @:uses@ command

usesCmd :: GhciMonad m => String -> m ()
usesCmd str = runExceptGhciMonad $ do
    (span',sample) <- exceptT $ parseSpanArg str
    infos  <- lift $ mod_infos <$> getGHCiState
    uses   <- findNameUses infos span' sample
    forM_ uses (liftIO . putStrLn . showSrcSpan)

-----------------------------------------------------------------------------
-- | @:loc-at@ command

locAtCmd :: GhciMonad m => String -> m ()
locAtCmd str = runExceptGhciMonad $ do
    (span',sample) <- exceptT $ parseSpanArg str
    infos    <- lift $ mod_infos <$> getGHCiState
    (_,_,sp) <- findLoc infos span' sample
    liftIO . putStrLn . showSrcSpan $ sp

-----------------------------------------------------------------------------
-- | @:all-types@ command

allTypesCmd :: GhciMonad m => String -> m ()
allTypesCmd _ = runExceptGhciMonad $ do
    infos <- lift $ mod_infos <$> getGHCiState
    forM_ (M.elems infos) $ \mi ->
        forM_ (modinfoSpans mi) (lift . printSpan)
  where
    printSpan span'
      | Just ty <- spaninfoType span' = do
        tyInfo <- (unwords . words) <$>
                  showSDocForUserQualify (pprSigmaType ty)
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
                              -- End column of RealSrcSpan is the column
                              -- after the end of the span.
                              (mkRealSrcLoc fs el (ec + 1))

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
showSrcSpan (UnhelpfulSpan s)  = unpackFS (unhelpfulSpanFS s)
showSrcSpan (RealSrcSpan spn _) = showRealSrcSpan spn

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
    -- The end column is the column after the end of the span see the
    -- RealSrcSpan module
    ec = let ec' = srcSpanEndCol    spn in if ec' == 0 then 0 else ec' - 1

-----------------------------------------------------------------------------
-- | @:kind@ command

kindOfType :: GhciMonad m => Bool -> String -> m ()
kindOfType norm str = handleSourceError printErrAndMaybeExit $ do
    (ty, kind) <- GHC.typeKind norm str
    printForUser $ vcat [ text str <+> dcolon <+> pprSigmaType kind
                        , ppWhen norm $ equals <+> pprSigmaType ty ]

-----------------------------------------------------------------------------
-- :quit

quit :: Monad m => String -> m CmdExecOutcome
quit _ = return CleanExit


-----------------------------------------------------------------------------
-- :script

-- running a script file #1363

scriptCmd :: String -> InputT GHCi ()
scriptCmd ws = do
  case words' ws of
    [s]    -> runScript s
    _      -> throwGhcException (CmdLineError "syntax:  :script <filename>")

-- | A version of 'words' that treats sequences enclosed in double quotes as
-- single words and that does not break on backslash-escaped spaces.
-- E.g., 'words\' "\"lorem ipsum\" dolor"' and 'words\' "lorem\\ ipsum dolor"'
-- yield '["lorem ipsum", "dolor"]'.
-- Used to scan for file paths in 'scriptCmd'.
words' :: String -> [String]
words' s = case dropWhile isSpace s of
  "" -> []
  s'@('\"' : _) | [(w, s'')] <- reads s' -> w : words' s''
  s' -> go id s'
 where
  go acc []                          = [acc []]
  go acc ('\\' : c : cs) | isSpace c = go (acc . (c :)) cs
  go acc (c : cs) | isSpace c = acc [] : words' cs
                  | otherwise = go (acc . (c :)) cs

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

isSafeCmd :: GHC.GhcMonad m => String -> m ()
isSafeCmd m =
    case words m of
        [s] | looksLikeModuleName s -> do
            md <- lookupModule s
            isSafeModule md
        [] -> do md <- guessCurrentModule "issafe"
                 isSafeModule md
        _ -> throwGhcException (CmdLineError "syntax:  :issafe <module>")

isSafeModule :: GHC.GhcMonad m => Module -> m ()
isSafeModule m = do
    mb_mod_info <- GHC.getModuleInfo m
    when (isNothing mb_mod_info)
         (throwGhcException $ CmdLineError $ "unknown module: " ++ mname)

    dflags <- getDynFlags
    hsc_env <- GHC.getSession
    let iface = GHC.modInfoIface $ fromJust mb_mod_info
    when (isNothing iface)
         (throwGhcException $ CmdLineError $ "can't load interface file for module: " ++
                                    (GHC.moduleNameString $ GHC.moduleName m))

    (msafe, pkgs) <- GHC.moduleTrustReqs m
    let trust  = show $ getSafeMode $ GHC.mi_trust $ fromJust iface
        pkg    = if packageTrusted hsc_env m then "trusted" else "untrusted"
        (good, bad) = tallyPkgs hsc_env pkgs

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

    packageTrusted hsc_env md
        | isHomeModule (hsc_home_unit hsc_env) md = True
        | otherwise = unitIsTrusted $ unsafeLookupUnit (hsc_units hsc_env) (moduleUnit md)

    tallyPkgs hsc_env deps | not (packageTrustOn dflags) = (S.empty, S.empty)
                          | otherwise = S.partition part deps
        where part pkg   = unitIsTrusted $ unsafeLookupUnitId unit_state pkg
              unit_state = hsc_units hsc_env
              dflags     = hsc_dflags hsc_env

-----------------------------------------------------------------------------
-- :browse

-- Browsing a module's contents

browseCmd :: GHC.GhcMonad m => Bool -> String -> m ()
browseCmd bang m =
  case words m of
    ['*':s] | looksLikeModuleName s -> do
        md <- wantInterpretedModule s
        browseModule bang md False
    [s] | looksLikeModuleName s -> do
        md <- lookupModule s
        browseModule bang md True
    [] -> do md <- guessCurrentModule ("browse" ++ if bang then "!" else "")
             browseModule bang md True
    _ -> throwGhcException (CmdLineError "syntax:  :browse <module>")

guessCurrentModule :: GHC.GhcMonad m => String -> m Module
-- Guess which module the user wants to browse.  Pick
-- modules that are interpreted first.  The most
-- recently-added module occurs last, it seems.
guessCurrentModule cmd = do
  imports <- GHC.getContext
  case imports of
    [] -> throwGhcException $ CmdLineError (':' : cmd ++ ": no current module")
    IIModule m : _ -> GHC.findQualifiedModule NoPkgQual m
    IIDecl d : _ -> do
      pkgqual <- GHC.renameRawPkgQualM (unLoc $ ideclName d) (ideclPkgQual d)
      GHC.findQualifiedModule pkgqual (unLoc (ideclName d))

-- without bang, show items in context of their parents and omit children
-- with bang, show class methods and data constructors separately, and
--            indicate import modules, to aid qualifying unqualified names
-- with sorted, sort items alphabetically
browseModule :: GHC.GhcMonad m => Bool -> Module -> Bool -> m ()
browseModule bang modl exports_only = do
  mb_mod_info <- GHC.getModuleInfo modl
  case mb_mod_info of
    Nothing -> throwGhcException (CmdLineError ("unknown module: " ++
                                GHC.moduleNameString (GHC.moduleName modl)))
    Just mod_info -> do
        names <-
          if exports_only
          then pure $ GHC.modInfoExports mod_info
          else do
            hsc_env <- GHC.getSession
            mmod_env <- liftIO $ mkTopLevEnv hsc_env (moduleName modl)
            case mmod_env of
              Left err -> throwGhcException (CmdLineError (GHC.moduleNameString (GHC.moduleName modl) ++ " " ++ err))
              Right mod_env -> pure $ map greName . globalRdrEnvElts $ mod_env
        let

                -- sort alphabetically name, but putting locally-defined
                -- identifiers first. We would like to improve this; see #1799.
            sorted_names = loc_sort local ++ occ_sort external
                where
                (local,external) = assert (all isExternalName names) $
                                   partition ((==modl) . nameModule) names
                occ_sort = sortBy (compare `on` nameOccName)
                -- try to sort by src location. If the first name in our list
                -- has a good source location, then they all should.
                loc_sort ns
                      | n:_ <- ns, isGoodSrcSpan (nameSrcSpan n)
                      = sortBy (SrcLoc.leftmost_smallest `on` nameSrcSpan) ns
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
            -- really come in order of source appearance.. (#1799)
            annotate mts = concatMap (\(m,ts)->labels m:ts)
                         $ sortBy cmpQualifiers $ grp mts
              where cmpQualifiers =
                      compare `on` (map (fmap (map (unpackFS . moduleNameFS))) . fst)
            grp []            = []
            grp mts@((m,_):_) = (m,map snd g) : grp ng
              where (g,ng) = partition ((==m).fst) mts

        let prettyThings, prettyThings' :: [SDoc]
            prettyThings = map pretty things
            prettyThings' | bang      = annotate $ zip modNames prettyThings
                          | otherwise = prettyThings

        -- :browse reports qualifiers wrt current context
        rendered_things <- showSDocForUser' (vcat prettyThings')
        liftIO $ putStrLn rendered_things
        -- ToDo: modInfoInstances currently throws an exception for
        -- package modules.  When it works, we can do this:
        --        $$ vcat (map GHC.pprInstance (GHC.modInfoInstances mod_info))


-----------------------------------------------------------------------------
-- :module

-- Setting the module context.  For details on context handling see
-- "remembered_ctx" and "transient_ctx" in GhciMonad.

moduleCmd :: GhciMonad m => String -> m ()
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

addModulesToContext :: GhciMonad m => [ModuleName] -> [ModuleName] -> m ()
addModulesToContext starred unstarred = restoreContextOnFailure $ do
   addModulesToContext_ starred unstarred

addModulesToContext_ :: GhciMonad m => [ModuleName] -> [ModuleName] -> m ()
addModulesToContext_ starred unstarred = do
   mapM_ addII (map mkIIModule starred ++ map mkIIDecl unstarred)
   setGHCContextFromGHCiState

remModulesFromContext :: GhciMonad m => [ModuleName] -> [ModuleName] -> m ()
remModulesFromContext  starred unstarred = do
   -- we do *not* call restoreContextOnFailure here.  If the user
   -- is trying to fix up a context that contains errors by removing
   -- modules, we don't want GHC to silently put them back in again.
   mapM_ rm (starred ++ unstarred)
   setGHCContextFromGHCiState
 where
   rm :: GhciMonad m => ModuleName -> m ()
   rm str = do
     m <- moduleName <$> lookupModuleName str
     let filt = filter ((/=) m . iiModuleName)
     modifyGHCiState $ \st ->
        st { remembered_ctx = filt (remembered_ctx st)
           , transient_ctx  = filt (transient_ctx st) }

setContext :: GhciMonad m => [ModuleName] -> [ModuleName] -> m ()
setContext starred unstarred = restoreContextOnFailure $ do
  modifyGHCiState $ \st -> st { remembered_ctx = [], transient_ctx = [] }
                                -- delete the transient context
  addModulesToContext_ starred unstarred

addImportToContext :: GhciMonad m => ImportDecl GhcPs -> m ()
addImportToContext idecl = restoreContextOnFailure $ do
  addII (IIDecl idecl)   -- #5836
  setGHCContextFromGHCiState

-- Util used by addImportToContext and addModulesToContext
addII :: GhciMonad m => InteractiveImport -> m ()
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
restoreContextOnFailure :: GhciMonad m => m a -> m a
restoreContextOnFailure do_this = do
  st <- getGHCiState
  let rc = remembered_ctx st; tc = transient_ctx st
  do_this `MC.onException` (modifyGHCiState $ \st' ->
     st' { remembered_ctx = rc, transient_ctx = tc })

-- -----------------------------------------------------------------------------
-- Validate a module that we want to add to the context

checkAdd :: GHC.GhcMonad m => InteractiveImport -> m ()
checkAdd ii = do
  dflags <- getDynFlags
  let safe = safeLanguageOn dflags
  case ii of
    IIModule modname
       | safe -> throwGhcException $ CmdLineError "can't use * imports with Safe Haskell"
       | otherwise -> wantInterpretedModuleName modname >> return ()

    IIDecl d -> do
       let modname = unLoc (ideclName d)
       pkgqual <- GHC.renameRawPkgQualM modname (ideclPkgQual d)
       m <- GHC.lookupQualifiedModule pkgqual modname
       when safe $ do
           t <- GHC.isModuleTrusted m
           unless t $ throwGhcException $ ProgramError $ ""

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
setGHCContextFromGHCiState :: GhciMonad m => m ()
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


getImplicitPreludeImports :: GhciMonad m
                          => [InteractiveImport] -> m [InteractiveImport]
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

  -- See Note [GHCi and local Preludes]
  keepPackageImports prel_iidecls

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
     && (not (isImportDeclQualified (ideclQualified d1)) || isImportDeclQualified (ideclQualified d2))
     && (ideclImportList d1 `hidingSubsumes` ideclImportList d2)
  where
     _                    `hidingSubsumes` Just (Exactly,L _ []) = True
     Just (Exactly, L _ xs) `hidingSubsumes` Just (Exactly,L _ ys)
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

setCmd :: GhciMonad m => String -> m ()
setCmd ""   = showOptions False
setCmd "-a" = showOptions True
setCmd str
  = case getCmd str of
    Right ("args",    rest) ->
        case toArgsNoLoc rest of
            Left err -> liftIO (hPutStrLn stderr err)
            Right args -> setArgs args
    Right ("prog",    rest) ->
        case toArgsNoLoc rest of
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
    Right ("local-config", rest) ->
        setLocalConfigBehaviour $ dropWhile isSpace rest
    _ -> case toArgsNoLoc str of
         Left err -> liftIO (hPutStrLn stderr err)
         Right wds -> () <$ keepGoing' setOptions wds

setiCmd :: GhciMonad m => String -> m ()
setiCmd ""   = GHC.getInteractiveDynFlags >>= liftIO . showDynFlags False
setiCmd "-a" = GHC.getInteractiveDynFlags >>= liftIO . showDynFlags True
setiCmd str  =
  case toArgsNoLoc str of
    Left err -> liftIO (hPutStrLn stderr err)
    Right wds -> newDynFlags True wds

showOptions :: GhciMonad m => Bool -> m ()
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
       liftIO $ showDynFlags show_all dflags


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
        setting :: String -> String -> (flag -> DynFlags -> Bool) -> FlagSpec flag -> SDoc
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

setArgs, setOptions :: GhciMonad m => [String] -> m ()
setProg, setEditor, setStop :: GhciMonad m => String -> m ()
setLocalConfigBehaviour :: GhciMonad m => String -> m ()

setArgs args = do
  st <- getGHCiState
  wrapper <- mkEvalWrapper (progname st) args
  setGHCiState st { GhciMonad.args = args, evalWrapper = wrapper }

setProg prog = do
  st <- getGHCiState
  wrapper <- mkEvalWrapper prog (GhciMonad.args st)
  setGHCiState st { progname = prog, evalWrapper = wrapper }

setEditor cmd = modifyGHCiState (\st -> st { editor = cmd })

setLocalConfigBehaviour s
  | s == "source" =
      modifyGHCiState (\st -> st { localConfig = SourceLocalConfig })
  | s == "ignore" =
      modifyGHCiState (\st -> st { localConfig = IgnoreLocalConfig })
  | otherwise = throwGhcException
      (CmdLineError "syntax:  :set local-config { source | ignore }")

setStop str@(c:_) | isDigit c
  = do let (nm_str,rest) = break (not.isDigit) str
           nm = read nm_str
       st <- getGHCiState
       let old_breaks = breaks st
       case IntMap.lookup nm old_breaks of
         Nothing ->  printForUser (text "Breakpoint" <+> ppr nm <+>
                                   text "does not exist")
         Just loc -> do
            let new_breaks = IntMap.insert nm
                                loc { onBreakCmd = dropWhile isSpace rest }
                                old_breaks
            setGHCiState st{ breaks = new_breaks }
setStop cmd = modifyGHCiState (\st -> st { stop = cmd })

setPrompt :: GhciMonad m => PromptFunction -> m ()
setPrompt v = modifyGHCiState (\st -> st {prompt = v})

setPromptCont :: GhciMonad m => PromptFunction -> m ()
setPromptCont v = modifyGHCiState (\st -> st {prompt_cont = v})

setPromptFunc :: GHC.GhcMonad m => (PromptFunction -> m ()) -> String -> m ()
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

setPromptString :: MonadIO m
                => (PromptFunction -> m ()) -> String -> String -> m ()
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

setParsedPromptString :: MonadIO m
                      => (PromptFunction -> m ()) ->  String -> m ()
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

-- | newDynFlags will *not* read package environment files, therefore we
-- use 'parseDynamicFlagsCmdLine' rather than 'parseDynamicFlags'. This
-- function is called very often and results in repeatedly loading
-- environment files (see #19650)
newDynFlags :: GhciMonad m => Bool -> [String] -> m ()
newDynFlags interactive_only minus_opts = do
      let lopts = map noLoc minus_opts

      logger <- getLogger
      idflags0 <- GHC.getInteractiveDynFlags
      (idflags1, leftovers, warns) <- DynFlags.parseDynamicFlagsCmdLine idflags0 lopts

      liftIO $ printOrThrowDiagnostics logger (initPrintConfig idflags1) (initDiagOpts idflags1) (GhcDriverMessage <$> warns)
      when (not $ null leftovers) (unknownFlagsErr $ map unLoc leftovers)

      when (interactive_only && packageFlagsChanged idflags1 idflags0) $ do
          liftIO $ hPutStrLn stderr "cannot set package flags with :seti; use :set"
      GHC.setInteractiveDynFlags idflags1
      installInteractivePrint (interactivePrint idflags1) False

      dflags0 <- getDynFlags

      when (not interactive_only) $ do
        (dflags1, _, _) <- liftIO $ DynFlags.parseDynamicFlagsCmdLine dflags0 lopts
        must_reload <- GHC.setProgramDynFlags dflags1

        -- if the package flags changed, reset the context and link
        -- the new packages.
        hsc_env <- GHC.getSession
        let dflags2 = hsc_dflags hsc_env
        let interp  = hscInterp hsc_env
        when (packageFlagsChanged dflags2 dflags0) $ do
          when (verbosity dflags2 > 0) $
            liftIO . putStrLn $
              "package flags have changed, resetting and loading new packages..."
          -- Clear caches and eventually defined breakpoints. (#1620)
          clearCaches
          when must_reload $ do
            let units = preloadUnits (hsc_units hsc_env)
            liftIO $ Loader.loadPackages interp hsc_env units
          -- package flags changed, we can't re-use any of the old context
          setContextAfterLoad False Nothing
          -- and copy the package flags to the interactive DynFlags
          idflags <- GHC.getInteractiveDynFlags
          GHC.setInteractiveDynFlags
              idflags{ packageFlags = packageFlags dflags2 }

        let ld0length   = length $ ldInputs dflags0
            fmrk0length = length $ cmdlineFrameworks dflags0

            newLdInputs     = drop ld0length (ldInputs dflags2)
            newCLFrameworks = drop fmrk0length (cmdlineFrameworks dflags2)

            dflags'  = dflags2 { ldInputs = newLdInputs
                               , cmdlineFrameworks = newCLFrameworks
                               }
            hsc_env' = hscSetFlags dflags' hsc_env

        when (not (null newLdInputs && null newCLFrameworks)) $
          liftIO $ Loader.loadCmdLineLibs (hscInterp hsc_env') hsc_env'

      return ()

unknownFlagsErr :: [String] -> a
unknownFlagsErr fs = throwGhcException $ CmdLineError $ concatMap oneError fs
  where
    oneError f =
        "unrecognised flag: " ++ f ++ "\n" ++
        (case flagSuggestions ghciFlags f of
            [] -> ""
            suggs -> "did you mean one of:\n" ++ unlines (map ("  " ++) suggs))
    ghciFlags = nubSort $ flagsForCompletion True

unsetOptions :: GhciMonad m => String -> m ()
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

     in case rest3 of
          opt:_ -> liftIO (putStrLn ("unknown option: '" ++ opt ++ "'"))
          [] -> do
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

setOpt, unsetOpt :: GhciMonad m => String -> m ()

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

showCmd :: forall m. GhciMonad m => String -> m ()
showCmd ""   = showOptions False
showCmd "-a" = showOptions True
showCmd str = do
    st <- getGHCiState
    dflags <- getDynFlags
    hsc_env <- GHC.getSession

    let lookupCmd :: String -> Maybe (m ())
        lookupCmd name = lookup name $ map (\(_,b,c) -> (b,c)) cmds

        -- (show in help?, command name, action)
        action :: String -> m () -> (Bool, String, m ())
        action name m = (True, name, m)

        hidden :: String -> m () -> (Bool, String, m ())
        hidden name m = (False, name, m)

        cmds =
            [ action "args"       $ liftIO $ putStrLn (show (GhciMonad.args st))
            , action "prog"       $ liftIO $ putStrLn (show (progname st))
            , action "editor"     $ liftIO $ putStrLn (show (editor st))
            , action "stop"       $ liftIO $ putStrLn (show (stop st))
            , action "imports"    $ showImports
            , action "modules"    $ showModules
            , action "bindings"   $ showBindings
            , action "linker"     $ do
               msg <- liftIO $ Loader.showLoaderState (hscInterp hsc_env)
               dflags <- getDynFlags
               liftIO $ putStrLn $ showSDoc dflags msg
            , action "breaks"     $ showBkptTable
            , action "context"    $ showContext
            , action "packages"   $ showUnits
            , action "paths"      $ showPaths
            , action "language"   $ showLanguages
            , hidden "languages"  $ showLanguages -- backwards compat
            , hidden "lang"       $ showLanguages -- useful abbreviation
            , action "targets"    $ showTargets
            ]

    case words str of
      [w] | Just action <- lookupCmd w -> action

      _ -> let helpCmds = [ text name | (True, name, _) <- cmds ]
           in throwGhcException $ CmdLineError $ showSDoc dflags
              $ hang (text "syntax:") 4
              $ hang (text ":show") 6
              $ brackets (fsep $ punctuate (text " |") helpCmds)

showiCmd :: GHC.GhcMonad m => String -> m ()
showiCmd str = do
  case words str of
        ["languages"]  -> showiLanguages -- backwards compat
        ["language"]   -> showiLanguages
        ["lang"]       -> showiLanguages -- useful abbreviation
        _ -> throwGhcException (CmdLineError ("syntax:  :showi language"))

showImports :: GhciMonad m => m ()
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

showModules :: GHC.GhcMonad m => m ()
showModules = do
  loaded_mods <- getLoadedModules
        -- we want *loaded* modules only, see #1734
  let show_one ms = do m <- GHC.showModule ms; liftIO (putStrLn m)
  mapM_ show_one loaded_mods

getLoadedModules :: GHC.GhcMonad m => m [GHC.ModSummary]
getLoadedModules = do
  graph <- GHC.getModuleGraph
  filterM isLoadedModSummary (GHC.mgModSummaries graph)

showBindings :: GHC.GhcMonad m => m ()
showBindings = do
    bindings <- GHC.getBindings
    (insts, finsts) <- GHC.getInsts
    let idocs  = map GHC.pprInstanceHdr insts
        fidocs = map GHC.pprFamInst finsts
        binds = filter (not . isDerivedOccName . getOccName) bindings -- #12525
        -- See Note [Filter bindings]
    docs <- mapM makeDoc (reverse binds)
                  -- reverse so the new ones come last
    mapM_ printForUserPartWay (docs ++ idocs ++ fidocs)
  where
    makeDoc (AnId i) = pprTypeAndContents i
    makeDoc tt = do
        mb_stuff <- GHC.getInfo False (getName tt)
        return $ maybe (text "") pprTT mb_stuff

    pprTT :: (TyThing, Fixity, [GHC.ClsInst], [GHC.FamInst], SDoc) -> SDoc
    pprTT (thing, fixity, _cls_insts, _fam_insts, _docs)
      = pprTyThing showToHeader thing
        $$ showFixity thing fixity


printTyThing :: GHC.GhcMonad m => TyThing -> m ()
printTyThing tyth = printForUser (pprTyThing showToHeader tyth)

isLoadedModSummary :: GHC.GhcMonad m => ModSummary -> m Bool
isLoadedModSummary ms = GHC.isLoadedModule (ms_unitid ms) (ms_mod_name ms)

{-
Note [Filter bindings]
~~~~~~~~~~~~~~~~~~~~~~
If we don't filter the bindings returned by the function GHC.getBindings,
then the :show bindings command will also show unwanted bound names,
internally generated by GHC, eg:
    $tcFoo :: GHC.Types.TyCon = _
    $trModule :: GHC.Unit.Module = _ .

The filter was introduced as a fix for #12525 [1]. Comment:1 [2] to this
ticket contains an analysis of the situation and suggests the solution
implemented above.

The same filter was also implemented to fix #11051 [3]. See the
Note [What to show to users] in GHC.Runtime.Eval

[1] https://gitlab.haskell.org/ghc/ghc/issues/12525
[2] https://gitlab.haskell.org/ghc/ghc/issues/12525#note_123489
[3] https://gitlab.haskell.org/ghc/ghc/issues/11051
-}


showBkptTable :: GhciMonad m => m ()
showBkptTable = do
  st <- getGHCiState
  printForUser $ prettyLocations (breaks st)

showContext :: GHC.GhcMonad m => m ()
showContext = do
   resumes <- GHC.getResumeContext
   printForUser $ vcat (map pp_resume (reverse resumes))
  where
   pp_resume res =
        text "--> " <> text (GHC.resumeStmt res)
        $$ nest 2 (pprStopped res)

pprStopped :: GHC.Resume -> SDoc
pprStopped res =
  text "Stopped in"
    <+> ((case mb_mod_name of
           Nothing -> empty
           Just mod_name -> ftext (moduleNameFS mod_name) <> char '.')
         <> text (GHC.resumeDecl res))
    <> char ',' <+> ppr (GHC.resumeSpan res)
 where
  mb_mod_name = moduleName <$> ibi_tick_mod <$> GHC.resumeBreakpointId res

showUnits :: GHC.GhcMonad m => m ()
showUnits = do
  dflags <- getDynFlags
  let pkg_flags = packageFlags dflags
  liftIO $ putStrLn $ showSDoc dflags $
    text ("active package flags:"++if null pkg_flags then " none" else "") $$
      nest 2 (vcat (map pprFlag pkg_flags))

showPaths :: GHC.GhcMonad m => m ()
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

showLanguages :: GHC.GhcMonad m => m ()
showLanguages = getDynFlags >>= liftIO . showLanguages' False

showiLanguages :: GHC.GhcMonad m => m ()
showiLanguages = GHC.getInteractiveDynFlags >>= liftIO . showLanguages' False

showLanguages' :: Bool -> DynFlags -> IO ()
showLanguages' show_all dflags =
  putStrLn $ showSDoc dflags $ vcat
     [ text "base language is: " <>
         case lang of
           Haskell98   -> text "Haskell98"
           Haskell2010 -> text "Haskell2010"
           GHC2021     -> text "GHC2021"
           GHC2024     -> text "GHC2024"
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

   default_dflags = defaultDynFlags (settings dflags) `lang_set` Just lang

   lang = fromMaybe defaultLanguage (language dflags)


showTargets :: GHC.GhcMonad m => m ()
showTargets = mapM_ showTarget =<< GHC.getTargets
  where
    showTarget :: GHC.GhcMonad m => Target -> m ()
    showTarget Target { targetId = TargetFile f _ } = liftIO (putStrLn f)
    showTarget Target { targetId = TargetModule m } =
      liftIO (putStrLn $ moduleNameString m)

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
    parseLine [] = Nothing
    parseLine argLine = case breakSpace argLine of
      (_, []) -> Nothing
      (dom, rest1@('"' : _)) -> (dom,,) <$> parseRange "" <*> (readMaybe rest1 :: Maybe String)
      (dom, rest1) -> (dom,,) <$> parseRange rng <*> readMaybe rest2
        where
          (rng, rest2) = breakSpace rest1

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
    completeSetOptions, completeShowOptions,
    completeHomeModuleOrFile, completeExpression, completeBreakpoint
    :: GhciMonad m => CompletionFunc m

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
  return $ filter (w `isPrefixOptOf`) candidates

completeMacro = wrapIdentCompleter $ \w -> do
  cmds <- ghci_macros <$> getGHCiState
  return (filter (w `isPrefixOf`) (map cmdName cmds))

completeIdentifier line@(left, _) =
  -- Note: `left` is a reversed input
  case left of
    ('.':_)  -> wrapCompleter (specials ++ spaces) complete line
               -- operator or qualification
    (x:_) | isSymbolChar x -> wrapCompleter' (\c -> c `elem` (specials ++ spaces) || not (isSymbolChar c))
                                 complete line         -- operator
    _                      -> wrapIdentCompleter complete line
  where
    complete w = do
      rdrs <- GHC.getRdrNamesInScope
      dflags <- GHC.getSessionDynFlags
      return (filter (w `isPrefixOf`) (map (showPpr dflags) rdrs))

-- TAB-completion for the :break command.
-- Build and return a list of breakpoint identifiers with a given prefix.
-- See Note [Tab-completion for :break]
completeBreakpoint = wrapCompleter spaces $ \w -> do          -- #3000
    -- bid ~ breakpoint identifier = a name of a function that is
    --       eligible to set a breakpoint.
    let (mod_str, _, _) = splitIdent w
    bids_mod_breaks <- bidsFromModBreaks mod_str
    bids_inscopes <- bidsFromInscopes
    pure $ nub $ filter (isPrefixOf w) $ bids_mod_breaks ++ bids_inscopes
  where
    -- Extract all bids from ModBreaks for a given module name prefix
    bidsFromModBreaks :: GhciMonad m => String -> m [String]
    bidsFromModBreaks mod_pref = do
        imods <- interpretedHomeMods
        let pmods = filter ((isPrefixOf mod_pref) . showModule) imods
        nonquals <- case null mod_pref of
          -- If the prefix is empty, then for functions declared in a module
          -- in scope, don't qualify the function name.
          -- (eg: `main` instead of `Main.main`)
            True -> do
                imports <- GHC.getContext
                pure [ m | IIModule m <- imports]
            False -> return []
        bidss <- mapM (bidsByModule nonquals) pmods
        pure $ concat bidss

    -- Return a list of interpreted home modules
    interpretedHomeMods :: GhciMonad m => m [Module]
    interpretedHomeMods = do
        graph <- GHC.getModuleGraph
        let hmods = ms_mod <$> GHC.mgModSummaries graph
        filterM GHC.moduleIsInterpreted hmods

    -- Return all possible bids for a given Module
    bidsByModule :: GhciMonad m => [ModuleName] -> Module -> m [String]
    bidsByModule nonquals mod = do
      (_, decls) <- getModBreak mod
      let bids = nub $ declPath <$> elems decls
      pure $ case (moduleName mod) `elem` nonquals of
              True  -> bids
              False -> (combineModIdent (showModule mod)) <$> bids

    -- Extract all bids from all top-level identifiers in scope.
    bidsFromInscopes :: GhciMonad m => m [String]
    bidsFromInscopes = do
        dflags <- getDynFlags
        rdrs <- GHC.getRdrNamesInScope
        inscopess <- mapM createInscope $ (showSDoc dflags . ppr) <$> rdrs
        imods <- interpretedHomeMods
        let topLevels = filter ((`elem` imods) . snd) $ concat inscopess
        bidss <- mapM (addNestedDecls) topLevels
        pure $ concat bidss

    -- Return a list of (bid,module) for a single top-level in-scope identifier
    createInscope :: GhciMonad m => String -> m [(String, Module)]
    createInscope str_rdr = do
        names <- GHC.parseName str_rdr
        pure $ map (str_rdr, ) $ NE.toList $ GHC.nameModule <$> names

    -- For every top-level identifier in scope, add the bids of the nested
    -- declarations. See Note [Field modBreaks_decls] in GHC.ByteCode.Types
    addNestedDecls :: GhciMonad m => (String, Module) -> m [String]
    addNestedDecls (ident, mod) = do
        (_, decls) <- getModBreak mod
        let (mod_str, topLvl, _) = splitIdent ident
            ident_decls = [ elm | elm@(el : _) <- elems decls, el == topLvl ]
            bids = nub $ declPath <$> ident_decls
        pure $ map (combineModIdent mod_str) bids

completeModule = wrapIdentCompleterMod $ \w -> do
  hsc_env <- GHC.getSession
  let pkg_mods = allVisibleModules (hsc_units hsc_env)
  loaded_mods <- liftM (map GHC.ms_mod_name) getLoadedModules
  return $ filter (w `isPrefixOf`)
        $ map (showPpr (hsc_dflags hsc_env)) $ loaded_mods ++ pkg_mods

completeSetModule = wrapIdentCompleterWithModifier "+-" $ \m w -> do
  hsc_env <- GHC.getSession
  modules <- case m of
    Just '-' -> do
      imports <- GHC.getContext
      return $ map iiModuleName imports
    _ -> do
      let pkg_mods = allVisibleModules (hsc_units hsc_env)
      loaded_mods <- liftM (map GHC.ms_mod_name) getLoadedModules
      return $ loaded_mods ++ pkg_mods
  return $ filter (w `isPrefixOf`) $ map (showPpr (hsc_dflags hsc_env)) modules

listHomeModules :: GHC.GhcMonad m => String -> m [String]
listHomeModules w = do
    g <- GHC.getModuleGraph
    let home_mods = map GHC.ms_mod_name (GHC.mgModSummaries g)
    dflags <- getDynFlags
    return $ sort $ filter (w `isPrefixOf`)
            $ map (showPpr dflags) home_mods

completeSetOptions = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) opts)
    where opts = "args":"prog":"prompt":"prompt-cont":"prompt-function":
                 "prompt-cont-function":"editor":"stop":flagList
          flagList = map NE.head $ NE.group $ sort allNonDeprecatedFlags

completeSeti = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) flagList)
    where flagList = map NE.head $ NE.group $ sort allNonDeprecatedFlags

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

wrapCompleter :: Monad m => String -> (String -> m [String]) -> CompletionFunc m
wrapCompleter breakChars = wrapCompleter' (`elem` breakChars)

wrapCompleter' :: Monad m => (Char -> Bool) -> (String -> m [String]) -> CompletionFunc m
wrapCompleter' breakPred fun = completeWord' Nothing breakPred
    $ fmap (map simpleCompletion . nubSort) . fun

wrapIdentCompleter :: Monad m => (String -> m [String]) -> CompletionFunc m
wrapIdentCompleter = wrapCompleter' word_break_chars_pred

wrapIdentCompleterMod :: Monad m => (String -> m [String]) -> CompletionFunc m
wrapIdentCompleterMod = wrapCompleter' go
  where
    go '.' = False -- Treated specially since it is a separator for module qualifiers
    go c = word_break_chars_pred c

wrapIdentCompleterWithModifier
  :: Monad m
  => String -> (Maybe Char -> String -> m [String]) -> CompletionFunc m
wrapIdentCompleterWithModifier modifChars fun = completeWordWithPrev Nothing word_break_chars
    $ \rest -> fmap (map simpleCompletion . nubSort) . fun (getModifier rest)
 where
  getModifier = find (`elem` modifChars)

-- | Return a list of visible module names for autocompletion.
-- (NB: exposed != visible)
allVisibleModules :: UnitState -> [ModuleName]
allVisibleModules unit_state = listVisibleModuleNames unit_state

completeExpression = completeQuotedWord (Just '\\') "\"" listFiles
                        completeIdentifier


{-
Note [Tab-completion for :break]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In tab-completion for the `:break` command, only those
identifiers should be shown, that are accepted in the
`:break` command. Hence these identifiers must be

- defined in an interpreted module
- listed in a `ModBreaks` value as a possible breakpoint.

The identifiers may be qualified or unqualified.

To get all possible top-level breakpoints for tab-completion
with the correct qualification do:

1. Build a list called `bids_mod_breaks` of identifier names eligible
for setting breakpoints: For every interpreted module with the
correct module prefix read all identifier names from the `decls` field
of the `ModBreaks` array.

2. Build a list called `bids_inscopess` of identifiers in scope:
Take all RdrNames in scope, and filter by interpreted modules.
Fore each of these top-level identifiers add from the `ModBreaks`
arrays the available identifiers of the nested functions.

3.) Combine both lists, filter by the given prefix, and remove duplicates.
-}

-- -----------------------------------------------------------------------------
-- commands for debugger

sprintCmd, printCmd, forceCmd :: GHC.GhcMonad m => String -> m ()
sprintCmd = pprintClosureCommand False False
printCmd  = pprintClosureCommand True False
forceCmd  = pprintClosureCommand False True

stepCmd :: GhciMonad m => String -> m ()
stepCmd arg = withSandboxOnly ":step" $ step arg
  where
  step []         = doContinue (const True) GHC.SingleStep
  step expression = runStmt expression GHC.SingleStep >> return ()

stepLocalCmd :: GhciMonad m => String -> m ()
stepLocalCmd arg = withSandboxOnly ":steplocal" $ step arg
  where
  step expr
   | not (null expr) = stepCmd expr
   | otherwise = do
      mb_span <- getCurrentBreakSpan
      case mb_span of
        Nothing  -> stepCmd []
        Just (UnhelpfulSpan _) -> liftIO $ putStrLn (            -- #14690
           ":steplocal is not possible." ++
           "\nCannot determine current top-level binding after " ++
           "a break on error / exception.\nUse :stepmodule.")
        Just loc -> do
           md <- fromMaybe (panic "stepLocalCmd") <$> getCurrentBreakModule
           current_toplevel_decl <- enclosingTickSpan md loc
           doContinue (`isSubspanOf` RealSrcSpan current_toplevel_decl Strict.Nothing) GHC.SingleStep

stepModuleCmd :: GhciMonad m => String -> m ()
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
enclosingTickSpan :: GhciMonad m => Module -> SrcSpan -> m RealSrcSpan
enclosingTickSpan _ (UnhelpfulSpan _) = panic "enclosingTickSpan UnhelpfulSpan"
enclosingTickSpan md (RealSrcSpan src _) = do
  ticks <- getTickArray md
  let line = srcSpanStartLine src
  massert (inRange (bounds ticks) line)
  let enclosing_spans = [ pan | (_,pan) <- ticks ! line
                               , realSrcSpanEnd pan >= realSrcSpanEnd src]
  return . minimumBy leftmostLargestRealSrcSpan $ enclosing_spans
 where

leftmostLargestRealSrcSpan :: RealSrcSpan -> RealSrcSpan -> Ordering
leftmostLargestRealSrcSpan = on compare realSrcSpanStart S.<> on (flip compare) realSrcSpanEnd

traceCmd :: GhciMonad m => String -> m ()
traceCmd arg
  = withSandboxOnly ":trace" $ tr arg
  where
  tr []         = doContinue (const True) GHC.RunAndLogSteps
  tr expression = runStmt expression GHC.RunAndLogSteps >> return ()

continueCmd :: GhciMonad m => String -> m ()                  -- #19157
continueCmd argLine = withSandboxOnly ":continue" $
  case contSwitch (words argLine) of
    Left sdoc   -> printForUser sdoc
    Right mbCnt -> doContinue' (const True) GHC.RunToCompletion mbCnt
    where
      contSwitch :: [String] -> Either SDoc (Maybe Int)
      contSwitch [ ] = Right Nothing
      contSwitch [x] = Just <$> getIgnoreCount x
      contSwitch  _  = Left $
          text "After ':continue' only one ignore count is allowed"

doContinue :: GhciMonad m => (SrcSpan -> Bool) -> SingleStep -> m ()
doContinue pre step = doContinue' pre step Nothing

doContinue' :: GhciMonad m => (SrcSpan -> Bool) -> SingleStep -> Maybe Int -> m ()
doContinue' pre step mbCnt= do
  runResult <- resume pre step mbCnt
  _ <- afterRunStmt pre runResult
  return ()

abandonCmd :: GhciMonad m => String -> m ()
abandonCmd = noArgs $ withSandboxOnly ":abandon" $ do
  b <- GHC.abandon -- the prompt will change to indicate the new context
  when (not b) $ liftIO $ putStrLn "There is no computation running."

deleteCmd :: GhciMonad m => String -> m ()
deleteCmd argLine = withSandboxOnly ":delete" $ do
   deleteSwitch $ words argLine
   where
   deleteSwitch :: GhciMonad m => [String] -> m ()
   deleteSwitch [] =
      liftIO $ putStrLn "The delete command requires at least one argument."
   -- delete all break points
   deleteSwitch ("*":_rest) = discardActiveBreakPoints
   deleteSwitch idents = do
      mapM_ deleteOneBreak idents
      where
      deleteOneBreak :: GhciMonad m => String -> m ()
      deleteOneBreak str
         | all isDigit str = deleteBreak (read str)
         | otherwise = return ()

enableCmd :: GhciMonad m => String -> m ()
enableCmd argLine = withSandboxOnly ":enable" $ do
    enaDisaSwitch True $ words argLine

disableCmd :: GhciMonad m => String -> m ()
disableCmd argLine = withSandboxOnly ":disable" $ do
    enaDisaSwitch False $ words argLine

enaDisaSwitch :: GhciMonad m => Bool -> [String] -> m ()
enaDisaSwitch enaDisa [] =
    printForUser (text "The" <+> text strCmd <+>
                  text "command requires at least one argument.")
  where
    strCmd = if enaDisa then ":enable" else ":disable"
enaDisaSwitch enaDisa ("*" : _) = enaDisaAllBreaks enaDisa
enaDisaSwitch enaDisa idents = do
    mapM_ (enaDisaOneBreak enaDisa) idents
  where
    enaDisaOneBreak :: GhciMonad m => Bool -> String -> m ()
    enaDisaOneBreak enaDisa strId = do
      sdoc_loc <- checkEnaDisa enaDisa strId
      case sdoc_loc of
        Left sdoc -> printForUser sdoc
        Right loc -> enaDisaAssoc enaDisa (read strId, loc)

checkEnaDisa :: GhciMonad m => Bool -> String -> m (Either SDoc BreakLocation)
checkEnaDisa enaDisa strId = do
    sdoc_loc <- getBreakLoc strId
    pure $ sdoc_loc >>= checkEnaDisaState enaDisa strId

getBreakLoc :: GhciMonad m => String -> m (Either SDoc BreakLocation)
getBreakLoc strId = do
    st <- getGHCiState
    case readMaybe strId >>= flip IntMap.lookup (breaks st) of
      Nothing -> return $ Left (text "Breakpoint" <+> text strId <+>
                                text "not found")
      Just loc -> return $ Right loc

checkEnaDisaState :: Bool -> String -> BreakLocation -> Either SDoc BreakLocation
checkEnaDisaState enaDisa strId loc = do
    if breakEnabled loc == enaDisa
    then Left $
        text "Breakpoint" <+> text strId <+> text "already in desired state"
    else Right loc

enaDisaAssoc :: GhciMonad m => Bool -> (Int, BreakLocation) -> m ()
enaDisaAssoc enaDisa (intId, loc) = do
    st <- getGHCiState
    newLoc <- turnBreakOnOff enaDisa loc
    let new_breaks = IntMap.insert intId newLoc (breaks st)
    setGHCiState $ st { breaks = new_breaks }

enaDisaAllBreaks :: GhciMonad m => Bool -> m()
enaDisaAllBreaks enaDisa = do
    st <- getGHCiState
    mapM_ (enaDisaAssoc enaDisa) $ IntMap.assocs $ breaks st

historyCmd :: GHC.GhcMonad m => String -> m ()
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

ignoreCmd  :: GhciMonad m => String -> m ()                     -- #19157
ignoreCmd argLine = withSandboxOnly ":ignore" $ do
    result <- ignoreSwitch (words argLine)
    case result of
      Left sdoc -> printForUser sdoc
      Right (loc, count)   -> do
        let bi = GHC.BreakpointId
                  { bi_tick_mod   = breakModule loc
                  , bi_tick_index = breakTick loc
                  }
        setupBreakpoint bi count

ignoreSwitch :: GhciMonad m => [String] -> m (Either SDoc (BreakLocation, Int))
ignoreSwitch [break, count] = do
    sdoc_loc <- getBreakLoc break
    pure $ (,) <$> sdoc_loc <*> getIgnoreCount count
ignoreSwitch _ = pure $ Left $ text "Syntax:  :ignore <breaknum> <count>"

getIgnoreCount :: String -> Either SDoc Int
getIgnoreCount str =
    case readMaybe str of
      Nothing              -> Left $ sdocIgnore <+> "is not numeric"
      Just cnt | cnt < 0   -> Left $ sdocIgnore <+> "must be >= 0"
               | otherwise -> Right cnt
    where
      sdocIgnore = text "Ignore count" <+> quotes (text str)

setupBreakpoint :: GhciMonad m => GHC.BreakpointId -> Int -> m()
setupBreakpoint loc count = do
    hsc_env <- GHC.getSession
    GHC.setupBreakpoint hsc_env loc count

backCmd :: GhciMonad m => String -> m ()
backCmd arg
  | null arg        = back 1
  | all isDigit arg = back (read arg)
  | otherwise       = liftIO $ putStrLn "Syntax:  :back [num]"
  where
  back num = withSandboxOnly ":back" $ do
      (names, _, pan, _) <- GHC.back num
      printForUser $ text "Logged breakpoint at" <+> ppr pan
      printTypeOfNames names
       -- run the command set with ":set stop <cmd>"
      st <- getGHCiState
      enqueueCommands [stop st]

forwardCmd :: GhciMonad m => String -> m ()
forwardCmd arg
  | null arg        = forward 1
  | all isDigit arg = forward (read arg)
  | otherwise       = liftIO $ putStrLn "Syntax:  :forward [num]"
  where
  forward num = withSandboxOnly ":forward" $ do
      (names, ix, pan, _) <- GHC.forward num
      printForUser $ (if (ix == 0)
                        then text "Stopped at"
                        else text "Logged breakpoint at") <+> ppr pan
      printTypeOfNames names
       -- run the command set with ":set stop <cmd>"
      st <- getGHCiState
      enqueueCommands [stop st]

-- handle the "break" command
breakCmd :: GhciMonad m => String -> m ()
breakCmd argLine = withSandboxOnly ":break" $ breakSwitch $ words argLine

breakSwitch :: GhciMonad m => [String] -> m ()
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
        breakById arg1

breakByModule :: GhciMonad m => Module -> [String] -> m ()
breakByModule md (arg1:rest)
   | all isDigit arg1 = do  -- looks like a line number
        breakByModuleLine md (read arg1) rest
breakByModule _ _
   = breakSyntax

breakByModuleLine :: GhciMonad m => Module -> Int -> [String] -> m ()
breakByModuleLine md line args
   | [] <- args = findBreakAndSet md $ maybeToList . findBreakByLine line
   | [col] <- args, all isDigit col =
        findBreakAndSet md $ maybeToList . findBreakByCoord Nothing (line, read col)
   | otherwise = breakSyntax

-- Set a breakpoint for an identifier
-- See Note [Setting Breakpoints by Id]
breakById :: GhciMonad m => String -> m ()                          -- #3000
breakById inp = do
    let (mod_str, top_level, fun_str) = splitIdent inp
        mod_top_lvl = combineModIdent mod_str top_level
    mb_mod <- catch (lookupModuleInscope mod_top_lvl)
                    (\(_ :: SomeException) -> lookupModuleInGraph mod_str)
      -- If the top-level name is not in scope, `lookupModuleInscope` will
      -- throw an exception, then lookup the module name in the module graph.
    mb_err_msg <- validateBP mod_str fun_str mb_mod
    case mb_err_msg of
        Just err_msg -> printForUser $
          text "Cannot set breakpoint on" <+> quotes (text inp)
          <> text ":" <+> err_msg
        Nothing -> do
          -- No errors found, go and set the breakpoint
          mb_mod_info  <- GHC.getModuleInfo $ fromJust mb_mod
          let modBreaks = case mb_mod_info of
                (Just mod_info) -> GHC.modInfoModBreaks mod_info
                Nothing         -> emptyModBreaks
          findBreakAndSet (fromJust mb_mod) $ findBreakForBind fun_str modBreaks
  where
    -- Try to lookup the module for an identifier that is in scope.
    -- `parseName` throws an exception, if the identifier is not in scope
    lookupModuleInscope :: GhciMonad m => String -> m (Maybe Module)
    lookupModuleInscope mod_top_lvl = do
        names <- GHC.parseName mod_top_lvl
        pure $ Just $ NE.head $ GHC.nameModule <$> names

    -- Lookup the Module of a module name in the module graph
    lookupModuleInGraph :: GhciMonad m => String -> m (Maybe Module)
    lookupModuleInGraph mod_str = do
        graph <- GHC.getModuleGraph
        let hmods = ms_mod <$> GHC.mgModSummaries graph
        pure $ find ((== mod_str) . showModule) hmods

    -- Check validity of an identifier to set a breakpoint:
    --  1. The module of the identifier must exist
    --  2. the identifier must be in an interpreted module
    --  3. the ModBreaks array for module `mod` must have an entry
    --     for the function
    validateBP :: GhciMonad m => String -> String -> Maybe Module
                       -> m (Maybe SDoc)
    validateBP mod_str fun_str Nothing = pure $ Just $ quotes (text
        (combineModIdent mod_str (Prelude.takeWhile (/= '.') fun_str)))
        <+> text "not in scope"
    validateBP _ "" (Just _) = pure $ Just $ text "Function name is missing"
    validateBP _ fun_str (Just modl) = do
        isInterpr <- GHC.moduleIsInterpreted modl
        (_, decls) <- getModBreak modl
        mb_err_msg <- case isInterpr of
          False -> pure $ Just $ text "Module" <+> quotes (ppr modl)
                        <+> text "is not interpreted"
          True -> case fun_str `elem` (declPath <$> elems decls) of
                False -> pure $ Just $
                   text "No breakpoint found for" <+> quotes (text fun_str)
                   <+> "in module" <+> quotes (ppr modl)
                True  -> pure Nothing
        pure mb_err_msg

breakSyntax :: a
breakSyntax = throwGhcException $ CmdLineError ("Syntax: :break [<mod>.]<func>[.<func>]\n"
                                             ++ "        :break [<mod>] <line> [<column>]")

findBreakAndSet :: GhciMonad m
                => Module -> (TickArray -> [(Int, RealSrcSpan)]) -> m ()
findBreakAndSet md lookupTickTree = do
   tickArray <- getTickArray md
   case lookupTickTree tickArray of
      []  -> liftIO $ putStrLn $ "No breakpoints found at that location."
      some -> mapM_ breakAt some
 where
   breakAt (tick, pan) = do
         setBreakFlag md tick True
         (alreadySet, nm) <-
               recordBreak $ BreakLocation
                       { breakModule = md
                       , breakLoc = RealSrcSpan pan Strict.Nothing
                       , breakTick = tick
                       , onBreakCmd = ""
                       , breakEnabled = True
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
--   (a) this binder only (it maybe a top-level or a nested declaration)
--   (b) that do not have an enclosing breakpoint
findBreakForBind :: String -> GHC.ModBreaks -> TickArray
                 -> [(BreakIndex,RealSrcSpan)]
findBreakForBind str_name modbreaks _ = filter (not . enclosed) ticks
  where
    ticks = [ (index, span)
            | (index, decls) <- assocs (GHC.modBreaks_decls modbreaks),
              str_name == declPath decls,
              RealSrcSpan span _ <- [GHC.modBreaks_locs modbreaks ! index] ]
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
        contains = [ tick | tick@(_,pan) <- ticks, RealSrcSpan pan Strict.Nothing `spans` (line,col),
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

{-
Note [Setting Breakpoints by Id]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To set a breakpoint first check whether a ModBreaks array contains a
breakpoint with the given function name:
In `:break M.foo` `M` may be a module name or a local alias of an import
statement. To lookup a breakpoint in the ModBreaks, the effective module
name is needed. Even if a module called `M` exists, `M` may still be
a local alias. To get the module name, parse the top-level identifier with
`GHC.parseName`. If this succeeds, extract the module name from the
returned value. If it fails, catch the exception and assume `M` is a real
module name.

The names of nested functions are stored in `ModBreaks.modBreaks_decls`.
-}

-----------------------------------------------------------------------------
-- :where

whereCmd :: GHC.GhcMonad m => String -> m ()
whereCmd = noArgs $ do
  mstrs <- getCallStackAtCurrentBreakpoint
  case mstrs of
    Nothing -> return ()
    Just strs -> liftIO $ putStrLn (renderStack strs)

-----------------------------------------------------------------------------
-- :list

listCmd :: GhciMonad m => String -> m ()
listCmd "" = do
   mb_span <- getCurrentBreakSpan
   case mb_span of
      Nothing ->
          printForUser $ text "Not stopped at a breakpoint; nothing to list"
      Just (RealSrcSpan pan _) ->
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
listCmd str = list2 (words str)

list2 :: GhciMonad m => [String] -> m ()
list2 [arg] | all isDigit arg = do
    imports <- GHC.getContext
    case iiModules imports of
        [] -> liftIO $ putStrLn "No module to list"
        (mn : _) -> do
          md <- lookupModuleName mn
          listModuleLine md (read arg)
list2 [arg1,arg2] | looksLikeModuleName arg1, all isDigit arg2 = do
        md <- wantInterpretedModule arg1
        listModuleLine md (read arg2)
list2 [arg] = do
        wantNameFromInterpretedModule noCanDo arg $ \name -> do
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        case loc of
            RealSrcLoc l _ ->
               do tickArray <- assert (isExternalName name) $
                               getTickArray (GHC.nameModule name)
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

listModuleLine :: GHC.GhcMonad m => Module -> Int -> m ()
listModuleLine modl line = do
   graph <- GHC.getModuleGraph
   let this = GHC.mgLookupModule graph modl
   case this of
     Nothing -> panic "listModuleLine"
     Just summ -> do
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
listAround :: MonadIO m => RealSrcSpan -> Bool -> m ()
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

getTickArray :: GhciMonad m => Module -> m TickArray
getTickArray modl = do
   st <- getGHCiState
   let arrmap = tickarrays st
   case lookupModuleEnv arrmap modl of
      Just arr -> return arr
      Nothing  -> do
        (ticks, _) <- getModBreak modl
        let arr = mkTickArray (assocs ticks)
        setGHCiState st{tickarrays = extendModuleEnv arrmap modl arr}
        return arr

discardTickArrays :: GhciMonad m => m ()
discardTickArrays = modifyGHCiState (\st -> st {tickarrays = emptyModuleEnv})

mkTickArray :: [(BreakIndex,SrcSpan)] -> TickArray
mkTickArray ticks
  = accumArray (flip (:)) [] (1, max_line)
        [ (line, (nm,pan)) | (nm,RealSrcSpan pan _) <- ticks, line <- srcSpanLines pan ]
    where
        max_line = foldr max 0 [ GHC.srcSpanEndLine sp | (_, RealSrcSpan sp _) <- ticks ]
        srcSpanLines pan = [ GHC.srcSpanStartLine pan ..  GHC.srcSpanEndLine pan ]

-- don't reset the counter back to zero?
discardActiveBreakPoints :: GhciMonad m => m ()
discardActiveBreakPoints = do
   st <- getGHCiState
   mapM_ (turnBreakOnOff False) $ breaks st
   setGHCiState $ st { breaks = IntMap.empty }

discardInterfaceCache :: GhciMonad m => m ()
discardInterfaceCache =
   void (liftIO . iface_clearCache . ifaceCache =<< getGHCiState)

clearHPTs :: GhciMonad m => m ()
clearHPTs = do
  let pruneHomeUnitEnv hme = hme { homeUnitEnv_hpt = emptyHomePackageTable }
      discardMG hsc = hsc { hsc_mod_graph = GHC.emptyMG }
  modifySession (discardMG . discardIC . hscUpdateHUG (unitEnv_map pruneHomeUnitEnv))


-- The unused package warning doesn't make sense once the targets get out of
-- sync with the package flags. See #21110
-- Therefore if it's turned on, the warnings are issued until the module context
-- changes (via :load or :cd), at which stage the package flags are not going to change
-- but the loaded modules will probably not use all the specified packages so the
-- warning becomes spurious. At that point the warning is silently disabled.
disableUnusedPackages :: GhciMonad m => m ()
disableUnusedPackages = newDynFlags False ["-Wno-unused-packages"]

deleteBreak :: GhciMonad m => Int -> m ()
deleteBreak identity = do
   st <- getGHCiState
   let oldLocations = breaks st
   case IntMap.lookup identity oldLocations of
       Nothing -> printForUser (text "Breakpoint" <+> ppr identity <+>
                                text "does not exist")
       Just loc -> do
           _ <- (turnBreakOnOff False) loc
           let rest = IntMap.delete identity oldLocations
           setGHCiState $ st { breaks = rest }

turnBreakOnOff :: GhciMonad m => Bool -> BreakLocation -> m BreakLocation
turnBreakOnOff onOff loc
  | onOff == breakEnabled loc = return loc
  | otherwise = do
      setBreakFlag (breakModule loc) (breakTick loc)  onOff
      return loc { breakEnabled = onOff }

getModBreak :: GHC.GhcMonad m
            => Module -> m (Array Int SrcSpan, Array Int [String])
getModBreak m = do
   mod_info      <- fromMaybe (panic "getModBreak") <$> GHC.getModuleInfo m
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let ticks      = GHC.modBreaks_locs  modBreaks
   let decls      = GHC.modBreaks_decls modBreaks
   return (ticks, decls)

setBreakFlag :: GhciMonad m => Module -> Int -> Bool ->m ()
setBreakFlag  md ix enaDisa = do
  let enaDisaToCount True = breakOn
      enaDisaToCount False = breakOff
  setupBreakpoint (GHC.BreakpointId md ix) $ enaDisaToCount enaDisa

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
-- handler around the flushing operation, so if stderr is closed
-- GHCi will just die gracefully rather than going into an infinite loop.
handler :: GhciMonad m => SomeException -> m Bool
handler exception = do
  flushInterpBuffers
  withSignalHandlers $
     ghciHandle handler (showException exception >> return False)

showException :: MonadIO m => SomeException -> m ()
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

failIfExprEvalMode :: GhciMonad m => m ()
failIfExprEvalMode = do
  s <- getGHCiState
  when (ghc_e s) $
    liftIO (exitWith (ExitFailure 1))

-- | When in expression evaluation mode (ghc -e), we want to exit immediately.
-- Otherwis, just print out the message.
printErrAndMaybeExit :: (GhciMonad m, MonadIO m, HasLogger m) => SourceError -> m ()
printErrAndMaybeExit = (>> failIfExprEvalMode) . printGhciException

-----------------------------------------------------------------------------
-- recursive exception handlers

-- Don't forget to unblock async exceptions in the handler, or if we're
-- in an exception loop (eg. let a = error a in a) the ^C exception
-- may never be delivered.  Thanks to Marcin for pointing out the bug.

ghciHandle :: (HasLogger m, ExceptionMonad m) => (SomeException -> m a) -> m a -> m a
ghciHandle h m = mask $ \restore -> do
                 -- Force dflags to avoid leaking the associated HscEnv
                 !log <- getLogger
                 catch (restore (GHC.prettyPrintGhcErrors log m)) $ \e -> restore (h e)

ghciTry :: ExceptionMonad m => m a -> m (Either SomeException a)
ghciTry m = fmap Right m `catch` \e -> return $ Left e

tryBool :: ExceptionMonad m => m a -> m Bool
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
lookupModuleName mName = GHC.lookupQualifiedModule NoPkgQual mName

isMainUnitModule :: Module -> Bool
isMainUnitModule m = GHC.moduleUnit m == mainUnit

showModule :: Module -> String
showModule = moduleNameString . moduleName

-- Return a String with the declPath of the function of a breakpoint.
-- See Note [Field modBreaks_decls] in GHC.ByteCode.Types
declPath :: [String] -> String
declPath = intercalate "."

-- | Optionally show a fixity declaration like @infixr 4 #@
--
-- We always display the fixity of terms with symbolic names (like <$>).
-- For other terms we only display the fixity if it has been set to a
-- value other than the default infixl 9.
--
-- We have no way of distinguishing between a fixity that has been
-- manually set to infixl 9 and a fixity that has assumed infixl 9 as
-- the default, so we choose to not display the fixity in both cases
-- (for terms with non-symbolic names).
--
-- See #19200.
showFixity :: TyThing -> Fixity -> SDoc
showFixity thing fixity
    | fixity /= GHC.defaultFixity || isSymOcc (getOccName thing)
        = ppr fixity <+> pprInfixName (GHC.getName thing)
    | otherwise = empty

-- TODO: won't work if home dir is encoded.
-- (changeDirectory may not work either in that case.)
expandPath :: MonadIO m => String -> m String
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
   home_unit <- hsc_home_unit <$> GHC.getSession
   unless (isHomeModule home_unit modl) $
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
  handleSourceError printGhciException $ do
    n NE.:| _ <- GHC.parseName str
    let modl = assert (isExternalName n) $ GHC.nameModule n
    if not (GHC.isExternalName n)
       then noCanDo n $ ppr n <>
                        text " is not defined in an interpreted module"
       else do
    is_interpreted <- GHC.moduleIsInterpreted modl
    if not is_interpreted
       then noCanDo n $ text "module " <> ppr modl <>
                        text " is not interpreted"
       else and_then n

clearCaches :: GhciMonad m => m ()
clearCaches = discardActiveBreakPoints
              >> discardInterfaceCache
              >> disableUnusedPackages
              >> clearHPTs



-- Split up a string with an eventually qualified declaration name into 3 components
--   1. module name
--   2. top-level decl
--   3. full-name of the eventually nested decl, but without module qualification
-- eg  "foo"           = ("", "foo", "foo")
--     "A.B.C.foo"     = ("A.B.C", "foo", "foo")
--     "M.N.foo.bar"   = ("M.N", "foo", "foo.bar")
splitIdent :: String -> (String, String, String)
splitIdent [] = ("", "", "")
splitIdent inp@(a : _)
    | (isUpper a) = case fixs of
        []            -> (inp, "", "")
        (i1 : [] )    -> (upto i1, from i1, from i1)
        (i1 : i2 : _) -> (upto i1, take (i2 - i1 - 1) (from i1), from i1)
    | otherwise = case ixs of
        []            -> ("", inp, inp)
        (i1 : _)      -> ("", upto i1, inp)
  where
    ixs = elemIndices '.' inp        -- indices of '.' in whole input
    fixs = dropWhile isNextUc ixs    -- indices of '.' in function names              --
    isNextUc ix = isUpper $ safeInp !! (ix+1)
    safeInp = inp ++ " "
    upto i = take i inp
    from i = drop (i + 1) inp

-- Qualify an identifier name with a module name
-- combineModIdent "A" "foo"  =  "A.foo"
-- combineModIdent ""  "foo"  =  "foo"
combineModIdent :: String -> String -> String
combineModIdent mod ident
          | null mod   = ident
          | null ident = mod
          | otherwise  = mod ++ "." ++ ident
