{-# OPTIONS -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

-----------------------------------------------------------------------------
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2005-2006
--
-----------------------------------------------------------------------------

module InteractiveUI (
        interactiveUI,
        GhciSettings(..),
        defaultGhciSettings,
        ghciCommands,
        ghciWelcomeMsg
    ) where

#include "HsVersions.h"

-- GHCi
import qualified GhciMonad ( args, runStmt )
import GhciMonad hiding ( args, runStmt )
import GhciTags
import Debugger

-- The GHC interface
import DynFlags
import GhcMonad ( modifySession )
import qualified GHC
import GHC ( LoadHowMuch(..), Target(..),  TargetId(..), InteractiveImport(..),
             TyThing(..), Phase, BreakIndex, Resume, SingleStep, Ghc,
             handleSourceError )
import HsImpExp
import HscTypes ( tyThingParent_maybe, handleFlagWarnings, getSafeMode, hsc_IC, 
                  setInteractivePrintName )
import Module
import Name
import Packages ( trusted, getPackageDetails, exposed, exposedModules, pkgIdMap )
import PprTyThing
import RdrName ( getGRE_NameQualifier_maybes )
import SrcLoc
import qualified Lexer

import StringBuffer
import UniqFM ( eltsUFM )
import Outputable hiding ( printForUser, printForUserPartWay, bold )

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

-- Haskell Libraries
import System.Console.Haskeline as Haskeline

import Control.Applicative hiding (empty)
import Control.Monad as Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Function
import Data.IORef ( IORef, readIORef, writeIORef )
import Data.List ( find, group, intercalate, intersperse, isPrefixOf, nub,
                   partition, sort, sortBy )
import Data.Maybe

import Exception hiding (catch)

import Foreign.C
import Foreign.Safe

import System.Cmd
import System.Directory
import System.Environment
import System.Exit ( exitWith, ExitCode(..) )
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.Unsafe ( unsafePerformIO )
import Text.Printf

#ifndef mingw32_HOST_OS
import System.Posix hiding ( getEnv )
#else
import qualified System.Win32
#endif

import GHC.Exts ( unsafeCoerce# )
import GHC.IO.Exception ( IOErrorType(InvalidArgument) )
import GHC.IO.Handle ( hFlushAll )
import GHC.TopHandler ( topHandler )


-----------------------------------------------------------------------------

data GhciSettings = GhciSettings {
        availableCommands :: [Command],
        shortHelpText     :: String,
        fullHelpText      :: String,
        defPrompt         :: String
    }

defaultGhciSettings :: GhciSettings
defaultGhciSettings =
    GhciSettings {
        availableCommands = ghciCommands,
        shortHelpText     = defShortHelpText,
        fullHelpText      = defFullHelpText,
        defPrompt         = default_prompt
    }

ghciWelcomeMsg :: String
ghciWelcomeMsg = "GHCi, version " ++ cProjectVersion ++
                 ": http://www.haskell.org/ghc/  :? for help"

cmdName :: Command -> String
cmdName (n,_,_) = n

GLOBAL_VAR(macros_ref, [], [Command])

ghciCommands :: [Command]
ghciCommands = [
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
  ("list",      keepGoing' listCmd,             noCompletion),
  ("module",    keepGoing moduleCmd,            completeSetModule),
  ("main",      keepGoing runMain,              completeFilename),
  ("print",     keepGoing printCmd,             completeExpression),
  ("quit",      quit,                           noCompletion),
  ("reload",    keepGoing' reloadModule,        noCompletion),
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
  ("unset",     keepGoing unsetOptions,         completeSetOptions)
  ]


-- We initialize readline (in the interactiveUI function) to use
-- word_break_chars as the default set of completion word break characters.
-- This can be overridden for a particular command (for example, filename
-- expansion shouldn't consider '/' to be a word break) by setting the third
-- entry in the Command tuple above.
--
-- NOTE: in order for us to override the default correctly, any custom entry
-- must be a SUBSET of word_break_chars.
word_break_chars :: String
word_break_chars = let symbols = "!#$%&*+/<=>?@\\^|-~"
                       specials = "(),;[]`{}"
                       spaces = " \t\n"
                   in spaces ++ specials ++ symbols

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
  "   :ctags[!] [<file>]          create tags file for Vi (default: \"tags\")\n" ++
  "                               (!: use regex instead of line number)\n" ++
  "   :def <cmd> <expr>           define command :<cmd> (later defined command has\n" ++
  "                               precedence, ::<cmd> is always a builtin command)\n" ++
  "   :edit <file>                edit file\n" ++
  "   :edit                       edit last module\n" ++
  "   :etags [<file>]             create tags file for Emacs (default: \"TAGS\")\n" ++
  "   :help, :?                   display this list of commands\n" ++
  "   :info[!] [<name> ...]       display information about the given names\n" ++
  "                               (!: do not filter instances)\n" ++
  "   :issafe [<mod>]             display safe haskell information of module <mod>\n" ++
  "   :kind <type>                show the kind of <type>\n" ++
  "   :load [*]<module> ...       load module(s) and their dependents\n" ++
  "   :main [<arguments> ...]     run the main function with the given arguments\n" ++
  "   :module [+/-] [*]<mod> ...  set the context for expression evaluation\n" ++
  "   :quit                       exit GHCi\n" ++
  "   :reload                     reload the current module set\n" ++
  "   :run function [<arguments> ...] run the function with the given arguments\n" ++
  "   :script <filename>          run the script <filename>\n" ++
  "   :type <expr>                show the type of <expr>\n" ++
  "   :undef <cmd>                undefine user-defined command :<cmd>\n" ++
  "   :!<command>                 run the shell command <command>\n" ++
  "\n" ++
  " -- Commands for debugging:\n" ++
  "\n" ++
  "   :abandon                    at a breakpoint, abandon current computation\n" ++
  "   :back                       go back in the history (after :trace)\n" ++
  "   :break [<mod>] <l> [<col>]  set a breakpoint at the specified location\n" ++
  "   :break <name>               set a breakpoint on the specified function\n" ++
  "   :continue                   resume after a breakpoint\n" ++
  "   :delete <number>            delete the specified breakpoint\n" ++
  "   :delete *                   delete all breakpoints\n" ++
  "   :force <expr>               print <expr>, forcing unevaluated parts\n" ++
  "   :forward                    go forward in the history (after :back)\n" ++
  "   :history [<n>]              after :trace, show the execution history\n" ++
  "   :list                       show the source code around current breakpoint\n" ++
  "   :list identifier            show the source code for <identifier>\n" ++
  "   :list [<module>] <line>     show the source code around line number <line>\n" ++
  "   :print [<name> ...]         prints a value without forcing its computation\n" ++
  "   :sprint [<name> ...]        simplifed version of :print\n" ++
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
  "    -<flags>      most GHC command line flags can also be set here\n" ++
  "                         (eg. -v2, -fglasgow-exts, etc.)\n" ++
  "                    for GHCi-specific flags, see User's Guide,\n"++
  "                    Flag reference, Interactive-mode options\n" ++
  "\n" ++
  " -- Commands for displaying information:\n" ++
  "\n" ++
  "   :show bindings              show the current bindings made at the prompt\n" ++
  "   :show breaks                show the active breakpoints\n" ++
  "   :show context               show the breakpoint context\n" ++
  "   :show imports               show the current imports\n" ++
  "   :show modules               show the currently loaded modules\n" ++
  "   :show packages              show the currently active package flags\n" ++
  "   :show language              show the currently active language flags\n" ++
  "   :show <setting>             show value of <setting>, which is one of\n" ++
  "                                  [args, prog, prompt, editor, stop]\n" ++
  "   :showi language             show language flags for interactive evaluation\n" ++
  "\n"

findEditor :: IO String
findEditor = do
  getEnv "EDITOR"
    `catchIO` \_ -> do
#if mingw32_HOST_OS
        win <- System.Win32.getWindowsDirectory
        return (win </> "notepad.exe")
#else
        return ""
#endif

foreign import ccall unsafe "rts_isProfiled" isProfiled :: IO CInt

default_progname, default_prompt, default_stop :: String
default_progname = "<interactive>"
default_prompt = "%s> "
default_stop = ""

default_args :: [String]
default_args = []

interactiveUI :: GhciSettings -> [(FilePath, Maybe Phase)] -> Maybe [String]
              -> Ghc ()
interactiveUI config srcs maybe_exprs = do
   -- although GHCi compiles with -prof, it is not usable: the byte-code
   -- compiler and interpreter don't work with profiling.  So we check for
   -- this up front and emit a helpful error message (#2197)
   i <- liftIO $ isProfiled
   when (i /= 0) $
     throwGhcException (InstallationError "GHCi cannot be used when compiled with -prof")

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
   initInterpBuffering

   -- The initial set of DynFlags used for interactive evaluation is the same
   -- as the global DynFlags, plus -XExtendedDefaultRules and
   -- -XNoMonomorphismRestriction.
   dflags <- getDynFlags
   let dflags' = (`xopt_set` Opt_ExtendedDefaultRules)
               . (`xopt_unset` Opt_MonomorphismRestriction)
               $ dflags
   GHC.setInteractiveDynFlags dflags'

   liftIO $ when (isNothing maybe_exprs) $ do
        -- Only for GHCi (not runghc and ghc -e):

        -- Turn buffering off for the compiled program's stdout/stderr
        turnOffBuffering
        -- Turn buffering off for GHCi's stdout
        hFlush stdout
        hSetBuffering stdout NoBuffering
        -- We don't want the cmd line to buffer any input that might be
        -- intended for the program, so unbuffer stdin.
        hSetBuffering stdin NoBuffering
#if defined(mingw32_HOST_OS)
        -- On Unix, stdin will use the locale encoding.  The IO library
        -- doesn't do this on Windows (yet), so for now we use UTF-8,
        -- for consistency with GHC 6.10 and to make the tests work.
        hSetEncoding stdin utf8
#endif

   default_editor <- liftIO $ findEditor

   startGHCi (runGHCi srcs maybe_exprs)
        GHCiState{ progname       = default_progname,
                   GhciMonad.args = default_args,
                   prompt         = defPrompt config,
                   def_prompt     = defPrompt config,
                   stop           = default_stop,
                   editor         = default_editor,
                   options        = [],
                   line_number    = 1,
                   break_ctr      = 0,
                   breaks         = [],
                   tickarrays     = emptyModuleEnv,
                   ghci_commands  = availableCommands config,
                   last_command   = Nothing,
                   cmdqueue       = [],
                   remembered_ctx = [],
                   transient_ctx  = [],
                   ghc_e          = isJust maybe_exprs,
                   short_help     = shortHelpText config,
                   long_help      = fullHelpText config
                 }

   return ()

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
   read_dot_files = not (gopt Opt_IgnoreDotGhci dflags)

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
       dir_ok  <- liftIO $ checkPerms (getDirectory file)
       file_ok <- liftIO $ checkPerms file
       when (dir_ok && file_ok) $ do
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
     where
      getDirectory f = case takeDirectory f of "" -> "."; d -> d
  --

  setGHCContextFromGHCiState

  when (read_dot_files) $ do
    mcfgs0 <- sequence $ [ current_dir, app_user_dir, home_dir ] ++ map (return . Just ) (ghciScripts dflags)
    mcfgs <- liftIO $ mapM canonicalizePath' (catMaybes mcfgs0)
    mapM_ sourceConfigFile $ nub $ catMaybes mcfgs
        -- nub, because we don't want to read .ghci twice if the
        -- CWD is $HOME.

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
  getGHCiState >>= \st -> setGHCiState st{line_number=1}

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
                runCommands' hdle (return Nothing)

  -- and finally, exit
  liftIO $ when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."

runGHCiInput :: InputT GHCi a -> GHCi a
runGHCiInput f = do
    dflags <- getDynFlags
    histFile <- if gopt Opt_GhciHistory dflags
                then liftIO $ withGhcAppData (\dir -> return (Just (dir </> "ghci_history")))
                                             (return Nothing)
                else return Nothing
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
-- and aren't world writable.  Otherwise, we could be accidentally
-- running code planted by a malicious third party.

-- Furthermore, We only read ./.ghci if . is owned by the current user
-- and isn't writable by anyone else.  I think this is sufficient: we
-- don't need to check .. and ../.. etc. because "."  always refers to
-- the same directory while a process is running.

checkPerms :: String -> IO Bool
#ifdef mingw32_HOST_OS
checkPerms _ = return True
#else
checkPerms name =
  handleIO (\_ -> return False) $ do
    st <- getFileStatus name
    me <- getRealUserID
    if fileOwner st /= me then do
        putStrLn $ "WARNING: " ++ name ++ " is owned by someone else, IGNORING!"
        return False
     else do
        let mode = System.Posix.fileMode st
        if (groupWriteMode == (mode `intersectFileModes` groupWriteMode))
            || (otherWriteMode == (mode `intersectFileModes` otherWriteMode))
            then do
                putStrLn $ "*** WARNING: " ++ name ++
                           " is writable by someone else, IGNORING!"
                return False
            else return True
#endif

incrementLineNo :: InputT GHCi ()
incrementLineNo = do
   st <- lift $ getGHCiState
   let ln = 1+(line_number st)
   lift $ setGHCiState st{line_number=ln}

fileLoop :: Handle -> InputT GHCi (Maybe String)
fileLoop hdl = do
   l <- liftIO $ tryIO $ hGetLine hdl
   case l of
        Left e | isEOFError e              -> return Nothing
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

mkPrompt :: GHCi String
mkPrompt = do
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
        modules_bit =
             hsep [ char '*' <> ppr m | IIModule m <- rev_imports ] <+>
             hsep (map ppr [ myIdeclName d | IIDecl d <- rev_imports ])

         --  use the 'as' name if there is one
        myIdeclName d | Just m <- ideclAs d = m
                      | otherwise           = unLoc (ideclName d)

        deflt_prompt = dots <> context_bit <> modules_bit

        f ('%':'s':xs) = deflt_prompt <> f xs
        f ('%':'%':xs) = char '%' <> f xs
        f (x:xs) = char x <> f xs
        f [] = empty

  st <- getGHCiState
  dflags <- getDynFlags
  return (showSDoc dflags (f (prompt st)))


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
runCommands = runCommands' handler

runCommands' :: (SomeException -> GHCi Bool) -- ^ Exception handler
             -> InputT GHCi (Maybe String) -> InputT GHCi ()
runCommands' eh gCmd = do
    b <- ghandle (\e -> case fromException e of
                          Just UserInterrupt -> return $ Just False
                          _ -> case fromException e of
                                 Just ghce ->
                                   do liftIO (print (ghce :: GhcException))
                                      return Nothing
                                 _other ->
                                   liftIO (Exception.throwIO e))
            (runOneCommand eh gCmd)
    case b of
      Nothing -> return ()
      Just _  -> runCommands' eh gCmd

-- | Evaluate a single line of user input (either :<command> or Haskell code)
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
             handleSourceError printErrorAndKeepGoing
               (doCommand c)
               -- source error's are handled by runStmt
               -- is the handler necessary here?
  where
    printErrorAndKeepGoing err = do
        GHC.printException err
        return $ Just True

    noSpace q = q >>= maybe (return Nothing)
                            (\c -> case removeSpaces c of
                                     ""   -> noSpace q
                                     ":{" -> multiLineCmd q
                                     c'   -> return (Just c') )
    multiLineCmd q = do
      st <- lift getGHCiState
      let p = prompt st
      lift $ setGHCiState st{ prompt = "%s| " }
      mb_cmd <- collectCommand q ""
      lift $ getGHCiState >>= \st' -> setGHCiState st'{ prompt = p }
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
                 then return (Just $ removeSpaces c)
                 else collectCommand q (c ++ "\n" ++ map normSpace l))
      where normSpace '\r' = ' '
            normSpace   x  = x
    -- SDM (2007-11-07): is userError the one to use here?
    collectError = userError "unterminated multiline command :{ .. :}"

    -- | Handle a line of input
    doCommand :: String -> InputT GHCi (Maybe Bool)

    -- command
    doCommand (':' : cmd) = do
      result <- specialCommand cmd
      case result of
        True -> return Nothing
        _    -> return $ Just True

    -- haskell
    doCommand stmt = do
      ml <- lift $ isOptionSet Multiline
      if ml
        then do
          mb_stmt <- checkInputForLayout stmt gCmd
          case mb_stmt of
            Nothing      -> return $ Just True
            Just ml_stmt -> do
              result <- timeIt $ lift $ runStmt ml_stmt GHC.RunToCompletion
              return $ Just result
        else do
          result <- timeIt $ lift $ runStmt stmt GHC.RunToCompletion
          return $ Just result

-- #4316
-- lex the input.  If there is an unclosed layout context, request input
checkInputForLayout :: String -> InputT GHCi (Maybe String)
                    -> InputT GHCi (Maybe String)
checkInputForLayout stmt getStmt = do
   dflags' <- lift $ getDynFlags
   let dflags = xopt_set dflags' Opt_AlternativeLayoutRule
   st0 <- lift $ getGHCiState
   let buf'   =  stringToStringBuffer stmt
       loc    = mkRealSrcLoc (fsLit (progname st0)) (line_number st0) 1
       pstate = Lexer.mkPState dflags buf' loc
   case Lexer.unP goToEnd pstate of
     (Lexer.POk _ False) -> return $ Just stmt
     _other              -> do
       st1 <- lift getGHCiState
       let p = prompt st1
       lift $ setGHCiState st1{ prompt = "%s| " }
       mb_stmt <- ghciHandle (\ex -> case fromException ex of
                            Just UserInterrupt -> return Nothing
                            _ -> case fromException ex of
                                 Just ghce ->
                                   do liftIO (print (ghce :: GhcException))
                                      return Nothing
                                 _other -> liftIO (Exception.throwIO ex))
                     getStmt
       lift $ getGHCiState >>= \st' -> setGHCiState st'{ prompt = p }
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
               else Lexer.lexer return >> goToEnd

enqueueCommands :: [String] -> GHCi ()
enqueueCommands cmds = do
  st <- getGHCiState
  setGHCiState st{ cmdqueue = cmds ++ cmdqueue st }

-- | If we one of these strings prefixes a command, then we treat it as a decl
-- rather than a stmt.
declPrefixes :: [String]
declPrefixes = ["class ","data ","newtype ","type ","instance ", "deriving ",
                "foreign ", "default ", "default("]

-- | Entry point to execute some haskell code from user
runStmt :: String -> SingleStep -> GHCi Bool
runStmt stmt step
 -- empty
 | null (filter (not.isSpace) stmt)
 = return False

 -- import
 | "import " `isPrefixOf` stmt
 = do addImportToContext stmt; return False

 -- data, class, newtype...
 | any (flip isPrefixOf stmt) declPrefixes
 = do _ <- liftIO $ tryIO $ hFlushAll stdin
      result <- GhciMonad.runDecls stmt
      afterRunStmt (const True) (GHC.RunOk result)

 | otherwise
 = do -- In the new IO library, read handles buffer data even if the Handle
      -- is set to NoBuffering.  This causes problems for GHCi where there
      -- are really two stdin Handles.  So we flush any bufferred data in
      -- GHCi's stdin Handle here (only relevant if stdin is attached to
      -- a file, otherwise the read buffer can't be flushed).
      _ <- liftIO $ tryIO $ hFlushAll stdin
      m_result <- GhciMonad.runStmt stmt step
      case m_result of
        Nothing     -> return False
        Just result -> afterRunStmt (const True) result

-- | Clean up the GHCi environment after a statement has run
afterRunStmt :: (SrcSpan -> Bool) -> GHC.RunResult -> GHCi Bool
afterRunStmt _ (GHC.RunException e) = throw e
afterRunStmt step_here run_result = do
  resumes <- GHC.getResumeContext
  case run_result of
     GHC.RunOk names -> do
        show_types <- isOptionSet ShowType
        when show_types $ printTypeOfNames names
     GHC.RunBreak _ names mb_info
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
     _ -> return ()

  flushInterpBuffers
  liftIO installSignalHandlers
  b <- isOptionSet RevertCAFs
  when b revertCAFs

  return (case run_result of GHC.RunOk _ -> True; _ -> False)

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
  printForUser $ ptext (sLit "Stopped at") <+>
    ppr (GHC.resumeSpan res)
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
  htxt <- lift $ short_help `fmap` getGHCiState
  case maybe_cmd of
    GotCommand (_,f,_) -> f (dropWhile isSpace rest)
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
  st <- getGHCiState
  setGHCiState st{ last_command = mc }
  return $ case mc of
           Just c -> GotCommand c
           Nothing -> BadCommand

lookupCommand' :: String -> GHCi (Maybe Command)
lookupCommand' ":" = return Nothing
lookupCommand' str' = do
  macros    <- liftIO $ readIORef macros_ref
  ghci_cmds <- ghci_commands `fmap` getGHCiState
  let{ (str, cmds) = case str' of
      ':' : rest -> (rest, ghci_cmds) -- "::" selects a builtin command
      _ -> (str', ghci_cmds ++ macros) } -- otherwise prefer macros
  -- look for exact match first, then the first prefix match
  return $ case [ c | c <- cmds, str == cmdName c ] of
           c:_ -> Just c
           [] -> case [ c | c@(s,_,_) <- cmds, str `isPrefixOf` s ] of
                 [] -> Nothing
                 c:_ -> Just c

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
    dflags    <- getDynFlags
    let pefas = gopt Opt_PrintExplicitForalls dflags
    names     <- GHC.parseName str
    mb_stuffs <- mapM (GHC.getInfo allInfo) names
    let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
    return $ vcat (intersperse (text "") $ map (pprInfo pefas) filtered)

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

pprInfo :: PrintExplicitForalls -> (TyThing, Fixity, [GHC.ClsInst]) -> SDoc
pprInfo pefas (thing, fixity, insts)
  =  pprTyThingInContextLoc pefas thing
  $$ show_fixity
  $$ vcat (map GHC.pprInstance insts)
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
                   case mainFunIs dflags of
                       Nothing -> doWithArgs args "main"
                       Just f  -> doWithArgs args f

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

trySuccess :: GHC.GhcMonad m => m SuccessFlag -> m SuccessFlag
trySuccess act =
    handleSourceError (\e -> do GHC.printException e
                                return Failed) $ do
      act

-----------------------------------------------------------------------------
-- :edit

editFile :: String -> InputT GHCi ()
editFile str =
  do file <- if null str then lift chooseEditFile else return str
     st <- lift getGHCiState
     let cmd = editor st
     when (null cmd)
       $ throwGhcException (CmdLineError "editor not set, use :set editor")
     code <- liftIO $ system (cmd ++ ' ':file)
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
  macros <- liftIO (readIORef macros_ref)
  let defined = map cmdName macros
  if (null macro_name)
        then if null defined
                then liftIO $ putStrLn "no macros defined"
                else liftIO $ putStr ("the following macros are defined:\n" ++
                                      unlines defined)
        else do
  if (not overwrite && macro_name `elem` defined)
        then throwGhcException (CmdLineError
                ("macro '" ++ macro_name ++ "' is already defined"))
        else do

  let filtered = [ cmd | cmd <- macros, cmdName cmd /= macro_name ]

  -- give the expression a type signature, so we can be sure we're getting
  -- something of the right type.
  let new_expr = '(' : definition ++ ") :: String -> IO String"

  -- compile the expression
  handleSourceError (\e -> GHC.printException e) $
   do
    hv <- GHC.compileExpr new_expr
    liftIO (writeIORef macros_ref -- later defined macros have precedence
            ((macro_name, lift . runMacro hv, noCompletion) : filtered))

runMacro :: GHC.HValue{-String -> IO String-} -> String -> GHCi Bool
runMacro fun s = do
  str <- liftIO ((unsafeCoerce# fun :: String -> IO String) s)
  -- make sure we force any exceptions in the result, while we are still
  -- inside the exception handler for commands:
  seqList str (return ())
  enqueueCommands (lines str)
  return False


-----------------------------------------------------------------------------
-- :undef

undefineMacro :: String -> GHCi ()
undefineMacro str = mapM_ undef (words str)
 where undef macro_name = do
        cmds <- liftIO (readIORef macros_ref)
        if (macro_name `notElem` map cmdName cmds)
           then throwGhcException (CmdLineError
                ("macro '" ++ macro_name ++ "' is not defined"))
           else do
            liftIO (writeIORef macros_ref (filter ((/= macro_name) . cmdName) cmds))


-----------------------------------------------------------------------------
-- :cmd

cmdCmd :: String -> GHCi ()
cmdCmd str = do
  let expr = '(' : str ++ ") :: IO String"
  handleSourceError (\e -> GHC.printException e) $
   do
    hv <- GHC.compileExpr expr
    cmds <- liftIO $ (unsafeCoerce# hv :: IO String)
    enqueueCommands (lines cmds)
    return ()


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

loadModule :: [(FilePath, Maybe Phase)] -> InputT GHCi SuccessFlag
loadModule fs = timeIt (loadModule' fs)

loadModule_ :: [FilePath] -> InputT GHCi ()
loadModule_ fs = loadModule (zip fs (repeat Nothing)) >> return ()

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
  doLoad False LoadAllTargets


-- :add
addModule :: [FilePath] -> InputT GHCi ()
addModule files = do
  lift revertCAFs -- always revert CAFs on load/add.
  files' <- mapM expandPath files
  targets <- mapM (\m -> GHC.guessTarget m Nothing) files'
  -- remove old targets with the same id; e.g. for :add *M
  mapM_ GHC.removeTarget [ tid | Target tid _ _ <- targets ]
  mapM_ GHC.addTarget targets
  _ <- doLoad False LoadAllTargets
  return ()


-- :reload
reloadModule :: String -> InputT GHCi ()
reloadModule m = do
  _ <- doLoad True $
        if null m then LoadAllTargets
                  else LoadUpTo (GHC.mkModuleName m)
  return ()


doLoad :: Bool -> LoadHowMuch -> InputT GHCi SuccessFlag
doLoad retain_context howmuch = do
  -- turn off breakpoints before we load: we can't turn them off later, because
  -- the ModBreaks will have gone away.
  lift discardActiveBreakPoints
  ok <- trySuccess $ GHC.load howmuch
  afterLoad ok retain_context
  return ok


afterLoad :: SuccessFlag
          -> Bool   -- keep the remembered_ctx, as far as possible (:reload)
          -> InputT GHCi ()
afterLoad ok retain_context = do
  lift revertCAFs  -- always revert CAFs on load.
  lift discardTickArrays
  loaded_mod_summaries <- getLoadedModules
  let loaded_mods = map GHC.ms_mod loaded_mod_summaries
      loaded_mod_names = map GHC.moduleName loaded_mods
  modulesLoadedMsg ok loaded_mod_names
  lift $ setContextAfterLoad retain_context loaded_mod_summaries


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


keepPackageImports :: [InteractiveImport] -> GHCi [InteractiveImport]
keepPackageImports = filterM is_pkg_import
  where
     is_pkg_import :: InteractiveImport -> GHCi Bool
     is_pkg_import (IIModule _) = return False
     is_pkg_import (IIDecl d)
         = do e <- gtry $ GHC.findModule mod_name (ideclPkgQual d)
              case e :: Either SomeException Module of
                Left _  -> return False
                Right m -> return (not (isHomeModule m))
        where
          mod_name = unLoc (ideclName d)


modulesLoadedMsg :: SuccessFlag -> [ModuleName] -> InputT GHCi ()
modulesLoadedMsg ok mods = do
  dflags <- getDynFlags
  when (verbosity dflags > 0) $ do
   let mod_commas
        | null mods = text "none."
        | otherwise = hsep (
            punctuate comma (map ppr mods)) <> text "."
   case ok of
    Failed ->
       liftIO $ putStrLn $ showSDoc dflags (text "Failed, modules loaded: " <> mod_commas)
    Succeeded  ->
       liftIO $ putStrLn $ showSDoc dflags (text "Ok, modules loaded: " <> mod_commas)


-----------------------------------------------------------------------------
-- :type

typeOfExpr :: String -> InputT GHCi ()
typeOfExpr str
  = handleSourceError GHC.printException
  $ do
       ty <- GHC.exprType str
       dflags <- getDynFlags
       let pefas = gopt Opt_PrintExplicitForalls dflags
       printForUser $ sep [text str, nest 2 (dcolon <+> pprTypeForUser pefas ty)]

-----------------------------------------------------------------------------
-- :kind

kindOfType :: Bool -> String -> InputT GHCi ()
kindOfType norm str
  = handleSourceError GHC.printException
  $ do
       (ty, kind) <- GHC.typeKind norm str
       printForUser $ vcat [ text str <+> dcolon <+> ppr kind
                           , ppWhen norm $ equals <+> ppr ty ]


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
  either_script <- liftIO $ tryIO (openFile filename ReadMode)
  case either_script of
    Left _err    -> throwGhcException (CmdLineError $ "IO error:  \""++filename++"\" "
                      ++(ioeGetErrorString _err))
    Right script -> do
      st <- lift $ getGHCiState
      let prog = progname st
          line = line_number st
      lift $ setGHCiState st{progname=filename,line_number=0}
      scriptLoop script
      liftIO $ hClose script
      new_st <- lift $ getGHCiState
      lift $ setGHCiState new_st{progname=prog,line_number=line}
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
    when (not $ null good)
         (liftIO $ putStrLn $ "Trusted package dependencies (trusted): " ++
                        (intercalate ", " $ map packageIdString good))
    case msafe && null bad of
        True -> liftIO $ putStrLn $ mname ++ " is trusted!"
        False -> do
            when (not $ null bad)
                 (liftIO $ putStrLn $ "Trusted package dependencies (untrusted): "
                            ++ (intercalate ", " $ map packageIdString bad))
            liftIO $ putStrLn $ mname ++ " is NOT trusted!"

  where
    mname = GHC.moduleNameString $ GHC.moduleName m

    packageTrusted dflags md
        | thisPackage dflags == modulePackageId md = True
        | otherwise = trusted $ getPackageDetails (pkgState dflags) (modulePackageId md)

    tallyPkgs dflags deps | not (packageTrustOn dflags) = ([], [])
                          | otherwise = partition part deps
        where state = pkgState dflags
              part pkg = trusted $ getPackageDetails state pkg

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
          IIDecl d   -> GHC.findModule (unLoc (ideclName d)) (ideclPkgQual d)

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

        let pefas              = gopt Opt_PrintExplicitForalls dflags
            things | bang      = catMaybes mb_things
                   | otherwise = filtered_things
            pretty | bang      = pprTyThing
                   | otherwise = pprTyThingInContext

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
            prettyThings = map (pretty pefas) things
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
       m <- GHC.lookupModule modname pkgqual
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
  dflags <- GHC.getSessionDynFlags
  GHC.setContext $
     if xopt Opt_ImplicitPrelude dflags && not (any isPreludeImport iidecls)
        then iidecls ++ [implicitPreludeImport]
        else iidecls
    -- XXX put prel at the end, so that guessCurrentModule doesn't pick it up.


-- -----------------------------------------------------------------------------
-- Utils on InteractiveImport

mkIIModule :: ModuleName -> InteractiveImport
mkIIModule = IIModule

mkIIDecl :: ModuleName -> InteractiveImport
mkIIDecl = IIDecl . simpleImportDecl

iiModules :: [InteractiveImport] -> [ModuleName]
iiModules is = [m | IIModule m <- is]

iiModuleName :: InteractiveImport -> ModuleName
iiModuleName (IIModule m) = m
iiModuleName (IIDecl d)   = unLoc (ideclName d)

preludeModuleName :: ModuleName
preludeModuleName = GHC.mkModuleName "Prelude"

implicitPreludeImport :: InteractiveImport
implicitPreludeImport = IIDecl (simpleImportDecl preludeModuleName)

isPreludeImport :: InteractiveImport -> Bool
isPreludeImport (IIModule {}) = True
isPreludeImport (IIDecl d)    = unLoc (ideclName d) == preludeModuleName

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
     _                `hidingSubsumes` Just (False,[]) = True
     Just (False, xs) `hidingSubsumes` Just (False,ys) = all (`elem` xs) ys
     h1               `hidingSubsumes` h2              = h1 == h2
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
    Right ("args",   rest) ->
        case toArgs rest of
            Left err -> liftIO (hPutStrLn stderr err)
            Right args -> setArgs args
    Right ("prog",   rest) ->
        case toArgs rest of
            Right [prog] -> setProg prog
            _ -> liftIO (hPutStrLn stderr "syntax: :set prog <progname>")
    Right ("prompt", rest) -> setPrompt $ Just $ dropWhile isSpace rest
    Right ("editor", rest) -> setEditor $ dropWhile isSpace rest
    Right ("stop",   rest) -> setStop   $ dropWhile isSpace rest
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
         nest 2 (vcat (map (setting gopt) ghciFlags))
  putStrLn $ showSDoc dflags $
     text "other dynamic, non-language, flag settings:" $$
         nest 2 (vcat (map (setting gopt) others))
  putStrLn $ showSDoc dflags $
     text "warning settings:" $$
         nest 2 (vcat (map (setting wopt) DynFlags.fWarningFlags))
  where
        setting test (str, f, _)
          | quiet     = empty
          | is_on     = fstr str
          | otherwise = fnostr str
          where is_on = test f dflags
                quiet = not show_all && test f default_dflags == is_on

        default_dflags = defaultDynFlags (settings dflags)

        fstr   str = text "-f"    <> text str
        fnostr str = text "-fno-" <> text str

        (ghciFlags,others)  = partition (\(_, f, _) -> f `elem` flgs)
                                        DynFlags.fFlags
        flgs = [Opt_PrintExplicitForalls
                ,Opt_PrintBindResult
                ,Opt_BreakOnException
                ,Opt_BreakOnError
                ,Opt_PrintEvldWithShow
                ]

setArgs, setOptions :: [String] -> GHCi ()
setProg, setEditor, setStop :: String -> GHCi ()

setArgs args = do
  st <- getGHCiState
  setGHCiState st{ GhciMonad.args = args }

setProg prog = do
  st <- getGHCiState
  setGHCiState st{ progname = prog }

setEditor cmd = do
  st <- getGHCiState
  setGHCiState st{ editor = cmd }

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
setStop cmd = do
  st <- getGHCiState
  setGHCiState st{ stop = cmd }

setPrompt :: Maybe String -> GHCi ()
setPrompt Nothing = do
    st <- getGHCiState
    setGHCiState ( st { prompt = def_prompt st } )

setPrompt (Just value) = do
  st <- getGHCiState
  if null value
      then liftIO $ hPutStrLn stderr $ "syntax: :set prompt <prompt>, currently \"" ++ prompt st ++ "\""
      else case value of
           '\"' : _ -> case reads value of
                       [(value', xs)] | all isSpace xs ->
                           setGHCiState (st { prompt = value' })
                       _ ->
                           liftIO $ hPutStrLn stderr "Can't parse prompt string. Use Haskell syntax."
           _ -> setGHCiState (st { prompt = value })

setOptions wds =
   do -- first, deal with the GHCi opts (+s, +t, etc.)
      let (plus_opts, minus_opts)  = partitionWith isPlus wds
      mapM_ setOpt plus_opts
      -- then, dynamic flags
      newDynFlags False minus_opts

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

      when (interactive_only &&
              packageFlags idflags1 /= packageFlags idflags0) $ do
          liftIO $ hPutStrLn stderr "cannot set package flags with :seti; use :set"
      GHC.setInteractiveDynFlags idflags1
      installInteractivePrint (interactivePrint idflags1) False

      dflags0 <- getDynFlags
      when (not interactive_only) $ do
        (dflags1, _, _) <- liftIO $ GHC.parseDynamicFlags dflags0 lopts
        new_pkgs <- GHC.setProgramDynFlags dflags1

        -- if the package flags changed, reset the context and link
        -- the new packages.
        dflags2 <- getDynFlags
        when (packageFlags dflags2 /= packageFlags dflags0) $ do
          when (verbosity dflags2 > 0) $
            liftIO . putStrLn $
              "package flags have changed, resetting and loading new packages..."
          GHC.setTargets []
          _ <- GHC.load LoadAllTargets
          liftIO $ linkPackages dflags2 new_pkgs
          -- package flags changed, we can't re-use any of the old context
          setContextAfterLoad False []
          -- and copy the package state to the interactive DynFlags
          idflags <- GHC.getInteractiveDynFlags
          GHC.setInteractiveDynFlags
              idflags{ pkgState = pkgState dflags2
                     , pkgDatabase = pkgDatabase dflags2
                     , packageFlags = packageFlags dflags2 }

      return ()


unsetOptions :: String -> GHCi ()
unsetOptions str
  =   -- first, deal with the GHCi opts (+s, +t, etc.)
     let opts = words str
         (minus_opts, rest1) = partition isMinus opts
         (plus_opts, rest2)  = partitionWith isPlus rest1
         (other_opts, rest3) = partition (`elem` map fst defaulters) rest2

         defaulters =
           [ ("args"  , setArgs default_args)
           , ("prog"  , setProg default_progname)
           , ("prompt", setPrompt Nothing)
           , ("editor", liftIO findEditor >>= setEditor)
           , ("stop"  , setStop default_stop)
           ]

         no_flag ('-':'f':rest) = return ("-fno-" ++ rest)
         no_flag f = throwGhcException (ProgramError ("don't know how to reverse " ++ f))

     in if (not (null rest3))
           then liftIO (putStrLn ("unknown option: '" ++ head rest3 ++ "'"))
           else do
             mapM_ (fromJust.flip lookup defaulters) other_opts

             mapM_ unsetOpt plus_opts

             no_flags <- mapM no_flag minus_opts
             newDynFlags False no_flags

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
strToGHCiOpt _   = Nothing

optToStr :: GHCiOption -> String
optToStr Multiline  = "m"
optToStr ShowTiming = "s"
optToStr ShowType   = "t"
optToStr RevertCAFs = "r"


-- ---------------------------------------------------------------------------
-- :show

showCmd :: String -> GHCi ()
showCmd ""   = showOptions False
showCmd "-a" = showOptions True
showCmd str = do
  st <- getGHCiState
  case words str of
        ["args"]     -> liftIO $ putStrLn (show (GhciMonad.args st))
        ["prog"]     -> liftIO $ putStrLn (show (progname st))
        ["prompt"]   -> liftIO $ putStrLn (show (prompt st))
        ["editor"]   -> liftIO $ putStrLn (show (editor st))
        ["stop"]     -> liftIO $ putStrLn (show (stop st))
        ["imports"]  -> showImports
        ["modules" ] -> showModules
        ["bindings"] -> showBindings
        ["linker"]   ->
            do dflags <- getDynFlags
               liftIO $ showLinkerState dflags
        ["breaks"]   -> showBkptTable
        ["context"]  -> showContext
        ["packages"]  -> showPackages
        ["languages"] -> showLanguages -- backwards compat
        ["language"]  -> showLanguages
        ["lang"]      -> showLanguages -- useful abbreviation
        _ -> throwGhcException (CmdLineError ("syntax:  :show [ args | prog | prompt | editor | stop | modules | bindings\n"++
                                     "               | breaks | context | packages | language ]"))

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

      prel_imp
        | any isPreludeImport (rem_ctx ++ trans_ctx) = []
        | otherwise = ["import Prelude -- implicit"]

      trans_comment s = s ++ " -- added automatically"
  --
  liftIO $ mapM_ putStrLn (prel_imp ++ map show_one rem_ctx
                                    ++ map (trans_comment . show_one) trans_ctx)

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
        dflags    <- getDynFlags
        let pefas = gopt Opt_PrintExplicitForalls dflags
        mb_stuff <- GHC.getInfo False (getName tt)
        return $ maybe (text "") (pprTT pefas) mb_stuff
    pprTT :: PrintExplicitForalls -> (TyThing, Fixity, [GHC.ClsInst]) -> SDoc
    pprTT pefas (thing, fixity, _insts) =
        pprTyThing pefas thing
        $$ show_fixity
      where
        show_fixity
            | fixity == GHC.defaultFixity  = empty
            | otherwise                    = ppr fixity <+> ppr (GHC.getName thing)


printTyThing :: TyThing -> GHCi ()
printTyThing tyth = do dflags <- getDynFlags
                       let pefas = gopt Opt_PrintExplicitForalls dflags
                       printForUser (pprTyThing pefas tyth)

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
        $$ nest 2 (ptext (sLit "Stopped at") <+> ppr (GHC.resumeSpan res))

showPackages :: GHCi ()
showPackages = do
  dflags <- getDynFlags
  let pkg_flags = packageFlags dflags
  liftIO $ putStrLn $ showSDoc dflags $ vcat $
    text ("active package flags:"++if null pkg_flags then " none" else "")
    : map showFlag pkg_flags
  where showFlag (ExposePackage   p) = text $ "  -package " ++ p
        showFlag (HidePackage     p) = text $ "  -hide-package " ++ p
        showFlag (IgnorePackage   p) = text $ "  -ignore-package " ++ p
        showFlag (ExposePackageId p) = text $ "  -package-id " ++ p
        showFlag (TrustPackage    p) = text $ "  -trust " ++ p
        showFlag (DistrustPackage p) = text $ "  -distrust " ++ p

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
   setting test (str, f, _)
          | quiet     = empty
          | is_on     = text "-X" <> text str
          | otherwise = text "-XNo" <> text str
          where is_on = test f dflags
                quiet = not show_all && test f default_dflags == is_on

   default_dflags =
       defaultDynFlags (settings dflags) `lang_set`
         case language dflags of
           Nothing -> Just Haskell2010
           other   -> other

-- -----------------------------------------------------------------------------
-- Completion

completeCmd, completeMacro, completeIdentifier, completeModule,
    completeSetModule, completeSeti, completeShowiOptions,
    completeHomeModule, completeSetOptions, completeShowOptions,
    completeHomeModuleOrFile, completeExpression
    :: CompletionFunc GHCi

ghciCompleteWord :: CompletionFunc GHCi
ghciCompleteWord line@(left,_) = case firstWord of
    ':':cmd     | null rest     -> completeCmd line
                | otherwise     -> do
                        completion <- lookupCompletion cmd
                        completion line
    "import"    -> completeModule line
    _           -> completeExpression line
  where
    (firstWord,rest) = break isSpace $ dropWhile isSpace $ reverse left
    lookupCompletion ('!':_) = return completeFilename
    lookupCompletion c = do
        maybe_cmd <- lookupCommand' c
        case maybe_cmd of
            Just (_,_,f) -> return f
            Nothing -> return completeFilename

completeCmd = wrapCompleter " " $ \w -> do
  macros <- liftIO $ readIORef macros_ref
  cmds   <- ghci_commands `fmap` getGHCiState
  let macro_names = map (':':) . map cmdName $ macros
  let command_names = map (':':) . map cmdName $ cmds
  let{ candidates = case w of
      ':' : ':' : _ -> map (':':) command_names
      _ -> nub $ macro_names ++ command_names }
  return $ filter (w `isPrefixOf`) candidates

completeMacro = wrapIdentCompleter $ \w -> do
  cmds <- liftIO $ readIORef macros_ref
  return (filter (w `isPrefixOf`) (map cmdName cmds))

completeIdentifier = wrapIdentCompleter $ \w -> do
  rdrs <- GHC.getRdrNamesInScope
  dflags <- GHC.getSessionDynFlags
  return (filter (w `isPrefixOf`) (map (showPpr dflags) rdrs))

completeModule = wrapIdentCompleter $ \w -> do
  dflags <- GHC.getSessionDynFlags
  let pkg_mods = allExposedModules dflags
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
      let pkg_mods = allExposedModules dflags
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
    where opts = "args":"prog":"prompt":"editor":"stop":flagList
          flagList = map head $ group $ sort allFlags

completeSeti = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) flagList)
    where flagList = map head $ group $ sort allFlags

completeShowOptions = wrapCompleter flagWordBreakChars $ \w -> do
  return (filter (w `isPrefixOf`) opts)
    where opts = ["args", "prog", "prompt", "editor", "stop",
                     "modules", "bindings", "linker", "breaks",
                     "context", "packages", "language"]

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
    $ fmap (map simpleCompletion) . fmap sort . fun

wrapIdentCompleter :: (String -> GHCi [String]) -> CompletionFunc GHCi
wrapIdentCompleter = wrapCompleter word_break_chars

wrapIdentCompleterWithModifier :: String -> (Maybe Char -> String -> GHCi [String]) -> CompletionFunc GHCi
wrapIdentCompleterWithModifier modifChars fun = completeWordWithPrev Nothing word_break_chars
    $ \rest -> fmap (map simpleCompletion) . fmap sort . fun (getModifier rest)
 where
  getModifier = find (`elem` modifChars)

allExposedModules :: DynFlags -> [ModuleName]
allExposedModules dflags
 = concat (map exposedModules (filter exposed (eltsUFM pkg_db)))
 where
  pkg_db = pkgIdMap (pkgState dflags)

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
           doContinue (`isSubspanOf` current_toplevel_decl) GHC.SingleStep

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
enclosingTickSpan :: Module -> SrcSpan -> GHCi SrcSpan
enclosingTickSpan _ (UnhelpfulSpan _) = panic "enclosingTickSpan UnhelpfulSpan"
enclosingTickSpan md (RealSrcSpan src) = do
  ticks <- getTickArray md
  let line = srcSpanStartLine src
  ASSERT (inRange (bounds ticks) line) do
  let toRealSrcSpan (UnhelpfulSpan _) = panic "enclosingTickSpan UnhelpfulSpan"
      toRealSrcSpan (RealSrcSpan s) = s
      enclosing_spans = [ pan | (_,pan) <- ticks ! line
                               , realSrcSpanEnd (toRealSrcSpan pan) >= realSrcSpanEnd src]
  return . head . sortBy leftmost_largest $ enclosing_spans

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
backCmd = noArgs $ withSandboxOnly ":back" $ do
  (names, _, pan) <- GHC.back
  printForUser $ ptext (sLit "Logged breakpoint at") <+> ppr pan
  printTypeOfNames names
   -- run the command set with ":set stop <cmd>"
  st <- getGHCiState
  enqueueCommands [stop st]

forwardCmd :: String -> GHCi ()
forwardCmd = noArgs $ withSandboxOnly ":forward" $ do
  (names, ix, pan) <- GHC.forward
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
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        case loc of
            RealSrcLoc l ->
               ASSERT( isExternalName name )
                    findBreakAndSet (GHC.nameModule name) $
                         findBreakByCoord (Just (GHC.srcLocFile l))
                                          (GHC.srcLocLine l,
                                           GHC.srcLocCol l)
            UnhelpfulLoc _ ->
                noCanDo name $ text "can't find its location: " <> ppr loc
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
   | [] <- args = findBreakAndSet md $ findBreakByLine line
   | [col] <- args, all isDigit col =
        findBreakAndSet md $ findBreakByCoord Nothing (line, read col)
   | otherwise = breakSyntax

breakSyntax :: a
breakSyntax = throwGhcException (CmdLineError "Syntax: :break [<mod>] <line> [<column>]")

findBreakAndSet :: Module -> (TickArray -> Maybe (Int, SrcSpan)) -> GHCi ()
findBreakAndSet md lookupTickTree = do
   dflags <- getDynFlags
   tickArray <- getTickArray md
   (breakArray, _) <- getModBreak md
   case lookupTickTree tickArray of
      Nothing  -> liftIO $ putStrLn $ "No breakpoints found at that location."
      Just (tick, pan) -> do
         success <- liftIO $ setBreakFlag dflags True breakArray tick
         if success
            then do
               (alreadySet, nm) <-
                     recordBreak $ BreakLocation
                             { breakModule = md
                             , breakLoc = pan
                             , breakTick = tick
                             , onBreakCmd = ""
                             }
               printForUser $
                  text "Breakpoint " <> ppr nm <>
                  if alreadySet
                     then text " was already set at " <> ppr pan
                     else text " activated at " <> ppr pan
            else do
            printForUser $ text "Breakpoint could not be activated at"
                                 <+> ppr pan

-- When a line number is specified, the current policy for choosing
-- the best breakpoint is this:
--    - the leftmost complete subexpression on the specified line, or
--    - the leftmost subexpression starting on the specified line, or
--    - the rightmost subexpression enclosing the specified line
--
findBreakByLine :: Int -> TickArray -> Maybe (BreakIndex,SrcSpan)
findBreakByLine line arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (leftmost_largest `on` snd)  comp)   `mplus`
    listToMaybe (sortBy (leftmost_smallest `on` snd) incomp) `mplus`
    listToMaybe (sortBy (rightmost `on` snd) ticks)
  where
        ticks = arr ! line

        starts_here = [ tick | tick@(_,pan) <- ticks,
                               GHC.srcSpanStartLine (toRealSpan pan) == line ]

        (comp, incomp) = partition ends_here starts_here
            where ends_here (_,pan) = GHC.srcSpanEndLine (toRealSpan pan) == line
        toRealSpan (RealSrcSpan pan) = pan
        toRealSpan (UnhelpfulSpan _) = panic "findBreakByLine UnhelpfulSpan"

findBreakByCoord :: Maybe FastString -> (Int,Int) -> TickArray
                 -> Maybe (BreakIndex,SrcSpan)
findBreakByCoord mb_file (line, col) arr
  | not (inRange (bounds arr) line) = Nothing
  | otherwise =
    listToMaybe (sortBy (rightmost `on` snd) contains ++
                 sortBy (leftmost_smallest `on` snd) after_here)
  where
        ticks = arr ! line

        -- the ticks that span this coordinate
        contains = [ tick | tick@(_,pan) <- ticks, pan `spans` (line,col),
                            is_correct_file pan ]

        is_correct_file pan
                 | Just f <- mb_file = GHC.srcSpanFile (toRealSpan pan) == f
                 | otherwise         = True

        after_here = [ tick | tick@(_,pan) <- ticks,
                              let pan' = toRealSpan pan,
                              GHC.srcSpanStartLine pan' == line,
                              GHC.srcSpanStartCol pan' >= col ]

        toRealSpan (RealSrcSpan pan) = pan
        toRealSpan (UnhelpfulSpan _) = panic "findBreakByCoord UnhelpfulSpan"

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
                    Just (_, UnhelpfulSpan _) -> panic "list2 UnhelpfulSpan"
                    Just (_, RealSrcSpan pan) -> listAround pan False
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
      let ls  = BS.split '\n' contents
          ls' = take (line2 - line1 + 1 + pad_before + pad_after) $
                        drop (line1 - 1 - pad_before) $ ls
          fst_line = max 1 (line1 - pad_before)
          line_nos = [ fst_line .. ]

          highlighted | do_highlight = zipWith highlight line_nos ls'
                      | otherwise    = [\p -> BS.concat[p,l] | l <- ls']

          bs_line_nos = [ BS.pack (show l ++ "  ") | l <- line_nos ]
          prefixed = zipWith ($) highlighted bs_line_nos
          output   = BS.intercalate (BS.pack "\n") prefixed

      utf8Decoded <- liftIO $ BS.useAsCStringLen output
                        $ \(p,n) -> utf8DecodeString (castPtr p) n
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
discardTickArrays = do
   st <- getGHCiState
   setGHCiState st{tickarrays = emptyModuleEnv}

mkTickArray :: [(BreakIndex,SrcSpan)] -> TickArray
mkTickArray ticks
  = accumArray (flip (:)) [] (1, max_line)
        [ (line, (nm,pan)) | (nm,pan) <- ticks,
                              let pan' = toRealSpan pan,
                              line <- srcSpanLines pan' ]
    where
        max_line = foldr max 0 (map (GHC.srcSpanEndLine . toRealSpan . snd) ticks)
        srcSpanLines pan = [ GHC.srcSpanStartLine pan ..  GHC.srcSpanEndLine pan ]
        toRealSpan (RealSrcSpan pan) = pan
        toRealSpan (UnhelpfulSpan _) = panic "mkTickArray UnhelpfulSpan"

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

turnOffBreak :: BreakLocation -> GHCi Bool
turnOffBreak loc = do
  dflags <- getDynFlags
  (arr, _) <- getModBreak (breakModule loc)
  liftIO $ setBreakFlag dflags False arr (breakTick loc)

getModBreak :: Module -> GHCi (GHC.BreakArray, Array Int SrcSpan)
getModBreak m = do
   Just mod_info <- GHC.getModuleInfo m
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let arr        = GHC.modBreaks_flags modBreaks
   let ticks      = GHC.modBreaks_locs  modBreaks
   return (arr, ticks)

setBreakFlag :: DynFlags -> Bool -> GHC.BreakArray -> Int -> IO Bool
setBreakFlag dflags toggle arr i
   | toggle    = GHC.setBreakOn  dflags arr i
   | otherwise = GHC.setBreakOff dflags arr i


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
  liftIO installSignalHandlers
  ghciHandle handler (showException exception >> return False)

showException :: SomeException -> GHCi ()
showException se =
  liftIO $ case fromException se of
           -- omit the location for CmdLineError:
           Just (CmdLineError s)    -> putException s
           -- ditto:
           Just ph@(PhaseFailed {}) -> putException (showGhcException ph "")
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

ghciHandle :: ExceptionMonad m => (SomeException -> m a) -> m a -> m a
ghciHandle h m = gcatch m $ \e -> gunblock (h e)

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
isHomeModule m = GHC.modulePackageId m == mainPackageId

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
   when (GHC.modulePackageId modl /= thisPackage dflags) $
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
