{-# OPTIONS -fno-cse #-}
-- -fno-cse is needed for GLOBAL_VAR's to behave properly

{-# OPTIONS -#include "Linker.h" #-}
-----------------------------------------------------------------------------
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2005-2006
--
-----------------------------------------------------------------------------

module InteractiveUI ( interactiveUI, ghciWelcomeMsg ) where

#include "HsVersions.h"

import qualified GhciMonad
import GhciMonad hiding (runStmt)
import GhciTags
import Debugger

-- The GHC interface
import qualified GHC hiding (resume, runStmt)
import GHC              ( LoadHowMuch(..), Target(..),  TargetId(..),
                          Module, ModuleName, TyThing(..), Phase,
                          BreakIndex, SrcSpan, Resume, SingleStep,
                          Ghc, handleSourceError )
import PprTyThing
import DynFlags

import Packages
#ifdef USE_EDITLINE
import PackageConfig
import UniqFM
#endif

import HscTypes		( implicitTyThings, reflectGhc, reifyGhc
                        , handleFlagWarnings )
import qualified RdrName ( getGRE_NameQualifier_maybes ) -- should this come via GHC?
import Outputable       hiding (printForUser, printForUserPartWay)
import Module           -- for ModuleEnv
import Name
import SrcLoc

-- Other random utilities
import CmdLineParser
import Digraph
import BasicTypes hiding (isTopLevel)
import Panic      hiding (showException)
import Config
import StaticFlags
import Linker
import Util
import NameSet
import Maybes		( orElse, expectJust )
import FastString
import Encoding
import MonadUtils       ( liftIO )

#ifndef mingw32_HOST_OS
import System.Posix hiding (getEnv)
#else
import GHC.ConsoleHandler ( flushConsole )
import qualified System.Win32
#endif

#ifdef USE_EDITLINE
import Control.Concurrent	( yield )	-- Used in readline loop
import System.Console.Editline.Readline as Readline
#endif

--import SystemExts

import Exception
-- import Control.Concurrent

import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import System.Cmd
import System.Environment
import System.Exit	( exitWith, ExitCode(..) )
import System.Directory
import System.IO
import System.IO.Error as IO
import Data.Char
import Data.Array
import Control.Monad as Monad
import Text.Printf
import Foreign
import Foreign.C
import GHC.Exts		( unsafeCoerce# )
import GHC.IOBase	( IOErrorType(InvalidArgument) )
import GHC.TopHandler

import Data.IORef	( IORef, readIORef, writeIORef )

-----------------------------------------------------------------------------

ghciWelcomeMsg :: String
ghciWelcomeMsg = "GHCi, version " ++ cProjectVersion ++
                 ": http://www.haskell.org/ghc/  :? for help"

cmdName :: Command -> String
cmdName (n,_,_,_) = n

GLOBAL_VAR(macros_ref, [], [Command])

builtin_commands :: [Command]
builtin_commands = [
	-- Hugs users are accustomed to :e, so make sure it doesn't overlap
  ("?",		keepGoing help,			Nothing, completeNone),
  ("add",	keepGoingPaths addModule,	Just filenameWordBreakChars, completeFilename),
  ("abandon",   keepGoing abandonCmd,           Nothing, completeNone),
  ("break",     keepGoing breakCmd,             Nothing, completeIdentifier),
  ("back",      keepGoing backCmd,              Nothing, completeNone),
  ("browse",    keepGoing (browseCmd False),	Nothing, completeModule),
  ("browse!",   keepGoing (browseCmd True),	Nothing, completeModule),
  ("cd",    	keepGoing changeDirectory,	Just filenameWordBreakChars, completeFilename),
  ("check",	keepGoing checkModule,		Nothing, completeHomeModule),
  ("continue",  keepGoing continueCmd,          Nothing, completeNone),
  ("cmd",       keepGoing cmdCmd,               Nothing, completeIdentifier),
  ("ctags",	keepGoing createCTagsFileCmd, 	Just filenameWordBreakChars, completeFilename),
  ("def",	keepGoing (defineMacro False),  Nothing, completeIdentifier),
  ("def!",	keepGoing (defineMacro True),   Nothing, completeIdentifier),
  ("delete",    keepGoing deleteCmd,            Nothing, completeNone),
  ("e", 	keepGoing editFile,		Just filenameWordBreakChars, completeFilename),
  ("edit",	keepGoing editFile,		Just filenameWordBreakChars, completeFilename),
  ("etags",	keepGoing createETagsFileCmd,	Just filenameWordBreakChars, completeFilename),
  ("force",     keepGoing forceCmd,             Nothing, completeIdentifier),
  ("forward",   keepGoing forwardCmd,           Nothing, completeNone),
  ("help",	keepGoing help,			Nothing, completeNone),
  ("history",   keepGoing historyCmd,           Nothing, completeNone), 
  ("info",      keepGoing info,			Nothing, completeIdentifier),
  ("kind",	keepGoing kindOfType,		Nothing, completeIdentifier),
  ("load",	keepGoingPaths loadModule_,     Just filenameWordBreakChars, completeHomeModuleOrFile),
  ("list",	keepGoing listCmd,              Nothing, completeNone),
  ("module",	keepGoing setContext,		Nothing, completeModule),
  ("main",	keepGoing runMain,		Nothing, completeIdentifier),
  ("print",     keepGoing printCmd,             Nothing, completeIdentifier),
  ("quit",	quit,				Nothing, completeNone),
  ("reload", 	keepGoing reloadModule,  	Nothing, completeNone),
  ("run",	keepGoing runRun,		Nothing, completeIdentifier),
  ("set",	keepGoing setCmd,		Just flagWordBreakChars, completeSetOptions),
  ("show",	keepGoing showCmd,		Nothing, completeNone),
  ("sprint",    keepGoing sprintCmd,            Nothing, completeIdentifier),
  ("step",      keepGoing stepCmd,              Nothing, completeIdentifier), 
  ("steplocal", keepGoing stepLocalCmd,         Nothing, completeIdentifier), 
  ("stepmodule",keepGoing stepModuleCmd,        Nothing, completeIdentifier), 
  ("type",	keepGoing typeOfExpr,		Nothing, completeIdentifier),
  ("trace",     keepGoing traceCmd,             Nothing, completeIdentifier), 
  ("undef",     keepGoing undefineMacro,	Nothing, completeMacro),
  ("unset",	keepGoing unsetOptions,		Just flagWordBreakChars,  completeSetOptions)
  ]


-- We initialize readline (in the interactiveUI function) to use 
-- word_break_chars as the default set of completion word break characters.
-- This can be overridden for a particular command (for example, filename
-- expansion shouldn't consider '/' to be a word break) by setting the third
-- entry in the Command tuple above.
-- 
-- NOTE: in order for us to override the default correctly, any custom entry
-- must be a SUBSET of word_break_chars.
#ifdef USE_EDITLINE
word_break_chars :: String
word_break_chars = let symbols = "!#$%&*+/<=>?@\\^|-~"
                       specials = "(),;[]`{}"
                       spaces = " \t\n"
                   in spaces ++ specials ++ symbols
#endif

flagWordBreakChars, filenameWordBreakChars :: String
flagWordBreakChars = " \t\n"
filenameWordBreakChars = " \t\n\\`@$><=;|&{(" -- bash defaults


keepGoing :: (String -> GHCi ()) -> (String -> GHCi Bool)
keepGoing a str = a str >> return False

keepGoingPaths :: ([FilePath] -> GHCi ()) -> (String -> GHCi Bool)
keepGoingPaths a str
 = do case toArgs str of
          Left err -> io (hPutStrLn stderr err)
          Right args -> a args
      return False

shortHelpText :: String
shortHelpText = "use :? for help.\n"

helpText :: String
helpText =
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
 "   :ctags [<file>]             create tags file for Vi (default: \"tags\")\n" ++
 "   :def <cmd> <expr>           define a command :<cmd>\n" ++
 "   :edit <file>                edit file\n" ++
 "   :edit                       edit last module\n" ++
 "   :etags [<file>]             create tags file for Emacs (default: \"TAGS\")\n" ++
 "   :help, :?                   display this list of commands\n" ++
 "   :info [<name> ...]          display information about the given names\n" ++
 "   :kind <type>                show the kind of <type>\n" ++
 "   :load [*]<module> ...       load module(s) and their dependents\n" ++
 "   :main [<arguments> ...]     run the main function with the given arguments\n" ++
 "   :module [+/-] [*]<mod> ...  set the context for expression evaluation\n" ++
 "   :quit                       exit GHCi\n" ++
 "   :reload                     reload the current module set\n" ++
 "   :run function [<arguments> ...] run the function with the given arguments\n" ++
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
 "   :set args <arg> ...         set the arguments returned by System.getArgs\n" ++
 "   :set prog <progname>        set the value returned by System.getProgName\n" ++
 "   :set prompt <prompt>        set the prompt used in GHCi\n" ++
 "   :set editor <cmd>           set the command used for :edit\n" ++
 "   :set stop [<n>] <cmd>       set the command to run when a breakpoint is hit\n" ++
 "   :unset <option> ...         unset options\n" ++
 "\n" ++
 "  Options for ':set' and ':unset':\n" ++
 "\n" ++
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
 "   :show modules               show the currently loaded modules\n" ++
 "   :show packages              show the currently active package flags\n" ++
 "   :show languages             show the currently active language flags\n" ++
 "   :show <setting>             show value of <setting>, which is one of\n" ++
 "                                  [args, prog, prompt, editor, stop]\n" ++
 "\n" 

findEditor :: IO String
findEditor = do
  getEnv "EDITOR" 
    `IO.catch` \_ -> do
#if mingw32_HOST_OS
        win <- System.Win32.getWindowsDirectory
        return (win </> "notepad.exe")
#else
        return ""
#endif

interactiveUI :: [(FilePath, Maybe Phase)] -> Maybe [String]
              -> Ghc ()
interactiveUI srcs maybe_exprs = withTerminalReset $ do
   -- HACK! If we happen to get into an infinite loop (eg the user
   -- types 'let x=x in x' at the prompt), then the thread will block
   -- on a blackhole, and become unreachable during GC.  The GC will
   -- detect that it is unreachable and send it the NonTermination
   -- exception.  However, since the thread is unreachable, everything
   -- it refers to might be finalized, including the standard Handles.
   -- This sounds like a bug, but we don't have a good solution right
   -- now.
   liftIO $ newStablePtr stdin
   liftIO $ newStablePtr stdout
   liftIO $ newStablePtr stderr

    -- Initialise buffering for the *interpreted* I/O system
   initInterpBuffering

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

#ifdef USE_EDITLINE
        is_tty <- hIsTerminalDevice stdin
        when is_tty $ withReadline $ do
            Readline.initialize

            withGhcAppData
                 (\dir -> Readline.readHistory (dir </> "ghci_history"))
                 (return True)
            
            Readline.setAttemptedCompletionFunction (Just completeWord)
            --Readline.parseAndBind "set show-all-if-ambiguous 1"

            Readline.setBasicWordBreakCharacters word_break_chars
            Readline.setCompleterWordBreakCharacters word_break_chars
            Readline.setCompletionAppendCharacter Nothing
#endif

   -- initial context is just the Prelude
   prel_mod <- GHC.findModule (GHC.mkModuleName "Prelude") Nothing
   GHC.setContext [] [prel_mod]

   default_editor <- liftIO $ findEditor

   cwd <- liftIO $ getCurrentDirectory

   startGHCi (runGHCi srcs maybe_exprs)
        GHCiState{ progname = "<interactive>",
                   args = [],
                   prompt = "%s> ",
                   stop = "",
                   editor = default_editor,
--                   session = session,
                   options = [],
                   prelude = prel_mod,
                   break_ctr = 0,
                   breaks = [],
                   tickarrays = emptyModuleEnv,
                   last_command = Nothing,
                   cmdqueue = [],
                   remembered_ctx = [],
                   virtual_path   = cwd,
                   ghc_e = isJust maybe_exprs
                 }

#ifdef USE_EDITLINE
   liftIO $ do
     Readline.stifleHistory 100
     withGhcAppData (\dir -> Readline.writeHistory (dir </> "ghci_history"))
                    (return True)
     Readline.resetTerminal Nothing
#endif

   return ()

withGhcAppData :: (FilePath -> IO a) -> IO a -> IO a
withGhcAppData right left = do
   either_dir <- IO.try (getAppUserDataDirectory "ghc")
   case either_dir of
      Right dir -> right dir
      _ -> left

-- libedit doesn't always restore the terminal settings correctly (as of at 
-- least 07/12/2008); see trac #2691.  Work around this by manually resetting
-- the terminal outselves.
withTerminalReset :: Ghc () -> Ghc ()
#ifdef mingw32_HOST_OS
withTerminalReset = id
#else
withTerminalReset f = do
    isTTY <- liftIO $ hIsTerminalDevice stdout
    if not isTTY
        then f
        else gbracket (liftIO $ getTerminalAttributes stdOutput)
                (\attrs -> liftIO $ setTerminalAttributes stdOutput attrs Immediately)
                (const f)
#endif

runGHCi :: [(FilePath, Maybe Phase)] -> Maybe [String] -> GHCi ()
runGHCi paths maybe_exprs = do
  let 
   read_dot_files = not opt_IgnoreDotGhci

   current_dir = return (Just ".ghci")

   app_user_dir = io $ withGhcAppData 
                    (\dir -> return (Just (dir </> "ghci.conf")))
                    (return Nothing)

   home_dir = do
    either_dir <- io $ IO.try (getEnv "HOME")
    case either_dir of
      Right home -> return (Just (home </> ".ghci"))
      _ -> return Nothing

   sourceConfigFile :: FilePath -> GHCi ()
   sourceConfigFile file = do
     exists <- io $ doesFileExist file
     when exists $ do
       dir_ok  <- io $ checkPerms (getDirectory file)
       file_ok <- io $ checkPerms file
       when (dir_ok && file_ok) $ do
         either_hdl <- io $ IO.try (openFile file ReadMode)
         case either_hdl of
           Left _e   -> return ()
           Right hdl -> runCommands (fileLoop hdl False False)
     where
      getDirectory f = case takeDirectory f of "" -> "."; d -> d

  when (read_dot_files) $ do
    cfgs0 <- sequence [ current_dir, app_user_dir, home_dir ]
    cfgs <- io $ mapM canonicalizePath (catMaybes cfgs0)
    mapM_ sourceConfigFile (nub cfgs)
        -- nub, because we don't want to read .ghci twice if the
        -- CWD is $HOME.

  -- Perform a :load for files given on the GHCi command line
  -- When in -e mode, if the load fails then we want to stop
  -- immediately rather than going on to evaluate the expression.
  when (not (null paths)) $ do
     ok <- ghciHandle (\e -> do showException e; return Failed) $
                loadModule paths
     when (isJust maybe_exprs && failed ok) $
        io (exitWith (ExitFailure 1))

  -- if verbosity is greater than 0, or we are connected to a
  -- terminal, display the prompt in the interactive loop.
  is_tty <- io (hIsTerminalDevice stdin)
  dflags <- getDynFlags
  let show_prompt = verbosity dflags > 0 || is_tty

  case maybe_exprs of
        Nothing ->
          do
#if defined(mingw32_HOST_OS)
            -- The win32 Console API mutates the first character of
            -- type-ahead when reading from it in a non-buffered manner. Work
            -- around this by flushing the input buffer of type-ahead characters,
            -- but only if stdin is available.
            flushed <- io (IO.try (GHC.ConsoleHandler.flushConsole stdin))
            case flushed of
             Left err | isDoesNotExistError err -> return ()
                      | otherwise -> io (ioError err)
             Right () -> return ()
#endif
            -- enter the interactive loop
            interactiveLoop is_tty show_prompt
        Just exprs -> do
            -- just evaluate the expression we were given
            enqueueCommands exprs
            let handle e = do st <- getGHCiState
                                   -- Jump through some hoops to get the
                                   -- current progname in the exception text:
                                   -- <progname>: <exception>
                              io $ withProgName (progname st)
                                   -- this used to be topHandlerFastExit, see #2228
                                 $ topHandler e
            runCommands' handle (return Nothing)

  -- and finally, exit
  io $ do when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."

interactiveLoop :: Bool -> Bool -> GHCi ()
interactiveLoop is_tty show_prompt =
  -- Ignore ^C exceptions caught here
  ghciHandleGhcException (\e -> case e of 
			Interrupted -> do
#if defined(mingw32_HOST_OS)
				io (putStrLn "")
#endif
				interactiveLoop is_tty show_prompt
			_other      -> return ()) $ 

  ghciUnblock $ do -- unblock necessary if we recursed from the 
		   -- exception handler above.

  -- read commands from stdin
#ifdef USE_EDITLINE
  if (is_tty) 
	then runCommands readlineLoop
	else runCommands (fileLoop stdin show_prompt is_tty)
#else
  runCommands (fileLoop stdin show_prompt is_tty)
#endif


-- NOTE: We only read .ghci files if they are owned by the current user,
-- and aren't world writable.  Otherwise, we could be accidentally 
-- running code planted by a malicious third party.

-- Furthermore, We only read ./.ghci if . is owned by the current user
-- and isn't writable by anyone else.  I think this is sufficient: we
-- don't need to check .. and ../.. etc. because "."  always refers to
-- the same directory while a process is running.

checkPerms :: String -> IO Bool
#ifdef mingw32_HOST_OS
checkPerms _ =
  return True
#else
checkPerms name =
  handleIO (\_ -> return False) $ do
     st <- getFileStatus name
     me <- getRealUserID
     if fileOwner st /= me then do
   	putStrLn $ "WARNING: " ++ name ++ " is owned by someone else, IGNORING!"
   	return False
      else do
   	let mode =  fileMode st
   	if (groupWriteMode == (mode `intersectFileModes` groupWriteMode))
   	   || (otherWriteMode == (mode `intersectFileModes` otherWriteMode)) 
   	   then do
   	       putStrLn $ "*** WARNING: " ++ name ++ 
   			  " is writable by someone else, IGNORING!"
   	       return False
   	  else return True
#endif

fileLoop :: Handle -> Bool -> Bool -> GHCi (Maybe String)
fileLoop hdl show_prompt is_tty = do
   when show_prompt $ do
        prompt <- mkPrompt
        (io (putStr prompt))
   l <- io (IO.try (hGetLine hdl))
   case l of
        Left e | isEOFError e              -> return Nothing
               | InvalidArgument <- etype  -> return Nothing
               | otherwise                 -> io (ioError e)
                where etype = ioeGetErrorType e
                -- treat InvalidArgument in the same way as EOF:
                -- this can happen if the user closed stdin, or
                -- perhaps did getContents which closes stdin at
                -- EOF.
        Right l -> do
                   str <- io $ consoleInputToUnicode is_tty l
                   return (Just str)

#ifdef mingw32_HOST_OS
-- Convert the console input into Unicode according to the current code page.
-- The Windows console stores Unicode characters directly, so this is a
-- rather roundabout way of doing things... oh well.
-- See #782, #1483, #1649
consoleInputToUnicode :: Bool -> String -> IO String
consoleInputToUnicode is_tty str
  | is_tty = do
    cp <- System.Win32.getConsoleCP
    System.Win32.stringToUnicode cp str
  | otherwise =
    decodeStringAsUTF8 str
#else
-- for Unix, assume the input is in UTF-8 and decode it to a Unicode String. 
-- See #782.
consoleInputToUnicode :: Bool -> String -> IO String
consoleInputToUnicode _is_tty str = decodeStringAsUTF8 str
#endif

decodeStringAsUTF8 :: String -> IO String
decodeStringAsUTF8 str =
  withCStringLen str $ \(cstr,len) -> 
    utf8DecodeString (castPtr cstr :: Ptr Word8) len

mkPrompt :: GHCi String
mkPrompt = do
  (toplevs,exports) <- GHC.getContext
  resumes <- GHC.getResumeContext
  -- st <- getGHCiState

  context_bit <-
        case resumes of
            [] -> return empty
            r:_ -> do
                let ix = GHC.resumeHistoryIx r
                if ix == 0
                   then return (brackets (ppr (GHC.resumeSpan r)) <> space)
                   else do
                        let hist = GHC.resumeHistory r !! (ix-1)
                        span <- GHC.getHistorySpan hist
                        return (brackets (ppr (negate ix) <> char ':' 
                                          <+> ppr span) <> space)
  let
        dots | _:rs <- resumes, not (null rs) = text "... "
             | otherwise = empty

        

        modules_bit = 
       -- ToDo: maybe...
       --  let (btoplevs, bexports) = fromMaybe ([],[]) (remembered_ctx st) in
       --  hsep (map (\m -> text "!*" <> ppr (GHC.moduleName m)) btoplevs) <+>
       --  hsep (map (\m -> char '!'  <> ppr (GHC.moduleName m)) bexports) <+>
             hsep (map (\m -> char '*'  <> ppr (GHC.moduleName m)) toplevs) <+>
             hsep (map (ppr . GHC.moduleName) exports)

        deflt_prompt = dots <> context_bit <> modules_bit

        f ('%':'s':xs) = deflt_prompt <> f xs
        f ('%':'%':xs) = char '%' <> f xs
        f (x:xs) = char x <> f xs
        f [] = empty
   --
  st <- getGHCiState
  return (showSDoc (f (prompt st)))


#ifdef USE_EDITLINE
readlineLoop :: GHCi (Maybe String)
readlineLoop = do
   io yield
   saveSession -- for use by completion
   prompt <- mkPrompt
   l <- io $ withReadline (readline prompt)
   splatSavedSession
   case l of
        Nothing -> return Nothing
        Just "" -> return (Just "") -- Don't put empty lines in the history
        Just l  -> do
                   io (addHistory l)
                   str <- io $ consoleInputToUnicode True l
                   return (Just str)

withReadline :: IO a -> IO a
withReadline = bracket_ stopTimer startTimer
     --    editline doesn't handle some of its system calls returning
     --    EINTR, so our timer signal confuses it, hence we turn off
     --    the timer signal when making calls to editline. (#2277)
     --    If editline is ever fixed, we can remove this.

-- These come from the RTS
foreign import ccall unsafe startTimer :: IO ()
foreign import ccall unsafe stopTimer  :: IO ()
#endif

queryQueue :: GHCi (Maybe String)
queryQueue = do
  st <- getGHCiState
  case cmdqueue st of
    []   -> return Nothing
    c:cs -> do setGHCiState st{ cmdqueue = cs }
               return (Just c)

runCommands :: GHCi (Maybe String) -> GHCi ()
runCommands = runCommands' handler

runCommands' :: (SomeException -> GHCi Bool) -- Exception handler
             -> GHCi (Maybe String) -> GHCi ()
runCommands' eh getCmd = do
  mb_cmd <- noSpace queryQueue
  mb_cmd <- maybe (noSpace getCmd) (return . Just) mb_cmd
  case mb_cmd of 
    Nothing -> return ()
    Just c  -> do
      b <- ghciHandle eh $
             handleSourceError printErrorAndKeepGoing
               (doCommand c)
      if b then return () else runCommands' eh getCmd
  where
    printErrorAndKeepGoing err = do
        GHC.printExceptionAndWarnings err
        return False

    noSpace q = q >>= maybe (return Nothing)
                            (\c->case removeSpaces c of 
                                   ""   -> noSpace q
                                   ":{" -> multiLineCmd q
                                   c    -> return (Just c) )
    multiLineCmd q = do
      st <- getGHCiState
      let p = prompt st
      setGHCiState st{ prompt = "%s| " }
      mb_cmd <- collectCommand q ""
      getGHCiState >>= \st->setGHCiState st{ prompt = p }
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
      maybe (io (ioError collectError))
            (\l->if removeSpaces l == ":}" 
                 then return (Just $ removeSpaces c) 
                 else collectCommand q (c++map normSpace l))
      where normSpace '\r' = ' '
            normSpace   c  = c
    -- QUESTION: is userError the one to use here?
    collectError = userError "unterminated multiline command :{ .. :}"
    doCommand (':' : cmd) = specialCommand cmd
    doCommand stmt        = do timeIt $ runStmt stmt GHC.RunToCompletion
                               return False

enqueueCommands :: [String] -> GHCi ()
enqueueCommands cmds = do
  st <- getGHCiState
  setGHCiState st{ cmdqueue = cmds ++ cmdqueue st }


runStmt :: String -> SingleStep -> GHCi Bool
runStmt stmt step
 | null (filter (not.isSpace) stmt) = return False
 | ["import", mod] <- words stmt    = keepGoing setContext ('+':mod)
 | otherwise
 = do result <- GhciMonad.runStmt stmt step
      afterRunStmt (const True) result

--afterRunStmt :: GHC.RunResult -> GHCi Bool
                                 -- False <=> the statement failed to compile
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
               printForUser $ ptext (sLit "Stopped at") <+> 
                       ppr (GHC.resumeSpan $ head resumes)
--               printTypeOfNames session names
               let namesSorted = sortBy compareNames names
               tythings <- catMaybes `liftM` 
                              mapM GHC.lookupName namesSorted
               docs <- pprTypeAndContents [id | AnId id <- tythings]
               printForUserPartWay docs
               maybe (return ()) runBreakCmd mb_info
               -- run the command set with ":set stop <cmd>"
               st <- getGHCiState
               enqueueCommands [stop st]
               return ()
         | otherwise -> resume GHC.SingleStep >>=
                        afterRunStmt step_here >> return ()
     _ -> return ()

  flushInterpBuffers
  io installSignalHandlers
  b <- isOptionSet RevertCAFs
  when b revertCAFs

  return (case run_result of GHC.RunOk _ -> True; _ -> False)

runBreakCmd :: GHC.BreakInfo -> GHCi ()
runBreakCmd info = do
  let mod = GHC.breakInfo_module info
      nm  = GHC.breakInfo_number info
  st <- getGHCiState
  case  [ loc | (_,loc) <- breaks st,
                breakModule loc == mod, breakTick loc == nm ] of
        []  -> return ()
        loc:_ | null cmd  -> return ()
              | otherwise -> do enqueueCommands [cmd]; return ()
              where cmd = onBreakCmd loc

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

specialCommand :: String -> GHCi Bool
specialCommand ('!':str) = shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  maybe_cmd <- lookupCommand cmd
  case maybe_cmd of
    GotCommand (_,f,_,_) -> f (dropWhile isSpace rest)
    BadCommand ->
      do io $ hPutStr stdout ("unknown command ':" ++ cmd ++ "'\n"
                           ++ shortHelpText)
         return False
    NoLastCommand ->
      do io $ hPutStr stdout ("there is no last command to perform\n"
                           ++ shortHelpText)
         return False

lookupCommand :: String -> GHCi (MaybeCommand)
lookupCommand "" = do
  st <- getGHCiState
  case last_command st of
      Just c -> return $ GotCommand c
      Nothing -> return NoLastCommand
lookupCommand str = do
  mc <- io $ lookupCommand' str
  st <- getGHCiState
  setGHCiState st{ last_command = mc }
  return $ case mc of
           Just c -> GotCommand c
           Nothing -> BadCommand

lookupCommand' :: String -> IO (Maybe Command)
lookupCommand' str = do
  macros <- readIORef macros_ref
  let cmds = builtin_commands ++ macros
  -- look for exact match first, then the first prefix match
  return $ case [ c | c <- cmds, str == cmdName c ] of
           c:_ -> Just c
           [] -> case [ c | c@(s,_,_,_) <- cmds, str `isPrefixOf` s ] of
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
                span <- GHC.getHistorySpan hist
                return (Just span)

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
-- Commands

noArgs :: GHCi () -> String -> GHCi ()
noArgs m "" = m
noArgs _ _  = io $ putStrLn "This command takes no arguments"

help :: String -> GHCi ()
help _ = io (putStr helpText)

info :: String -> GHCi ()
info "" = ghcError (CmdLineError "syntax: ':i <thing-you-want-info-about>'")
info s  = handleSourceError GHC.printExceptionAndWarnings $ do
             { let names = words s
	     ; dflags <- getDynFlags
	     ; let pefas = dopt Opt_PrintExplicitForalls dflags
	     ; mapM_ (infoThing pefas) names }
  where
    infoThing pefas str = do
	names     <- GHC.parseName str
	mb_stuffs <- mapM GHC.getInfo names
	let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
	unqual <- GHC.getPrintUnqual
	liftIO $
          putStrLn (showSDocForUser unqual $
     		     vcat (intersperse (text "") $
		           map (pprInfo pefas) filtered))

  -- Filter out names whose parent is also there Good
  -- example is '[]', which is both a type and data
  -- constructor in the same type
filterOutChildren :: (a -> TyThing) -> [a] -> [a]
filterOutChildren get_thing xs 
  = [x | x <- xs, not (getName (get_thing x) `elemNameSet` implicits)]
  where
    implicits = mkNameSet [getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: PrintExplicitForalls -> (TyThing, Fixity, [GHC.Instance]) -> SDoc
pprInfo pefas (thing, fixity, insts)
  =  pprTyThingInContextLoc pefas thing
  $$ show_fixity fixity
  $$ vcat (map GHC.pprInstance insts)
  where
    show_fixity fix 
	| fix == GHC.defaultFixity = empty
	| otherwise		   = ppr fix <+> ppr (GHC.getName thing)

runMain :: String -> GHCi ()
runMain s = case toArgs s of
            Left err   -> io (hPutStrLn stderr err)
            Right args ->
                do dflags <- getDynFlags
                   case mainFunIs dflags of
                       Nothing -> doWithArgs args "main"
                       Just f  -> doWithArgs args f

runRun :: String -> GHCi ()
runRun s = case toCmdArgs s of
           Left err          -> io (hPutStrLn stderr err)
           Right (cmd, args) -> doWithArgs args cmd

doWithArgs :: [String] -> String -> GHCi ()
doWithArgs args cmd = enqueueCommands ["System.Environment.withArgs " ++
                                       show args ++ " (" ++ cmd ++ ")"]

addModule :: [FilePath] -> GHCi ()
addModule files = do
  revertCAFs			-- always revert CAFs on load/add.
  files <- mapM expandPath files
  targets <- mapM (\m -> GHC.guessTarget m Nothing) files
  -- remove old targets with the same id; e.g. for :add *M
  mapM_ GHC.removeTarget [ tid | Target tid _ _ <- targets ]
  mapM_ GHC.addTarget targets
  prev_context <- GHC.getContext
  ok <- trySuccess $ GHC.load LoadAllTargets
  afterLoad ok False prev_context

changeDirectory :: String -> GHCi ()
changeDirectory "" = do
  -- :cd on its own changes to the user's home directory
  either_dir <- io (IO.try getHomeDirectory)
  case either_dir of
     Left _e -> return ()
     Right dir -> changeDirectory dir
changeDirectory dir = do
  graph <- GHC.getModuleGraph
  when (not (null graph)) $
	io $ putStr "Warning: changing directory causes all loaded modules to be unloaded,\nbecause the search path has changed.\n"
  prev_context <- GHC.getContext
  GHC.setTargets []
  GHC.load LoadAllTargets
  setContextAfterLoad prev_context False []
  GHC.workingDirectoryChanged
  dir <- expandPath dir
  io (setCurrentDirectory dir)

trySuccess :: GHC.GhcMonad m => m SuccessFlag -> m SuccessFlag
trySuccess act =
    handleSourceError (\e -> do GHC.printExceptionAndWarnings e
                                return Failed) $ do
      act

editFile :: String -> GHCi ()
editFile str =
  do file <- if null str then chooseEditFile else return str
     st <- getGHCiState
     let cmd = editor st
     when (null cmd) 
       $ ghcError (CmdLineError "editor not set, use :set editor")
     io $ system (cmd ++ ' ':file)
     return ()

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
              Nothing   -> ghcError (CmdLineError "No files to edit.")
          
  where fromTarget (GHC.Target (GHC.TargetFile f _) _ _) = Just f
        fromTarget _ = Nothing -- when would we get a module target?

defineMacro :: Bool{-overwrite-} -> String -> GHCi ()
defineMacro overwrite s = do
  let (macro_name, definition) = break isSpace s
  macros <- io (readIORef macros_ref)
  let defined = map cmdName macros
  if (null macro_name) 
	then if null defined
                then io $ putStrLn "no macros defined"
                else io $ putStr ("the following macros are defined:\n" ++
                                  unlines defined)
	else do
  if (not overwrite && macro_name `elem` defined)
	then ghcError (CmdLineError 
		("macro '" ++ macro_name ++ "' is already defined"))
	else do

  let filtered = [ cmd | cmd <- macros, cmdName cmd /= macro_name ]

  -- give the expression a type signature, so we can be sure we're getting
  -- something of the right type.
  let new_expr = '(' : definition ++ ") :: String -> IO String"

  -- compile the expression
  handleSourceError (\e -> GHC.printExceptionAndWarnings e) $ do
    hv <- GHC.compileExpr new_expr
    io (writeIORef macros_ref --
	(filtered ++ [(macro_name, runMacro hv, Nothing, completeNone)]))

runMacro :: GHC.HValue{-String -> IO String-} -> String -> GHCi Bool
runMacro fun s = do
  str <- io ((unsafeCoerce# fun :: String -> IO String) s)
  enqueueCommands (lines str)
  return False

undefineMacro :: String -> GHCi ()
undefineMacro str = mapM_ undef (words str) 
 where undef macro_name = do
        cmds <- io (readIORef macros_ref)
        if (macro_name `notElem` map cmdName cmds) 
      	   then ghcError (CmdLineError 
      		("macro '" ++ macro_name ++ "' is not defined"))
      	   else do
            io (writeIORef macros_ref (filter ((/= macro_name) . cmdName) cmds))

cmdCmd :: String -> GHCi ()
cmdCmd str = do
  let expr = '(' : str ++ ") :: IO String"
  handleSourceError (\e -> GHC.printExceptionAndWarnings e) $ do
    hv <- GHC.compileExpr expr
    cmds <- io $ (unsafeCoerce# hv :: IO String)
    enqueueCommands (lines cmds)
    return ()

loadModule :: [(FilePath, Maybe Phase)] -> GHCi SuccessFlag
loadModule fs = timeIt (loadModule' fs)

loadModule_ :: [FilePath] -> GHCi ()
loadModule_ fs = do loadModule (zip fs (repeat Nothing)); return ()

loadModule' :: [(FilePath, Maybe Phase)] -> GHCi SuccessFlag
loadModule' files = do
  prev_context <- GHC.getContext

  -- unload first
  GHC.abandonAll
  discardActiveBreakPoints
  GHC.setTargets []
  GHC.load LoadAllTargets

  -- expand tildes
  let (filenames, phases) = unzip files
  exp_filenames <- mapM expandPath filenames
  let files' = zip exp_filenames phases
  targets <- mapM (uncurry GHC.guessTarget) files'

  -- NOTE: we used to do the dependency anal first, so that if it
  -- fails we didn't throw away the current set of modules.  This would
  -- require some re-working of the GHC interface, so we'll leave it
  -- as a ToDo for now.

  GHC.setTargets targets
  doLoad False prev_context LoadAllTargets

checkModule :: String -> GHCi ()
checkModule m = do
  let modl = GHC.mkModuleName m
  prev_context <- GHC.getContext
  ok <- handleSourceError (\e -> GHC.printExceptionAndWarnings e >> return False) $ do
          r <- GHC.typecheckModule =<< GHC.parseModule =<< GHC.getModSummary modl
          io $ putStrLn (showSDoc (
	   case GHC.moduleInfo r of
	     cm | Just scope <- GHC.modInfoTopLevelScope cm ->
		let
		    (local,global) = ASSERT( all isExternalName scope )
		    		     partition ((== modl) . GHC.moduleName . GHC.nameModule) scope
		in
			(text "global names: " <+> ppr global) $$
		        (text "local  names: " <+> ppr local)
	     _ -> empty))
          return True
  afterLoad (successIf ok) False prev_context

reloadModule :: String -> GHCi ()
reloadModule m = do
  prev_context <- GHC.getContext
  doLoad True prev_context $
        if null m then LoadAllTargets 
                  else LoadUpTo (GHC.mkModuleName m)
  return ()

doLoad :: Bool -> ([Module],[Module]) -> LoadHowMuch -> GHCi SuccessFlag
doLoad retain_context prev_context howmuch = do
  -- turn off breakpoints before we load: we can't turn them off later, because
  -- the ModBreaks will have gone away.
  discardActiveBreakPoints
  ok <- trySuccess $ GHC.load howmuch
  afterLoad ok retain_context prev_context
  return ok

afterLoad :: SuccessFlag -> Bool -> ([Module],[Module]) -> GHCi ()
afterLoad ok retain_context prev_context = do
  revertCAFs  -- always revert CAFs on load.
  discardTickArrays
  loaded_mod_summaries <- getLoadedModules
  let loaded_mods = map GHC.ms_mod loaded_mod_summaries
      loaded_mod_names = map GHC.moduleName loaded_mods
  modulesLoadedMsg ok loaded_mod_names

  setContextAfterLoad prev_context retain_context loaded_mod_summaries


setContextAfterLoad :: ([Module],[Module]) -> Bool -> [GHC.ModSummary] -> GHCi ()
setContextAfterLoad prev keep_ctxt [] = do
  prel_mod <- getPrelude
  setContextKeepingPackageModules prev keep_ctxt ([], [prel_mod])
setContextAfterLoad prev keep_ctxt ms = do
  -- load a target if one is available, otherwise load the topmost module.
  targets <- GHC.getTargets
  case [ m | Just m <- map (findTarget ms) targets ] of
	[]    -> 
	  let graph' = flattenSCCs (GHC.topSortModuleGraph True ms Nothing) in
	  load_this (last graph')	  
	(m:_) -> 
	  load_this m
 where
   findTarget ms t
    = case filter (`matches` t) ms of
	[]    -> Nothing
	(m:_) -> Just m

   summary `matches` Target (TargetModule m) _ _
	= GHC.ms_mod_name summary == m
   summary `matches` Target (TargetFile f _) _ _ 
	| Just f' <- GHC.ml_hs_file (GHC.ms_location summary)	= f == f'
   _ `matches` _
	= False

   load_this summary | m <- GHC.ms_mod summary = do
	b <- GHC.moduleIsInterpreted m
	if b then setContextKeepingPackageModules prev keep_ctxt ([m], [])
       	     else do
                prel_mod <- getPrelude
                setContextKeepingPackageModules prev keep_ctxt ([],[prel_mod,m])

-- | Keep any package modules (except Prelude) when changing the context.
setContextKeepingPackageModules
        :: ([Module],[Module])          -- previous context
        -> Bool                         -- re-execute :module commands
        -> ([Module],[Module])          -- new context
        -> GHCi ()
setContextKeepingPackageModules prev_context keep_ctxt (as,bs) = do
  let (_,bs0) = prev_context
  prel_mod <- getPrelude
  let pkg_modules = filter (\p -> not (isHomeModule p) && p /= prel_mod) bs0
  let bs1 = if null as then nub (prel_mod : bs) else bs
  GHC.setContext as (nub (bs1 ++ pkg_modules))
  if keep_ctxt
     then do
          st <- getGHCiState
          mapM_ (playCtxtCmd False) (remembered_ctx st)
     else do
          st <- getGHCiState
          setGHCiState st{ remembered_ctx = [] }

isHomeModule :: Module -> Bool
isHomeModule mod = GHC.modulePackageId mod == mainPackageId

modulesLoadedMsg :: SuccessFlag -> [ModuleName] -> GHCi ()
modulesLoadedMsg ok mods = do
  dflags <- getDynFlags
  when (verbosity dflags > 0) $ do
   let mod_commas 
	| null mods = text "none."
	| otherwise = hsep (
	    punctuate comma (map ppr mods)) <> text "."
   case ok of
    Failed ->
       io (putStrLn (showSDoc (text "Failed, modules loaded: " <> mod_commas)))
    Succeeded  ->
       io (putStrLn (showSDoc (text "Ok, modules loaded: " <> mod_commas)))


typeOfExpr :: String -> GHCi ()
typeOfExpr str 
  = handleSourceError (\e -> GHC.printExceptionAndWarnings e) $ do
       ty <- GHC.exprType str
       dflags <- getDynFlags
       let pefas = dopt Opt_PrintExplicitForalls dflags
       printForUser $ sep [text str, nest 2 (dcolon <+> pprTypeForUser pefas ty)]

kindOfType :: String -> GHCi ()
kindOfType str 
  = handleSourceError (\e -> GHC.printExceptionAndWarnings e) $ do
       ty <- GHC.typeKind str
       printForUser $ text str <+> dcolon <+> ppr ty
          
quit :: String -> GHCi Bool
quit _ = return True

shellEscape :: String -> GHCi Bool
shellEscape str = io (system str >> return False)

-----------------------------------------------------------------------------
-- Browsing a module's contents

browseCmd :: Bool -> String -> GHCi ()
browseCmd bang m = 
  case words m of
    ['*':s] | looksLikeModuleName s -> do 
        m <-  wantInterpretedModule s
        browseModule bang m False
    [s] | looksLikeModuleName s -> do
        m <- lookupModule s
        browseModule bang m True
    [] -> do
        (as,bs) <- GHC.getContext
                -- Guess which module the user wants to browse.  Pick
                -- modules that are interpreted first.  The most
                -- recently-added module occurs last, it seems.
        case (as,bs) of
          (as@(_:_), _)   -> browseModule bang (last as) True
          ([],  bs@(_:_)) -> browseModule bang (last bs) True
          ([],  [])  -> ghcError (CmdLineError ":browse: no current module")
    _ -> ghcError (CmdLineError "syntax:  :browse <module>")

-- without bang, show items in context of their parents and omit children
-- with bang, show class methods and data constructors separately, and
--            indicate import modules, to aid qualifying unqualified names
-- with sorted, sort items alphabetically
browseModule :: Bool -> Module -> Bool -> GHCi ()
browseModule bang modl exports_only = do
  -- :browse! reports qualifiers wrt current context
  current_unqual <- GHC.getPrintUnqual
  -- Temporarily set the context to the module we're interested in,
  -- just so we can get an appropriate PrintUnqualified
  (as,bs) <- GHC.getContext
  prel_mod <- getPrelude
  if exports_only then GHC.setContext [] [prel_mod,modl]
                  else GHC.setContext [modl] []
  target_unqual <- GHC.getPrintUnqual
  GHC.setContext as bs

  let unqual = if bang then current_unqual else target_unqual

  mb_mod_info <- GHC.getModuleInfo modl
  case mb_mod_info of
    Nothing -> ghcError (CmdLineError ("unknown module: " ++
                                GHC.moduleNameString (GHC.moduleName modl)))
    Just mod_info -> do
        dflags <- getDynFlags
        let names
               | exports_only = GHC.modInfoExports mod_info
               | otherwise    = GHC.modInfoTopLevelScope mod_info
                                `orElse` []

                -- sort alphabetically name, but putting
                -- locally-defined identifiers first.
                -- We would like to improve this; see #1799.
            sorted_names = loc_sort local ++ occ_sort external
                where 
                (local,external) = ASSERT( all isExternalName names )
				   partition ((==modl) . nameModule) names
                occ_sort = sortBy (compare `on` nameOccName) 
                -- try to sort by src location.  If the first name in
                -- our list has a good source location, then they all should.
                loc_sort names
                      | n:_ <- names, isGoodSrcSpan (nameSrcSpan n)
                      = sortBy (compare `on` nameSrcSpan) names
                      | otherwise
                      = occ_sort names

        mb_things <- mapM GHC.lookupName sorted_names
        let filtered_things = filterOutChildren (\t -> t) (catMaybes mb_things)

        rdr_env <- GHC.getGRE

        let pefas              = dopt Opt_PrintExplicitForalls dflags
            things | bang      = catMaybes mb_things
                   | otherwise = filtered_things
            pretty | bang      = pprTyThing
                   | otherwise = pprTyThingInContext

            labels  [] = text "-- not currently imported"
            labels  l  = text $ intercalate "\n" $ map qualifier l
            qualifier  = maybe "-- defined locally" 
                             (("-- imported via "++) . intercalate ", " 
                               . map GHC.moduleNameString)
            importInfo = RdrName.getGRE_NameQualifier_maybes rdr_env
            modNames   = map (importInfo . GHC.getName) things
                                        
            -- annotate groups of imports with their import modules
            -- the default ordering is somewhat arbitrary, so we group 
            -- by header and sort groups; the names themselves should
            -- really come in order of source appearance.. (trac #1799)
            annotate mts = concatMap (\(m,ts)->labels m:ts)
                         $ sortBy cmpQualifiers $ group mts
              where cmpQualifiers = 
                      compare `on` (map (fmap (map moduleNameFS)) . fst)
            group []            = []
            group mts@((m,_):_) = (m,map snd g) : group ng
              where (g,ng) = partition ((==m).fst) mts

        let prettyThings = map (pretty pefas) things
            prettyThings' | bang      = annotate $ zip modNames prettyThings
                          | otherwise = prettyThings
        io (putStrLn $ showSDocForUser unqual (vcat prettyThings'))
        -- ToDo: modInfoInstances currently throws an exception for
        -- package modules.  When it works, we can do this:
        --        $$ vcat (map GHC.pprInstance (GHC.modInfoInstances mod_info))

-----------------------------------------------------------------------------
-- Setting the module context

setContext :: String -> GHCi ()
setContext str
  | all sensible strs = do
       playCtxtCmd True (cmd, as, bs)
       st <- getGHCiState
       setGHCiState st{ remembered_ctx = remembered_ctx st ++ [(cmd,as,bs)] }
  | otherwise = ghcError (CmdLineError "syntax:  :module [+/-] [*]M1 ... [*]Mn")
  where
    (cmd, strs, as, bs) =
        case str of 
                '+':stuff -> rest AddModules stuff
                '-':stuff -> rest RemModules stuff
                stuff     -> rest SetContext stuff

    rest cmd stuff = (cmd, strs, as, bs)
       where strs = words stuff
             (as,bs) = partitionWith starred strs

    sensible ('*':m) = looksLikeModuleName m
    sensible m       = looksLikeModuleName m

    starred ('*':m) = Left m
    starred m       = Right m

playCtxtCmd :: Bool -> (CtxtCmd, [String], [String]) -> GHCi ()
playCtxtCmd fail (cmd, as, bs)
  = do
    (as',bs') <- do_checks fail
    (prev_as,prev_bs) <- GHC.getContext
    (new_as, new_bs) <-
      case cmd of
        SetContext -> do
          prel_mod <- getPrelude
          let bs'' = if null as && prel_mod `notElem` bs' then prel_mod:bs'
                                                          else bs'
          return (as',bs'')
        AddModules -> do
          let as_to_add = as' \\ (prev_as ++ prev_bs)
              bs_to_add = bs' \\ (prev_as ++ prev_bs)
          return (prev_as ++ as_to_add, prev_bs ++ bs_to_add)
        RemModules -> do
          let new_as = prev_as \\ (as' ++ bs')
              new_bs = prev_bs \\ (as' ++ bs')
          return (new_as, new_bs)
    GHC.setContext new_as new_bs
  where
    do_checks True = do
      as' <- mapM wantInterpretedModule as
      bs' <- mapM lookupModule bs
      return (as',bs')
    do_checks False = do
      as' <- mapM (trymaybe . wantInterpretedModule) as
      bs' <- mapM (trymaybe . lookupModule) bs
      return (catMaybes as', catMaybes bs')

    trymaybe m = do
        r <- ghciTry m
        case r of
          Left _  -> return Nothing
          Right a -> return (Just a)

----------------------------------------------------------------------------
-- Code for `:set'

-- set options in the interpreter.  Syntax is exactly the same as the
-- ghc command line, except that certain options aren't available (-C,
-- -E etc.)
--
-- This is pretty fragile: most options won't work as expected.  ToDo:
-- figure out which ones & disallow them.

setCmd :: String -> GHCi ()
setCmd ""
  = do st <- getGHCiState
       let opts = options st
       io $ putStrLn (showSDoc (
   	      text "options currently set: " <> 
   	      if null opts
   		   then text "none."
   		   else hsep (map (\o -> char '+' <> text (optToStr o)) opts)
   	   ))
       dflags <- getDynFlags
       io $ putStrLn (showSDoc (
          vcat (text "GHCi-specific dynamic flag settings:" 
               :map (flagSetting dflags) ghciFlags)
          ))
       io $ putStrLn (showSDoc (
          vcat (text "other dynamic, non-language, flag settings:" 
               :map (flagSetting dflags) nonLanguageDynFlags)
          ))
  where flagSetting dflags (str, f, _)
          | dopt f dflags = text "  " <> text "-f"    <> text str
          | otherwise     = text "  " <> text "-fno-" <> text str
        (ghciFlags,others)  = partition (\(_, f, _) -> f `elem` flags)
                                        DynFlags.fFlags
        nonLanguageDynFlags = filterOut (\(_, f, _) -> f `elem` languageOptions)
                                        others
        flags = [Opt_PrintExplicitForalls
                ,Opt_PrintBindResult
                ,Opt_BreakOnException
                ,Opt_BreakOnError
                ,Opt_PrintEvldWithShow
                ] 
setCmd str
  = case getCmd str of
    Right ("args",   rest) ->
        case toArgs rest of
            Left err -> io (hPutStrLn stderr err)
            Right args -> setArgs args
    Right ("prog",   rest) ->
        case toArgs rest of
            Right [prog] -> setProg prog
            _ -> io (hPutStrLn stderr "syntax: :set prog <progname>")
    Right ("prompt", rest) -> setPrompt $ dropWhile isSpace rest
    Right ("editor", rest) -> setEditor $ dropWhile isSpace rest
    Right ("stop",   rest) -> setStop   $ dropWhile isSpace rest
    _ -> case toArgs str of
         Left err -> io (hPutStrLn stderr err)
         Right wds -> setOptions wds

setArgs, setOptions :: [String] -> GHCi ()
setProg, setEditor, setStop, setPrompt :: String -> GHCi ()

setArgs args = do
  st <- getGHCiState
  setGHCiState st{ args = args }

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

setPrompt value = do
  st <- getGHCiState
  if null value
      then io $ hPutStrLn stderr $ "syntax: :set prompt <prompt>, currently \"" ++ prompt st ++ "\""
      else case value of
           '\"' : _ -> case reads value of
                       [(value', xs)] | all isSpace xs ->
                           setGHCiState (st { prompt = value' })
                       _ ->
                           io $ hPutStrLn stderr "Can't parse prompt string. Use Haskell syntax."
           _ -> setGHCiState (st { prompt = value })

setOptions wds =
   do -- first, deal with the GHCi opts (+s, +t, etc.)
      let (plus_opts, minus_opts)  = partitionWith isPlus wds
      mapM_ setOpt plus_opts
      -- then, dynamic flags
      newDynFlags minus_opts

newDynFlags :: [String] -> GHCi ()
newDynFlags minus_opts = do
      dflags <- getDynFlags
      let pkg_flags = packageFlags dflags
      (dflags', leftovers, warns) <- io $ GHC.parseDynamicFlags dflags $ map noLoc minus_opts
      handleFlagWarnings dflags' warns

      if (not (null leftovers))
        then ghcError $ errorsToGhcException leftovers
        else return ()

      new_pkgs <- setDynFlags dflags'

      -- if the package flags changed, we should reset the context
      -- and link the new packages.
      dflags <- getDynFlags
      when (packageFlags dflags /= pkg_flags) $ do
        io $ hPutStrLn stderr "package flags have changed, resetting and loading new packages..."
        GHC.setTargets []
        GHC.load LoadAllTargets
        io (linkPackages dflags new_pkgs)
        -- package flags changed, we can't re-use any of the old context
        setContextAfterLoad ([],[]) False []
      return ()


unsetOptions :: String -> GHCi ()
unsetOptions str
  = do -- first, deal with the GHCi opts (+s, +t, etc.)
       let opts = words str
	   (minus_opts, rest1) = partition isMinus opts
	   (plus_opts, rest2)  = partitionWith isPlus rest1

       if (not (null rest2)) 
	  then io (putStrLn ("unknown option: '" ++ head rest2 ++ "'"))
	  else do

       mapM_ unsetOpt plus_opts
 
       let no_flag ('-':'f':rest) = return ("-fno-" ++ rest)
           no_flag f = ghcError (ProgramError ("don't know how to reverse " ++ f))

       no_flags <- mapM no_flag minus_opts
       newDynFlags no_flags

isMinus :: String -> Bool
isMinus ('-':_) = True
isMinus _ = False

isPlus :: String -> Either String String
isPlus ('+':opt) = Left opt
isPlus other     = Right other

setOpt, unsetOpt :: String -> GHCi ()

setOpt str
  = case strToGHCiOpt str of
	Nothing -> io (putStrLn ("unknown option: '" ++ str ++ "'"))
	Just o  -> setOption o

unsetOpt str
  = case strToGHCiOpt str of
	Nothing -> io (putStrLn ("unknown option: '" ++ str ++ "'"))
	Just o  -> unsetOption o

strToGHCiOpt :: String -> (Maybe GHCiOption)
strToGHCiOpt "s" = Just ShowTiming
strToGHCiOpt "t" = Just ShowType
strToGHCiOpt "r" = Just RevertCAFs
strToGHCiOpt _   = Nothing

optToStr :: GHCiOption -> String
optToStr ShowTiming = "s"
optToStr ShowType   = "t"
optToStr RevertCAFs = "r"

-- ---------------------------------------------------------------------------
-- code for `:show'

showCmd :: String -> GHCi ()
showCmd str = do
  st <- getGHCiState
  case words str of
        ["args"]     -> io $ putStrLn (show (args st))
        ["prog"]     -> io $ putStrLn (show (progname st))
        ["prompt"]   -> io $ putStrLn (show (prompt st))
        ["editor"]   -> io $ putStrLn (show (editor st))
        ["stop"]     -> io $ putStrLn (show (stop st))
	["modules" ] -> showModules
	["bindings"] -> showBindings
	["linker"]   -> io showLinkerState
        ["breaks"]   -> showBkptTable
        ["context"]  -> showContext
        ["packages"]  -> showPackages
        ["languages"]  -> showLanguages
	_ -> ghcError (CmdLineError ("syntax:  :show [ args | prog | prompt | editor | stop | modules | bindings\n"++
                                     "               | breaks | context | packages | languages ]"))

showModules :: GHCi ()
showModules = do
  loaded_mods <- getLoadedModules
        -- we want *loaded* modules only, see #1734
  let show_one ms = do m <- GHC.showModule ms; io (putStrLn m)
  mapM_ show_one loaded_mods

getLoadedModules :: GHCi [GHC.ModSummary]
getLoadedModules = do
  graph <- GHC.getModuleGraph
  filterM (GHC.isLoaded . GHC.ms_mod_name) graph

showBindings :: GHCi ()
showBindings = do
  bindings <- GHC.getBindings
  docs     <- pprTypeAndContents
                  [ id | AnId id <- sortBy compareTyThings bindings]
  printForUserPartWay docs

compareTyThings :: TyThing -> TyThing -> Ordering
t1 `compareTyThings` t2 = getName t1 `compareNames` getName t2

printTyThing :: TyThing -> GHCi ()
printTyThing tyth = do dflags <- getDynFlags
                       let pefas = dopt Opt_PrintExplicitForalls dflags
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
   pp_resume resume =
        ptext (sLit "--> ") <> text (GHC.resumeStmt resume)
        $$ nest 2 (ptext (sLit "Stopped at") <+> ppr (GHC.resumeSpan resume))

showPackages :: GHCi ()
showPackages = do
  pkg_flags <- fmap packageFlags getDynFlags
  io $ putStrLn $ showSDoc $ vcat $
    text ("active package flags:"++if null pkg_flags then " none" else "")
    : map showFlag pkg_flags
  pkg_ids <- fmap (preloadPackages . pkgState) getDynFlags
  io $ putStrLn $ showSDoc $ vcat $
    text "packages currently loaded:" 
    : map (nest 2 . text . packageIdString) 
               (sortBy (compare `on` packageIdFS) pkg_ids)
  where showFlag (ExposePackage p) = text $ "  -package " ++ p
        showFlag (HidePackage p)   = text $ "  -hide-package " ++ p
        showFlag (IgnorePackage p) = text $ "  -ignore-package " ++ p

showLanguages :: GHCi ()
showLanguages = do
   dflags <- getDynFlags
   io $ putStrLn $ showSDoc $ vcat $
      text "active language flags:" :
      [text ("  -X" ++ str) | (str, f, _) <- DynFlags.xFlags, dopt f dflags]

-- -----------------------------------------------------------------------------
-- Completion

completeNone :: String -> IO [String]
completeNone _w = return []

completeMacro, completeIdentifier, completeModule,
    completeHomeModule, completeSetOptions, completeFilename,
    completeHomeModuleOrFile 
    :: String -> IO [String]

#ifdef USE_EDITLINE
completeWord :: String -> Int -> Int -> IO (Maybe (String, [String]))
completeWord w start end = do
  line <- Readline.getLineBuffer
  let line_words = words (dropWhile isSpace line)
  case w of
     ':':_ | all isSpace (take (start-1) line) -> wrapCompleter completeCmd w
     _other
	| ((':':c) : _) <- line_words -> do
           completionVars <- lookupCompletionVars c
	   case completionVars of
	     (Nothing,complete) -> wrapCompleter complete w
	     (Just breakChars,complete) 
                    -> let (n,w') = selectWord 
                                        (words' (`elem` breakChars) 0 line)
                           complete' w = do rets <- complete w
                                            return (map (drop n) rets)
                       in wrapCompleter complete' w'
        | ("import" : _) <- line_words ->
                wrapCompleter completeModule w
	| otherwise     -> do
		--printf "complete %s, start = %d, end = %d\n" w start end
		wrapCompleter completeIdentifier w
    where words' _ _ [] = []
          words' isBreak n str = let (w,r) = break isBreak str
                                     (s,r') = span isBreak r
                                 in (n,w):words' isBreak (n+length w+length s) r'
          -- In a Haskell expression we want to parse 'a-b' as three words
          -- where a compiler flag (e.g. -ddump-simpl) should
          -- only be a single word.
          selectWord [] = (0,w)
          selectWord ((offset,x):xs)
              | offset+length x >= start = (start-offset,take (end-offset) x)
              | otherwise = selectWord xs
          
          lookupCompletionVars ('!':_) = return (Just filenameWordBreakChars,
                                            completeFilename)
          lookupCompletionVars c = do
              maybe_cmd <- lookupCommand' c
              case maybe_cmd of
                  Just (_,_,ws,f) -> return (ws,f)
                  Nothing -> return (Just filenameWordBreakChars,
                                        completeFilename)


completeCmd :: String -> IO [String]
completeCmd w = do
  cmds <- readIORef macros_ref
  return (filter (w `isPrefixOf`) (map (':':) 
             (map cmdName (builtin_commands ++ cmds))))

completeMacro w = do
  cmds <- readIORef macros_ref
  return (filter (w `isPrefixOf`) (map cmdName cmds))

completeIdentifier w = do
  rdrs <- withRestoredSession GHC.getRdrNamesInScope
  return (filter (w `isPrefixOf`) (map (showSDoc.ppr) rdrs))

completeModule w = do
  dflags <- withRestoredSession GHC.getSessionDynFlags
  let pkg_mods = allExposedModules dflags
  return (filter (w `isPrefixOf`) (map (showSDoc.ppr) pkg_mods))

completeHomeModule w = do
  g <- withRestoredSession GHC.getModuleGraph
  let home_mods = map GHC.ms_mod_name g
  return (filter (w `isPrefixOf`) (map (showSDoc.ppr) home_mods))

completeSetOptions w = do
  return (filter (w `isPrefixOf`) options)
    where options = "args":"prog":flagList
          flagList = map head $ group $ sort allFlags

completeFilename w = do
    ws <- Readline.filenameCompletionFunction w
    case ws of
        -- If we only found one result, and it's a directory, 
        -- add a trailing slash.
        [file] -> do
                isDir <- expandPathIO file >>= doesDirectoryExist
                if isDir && last file /= '/'
                    then return [file ++ "/"]
                    else return [file]
        _ -> return ws
                

completeHomeModuleOrFile = unionComplete completeHomeModule completeFilename

unionComplete :: (String -> IO [String]) -> (String -> IO [String]) -> String -> IO [String]
unionComplete f1 f2 w = do
  s1 <- f1 w
  s2 <- f2 w
  return (s1 ++ s2)

wrapCompleter :: (String -> IO [String]) -> String -> IO (Maybe (String,[String]))
wrapCompleter fun w =  do
  strs <- fun w
  case strs of
    []  -> Readline.setAttemptedCompletionOver True >> return Nothing
    [x] -> -- Add a trailing space, unless it already has an appended slash.
           let appended = if last x == '/' then x else x ++ " "
           in return (Just (appended,[]))
    xs  -> case getCommonPrefix xs of
		""   -> return (Just ("",xs))
		pref -> return (Just (pref,xs))

getCommonPrefix :: [String] -> String
getCommonPrefix [] = ""
getCommonPrefix (s:ss) = foldl common s ss
  where common _s "" = ""
	common "" _s = ""
	common (c:cs) (d:ds)
	   | c == d = c : common cs ds
	   | otherwise = ""

allExposedModules :: DynFlags -> [ModuleName]
allExposedModules dflags 
 = concat (map exposedModules (filter exposed (eltsUFM pkg_db)))
 where
  pkg_db = pkgIdMap (pkgState dflags)
#else
completeMacro      = completeNone
completeIdentifier = completeNone
completeModule     = completeNone
completeHomeModule = completeNone
completeSetOptions = completeNone
completeFilename   = completeNone
completeHomeModuleOrFile=completeNone
#endif

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
  io installSignalHandlers
  ghciHandle handler (showException exception >> return False)

showException :: SomeException -> GHCi ()
showException se =
  io $ case fromException se of
       Just Interrupted         -> putStrLn "Interrupted."
       -- omit the location for CmdLineError:
       Just (CmdLineError s)    -> putStrLn s
       -- ditto:
       Just ph@(PhaseFailed {}) -> putStrLn (showGhcException ph "")
       Just other_ghc_ex        -> print other_ghc_ex
       Nothing                  -> putStrLn ("*** Exception: " ++ show se)

-----------------------------------------------------------------------------
-- recursive exception handlers

-- Don't forget to unblock async exceptions in the handler, or if we're
-- in an exception loop (eg. let a = error a in a) the ^C exception
-- may never be delivered.  Thanks to Marcin for pointing out the bug.

ghciHandle :: (SomeException -> GHCi a) -> GHCi a -> GHCi a
ghciHandle h (GHCi m) = GHCi $ \s -> 
   gcatch (m s)
	(\e -> unGHCi (ghciUnblock (h e)) s)

ghciUnblock :: GHCi a -> GHCi a
ghciUnblock (GHCi a) =
    GHCi $ \s -> reifyGhc $ \gs ->
                   Exception.unblock (reflectGhc (a s) gs)

ghciTry :: GHCi a -> GHCi (Either SomeException a)
ghciTry (GHCi m) = GHCi $ \s -> gtry (m s)

-- ----------------------------------------------------------------------------
-- Utils

expandPath :: String -> GHCi String
expandPath path = io (expandPathIO path)

expandPathIO :: String -> IO String
expandPathIO path = 
  case dropWhile isSpace path of
   ('~':d) -> do
	tilde <- getHomeDirectory -- will fail if HOME not defined
	return (tilde ++ '/':d)
   other -> 
	return other

wantInterpretedModule :: String -> GHCi Module
wantInterpretedModule str = do
   modl <- lookupModule str
   dflags <- getDynFlags
   when (GHC.modulePackageId modl /= thisPackage dflags) $
      ghcError (CmdLineError ("module '" ++ str ++ "' is from another package;\nthis command requires an interpreted module"))
   is_interpreted <- GHC.moduleIsInterpreted modl
   when (not is_interpreted) $
       ghcError (CmdLineError ("module '" ++ str ++ "' is not interpreted; try \':add *" ++ str ++ "' first"))
   return modl

wantNameFromInterpretedModule :: (Name -> SDoc -> GHCi ()) -> String
                              -> (Name -> GHCi ())
                              -> GHCi ()
wantNameFromInterpretedModule noCanDo str and_then =
  handleSourceError (GHC.printExceptionAndWarnings) $ do
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
stepCmd []         = doContinue (const True) GHC.SingleStep
stepCmd expression = do runStmt expression GHC.SingleStep; return ()

stepLocalCmd :: String -> GHCi ()
stepLocalCmd  [] = do 
  mb_span <- getCurrentBreakSpan
  case mb_span of
    Nothing  -> stepCmd []
    Just loc -> do
       Just mod <- getCurrentBreakModule
       current_toplevel_decl <- enclosingTickSpan mod loc
       doContinue (`isSubspanOf` current_toplevel_decl) GHC.SingleStep

stepLocalCmd expression = stepCmd expression

stepModuleCmd :: String -> GHCi ()
stepModuleCmd  [] = do 
  mb_span <- getCurrentBreakSpan
  case mb_span of
    Nothing  -> stepCmd []
    Just _ -> do
       Just span <- getCurrentBreakSpan
       let f some_span = srcSpanFileName_maybe span == srcSpanFileName_maybe some_span
       doContinue f GHC.SingleStep

stepModuleCmd expression = stepCmd expression

-- | Returns the span of the largest tick containing the srcspan given
enclosingTickSpan :: Module -> SrcSpan -> GHCi SrcSpan
enclosingTickSpan mod src = do
  ticks <- getTickArray mod
  let line = srcSpanStartLine src
  ASSERT (inRange (bounds ticks) line) do
  let enclosing_spans = [ span | (_,span) <- ticks ! line
                               , srcSpanEnd span >= srcSpanEnd src]
  return . head . sortBy leftmost_largest $ enclosing_spans

traceCmd :: String -> GHCi ()
traceCmd []         = doContinue (const True) GHC.RunAndLogSteps
traceCmd expression = do runStmt expression GHC.RunAndLogSteps; return ()

continueCmd :: String -> GHCi ()
continueCmd = noArgs $ doContinue (const True) GHC.RunToCompletion

-- doContinue :: SingleStep -> GHCi ()
doContinue :: (SrcSpan -> Bool) -> SingleStep -> GHCi ()
doContinue pred step = do 
  runResult <- resume step
  afterRunStmt pred runResult
  return ()

abandonCmd :: String -> GHCi ()
abandonCmd = noArgs $ do
  b <- GHC.abandon -- the prompt will change to indicate the new context
  when (not b) $ io $ putStrLn "There is no computation running."
  return ()

deleteCmd :: String -> GHCi ()
deleteCmd argLine = do
   deleteSwitch $ words argLine
   where
   deleteSwitch :: [String] -> GHCi ()
   deleteSwitch [] = 
      io $ putStrLn "The delete command requires at least one argument."
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
  | otherwise       = io $ putStrLn "Syntax:  :history [num]"
  where
  history num = do
    resumes <- GHC.getResumeContext
    case resumes of
      [] -> io $ putStrLn "Not stopped at a breakpoint"
      (r:_) -> do
        let hist = GHC.resumeHistory r
            (took,rest) = splitAt num hist
        case hist of
          [] -> io $ putStrLn $ 
                   "Empty history. Perhaps you forgot to use :trace?"
          _  -> do
                 spans <- mapM GHC.getHistorySpan took
                 let nums  = map (printf "-%-3d:") [(1::Int)..]
                     names = map GHC.historyEnclosingDecl took
                 printForUser (vcat(zipWith3 
                                 (\x y z -> x <+> y <+> z) 
                                 (map text nums) 
                                 (map (bold . ppr) names)
                                 (map (parens . ppr) spans)))
                 io $ putStrLn $ if null rest then "<end of history>" else "..."

bold :: SDoc -> SDoc
bold c | do_bold   = text start_bold <> c <> text end_bold
       | otherwise = c

backCmd :: String -> GHCi ()
backCmd = noArgs $ do
  (names, _, span) <- GHC.back
  printForUser $ ptext (sLit "Logged breakpoint at") <+> ppr span
  printTypeOfNames names
   -- run the command set with ":set stop <cmd>"
  st <- getGHCiState
  enqueueCommands [stop st]

forwardCmd :: String -> GHCi ()
forwardCmd = noArgs $ do
  (names, ix, span) <- GHC.forward
  printForUser $ (if (ix == 0)
                    then ptext (sLit "Stopped at")
                    else ptext (sLit "Logged breakpoint at")) <+> ppr span
  printTypeOfNames names
   -- run the command set with ":set stop <cmd>"
  st <- getGHCiState
  enqueueCommands [stop st]

-- handle the "break" command
breakCmd :: String -> GHCi ()
breakCmd argLine = do
   breakSwitch $ words argLine

breakSwitch :: [String] -> GHCi ()
breakSwitch [] = do
   io $ putStrLn "The break command requires at least one argument."
breakSwitch (arg1:rest)
   | looksLikeModuleName arg1 && not (null rest) = do
        mod <- wantInterpretedModule arg1
        breakByModule mod rest
   | all isDigit arg1 = do
        (toplevel, _) <- GHC.getContext
        case toplevel of
           (mod : _) -> breakByModuleLine mod (read arg1) rest
           [] -> do 
              io $ putStrLn "Cannot find default module for breakpoint." 
              io $ putStrLn "Perhaps no modules are loaded for debugging?"
   | otherwise = do -- try parsing it as an identifier
        wantNameFromInterpretedModule noCanDo arg1 $ \name -> do
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        if GHC.isGoodSrcLoc loc
               then ASSERT( isExternalName name ) 
	       	    findBreakAndSet (GHC.nameModule name) $ 
                         findBreakByCoord (Just (GHC.srcLocFile loc))
                                          (GHC.srcLocLine loc, 
                                           GHC.srcLocCol loc)
               else noCanDo name $ text "can't find its location: " <> ppr loc
       where
          noCanDo n why = printForUser $
                text "cannot set breakpoint on " <> ppr n <> text ": " <> why

breakByModule :: Module -> [String] -> GHCi () 
breakByModule mod (arg1:rest)
   | all isDigit arg1 = do  -- looks like a line number
        breakByModuleLine mod (read arg1) rest
breakByModule _ _
   = breakSyntax

breakByModuleLine :: Module -> Int -> [String] -> GHCi ()
breakByModuleLine mod line args
   | [] <- args = findBreakAndSet mod $ findBreakByLine line
   | [col] <- args, all isDigit col =
        findBreakAndSet mod $ findBreakByCoord Nothing (line, read col)
   | otherwise = breakSyntax

breakSyntax :: a
breakSyntax = ghcError (CmdLineError "Syntax: :break [<mod>] <line> [<column>]")

findBreakAndSet :: Module -> (TickArray -> Maybe (Int, SrcSpan)) -> GHCi ()
findBreakAndSet mod lookupTickTree = do 
   tickArray <- getTickArray mod
   (breakArray, _) <- getModBreak mod
   case lookupTickTree tickArray of 
      Nothing  -> io $ putStrLn $ "No breakpoints found at that location."
      Just (tick, span) -> do
         success <- io $ setBreakFlag True breakArray tick 
         if success 
            then do
               (alreadySet, nm) <- 
                     recordBreak $ BreakLocation
                             { breakModule = mod
                             , breakLoc = span
                             , breakTick = tick
                             , onBreakCmd = ""
                             }
               printForUser $
                  text "Breakpoint " <> ppr nm <>
                  if alreadySet 
                     then text " was already set at " <> ppr span
                     else text " activated at " <> ppr span
            else do
            printForUser $ text "Breakpoint could not be activated at" 
                                 <+> ppr span

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
    listToMaybe (sortBy (leftmost_largest `on` snd)  complete)   `mplus`
    listToMaybe (sortBy (leftmost_smallest `on` snd) incomplete) `mplus`
    listToMaybe (sortBy (rightmost `on` snd) ticks)
  where 
        ticks = arr ! line

        starts_here = [ tick | tick@(_,span) <- ticks,
                               GHC.srcSpanStartLine span == line ]

        (complete,incomplete) = partition ends_here starts_here
            where ends_here (_,span) = GHC.srcSpanEndLine span == line

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
        contains = [ tick | tick@(_,span) <- ticks, span `spans` (line,col),
                            is_correct_file span ]

        is_correct_file span
                 | Just f <- mb_file = GHC.srcSpanFile span == f
                 | otherwise         = True

        after_here = [ tick | tick@(_,span) <- ticks,
                              GHC.srcSpanStartLine span == line,
                              GHC.srcSpanStartCol span >= col ]

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

listCmd :: String -> GHCi ()
listCmd "" = do
   mb_span <- getCurrentBreakSpan
   case mb_span of
      Nothing ->
          printForUser $ text "Not stopped at a breakpoint; nothing to list"
      Just span
       | GHC.isGoodSrcSpan span -> io $ listAround span True
       | otherwise ->
          do resumes <- GHC.getResumeContext
             case resumes of
                 [] -> panic "No resumes"
                 (r:_) ->
                     do let traceIt = case GHC.resumeHistory r of
                                      [] -> text "rerunning with :trace,"
                                      _ -> empty
                            doWhat = traceIt <+> text ":back then :list"
                        printForUser (text "Unable to list source for" <+>
                                      ppr span
                                   $$ text "Try" <+> doWhat)
listCmd str = list2 (words str)

list2 :: [String] -> GHCi ()
list2 [arg] | all isDigit arg = do
    (toplevel, _) <- GHC.getContext
    case toplevel of
        [] -> io $ putStrLn "No module to list"
        (mod : _) -> listModuleLine mod (read arg)
list2 [arg1,arg2] | looksLikeModuleName arg1, all isDigit arg2 = do
        mod <- wantInterpretedModule arg1
        listModuleLine mod (read arg2)
list2 [arg] = do
        wantNameFromInterpretedModule noCanDo arg $ \name -> do
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        if GHC.isGoodSrcLoc loc
               then do
                  tickArray <- ASSERT( isExternalName name )
		  	       getTickArray (GHC.nameModule name)
                  let mb_span = findBreakByCoord (Just (GHC.srcLocFile loc))
                                        (GHC.srcLocLine loc, GHC.srcLocCol loc)
                                        tickArray
                  case mb_span of
                    Nothing       -> io $ listAround (GHC.srcLocSpan loc) False
                    Just (_,span) -> io $ listAround span False
               else
                  noCanDo name $ text "can't find its location: " <>
                                 ppr loc
    where
        noCanDo n why = printForUser $
            text "cannot list source code for " <> ppr n <> text ": " <> why
list2  _other = 
        io $ putStrLn "syntax:  :list [<line> | <module> <line> | <identifier>]"

listModuleLine :: Module -> Int -> GHCi ()
listModuleLine modl line = do
   graph <- GHC.getModuleGraph
   let this = filter ((== modl) . GHC.ms_mod) graph
   case this of
     [] -> panic "listModuleLine"
     summ:_ -> do
           let filename = expectJust "listModuleLine" (ml_hs_file (GHC.ms_location summ))
               loc = GHC.mkSrcLoc (mkFastString (filename)) line 0
           io $ listAround (GHC.srcLocSpan loc) False

-- | list a section of a source file around a particular SrcSpan.
-- If the highlight flag is True, also highlight the span using
-- start_bold\/end_bold.
listAround :: SrcSpan -> Bool -> IO ()
listAround span do_highlight = do
      contents <- BS.readFile (unpackFS file)
      let 
          lines = BS.split '\n' contents
          these_lines = take (line2 - line1 + 1 + pad_before + pad_after) $ 
                        drop (line1 - 1 - pad_before) $ lines
          fst_line = max 1 (line1 - pad_before)
          line_nos = [ fst_line .. ]

          highlighted | do_highlight = zipWith highlight line_nos these_lines
                      | otherwise    = [\p -> BS.concat[p,l] | l <- these_lines]

          bs_line_nos = [ BS.pack (show l ++ "  ") | l <- line_nos ]
          prefixed = zipWith ($) highlighted bs_line_nos
      --
      BS.putStrLn (BS.intercalate (BS.pack "\n") prefixed)
  where
        file  = GHC.srcSpanFile span
        line1 = GHC.srcSpanStartLine span
        col1  = GHC.srcSpanStartCol span
        line2 = GHC.srcSpanEndLine span
        col2  = GHC.srcSpanEndCol span

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
        [ (line, (nm,span)) | (nm,span) <- ticks,
                              line <- srcSpanLines span ]
    where
        max_line = foldr max 0 (map GHC.srcSpanEndLine (map snd ticks))
        srcSpanLines span = [ GHC.srcSpanStartLine span .. 
                              GHC.srcSpanEndLine span ]

lookupModule :: String -> GHCi Module
lookupModule modName
   = GHC.findModule (GHC.mkModuleName modName) Nothing

-- don't reset the counter back to zero?
discardActiveBreakPoints :: GHCi ()
discardActiveBreakPoints = do
   st <- getGHCiState
   mapM (turnOffBreak.snd) (breaks st)
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
           mapM (turnOffBreak.snd) this
           setGHCiState $ st { breaks = rest }

turnOffBreak :: BreakLocation -> GHCi Bool
turnOffBreak loc = do
  (arr, _) <- getModBreak (breakModule loc)
  io $ setBreakFlag False arr (breakTick loc)

getModBreak :: Module -> GHCi (GHC.BreakArray, Array Int SrcSpan)
getModBreak mod = do
   Just mod_info <- GHC.getModuleInfo mod
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let array      = GHC.modBreaks_flags modBreaks
   let ticks      = GHC.modBreaks_locs  modBreaks
   return (array, ticks)

setBreakFlag :: Bool -> GHC.BreakArray -> Int -> IO Bool 
setBreakFlag toggle array index
   | toggle    = GHC.setBreakOn array index 
   | otherwise = GHC.setBreakOff array index
