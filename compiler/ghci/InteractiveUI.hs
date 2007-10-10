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

import GhciMonad
import GhciTags
import Debugger

-- The GHC interface
import qualified GHC
import GHC              ( Session, LoadHowMuch(..), Target(..),  TargetId(..),
                          Module, ModuleName, TyThing(..), Phase,
                          BreakIndex, SrcSpan, Resume, SingleStep )
import PprTyThing
import DynFlags

#ifdef USE_READLINE
import Packages
import PackageConfig
import UniqFM
#endif

import HscTypes		( implicitTyThings )
import Outputable       hiding (printForUser)
import Module           -- for ModuleEnv
import Name
import SrcLoc

-- Other random utilities
import Digraph
import BasicTypes hiding (isTopLevel)
import Panic      hiding (showException)
import Config
import StaticFlags
import Linker
import Util
import NameSet
import Maybes		( orElse )
import FastString

#ifndef mingw32_HOST_OS
import System.Posix hiding (getEnv)
#else
import GHC.ConsoleHandler ( flushConsole )
import System.Win32	  ( setConsoleCP, setConsoleOutputCP )
import qualified System.Win32
#endif

#ifdef USE_READLINE
import Control.Concurrent	( yield )	-- Used in readline loop
import System.Console.Readline as Readline
#endif

--import SystemExts

import Control.Exception as Exception
-- import Control.Concurrent

import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe
import System.Cmd
import System.Environment
import System.Exit	( exitWith, ExitCode(..) )
import System.Directory
import System.IO
import System.IO.Error as IO
import System.IO.Unsafe
import Data.Char
import Data.Dynamic
import Data.Array
import Control.Monad as Monad
import Text.Printf

import Foreign.StablePtr	( newStablePtr )
import GHC.Exts		( unsafeCoerce# )
import GHC.IOBase	( IOErrorType(InvalidArgument) )

import Data.IORef	( IORef, readIORef, writeIORef )

#ifdef USE_READLINE
import System.Posix.Internals ( setNonBlockingFD )
#endif

-----------------------------------------------------------------------------

ghciWelcomeMsg :: String
ghciWelcomeMsg = "GHCi, version " ++ cProjectVersion ++
                 ": http://www.haskell.org/ghc/  :? for help"

type Command = (String, String -> GHCi Bool, Bool, String -> IO [String])

cmdName :: Command -> String
cmdName (n,_,_,_) = n

commands :: IORef [Command]
GLOBAL_VAR(commands, builtin_commands, [Command])

builtin_commands :: [Command]
builtin_commands = [
	-- Hugs users are accustomed to :e, so make sure it doesn't overlap
  ("?",		keepGoing help,			False, completeNone),
  ("add",	keepGoingPaths addModule,	False, completeFilename),
  ("abandon",   keepGoing abandonCmd,           False, completeNone),
  ("break",     keepGoing breakCmd,             False, completeIdentifier),
  ("back",      keepGoing backCmd,              False, completeNone),
  ("browse",    keepGoing browseCmd,		False, completeModule),
  ("cd",    	keepGoing changeDirectory,	False, completeFilename),
  ("check",	keepGoing checkModule,		False, completeHomeModule),
  ("continue",  keepGoing continueCmd,          False, completeNone),
  ("cmd",       keepGoing cmdCmd,               False, completeIdentifier),
  ("ctags",	keepGoing createCTagsFileCmd, 	False, completeFilename),
  ("def",	keepGoing defineMacro,		False, completeIdentifier),
  ("delete",    keepGoing deleteCmd,            False, completeNone),
  ("e", 	keepGoing editFile,		False, completeFilename),
  ("edit",	keepGoing editFile,		False, completeFilename),
  ("etags",	keepGoing createETagsFileCmd,	False, completeFilename),
  ("force",     keepGoing forceCmd,             False, completeIdentifier),
  ("forward",   keepGoing forwardCmd,           False, completeNone),
  ("help",	keepGoing help,			False, completeNone),
  ("history",   keepGoing historyCmd,           False, completeNone), 
  ("info",      keepGoing info,			False, completeIdentifier),
  ("kind",	keepGoing kindOfType,		False, completeIdentifier),
  ("load",	keepGoingPaths loadModule_,     False, completeHomeModuleOrFile),
  ("list",	keepGoing listCmd,              False, completeNone),
  ("module",	keepGoing setContext,		False, completeModule),
  ("main",	keepGoing runMain,		False, completeIdentifier),
  ("print",     keepGoing printCmd,             False, completeIdentifier),
  ("quit",	quit,				False, completeNone),
  ("reload", 	keepGoing reloadModule,  	False, completeNone),
  ("set",	keepGoing setCmd,		True,  completeSetOptions),
  ("show",	keepGoing showCmd,		False, completeNone),
  ("sprint",    keepGoing sprintCmd,            False, completeIdentifier),
  ("step",      keepGoing stepCmd,              False, completeIdentifier), 
  ("steplocal", keepGoing stepLocalCmd,         False, completeIdentifier), 
  ("stepmodule",keepGoing stepModuleCmd,        False, completeIdentifier), 
  ("type",	keepGoing typeOfExpr,		False, completeIdentifier),
  ("trace",     keepGoing traceCmd,             False, completeIdentifier), 
  ("undef",     keepGoing undefineMacro,	False, completeMacro),
  ("unset",	keepGoing unsetOptions,		True,  completeSetOptions)
  ]

keepGoing :: (String -> GHCi ()) -> (String -> GHCi Bool)
keepGoing a str = a str >> return False

keepGoingPaths :: ([FilePath] -> GHCi ()) -> (String -> GHCi Bool)
keepGoingPaths a str = a (toArgs str) >> return False

shortHelpText :: String
shortHelpText = "use :? for help.\n"

helpText :: String
helpText =
 " Commands available from the prompt:\n" ++
 "\n" ++
 "   <statement>                 evaluate/run <statement>\n" ++
 "   :add <filename> ...         add module(s) to the current target set\n" ++
 "   :browse [*]<module>         display the names defined by <module>\n" ++
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
 "   :load <filename> ...        load module(s) and their dependents\n" ++
 "   :main [<arguments> ...]     run the main function with the given arguments\n" ++
 "   :module [+/-] [*]<mod> ...  set the context for expression evaluation\n" ++
 "   :quit                       exit GHCi\n" ++
 "   :reload                     reload the current module set\n" ++
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
 "   :history [<n>]              show the last <n> items in the history (after :trace)\n" ++
 "   :print [<name> ...]         prints a value without forcing its computation\n" ++
 "   :sprint [<name> ...]        simplifed version of :print\n" ++
 "   :step                       single-step after stopping at a breakpoint\n"++
 "   :step <expr>                single-step into <expr>\n"++
 "   :steplocal                  single-step restricted to the current top level decl.\n"++
 "   :stepmodule                 single-step restricted to the current module\n"++
 "   :trace                      trace after stopping at a breakpoint\n"++
 "   :trace <expr>               trace into <expr> (remembers breakpoints for :history)\n"++

 "\n" ++
 " -- Commands for changing settings:\n" ++
 "\n" ++
 "   :set <option> ...           set options\n" ++
 "   :set args <arg> ...         set the arguments returned by System.getArgs\n" ++
 "   :set prog <progname>        set the value returned by System.getProgName\n" ++
 "   :set prompt <prompt>        set the prompt used in GHCi\n" ++
 "   :set editor <cmd>           set the command used for :edit\n" ++
 "   :set stop <cmd>             set the command to run when a breakpoint is hit\n" ++
 "   :unset <option> ...         unset options\n" ++
 "\n" ++
 "  Options for ':set' and ':unset':\n" ++
 "\n" ++
 "    +r            revert top-level expressions after each evaluation\n" ++
 "    +s            print timing/memory stats after each evaluation\n" ++
 "    +t            print type after evaluation\n" ++
 "    -<flags>      most GHC command line flags can also be set here\n" ++
 "                         (eg. -v2, -fglasgow-exts, etc.)\n" ++
 "\n" ++
 " -- Commands for displaying information:\n" ++
 "\n" ++
 "   :show bindings              show the current bindings made at the prompt\n" ++
 "   :show breaks                show the active breakpoints\n" ++
 "   :show context               show the breakpoint context\n" ++
 "   :show modules               show the currently loaded modules\n" ++
 "   :show <setting>             show anything that can be set with :set (e.g. args)\n" ++
 "\n" 

findEditor :: IO String
findEditor = do
  getEnv "EDITOR" 
    `IO.catch` \_ -> do
#if mingw32_HOST_OS
	win <- System.Win32.getWindowsDirectory
	return (win `joinFileName` "notepad.exe")
#else
	return ""
#endif

interactiveUI :: Session -> [(FilePath, Maybe Phase)] -> Maybe String -> IO ()
interactiveUI session srcs maybe_expr = do
   -- HACK! If we happen to get into an infinite loop (eg the user
   -- types 'let x=x in x' at the prompt), then the thread will block
   -- on a blackhole, and become unreachable during GC.  The GC will
   -- detect that it is unreachable and send it the NonTermination
   -- exception.  However, since the thread is unreachable, everything
   -- it refers to might be finalized, including the standard Handles.
   -- This sounds like a bug, but we don't have a good solution right
   -- now.
   newStablePtr stdin
   newStablePtr stdout
   newStablePtr stderr

    -- Initialise buffering for the *interpreted* I/O system
   initInterpBuffering session

   when (isNothing maybe_expr) $ do
        -- Only for GHCi (not runghc and ghc -e):

        -- Turn buffering off for the compiled program's stdout/stderr
        turnOffBuffering
        -- Turn buffering off for GHCi's stdout
        hFlush stdout
        hSetBuffering stdout NoBuffering
        -- We don't want the cmd line to buffer any input that might be
        -- intended for the program, so unbuffer stdin.
        hSetBuffering stdin NoBuffering

        -- initial context is just the Prelude
   prel_mod <- GHC.findModule session (GHC.mkModuleName "Prelude") 
                                      (Just basePackageId)
   GHC.setContext session [] [prel_mod]

#ifdef USE_READLINE
   Readline.initialize
   Readline.setAttemptedCompletionFunction (Just completeWord)
   --Readline.parseAndBind "set show-all-if-ambiguous 1"

   let symbols = "!#$%&*+/<=>?@\\^|-~"
       specials = "(),;[]`{}"
       spaces = " \t\n"
       word_break_chars = spaces ++ specials ++ symbols

   Readline.setBasicWordBreakCharacters word_break_chars
   Readline.setCompleterWordBreakCharacters word_break_chars
#endif

   default_editor <- findEditor

   startGHCi (runGHCi srcs maybe_expr)
	GHCiState{ progname = "<interactive>",
		   args = [],
                   prompt = "%s> ",
                   stop = "",
		   editor = default_editor,
		   session = session,
		   options = [],
                   prelude = prel_mod,
                   break_ctr = 0,
                   breaks = [],
                   tickarrays = emptyModuleEnv,
                   cmdqueue = []
                 }

#ifdef USE_READLINE
   Readline.resetTerminal Nothing
#endif

   return ()

runGHCi :: [(FilePath, Maybe Phase)] -> Maybe String -> GHCi ()
runGHCi paths maybe_expr = do
  let read_dot_files = not opt_IgnoreDotGhci

  when (read_dot_files) $ do
    -- Read in ./.ghci.
    let file = "./.ghci"
    exists <- io (doesFileExist file)
    when exists $ do
       dir_ok  <- io (checkPerms ".")
       file_ok <- io (checkPerms file)
       when (dir_ok && file_ok) $ do
  	  either_hdl <- io (IO.try (openFile "./.ghci" ReadMode))
  	  case either_hdl of
  	     Left _e   -> return ()
  	     Right hdl -> fileLoop hdl False
    
  when (read_dot_files) $ do
    -- Read in $HOME/.ghci
    either_dir <- io (IO.try (getEnv "HOME"))
    case either_dir of
       Left _e -> return ()
       Right dir -> do
  	  cwd <- io (getCurrentDirectory)
  	  when (dir /= cwd) $ do
  	     let file = dir ++ "/.ghci"
  	     ok <- io (checkPerms file)
  	     when ok $ do
  	       either_hdl <- io (IO.try (openFile file ReadMode))
  	       case either_hdl of
  		  Left _e   -> return ()
  		  Right hdl -> fileLoop hdl False

  -- Perform a :load for files given on the GHCi command line
  -- When in -e mode, if the load fails then we want to stop
  -- immediately rather than going on to evaluate the expression.
  when (not (null paths)) $ do
     ok <- ghciHandle (\e -> do showException e; return Failed) $ 
		loadModule paths
     when (isJust maybe_expr && failed ok) $
	io (exitWith (ExitFailure 1))

  -- if verbosity is greater than 0, or we are connected to a
  -- terminal, display the prompt in the interactive loop.
  is_tty <- io (hIsTerminalDevice stdin)
  dflags <- getDynFlags
  let show_prompt = verbosity dflags > 0 || is_tty

  case maybe_expr of
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
            -- initialise the console if necessary
            io setUpConsole

            -- enter the interactive loop
            interactiveLoop is_tty show_prompt
        Just expr -> do
            -- just evaluate the expression we were given
            runCommandEval expr
            return ()

  -- and finally, exit
  io $ do when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."


interactiveLoop :: Bool -> Bool -> GHCi ()
interactiveLoop is_tty show_prompt =
  -- Ignore ^C exceptions caught here
  ghciHandleDyn (\e -> case e of 
			Interrupted -> do
#if defined(mingw32_HOST_OS)
				io (putStrLn "")
#endif
				interactiveLoop is_tty show_prompt
			_other      -> return ()) $ 

  ghciUnblock $ do -- unblock necessary if we recursed from the 
		   -- exception handler above.

  -- read commands from stdin
#ifdef USE_READLINE
  if (is_tty) 
	then readlineLoop
	else fileLoop stdin show_prompt
#else
  fileLoop stdin show_prompt
#endif


-- NOTE: We only read .ghci files if they are owned by the current user,
-- and aren't world writable.  Otherwise, we could be accidentally 
-- running code planted by a malicious third party.

-- Furthermore, We only read ./.ghci if . is owned by the current user
-- and isn't writable by anyone else.  I think this is sufficient: we
-- don't need to check .. and ../.. etc. because "."  always refers to
-- the same directory while a process is running.

checkPerms :: String -> IO Bool
checkPerms name =
#ifdef mingw32_HOST_OS
  return True
#else
  Util.handle (\_ -> return False) $ do
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

fileLoop :: Handle -> Bool -> GHCi ()
fileLoop hdl show_prompt = do
   when show_prompt $ do
        prompt <- mkPrompt
        (io (putStr prompt))
   l <- io (IO.try (hGetLine hdl))
   case l of
	Left e | isEOFError e		   -> return ()
	       | InvalidArgument <- etype  -> return ()
	       | otherwise		   -> io (ioError e)
		where etype = ioeGetErrorType e
		-- treat InvalidArgument in the same way as EOF:
		-- this can happen if the user closed stdin, or
		-- perhaps did getContents which closes stdin at
		-- EOF.
	Right l -> 
	  case removeSpaces l of
            "" -> fileLoop hdl show_prompt
	    l  -> do quit <- runCommands l
                     if quit then return () else fileLoop hdl show_prompt

mkPrompt :: GHCi String
mkPrompt = do
  session <- getSession
  (toplevs,exports) <- io (GHC.getContext session)
  resumes <- io $ GHC.getResumeContext session

  context_bit <-
        case resumes of
            [] -> return empty
            r:_ -> do
                let ix = GHC.resumeHistoryIx r
                if ix == 0
                   then return (brackets (ppr (GHC.resumeSpan r)) <> space)
                   else do
                        let hist = GHC.resumeHistory r !! (ix-1)
                        span <- io$ GHC.getHistorySpan session hist
                        return (brackets (ppr (negate ix) <> char ':' 
                                          <+> ppr span) <> space)
  let
        dots | _:rs <- resumes, not (null rs) = text "... "
             | otherwise = empty

        modules_bit = 
             hsep (map (\m -> char '*' <> ppr (GHC.moduleName m)) toplevs) <+>
             hsep (map (ppr . GHC.moduleName) exports)

        deflt_prompt = dots <> context_bit <> modules_bit

        f ('%':'s':xs) = deflt_prompt <> f xs
        f ('%':'%':xs) = char '%' <> f xs
        f (x:xs) = char x <> f xs
        f [] = empty
   --
  st <- getGHCiState
  return (showSDoc (f (prompt st)))


#ifdef USE_READLINE
readlineLoop :: GHCi ()
readlineLoop = do
   io yield
   saveSession -- for use by completion
   prompt <- mkPrompt
   l <- io (readline prompt `finally` setNonBlockingFD 0)
		-- readline sometimes puts stdin into blocking mode,
		-- so we need to put it back for the IO library
   splatSavedSession
   case l of
	Nothing -> return ()
	Just l  ->
	  case removeSpaces l of
	    "" -> readlineLoop
	    l  -> do
        	  io (addHistory l)
  	  	  quit <- runCommands l
          	  if quit then return () else readlineLoop
#endif

runCommands :: String -> GHCi Bool
runCommands cmd = do
        q <- ghciHandle handler (doCommand cmd)
        if q then return True else runNext
  where
       runNext = do
          st <- getGHCiState
          case cmdqueue st of
            []   -> return False
            c:cs -> do setGHCiState st{ cmdqueue = cs }
                       runCommands c

       doCommand (':' : cmd) = specialCommand cmd
       doCommand stmt        = do timeIt $ runStmt stmt GHC.RunToCompletion
                                  return False

enqueueCommands :: [String] -> GHCi ()
enqueueCommands cmds = do
  st <- getGHCiState
  setGHCiState st{ cmdqueue = cmds ++ cmdqueue st }


-- This version is for the GHC command-line option -e.  The only difference
-- from runCommand is that it catches the ExitException exception and
-- exits, rather than printing out the exception.
runCommandEval :: String -> GHCi Bool
runCommandEval c = ghciHandle handleEval (doCommand c)
  where 
    handleEval (ExitException code) = io (exitWith code)
    handleEval e                    = do handler e
				         io (exitWith (ExitFailure 1))

    doCommand (':' : command) = specialCommand command
    doCommand stmt
       = do r <- runStmt stmt GHC.RunToCompletion
	    case r of 
		False -> io (exitWith (ExitFailure 1))
		  -- failure to run the command causes exit(1) for ghc -e.
		_       -> return True

runStmt :: String -> SingleStep -> GHCi Bool
runStmt stmt step
 | null (filter (not.isSpace) stmt) = return False
 | ["import", mod] <- words stmt    = keepGoing setContext ('+':mod)
 | otherwise
 = do st <- getGHCiState
      session <- getSession
      result <- io $ withProgName (progname st) $ withArgs (args st) $
	     	     GHC.runStmt session stmt step
      afterRunStmt (const True) result


--afterRunStmt :: GHC.RunResult -> GHCi Bool
                                 -- False <=> the statement failed to compile
afterRunStmt :: (SrcSpan -> Bool) -> GHC.RunResult -> GHCi Bool
afterRunStmt _ (GHC.RunException e) = throw e
afterRunStmt step_here run_result = do
  session     <- getSession
  resumes <- io $ GHC.getResumeContext session
  case run_result of
     GHC.RunOk names -> do
        show_types <- isOptionSet ShowType
        when show_types $ printTypeOfNames session names
     GHC.RunBreak _ names mb_info 
         | isNothing  mb_info || 
           step_here (GHC.resumeSpan $ head resumes) -> do
               printForUser $ ptext SLIT("Stopped at") <+> 
                       ppr (GHC.resumeSpan $ head resumes)
--               printTypeOfNames session names
               printTypeAndContentOfNames session names
               maybe (return ()) runBreakCmd mb_info
               -- run the command set with ":set stop <cmd>"
               st <- getGHCiState
               enqueueCommands [stop st]
               return ()
         | otherwise -> io(GHC.resume session GHC.SingleStep) >>= 
                        afterRunStmt step_here >> return ()
     _ -> return ()

  flushInterpBuffers
  io installSignalHandlers
  b <- isOptionSet RevertCAFs
  io (when b revertCAFs)

  return (case run_result of GHC.RunOk _ -> True; _ -> False)

      where printTypeAndContentOfNames session names = do
              let namesSorted = sortBy compareNames names
              tythings <- catMaybes `liftM` 
                              io (mapM (GHC.lookupName session) namesSorted)
	      let ids = [id | AnId id <- tythings]
              terms <- mapM (io . GHC.obtainTermB session 10 False) ids
              docs_terms <- mapM (io . showTerm session) terms                                   
	      dflags <- getDynFlags
	      let pefas = dopt Opt_PrintExplicitForalls dflags
              printForUser $ vcat $ zipWith (\ty cts -> ty <+> equals <+> cts)
                                            (map (pprTyThing pefas . AnId) ids)
                                            docs_terms

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

printTypeOfNames :: Session -> [Name] -> GHCi ()
printTypeOfNames session names
 = mapM_ (printTypeOfName session) $ sortBy compareNames names

compareNames :: Name -> Name -> Ordering
n1 `compareNames` n2 = compareWith n1 `compare` compareWith n2
    where compareWith n = (getOccString n, getSrcSpan n)

printTypeOfName :: Session -> Name -> GHCi ()
printTypeOfName session n
   = do maybe_tything <- io (GHC.lookupName session n)
        case maybe_tything of
            Nothing    -> return ()
            Just thing -> printTyThing thing

specialCommand :: String -> GHCi Bool
specialCommand ('!':str) = shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  maybe_cmd <- io (lookupCommand cmd)
  case maybe_cmd of
    Nothing -> io (hPutStr stdout ("unknown command ':" ++ cmd ++ "'\n" 
		                    ++ shortHelpText) >> return False)
    Just (_,f,_,_) -> f (dropWhile isSpace rest)

lookupCommand :: String -> IO (Maybe Command)
lookupCommand str = do
  cmds <- readIORef commands
  -- look for exact match first, then the first prefix match
  case [ c | c <- cmds, str == cmdName c ] of
     c:_ -> return (Just c)
     [] -> case [ c | c@(s,_,_,_) <- cmds, str `isPrefixOf` s ] of
     		[] -> return Nothing
     		c:_ -> return (Just c)


getCurrentBreakSpan :: GHCi (Maybe SrcSpan)
getCurrentBreakSpan = do
  session <- getSession
  resumes <- io $ GHC.getResumeContext session
  case resumes of
    [] -> return Nothing
    (r:_) -> do
        let ix = GHC.resumeHistoryIx r
        if ix == 0
           then return (Just (GHC.resumeSpan r))
           else do
                let hist = GHC.resumeHistory r !! (ix-1)
                span <- io $ GHC.getHistorySpan session hist
                return (Just span)

getCurrentBreakModule :: GHCi (Maybe Module)
getCurrentBreakModule = do
  session <- getSession
  resumes <- io $ GHC.getResumeContext session
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
info "" = throwDyn (CmdLineError "syntax: ':i <thing-you-want-info-about>'")
info s  = do { let names = words s
	     ; session <- getSession
	     ; dflags <- getDynFlags
	     ; let pefas = dopt Opt_PrintExplicitForalls dflags
	     ; mapM_ (infoThing pefas session) names }
  where
    infoThing pefas session str = io $ do
	names     <- GHC.parseName session str
	mb_stuffs <- mapM (GHC.getInfo session) names
	let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
	unqual <- GHC.getPrintUnqual session
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
runMain args = do
  let ss = concat $ intersperse "," (map (\ s -> ('"':s)++"\"") (toArgs args))
  enqueueCommands  ['[': ss ++ "] `System.Environment.withArgs` main"]

addModule :: [FilePath] -> GHCi ()
addModule files = do
  io (revertCAFs)			-- always revert CAFs on load/add.
  files <- mapM expandPath files
  targets <- mapM (\m -> io (GHC.guessTarget m Nothing)) files
  session <- getSession
  io (mapM_ (GHC.addTarget session) targets)
  ok <- io (GHC.load session LoadAllTargets)
  afterLoad ok session

changeDirectory :: String -> GHCi ()
changeDirectory dir = do
  session <- getSession
  graph <- io (GHC.getModuleGraph session)
  when (not (null graph)) $
	io $ putStr "Warning: changing directory causes all loaded modules to be unloaded,\nbecause the search path has changed.\n"
  io (GHC.setTargets session [])
  io (GHC.load session LoadAllTargets)
  setContextAfterLoad session []
  io (GHC.workingDirectoryChanged session)
  dir <- expandPath dir
  io (setCurrentDirectory dir)

editFile :: String -> GHCi ()
editFile str =
  do file <- if null str then chooseEditFile else return str
     st <- getGHCiState
     let cmd = editor st
     when (null cmd) 
       $ throwDyn (CmdLineError "editor not set, use :set editor")
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
  do session <- getSession
     let hasFailed x = io $ fmap not $ GHC.isLoaded session $ GHC.ms_mod_name x

     graph <- io (GHC.getModuleGraph session)
     failed_graph <- filterM hasFailed graph
     let order g  = flattenSCCs $ GHC.topSortModuleGraph True g Nothing
         pick xs  = case xs of
                      x : _ -> GHC.ml_hs_file (GHC.ms_location x)
                      _     -> Nothing

     case pick (order failed_graph) of
       Just file -> return file
       Nothing   -> 
         do targets <- io (GHC.getTargets session)
            case msum (map fromTarget targets) of
              Just file -> return file
              Nothing   -> throwDyn (CmdLineError "No files to edit.")
          
  where fromTarget (GHC.Target (GHC.TargetFile f _) _) = Just f
        fromTarget _ = Nothing -- when would we get a module target?

defineMacro :: String -> GHCi ()
defineMacro s = do
  let (macro_name, definition) = break isSpace s
  cmds <- io (readIORef commands)
  if (null macro_name) 
	then throwDyn (CmdLineError "invalid macro name") 
	else do
  if (macro_name `elem` map cmdName cmds)
	then throwDyn (CmdLineError 
		("command '" ++ macro_name ++ "' is already defined"))
	else do

  -- give the expression a type signature, so we can be sure we're getting
  -- something of the right type.
  let new_expr = '(' : definition ++ ") :: String -> IO String"

  -- compile the expression
  cms <- getSession
  maybe_hv <- io (GHC.compileExpr cms new_expr)
  case maybe_hv of
     Nothing -> return ()
     Just hv -> io (writeIORef commands --
		    (cmds ++ [(macro_name, runMacro hv, False, completeNone)]))

runMacro :: GHC.HValue{-String -> IO String-} -> String -> GHCi Bool
runMacro fun s = do
  str <- io ((unsafeCoerce# fun :: String -> IO String) s)
  enqueueCommands (lines str)
  return False

undefineMacro :: String -> GHCi ()
undefineMacro macro_name = do
  cmds <- io (readIORef commands)
  if (macro_name `elem` map cmdName builtin_commands) 
	then throwDyn (CmdLineError
		("command '" ++ macro_name ++ "' cannot be undefined"))
	else do
  if (macro_name `notElem` map cmdName cmds) 
	then throwDyn (CmdLineError 
		("command '" ++ macro_name ++ "' not defined"))
	else do
  io (writeIORef commands (filter ((/= macro_name) . cmdName) cmds))

cmdCmd :: String -> GHCi ()
cmdCmd str = do
  let expr = '(' : str ++ ") :: IO String"
  session <- getSession
  maybe_hv <- io (GHC.compileExpr session expr)
  case maybe_hv of
    Nothing -> return ()
    Just hv -> do 
        cmds <- io $ (unsafeCoerce# hv :: IO String)
        enqueueCommands (lines cmds)
        return ()

loadModule :: [(FilePath, Maybe Phase)] -> GHCi SuccessFlag
loadModule fs = timeIt (loadModule' fs)

loadModule_ :: [FilePath] -> GHCi ()
loadModule_ fs = do loadModule (zip fs (repeat Nothing)); return ()

loadModule' :: [(FilePath, Maybe Phase)] -> GHCi SuccessFlag
loadModule' files = do
  session <- getSession

  -- unload first
  discardActiveBreakPoints
  io (GHC.setTargets session [])
  io (GHC.load session LoadAllTargets)

  -- expand tildes
  let (filenames, phases) = unzip files
  exp_filenames <- mapM expandPath filenames
  let files' = zip exp_filenames phases
  targets <- io (mapM (uncurry GHC.guessTarget) files')

  -- NOTE: we used to do the dependency anal first, so that if it
  -- fails we didn't throw away the current set of modules.  This would
  -- require some re-working of the GHC interface, so we'll leave it
  -- as a ToDo for now.

  io (GHC.setTargets session targets)
  doLoad session LoadAllTargets

checkModule :: String -> GHCi ()
checkModule m = do
  let modl = GHC.mkModuleName m
  session <- getSession
  result <- io (GHC.checkModule session modl False)
  case result of
    Nothing -> io $ putStrLn "Nothing"
    Just r  -> io $ putStrLn (showSDoc (
	case GHC.checkedModuleInfo r of
	   Just cm | Just scope <- GHC.modInfoTopLevelScope cm -> 
		let
		    (local,global) = partition ((== modl) . GHC.moduleName . GHC.nameModule) scope
		in
			(text "global names: " <+> ppr global) $$
		        (text "local  names: " <+> ppr local)
	   _ -> empty))
  afterLoad (successIf (isJust result)) session

reloadModule :: String -> GHCi ()
reloadModule m = do
  session <- getSession
  doLoad session $ if null m then LoadAllTargets 
                             else LoadUpTo (GHC.mkModuleName m)
  return ()

doLoad :: Session -> LoadHowMuch -> GHCi SuccessFlag
doLoad session howmuch = do
  -- turn off breakpoints before we load: we can't turn them off later, because
  -- the ModBreaks will have gone away.
  discardActiveBreakPoints
  ok <- io (GHC.load session howmuch)
  afterLoad ok session
  return ok

afterLoad :: SuccessFlag -> Session -> GHCi ()
afterLoad ok session = do
  io (revertCAFs)  -- always revert CAFs on load.
  discardTickArrays
  loaded_mods <- getLoadedModules session
  setContextAfterLoad session loaded_mods
  modulesLoadedMsg ok (map GHC.ms_mod_name loaded_mods)

setContextAfterLoad :: Session -> [GHC.ModSummary] -> GHCi ()
setContextAfterLoad session [] = do
  prel_mod <- getPrelude
  io (GHC.setContext session [] [prel_mod])
setContextAfterLoad session ms = do
  -- load a target if one is available, otherwise load the topmost module.
  targets <- io (GHC.getTargets session)
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

   summary `matches` Target (TargetModule m) _
	= GHC.ms_mod_name summary == m
   summary `matches` Target (TargetFile f _) _ 
	| Just f' <- GHC.ml_hs_file (GHC.ms_location summary)	= f == f'
   _ `matches` _
	= False

   load_this summary | m <- GHC.ms_mod summary = do
	b <- io (GHC.moduleIsInterpreted session m)
	if b then io (GHC.setContext session [m] []) 
       	     else do
                   prel_mod <- getPrelude
                   io (GHC.setContext session []  [prel_mod,m])


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
  = do cms <- getSession
       maybe_ty <- io (GHC.exprType cms str)
       case maybe_ty of
	  Nothing -> return ()
	  Just ty -> do dflags <- getDynFlags
		        let pefas = dopt Opt_PrintExplicitForalls dflags
                        printForUser $ text str <+> dcolon
					<+> pprTypeForUser pefas ty

kindOfType :: String -> GHCi ()
kindOfType str 
  = do cms <- getSession
       maybe_ty <- io (GHC.typeKind cms str)
       case maybe_ty of
	  Nothing    -> return ()
	  Just ty    -> printForUser $ text str <+> dcolon <+> ppr ty
          
quit :: String -> GHCi Bool
quit _ = return True

shellEscape :: String -> GHCi Bool
shellEscape str = io (system str >> return False)

-----------------------------------------------------------------------------
-- Browsing a module's contents

browseCmd :: String -> GHCi ()
browseCmd m = 
  case words m of
    ['*':m] | looksLikeModuleName m -> browseModule m False
    [m]     | looksLikeModuleName m -> browseModule m True
    _ -> throwDyn (CmdLineError "syntax:  :browse <module>")

browseModule :: String -> Bool -> GHCi ()
browseModule m exports_only = do
  s <- getSession
  modl <- if exports_only then lookupModule m
                          else wantInterpretedModule m

  -- Temporarily set the context to the module we're interested in,
  -- just so we can get an appropriate PrintUnqualified
  (as,bs) <- io (GHC.getContext s)
  prel_mod <- getPrelude
  io (if exports_only then GHC.setContext s [] [prel_mod,modl]
		      else GHC.setContext s [modl] [])
  unqual <- io (GHC.getPrintUnqual s)
  io (GHC.setContext s as bs)

  mb_mod_info <- io $ GHC.getModuleInfo s modl
  case mb_mod_info of
    Nothing -> throwDyn (CmdLineError ("unknown module: " ++ m))
    Just mod_info -> do
        let names
	       | exports_only = GHC.modInfoExports mod_info
	       | otherwise    = GHC.modInfoTopLevelScope mod_info
				`orElse` []

        mb_things <- io $ mapM (GHC.lookupName s) names
 	let filtered_things = filterOutChildren (\t -> t) (catMaybes mb_things)

        dflags <- getDynFlags
	let pefas = dopt Opt_PrintExplicitForalls dflags
	io (putStrLn (showSDocForUser unqual (
		vcat (map (pprTyThingInContext pefas) filtered_things)
	   )))
	-- ToDo: modInfoInstances currently throws an exception for
	-- package modules.  When it works, we can do this:
	--	$$ vcat (map GHC.pprInstance (GHC.modInfoInstances mod_info))

-----------------------------------------------------------------------------
-- Setting the module context

setContext :: String -> GHCi ()
setContext str
  | all sensible mods = fn mods
  | otherwise = throwDyn (CmdLineError "syntax:  :module [+/-] [*]M1 ... [*]Mn")
  where
    (fn, mods) = case str of 
			'+':stuff -> (addToContext,      words stuff)
			'-':stuff -> (removeFromContext, words stuff)
			stuff     -> (newContext,        words stuff) 

    sensible ('*':m) = looksLikeModuleName m
    sensible m       = looksLikeModuleName m

separate :: Session -> [String] -> [Module] -> [Module] 
        -> GHCi ([Module],[Module])
separate _       []             as bs = return (as,bs)
separate session (('*':str):ms) as bs = do
   m <- io $ GHC.findModule session (GHC.mkModuleName str) Nothing
   b <- io $ GHC.moduleIsInterpreted session m
   if b then separate session ms (m:as) bs
   	else throwDyn (CmdLineError ("module '"
                        ++ GHC.moduleNameString (GHC.moduleName m)
                        ++ "' is not interpreted"))
separate session (str:ms) as bs = do
  m <- io $ GHC.findModule session (GHC.mkModuleName str) Nothing
  separate session ms as (m:bs)

newContext :: [String] -> GHCi ()
newContext strs = do
  s <- getSession
  (as,bs) <- separate s strs [] []
  prel_mod <- getPrelude
  let bs' = if null as && prel_mod `notElem` bs then prel_mod:bs else bs
  io $ GHC.setContext s as bs'


addToContext :: [String] -> GHCi ()
addToContext strs = do
  s <- getSession
  (as,bs) <- io $ GHC.getContext s

  (new_as,new_bs) <- separate s strs [] []

  let as_to_add = new_as \\ (as ++ bs)
      bs_to_add = new_bs \\ (as ++ bs)

  io $ GHC.setContext s (as ++ as_to_add) (bs ++ bs_to_add)


removeFromContext :: [String] -> GHCi ()
removeFromContext strs = do
  s <- getSession
  (as,bs) <- io $ GHC.getContext s

  (as_to_remove,bs_to_remove) <- separate s strs [] []

  let as' = as \\ (as_to_remove ++ bs_to_remove)
      bs' = bs \\ (as_to_remove ++ bs_to_remove)

  io $ GHC.setContext s as' bs'

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
setCmd str
  = case toArgs str of
	("args":args) -> setArgs args
	("prog":prog) -> setProg prog
        ("prompt":_)  -> setPrompt (after 6)
        ("editor":_)  -> setEditor (after 6)
        ("stop":_)    -> setStop (after 4)
	wds -> setOptions wds
   where after n = dropWhile isSpace $ drop n $ dropWhile isSpace str

setArgs, setProg, setOptions :: [String] -> GHCi ()
setEditor, setStop, setPrompt :: String -> GHCi ()

setArgs args = do
  st <- getGHCiState
  setGHCiState st{ args = args }

setProg [prog] = do
  st <- getGHCiState
  setGHCiState st{ progname = prog }
setProg _ = do
  io (hPutStrLn stderr "syntax: :set prog <progname>")

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
      else setGHCiState st{ prompt = remQuotes value }
  where
     remQuotes ('\"':xs) | not (null xs) && last xs == '\"' = init xs
     remQuotes x = x

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
      (dflags',leftovers) <- io $ GHC.parseDynamicFlags dflags minus_opts

      if (not (null leftovers))
		then throwDyn (CmdLineError ("unrecognised flags: " ++ 
						unwords leftovers))
		else return ()

      new_pkgs <- setDynFlags dflags'

      -- if the package flags changed, we should reset the context
      -- and link the new packages.
      dflags <- getDynFlags
      when (packageFlags dflags /= pkg_flags) $ do
        io $ hPutStrLn stderr "package flags have changed, ressetting and loading new packages..."
        session <- getSession
        io (GHC.setTargets session [])
        io (GHC.load session LoadAllTargets)
        io (linkPackages dflags new_pkgs)
        setContextAfterLoad session []
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
           no_flag f = throwDyn (ProgramError ("don't know how to reverse " ++ f))

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
	_ -> throwDyn (CmdLineError "syntax:  :show [args|prog|prompt|editor|stop|modules|bindings|breaks|context]")

showModules :: GHCi ()
showModules = do
  session <- getSession
  loaded_mods <- getLoadedModules session
        -- we want *loaded* modules only, see #1734
  let show_one ms = do m <- io (GHC.showModule session ms); io (putStrLn m)
  mapM_ show_one loaded_mods

getLoadedModules :: GHC.Session -> GHCi [GHC.ModSummary]
getLoadedModules session = do
  graph <- io (GHC.getModuleGraph session)
  filterM (io . GHC.isLoaded session . GHC.ms_mod_name) graph

showBindings :: GHCi ()
showBindings = do
  s <- getSession
  bindings <- io (GHC.getBindings s)
  mapM_ printTyThing $ sortBy compareTyThings bindings
  return ()

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
   session <- getSession
   resumes <- io $ GHC.getResumeContext session
   printForUser $ vcat (map pp_resume (reverse resumes))
  where
   pp_resume resume =
        ptext SLIT("--> ") <> text (GHC.resumeStmt resume)
        $$ nest 2 (ptext SLIT("Stopped at") <+> ppr (GHC.resumeSpan resume))


-- -----------------------------------------------------------------------------
-- Completion

completeNone :: String -> IO [String]
completeNone _w = return []

completeMacro, completeIdentifier, completeModule,
    completeHomeModule, completeSetOptions, completeFilename,
    completeHomeModuleOrFile 
    :: String -> IO [String]

#ifdef USE_READLINE
completeWord :: String -> Int -> Int -> IO (Maybe (String, [String]))
completeWord w start end = do
  line <- Readline.getLineBuffer
  let line_words = words (dropWhile isSpace line)
  case w of
     ':':_ | all isSpace (take (start-1) line) -> wrapCompleter completeCmd w
     _other
	| ((':':c) : _) <- line_words -> do
	   maybe_cmd <- lookupCommand c
           let (n,w') = selectWord (words' 0 line)
	   case maybe_cmd of
	     Nothing -> return Nothing
	     Just (_,_,False,complete) -> wrapCompleter complete w
	     Just (_,_,True,complete) -> let complete' w = do rets <- complete w
                                                              return (map (drop n) rets)
                                         in wrapCompleter complete' w'
        | ("import" : _) <- line_words ->
                wrapCompleter completeModule w
	| otherwise     -> do
		--printf "complete %s, start = %d, end = %d\n" w start end
		wrapCompleter completeIdentifier w
    where words' _ [] = []
          words' n str = let (w,r) = break isSpace str
                             (s,r') = span isSpace r
                         in (n,w):words' (n+length w+length s) r'
          -- In a Haskell expression we want to parse 'a-b' as three words
          -- where a compiler flag (ie. -fno-monomorphism-restriction) should
          -- only be a single word.
          selectWord [] = (0,w)
          selectWord ((offset,x):xs)
              | offset+length x >= start = (start-offset,take (end-offset) x)
              | otherwise = selectWord xs

completeCmd :: String -> IO [String]
completeCmd w = do
  cmds <- readIORef commands
  return (filter (w `isPrefixOf`) (map (':':) (map cmdName cmds)))

completeMacro w = do
  cmds <- readIORef commands
  let cmds' = [ cmd | cmd <- map cmdName cmds, cmd `elem` map cmdName builtin_commands ]
  return (filter (w `isPrefixOf`) cmds')

completeIdentifier w = do
  s <- restoreSession
  rdrs <- GHC.getRdrNamesInScope s
  return (filter (w `isPrefixOf`) (map (showSDoc.ppr) rdrs))

completeModule w = do
  s <- restoreSession
  dflags <- GHC.getSessionDynFlags s
  let pkg_mods = allExposedModules dflags
  return (filter (w `isPrefixOf`) (map (showSDoc.ppr) pkg_mods))

completeHomeModule w = do
  s <- restoreSession
  g <- GHC.getModuleGraph s
  let home_mods = map GHC.ms_mod_name g
  return (filter (w `isPrefixOf`) (map (showSDoc.ppr) home_mods))

completeSetOptions w = do
  return (filter (w `isPrefixOf`) options)
    where options = "args":"prog":allFlags

completeFilename = Readline.filenameCompletionFunction

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
    []  -> return Nothing
    [x] -> return (Just (x,[]))
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
handler :: Exception -> GHCi Bool

handler exception = do
  flushInterpBuffers
  io installSignalHandlers
  ghciHandle handler (showException exception >> return False)

showException :: Exception -> GHCi ()
showException (DynException dyn) =
  case fromDynamic dyn of
    Nothing               -> io (putStrLn ("*** Exception: (unknown)"))
    Just Interrupted      -> io (putStrLn "Interrupted.")
    Just (CmdLineError s) -> io (putStrLn s)	 -- omit the location for CmdLineError
    Just ph@PhaseFailed{} -> io (putStrLn (showGhcException ph "")) -- ditto
    Just other_ghc_ex     -> io (print other_ghc_ex)

showException other_exception
  = io (putStrLn ("*** Exception: " ++ show other_exception))

-----------------------------------------------------------------------------
-- recursive exception handlers

-- Don't forget to unblock async exceptions in the handler, or if we're
-- in an exception loop (eg. let a = error a in a) the ^C exception
-- may never be delivered.  Thanks to Marcin for pointing out the bug.

ghciHandle :: (Exception -> GHCi a) -> GHCi a -> GHCi a
ghciHandle h (GHCi m) = GHCi $ \s -> 
   Exception.catch (m s) 
	(\e -> unGHCi (ghciUnblock (h e)) s)

ghciUnblock :: GHCi a -> GHCi a
ghciUnblock (GHCi a) = GHCi $ \s -> Exception.unblock (a s)


-- ----------------------------------------------------------------------------
-- Utils

expandPath :: String -> GHCi String
expandPath path = 
  case dropWhile isSpace path of
   ('~':d) -> do
	tilde <- io (getEnv "HOME")	-- will fail if HOME not defined
	return (tilde ++ '/':d)
   other -> 
	return other

wantInterpretedModule :: String -> GHCi Module
wantInterpretedModule str = do
   session <- getSession
   modl <- lookupModule str
   is_interpreted <- io (GHC.moduleIsInterpreted session modl)
   when (not is_interpreted) $
       throwDyn (CmdLineError ("module '" ++ str ++ "' is not interpreted"))
   return modl

wantNameFromInterpretedModule :: (Name -> SDoc -> GHCi ()) -> String
                              -> (Name -> GHCi ())
                              -> GHCi ()
wantNameFromInterpretedModule noCanDo str and_then = do
   session <- getSession
   names <- io $ GHC.parseName session str
   case names of
      []    -> return ()
      (n:_) -> do
            let modl = GHC.nameModule n
            if not (GHC.isExternalName n)
               then noCanDo n $ ppr n <>
                                text " is not defined in an interpreted module"
               else do
            is_interpreted <- io (GHC.moduleIsInterpreted session modl)
            if not is_interpreted
               then noCanDo n $ text "module " <> ppr modl <>
                                text " is not interpreted"
               else and_then n

-- ----------------------------------------------------------------------------
-- Windows console setup

setUpConsole :: IO ()
setUpConsole = do
#ifdef mingw32_HOST_OS
	-- On Windows we need to set a known code page, otherwise the characters
  	-- we read from the console will be be in some strange encoding, and
	-- similarly for characters we write to the console.
	--
	-- At the moment, GHCi pretends all input is Latin-1.  In the
	-- future we should support UTF-8, but for now we set the code
	-- pages to Latin-1.  Doing it this way does lead to problems,
	-- however: see bug #1649.
	--
	-- It seems you have to set the font in the console window to
	-- a Unicode font in order for output to work properly,
	-- otherwise non-ASCII characters are mapped wrongly.  sigh.
	-- (see MSDN for SetConsoleOutputCP()).
	--
        -- This call has been known to hang on some machines, see bug #1483
        --
	setConsoleCP 28591       -- ISO Latin-1
	setConsoleOutputCP 28591 -- ISO Latin-1
#endif
	return ()

-- -----------------------------------------------------------------------------
-- commands for debugger

sprintCmd, printCmd, forceCmd :: String -> GHCi ()
sprintCmd = pprintCommand False False
printCmd  = pprintCommand True False
forceCmd  = pprintCommand False True

pprintCommand :: Bool -> Bool -> String -> GHCi ()
pprintCommand bind force str = do
  session <- getSession
  io $ pprintClosureCommand session bind force str

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
       let f some_span = optSrcSpanFileName span == optSrcSpanFileName some_span
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
  session <- getSession
  runResult <- io $ GHC.resume session step
  afterRunStmt pred runResult
  return ()

abandonCmd :: String -> GHCi ()
abandonCmd = noArgs $ do
  s <- getSession
  b <- io $ GHC.abandon s -- the prompt will change to indicate the new context
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
    s <- getSession
    resumes <- io $ GHC.getResumeContext s
    case resumes of
      [] -> io $ putStrLn "Not stopped at a breakpoint"
      (r:_) -> do
        let hist = GHC.resumeHistory r
            (took,rest) = splitAt num hist
        spans <- mapM (io . GHC.getHistorySpan s) took
        let nums  = map (printf "-%-3d:") [(1::Int)..]
        let names = map GHC.historyEnclosingDecl took
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
  s <- getSession
  (names, _, span) <- io $ GHC.back s
  printForUser $ ptext SLIT("Logged breakpoint at") <+> ppr span
  printTypeOfNames s names
   -- run the command set with ":set stop <cmd>"
  st <- getGHCiState
  enqueueCommands [stop st]

forwardCmd :: String -> GHCi ()
forwardCmd = noArgs $ do
  s <- getSession
  (names, ix, span) <- io $ GHC.forward s
  printForUser $ (if (ix == 0)
                    then ptext SLIT("Stopped at")
                    else ptext SLIT("Logged breakpoint at")) <+> ppr span
  printTypeOfNames s names
   -- run the command set with ":set stop <cmd>"
  st <- getGHCiState
  enqueueCommands [stop st]

-- handle the "break" command
breakCmd :: String -> GHCi ()
breakCmd argLine = do
   session <- getSession
   breakSwitch session $ words argLine

breakSwitch :: Session -> [String] -> GHCi ()
breakSwitch _session [] = do
   io $ putStrLn "The break command requires at least one argument."
breakSwitch session (arg1:rest) 
   | looksLikeModuleName arg1 = do
        mod <- wantInterpretedModule arg1
        breakByModule mod rest
   | all isDigit arg1 = do
        (toplevel, _) <- io $ GHC.getContext session 
        case toplevel of
           (mod : _) -> breakByModuleLine mod (read arg1) rest
           [] -> do 
              io $ putStrLn "Cannot find default module for breakpoint." 
              io $ putStrLn "Perhaps no modules are loaded for debugging?"
   | otherwise = do -- try parsing it as an identifier
        wantNameFromInterpretedModule noCanDo arg1 $ \name -> do
        let loc = GHC.srcSpanStart (GHC.nameSrcSpan name)
        if GHC.isGoodSrcLoc loc
               then findBreakAndSet (GHC.nameModule name) $ 
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
breakSyntax = throwDyn (CmdLineError "Syntax: :break [<mod>] <line> [<column>]")

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
                  `Exception.catch` \_ -> return "TERM not set"

start_bold :: String
start_bold = "\ESC[1m"
end_bold :: String
end_bold   = "\ESC[0m"

listCmd :: String -> GHCi ()
listCmd "" = do
   mb_span <- getCurrentBreakSpan
   case mb_span of
      Nothing  -> printForUser $ text "not stopped at a breakpoint; nothing to list"
      Just span | GHC.isGoodSrcSpan span -> io $ listAround span True
                | otherwise              -> printForUser $ text "unable to list source for" <+> ppr span
listCmd str = list2 (words str)

list2 :: [String] -> GHCi ()
list2 [arg] | all isDigit arg = do
    session <- getSession
    (toplevel, _) <- io $ GHC.getContext session 
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
                  tickArray <- getTickArray (GHC.nameModule name)
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
   session <- getSession
   graph <- io (GHC.getModuleGraph session)
   let this = filter ((== modl) . GHC.ms_mod) graph
   case this of
     [] -> panic "listModuleLine"
     summ:_ -> do
           let filename = fromJust (ml_hs_file (GHC.ms_location summ))
               loc = GHC.mkSrcLoc (mkFastString (filename)) line 0
           io $ listAround (GHC.srcLocSpan loc) False

-- | list a section of a source file around a particular SrcSpan.
-- If the highlight flag is True, also highlight the span using
-- start_bold/end_bold.
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
   = do session <- getSession 
        io (GHC.findModule session (GHC.mkModuleName modName) Nothing)

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
   session <- getSession
   Just mod_info <- io $ GHC.getModuleInfo session mod
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let array      = GHC.modBreaks_flags modBreaks
   let ticks      = GHC.modBreaks_locs  modBreaks
   return (array, ticks)

setBreakFlag :: Bool -> GHC.BreakArray -> Int -> IO Bool 
setBreakFlag toggle array index
   | toggle    = GHC.setBreakOn array index 
   | otherwise = GHC.setBreakOff array index

