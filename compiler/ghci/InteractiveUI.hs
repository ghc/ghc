{-# OPTIONS -#include "Linker.h" #-}
-----------------------------------------------------------------------------
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2005-2006
--
-----------------------------------------------------------------------------
module InteractiveUI ( 
	interactiveUI,
	ghciWelcomeMsg
   ) where

#include "HsVersions.h"

import GhciMonad

-- The GHC interface
import qualified GHC
import GHC              ( Session, LoadHowMuch(..), Target(..),  TargetId(..),
                          Type, Module, ModuleName, TyThing(..), Phase,
                          BreakIndex )
import Debugger
import DynFlags
import Packages
import PackageConfig
import UniqFM
import PprTyThing
import Outputable
import Module           -- for ModuleEnv

-- for createtags
import Name
import OccName
import SrcLoc

-- Other random utilities
import Digraph
import BasicTypes hiding (isTopLevel)
import Panic      hiding (showException)
import Config
import StaticFlags
import Linker
import Util

#ifndef mingw32_HOST_OS
import System.Posix
#if __GLASGOW_HASKELL__ > 504
	hiding (getEnv)
#endif
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

import Data.List
import Data.Maybe
import System.Cmd
import System.Environment
import System.Exit	( exitWith, ExitCode(..) )
import System.Directory
import System.IO
import System.IO.Error as IO
import Data.Char
import Data.Dynamic
import Data.Array
import Control.Monad as Monad

import Foreign.StablePtr	( newStablePtr )
import GHC.Exts		( unsafeCoerce# )
import GHC.IOBase	( IOErrorType(InvalidArgument), IO(IO) )

import Data.IORef	( IORef, readIORef, writeIORef )

import System.Posix.Internals ( setNonBlockingFD )

-- these are needed by the new ghci debugger
import ByteCodeLink (HValue)
import ByteCodeInstr (BreakInfo (..))
import BreakArray

-----------------------------------------------------------------------------

ghciWelcomeMsg =
 "   ___         ___ _\n"++
 "  / _ \\ /\\  /\\/ __(_)\n"++
 " / /_\\// /_/ / /  | |      GHC Interactive, version " ++ cProjectVersion ++ ", for Haskell 98.\n"++
 "/ /_\\\\/ __  / /___| |      http://www.haskell.org/ghc/\n"++
 "\\____/\\/ /_/\\____/|_|      Type :? for help.\n"

type Command = (String, String -> GHCi Bool, Bool, String -> IO [String])
cmdName (n,_,_,_) = n

GLOBAL_VAR(commands, builtin_commands, [Command])

builtin_commands :: [Command]
builtin_commands = [
	-- Hugs users are accustomed to :e, so make sure it doesn't overlap
  ("?",		keepGoing help,			False, completeNone),
  ("add",	keepGoingPaths addModule,	False, completeFilename),
  ("break",     breakCmd, False, completeNone),   
  ("browse",    keepGoing browseCmd,		False, completeModule),
  ("cd",    	keepGoing changeDirectory,	False, completeFilename),
  ("check",	keepGoing checkModule,		False, completeHomeModule),
  ("continue",  continueCmd, False, completeNone),
  ("ctags",	keepGoing createCTagsFileCmd, 	False, completeFilename),
  ("def",	keepGoing defineMacro,		False, completeIdentifier),
  ("delete",    deleteCmd, False, completeNone),   
  ("e", 	keepGoing editFile,		False, completeFilename),
  ("edit",	keepGoing editFile,		False, completeFilename),
  ("etags",	keepGoing createETagsFileCmd,	False, completeFilename),
  ("force",     keepGoing (pprintClosureCommand False True), False, completeIdentifier),
  ("help",	keepGoing help,			False, completeNone),
  ("info",      keepGoing info,			False, completeIdentifier),
  ("kind",	keepGoing kindOfType,		False, completeIdentifier),
  ("load",	keepGoingPaths loadModule_,False, completeHomeModuleOrFile),
  ("module",	keepGoing setContext,		False, completeModule),
  ("main",	keepGoing runMain,		False, completeIdentifier),
  ("print",     keepGoing (pprintClosureCommand True False), False, completeIdentifier),
  ("quit",	quit,				False, completeNone),
  ("reload", 	keepGoing reloadModule,  	False, completeNone),
  ("set",	keepGoing setCmd,		True,  completeSetOptions),
  ("show",	keepGoing showCmd,		False, completeNone),
  ("sprint",    keepGoing (pprintClosureCommand False False),False, completeIdentifier),
  ("step",      stepCmd, False, completeNone), 
  ("type",	keepGoing typeOfExpr,		False, completeIdentifier),
  ("undef",     keepGoing undefineMacro,	False, completeMacro),
  ("unset",	keepGoing unsetOptions,		True,  completeSetOptions)
  ]

keepGoing :: (String -> GHCi ()) -> (String -> GHCi Bool)
keepGoing a str = a str >> return False

keepGoingPaths :: ([FilePath] -> GHCi ()) -> (String -> GHCi Bool)
keepGoingPaths a str = a (toArgs str) >> return False

shortHelpText = "use :? for help.\n"

-- NOTE: spaces at the end of each line to workaround CPP/string gap bug.
helpText =
 " Commands available from the prompt:\n" ++
 "\n" ++
 "   <stmt>                      evaluate/run <stmt>\n" ++
 "   :add <filename> ...         add module(s) to the current target set\n" ++
 "   :browse [*]<module>         display the names defined by <module>\n" ++
 "   :cd <dir>                   change directory to <dir>\n" ++
 "   :def <cmd> <expr>           define a command :<cmd>\n" ++
 "   :edit <file>                edit file\n" ++
 "   :edit                       edit last module\n" ++
 "   :help, :?                   display this list of commands\n" ++
 "   :info [<name> ...]          display information about the given names\n" ++
 "   :print [<name> ...]         prints a value without forcing its computation\n" ++
 "   :sprint [<name> ...]        simplified version of :print\n" ++
 "   :load <filename> ...        load module(s) and their dependents\n" ++
 "   :module [+/-] [*]<mod> ...  set the context for expression evaluation\n" ++
 "   :main [<arguments> ...]     run the main function with the given arguments\n" ++
 "   :reload                     reload the current module set\n" ++
 "\n" ++
 "   :set <option> ...           set options\n" ++
 "   :set args <arg> ...         set the arguments returned by System.getArgs\n" ++
 "   :set prog <progname>        set the value returned by System.getProgName\n" ++
 "   :set prompt <prompt>        set the prompt used in GHCi\n" ++
 "   :set editor <cmd>           set the command used for :edit\n" ++
 "\n" ++
 "   :show modules               show the currently loaded modules\n" ++
 "   :show bindings              show the current bindings made at the prompt\n" ++
 "\n" ++
 "   :ctags [<file>]             create tags file for Vi (default: \"tags\")\n" ++
 "   :etags [<file>]             create tags file for Emacs (default: \"TAGS\")\n" ++
 "   :type <expr>                show the type of <expr>\n" ++
 "   :kind <type>                show the kind of <type>\n" ++
 "   :undef <cmd>                undefine user-defined command :<cmd>\n" ++
 "   :unset <option> ...         unset options\n" ++
 "   :quit                       exit GHCi\n" ++
 "   :!<command>                 run the shell command <command>\n" ++
 "\n" ++
 " Options for ':set' and ':unset':\n" ++
 "\n" ++
 "    +r            revert top-level expressions after each evaluation\n" ++
 "    +s            print timing/memory stats after each evaluation\n" ++
 "    +t            print type after evaluation\n" ++
 "    -<flags>      most GHC command line flags can also be set here\n" ++
 "                         (eg. -v2, -fglasgow-exts, etc.)\n" ++
 "\n" 
-- Todo: add help for breakpoint commands here

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
   prel_mod <- GHC.findModule session prel_name (Just basePackageId)
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
		   editor = default_editor,
		   session = session,
		   options = [],
                   prelude = prel_mod,
                   resume = [],
                   breaks = emptyActiveBreakPoints,
                   tickarrays = emptyModuleEnv
                 }

#ifdef USE_READLINE
   Readline.resetTerminal Nothing
#endif

   return ()

prel_name = GHC.mkModuleName "Prelude"

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
  	     Left e    -> return ()
  	     Right hdl -> fileLoop hdl False
    
  when (read_dot_files) $ do
    -- Read in $HOME/.ghci
    either_dir <- io (IO.try (getEnv "HOME"))
    case either_dir of
       Left e -> return ()
       Right dir -> do
  	  cwd <- io (getCurrentDirectory)
  	  when (dir /= cwd) $ do
  	     let file = dir ++ "/.ghci"
  	     ok <- io (checkPerms file)
  	     when ok $ do
  	       either_hdl <- io (IO.try (openFile file ReadMode))
  	       case either_hdl of
  		  Left e    -> return ()
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
   session <- getSession
   (mod,imports) <- io (GHC.getContext session)
   st <- getGHCiState
   when show_prompt (io (putStr (mkPrompt mod imports (prompt st))))
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
	    l  -> do quit <- runCommand l
                     if quit then return () else fileLoop hdl show_prompt

stringLoop :: [String] -> GHCi Bool{-True: we quit-}
stringLoop [] = return False
stringLoop (s:ss) = do
   case removeSpaces s of
	"" -> stringLoop ss
	l  -> do quit <- runCommand l
                 if quit then return True else stringLoop ss

mkPrompt toplevs exports prompt
  = showSDoc $ f prompt
    where
        f ('%':'s':xs) = perc_s <> f xs
        f ('%':'%':xs) = char '%' <> f xs
        f (x:xs) = char x <> f xs
        f [] = empty
    
        perc_s = hsep (map (\m -> char '*' <> ppr (GHC.moduleName m)) toplevs) <+>
                 hsep (map (ppr . GHC.moduleName) exports)


#ifdef USE_READLINE
readlineLoop :: GHCi ()
readlineLoop = do
   session <- getSession
   (mod,imports) <- io (GHC.getContext session)
   io yield
   saveSession -- for use by completion
   st <- getGHCiState
   l <- io (readline (mkPrompt mod imports (prompt st))
	  	`finally` setNonBlockingFD 0)
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
  	  	  quit <- runCommand l
          	  if quit then return () else readlineLoop
#endif

runCommand :: String -> GHCi Bool
runCommand c = ghciHandle handler (doCommand c)
  where 
    doCommand (':' : command) = specialCommand command
    doCommand stmt
       = do timeIt (do nms <- runStmt stmt; finishEvalExpr nms)
            return False

-- This version is for the GHC command-line option -e.  The only difference
-- from runCommand is that it catches the ExitException exception and
-- exits, rather than printing out the exception.
runCommandEval c = ghciHandle handleEval (doCommand c)
  where 
    handleEval (ExitException code) = io (exitWith code)
    handleEval e                    = do handler e
				         io (exitWith (ExitFailure 1))

    doCommand (':' : command) = specialCommand command
    doCommand stmt
       = do nms <- runStmt stmt
	    case nms of 
		Nothing -> io (exitWith (ExitFailure 1))
		  -- failure to run the command causes exit(1) for ghc -e.
		_       -> finishEvalExpr nms

runStmt :: String -> GHCi (Maybe (Bool,[Name]))
runStmt stmt
 | null (filter (not.isSpace) stmt) = return (Just (False,[]))
 | otherwise
 = do st <- getGHCiState
      session <- getSession
      result <- io $ withProgName (progname st) $ withArgs (args st) $
	     	     GHC.runStmt session stmt
      switchOnRunResult result

switchOnRunResult :: GHC.RunResult -> GHCi (Maybe (Bool,[Name]))
switchOnRunResult GHC.RunFailed = return Nothing
switchOnRunResult (GHC.RunException e) = throw e
switchOnRunResult (GHC.RunOk names) = return $ Just (False,names)
switchOnRunResult (GHC.RunBreak threadId names info resume) = do
   session <- getSession
   Just mod_info <- io $ GHC.getModuleInfo session (breakInfo_module info) 
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let ticks      = GHC.modBreaks_locs modBreaks

   -- display information about the breakpoint
   let location = ticks ! breakInfo_number info
   unqual <- io $ GHC.getPrintUnqual session
   io $ printForUser stdout unqual $
      ptext SLIT("Stopped at") <+> ppr location

   pushResume location threadId resume
   return (Just (True,names))

-- possibly print the type and revert CAFs after evaluating an expression
finishEvalExpr mb_names
 = do show_types <- isOptionSet ShowType
      session <- getSession
      case mb_names of
	Nothing    -> return ()      
	Just (is_break,names) -> 
                when (is_break || show_types) $
                      mapM_ (showTypeOfName session) names

      flushInterpBuffers
      io installSignalHandlers
      b <- isOptionSet RevertCAFs
      io (when b revertCAFs)
      return True

showTypeOfName :: Session -> Name -> GHCi ()
showTypeOfName session n
   = do maybe_tything <- io (GHC.lookupName session n)
	case maybe_tything of
	  Nothing    -> return ()
	  Just thing -> showTyThing thing

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
     [] -> case [ c | c@(s,_,_,_) <- cmds, prefixMatch str s ] of
     		[] -> return Nothing
     		c:_ -> return (Just c)

-----------------------------------------------------------------------------
-- Commands

help :: String -> GHCi ()
help _ = io (putStr helpText)

info :: String -> GHCi ()
info "" = throwDyn (CmdLineError "syntax: ':i <thing-you-want-info-about>'")
info s  = do { let names = words s
	     ; session <- getSession
	     ; dflags <- getDynFlags
	     ; let exts = dopt Opt_GlasgowExts dflags
	     ; mapM_ (infoThing exts session) names }
  where
    infoThing exts session str = io $ do
	names <- GHC.parseName session str
	let filtered = filterOutChildren names
	mb_stuffs <- mapM (GHC.getInfo session) filtered
	unqual <- GHC.getPrintUnqual session
	putStrLn (showSDocForUser unqual $
     		   vcat (intersperse (text "") $
		   [ pprInfo exts stuff | Just stuff <-  mb_stuffs ]))

  -- Filter out names whose parent is also there Good
  -- example is '[]', which is both a type and data
  -- constructor in the same type
filterOutChildren :: [Name] -> [Name]
filterOutChildren names = filter (not . parent_is_there) names
 where parent_is_there n 
--	 | Just p <- GHC.nameParent_maybe n = p `elem` names
-- ToDo!!
	 | otherwise		           = False

pprInfo exts (thing, fixity, insts)
  =  pprTyThingInContextLoc exts thing 
  $$ show_fixity fixity
  $$ vcat (map GHC.pprInstance insts)
  where
    show_fixity fix 
	| fix == GHC.defaultFixity = empty
	| otherwise		   = ppr fix <+> ppr (GHC.getName thing)

-----------------------------------------------------------------------------
-- Commands

runMain :: String -> GHCi ()
runMain args = do
  let ss = concat $ intersperse "," (map (\ s -> ('"':s)++"\"") (toArgs args))
  runCommand $ '[': ss ++ "] `System.Environment.withArgs` main"
  return ()

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
editFile str
  | null str  = do
	-- find the name of the "topmost" file loaded
     session <- getSession
     graph0 <- io (GHC.getModuleGraph session)
     graph1 <- filterM (io . GHC.isLoaded session . GHC.ms_mod_name) graph0
     let graph2 = flattenSCCs (GHC.topSortModuleGraph True graph1 Nothing)
     case GHC.ml_hs_file (GHC.ms_location (last graph2)) of
	Just file -> do_edit file
	Nothing   -> throwDyn (CmdLineError "unknown file name")
  | otherwise = do_edit str
  where
	do_edit file = do
	   st <- getGHCiState
	   let cmd = editor st
	   when (null cmd) $ 
		throwDyn (CmdLineError "editor not set, use :set editor")
	   io $ system (cmd ++ ' ':file)
           return ()

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
  stringLoop (lines str)

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


loadModule :: [(FilePath, Maybe Phase)] -> GHCi SuccessFlag
loadModule fs = timeIt (loadModule' fs)

loadModule_ :: [FilePath] -> GHCi ()
loadModule_ fs = do loadModule (zip fs (repeat Nothing)); return ()

loadModule' :: [(FilePath, Maybe Phase)] -> GHCi SuccessFlag
loadModule' files = do
  session <- getSession

  -- unload first
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
  ok <- io (GHC.load session LoadAllTargets)
  afterLoad ok session
  return ok

checkModule :: String -> GHCi ()
checkModule m = do
  let modl = GHC.mkModuleName m
  session <- getSession
  result <- io (GHC.checkModule session modl)
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
reloadModule "" = do
  io (revertCAFs)		-- always revert CAFs on reload.
  session <- getSession
  ok <- io (GHC.load session LoadAllTargets)
  afterLoad ok session
reloadModule m = do
  io (revertCAFs)		-- always revert CAFs on reload.
  session <- getSession
  ok <- io (GHC.load session (LoadUpTo (GHC.mkModuleName m)))
  afterLoad ok session

afterLoad ok session = do
  io (revertCAFs)  -- always revert CAFs on load.
  discardResumeContext
  discardTickArrays
  discardActiveBreakPoints
  graph <- io (GHC.getModuleGraph session)
  graph' <- filterM (io . GHC.isLoaded session . GHC.ms_mod_name) graph
  setContextAfterLoad session graph'
  modulesLoadedMsg ok (map GHC.ms_mod_name graph')

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
   summary `matches` target
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
	  Just ty -> do ty' <- cleanType ty
			tystr <- showForUser (ppr ty')
		        io (putStrLn (str ++ " :: " ++ tystr))

kindOfType :: String -> GHCi ()
kindOfType str 
  = do cms <- getSession
       maybe_ty <- io (GHC.typeKind cms str)
       case maybe_ty of
	  Nothing    -> return ()
	  Just ty    -> do tystr <- showForUser (ppr ty)
		           io (putStrLn (str ++ " :: " ++ tystr))
          
quit :: String -> GHCi Bool
quit _ = return True

shellEscape :: String -> GHCi Bool
shellEscape str = io (system str >> return False)

-----------------------------------------------------------------------------
-- create tags file for currently loaded modules.

createETagsFileCmd, createCTagsFileCmd :: String -> GHCi ()

createCTagsFileCmd ""   = ghciCreateTagsFile CTags "tags"
createCTagsFileCmd file = ghciCreateTagsFile CTags file

createETagsFileCmd ""    = ghciCreateTagsFile ETags "TAGS"
createETagsFileCmd file  = ghciCreateTagsFile ETags file

data TagsKind = ETags | CTags

ghciCreateTagsFile :: TagsKind -> FilePath -> GHCi ()
ghciCreateTagsFile kind file = do
  session <- getSession
  io $ createTagsFile session kind file

-- ToDo: 
-- 	- remove restriction that all modules must be interpreted
--	  (problem: we don't know source locations for entities unless
--	  we compiled the module.
--
--	- extract createTagsFile so it can be used from the command-line
--	  (probably need to fix first problem before this is useful).
--
createTagsFile :: Session -> TagsKind -> FilePath -> IO ()
createTagsFile session tagskind tagFile = do
  graph <- GHC.getModuleGraph session
  let ms = map GHC.ms_mod graph
      tagModule m = do 
        is_interpreted <- GHC.moduleIsInterpreted session m
        -- should we just skip these?
        when (not is_interpreted) $
          throwDyn (CmdLineError ("module '" 
                                ++ GHC.moduleNameString (GHC.moduleName m)
                                ++ "' is not interpreted"))
        mbModInfo <- GHC.getModuleInfo session m
        let unqual 
	      | Just modinfo <- mbModInfo,
		Just unqual <- GHC.modInfoPrintUnqualified modinfo = unqual
	      | otherwise = GHC.alwaysQualify

        case mbModInfo of 
          Just modInfo -> return $! listTags unqual modInfo 
          _            -> return []

  mtags <- mapM tagModule ms
  either_res <- collateAndWriteTags tagskind tagFile $ concat mtags
  case either_res of
    Left e  -> hPutStrLn stderr $ ioeGetErrorString e
    Right _ -> return ()

listTags :: PrintUnqualified -> GHC.ModuleInfo -> [TagInfo]
listTags unqual modInfo =
	   [ tagInfo unqual name loc 
           | name <- GHC.modInfoExports modInfo
           , let loc = nameSrcLoc name
           , isGoodSrcLoc loc
           ]

type TagInfo = (String -- tag name
               ,String -- file name
               ,Int    -- line number
               ,Int    -- column number
               )

-- get tag info, for later translation into Vim or Emacs style
tagInfo :: PrintUnqualified -> Name -> SrcLoc -> TagInfo
tagInfo unqual name loc
    = ( showSDocForUser unqual $ pprOccName (nameOccName name)
      , showSDocForUser unqual $ ftext (srcLocFile loc)
      , srcLocLine loc
      , srcLocCol loc
      )

collateAndWriteTags :: TagsKind -> FilePath -> [TagInfo] -> IO (Either IOError ())
collateAndWriteTags CTags file tagInfos = do -- ctags style, Vim et al
  let tags = unlines $ sortLe (<=) $ nub $ map showTag tagInfos
  IO.try (writeFile file tags)
collateAndWriteTags ETags file tagInfos = do -- etags style, Emacs/XEmacs
  let byFile op (_,f1,_,_) (_,f2,_,_) = f1 `op` f2
      groups = groupBy (byFile (==)) $ sortLe (byFile (<=)) tagInfos
  tagGroups <- mapM tagFileGroup groups 
  IO.try (writeFile file $ concat tagGroups)
  where
    tagFileGroup group@[] = throwDyn (CmdLineError "empty tag file group??")
    tagFileGroup group@((_,fileName,_,_):_) = do
      file <- readFile fileName -- need to get additional info from sources..
      let byLine (_,_,l1,_) (_,_,l2,_) = l1 <= l2
          sortedGroup = sortLe byLine group
          tags = unlines $ perFile sortedGroup 1 0 $ lines file
      return $ "\x0c\n" ++ fileName ++ "," ++ show (length tags) ++ "\n" ++ tags
    perFile (tagInfo@(tag,file,lNo,colNo):tags) count pos (line:lines) | lNo>count =
      perFile (tagInfo:tags) (count+1) (pos+length line) lines
    perFile (tagInfo@(tag,file,lNo,colNo):tags) count pos lines@(line:_) | lNo==count =
      showETag tagInfo line pos : perFile tags count pos lines
    perFile tags count pos lines = []

-- simple ctags format, for Vim et al
showTag :: TagInfo -> String
showTag (tag,file,lineNo,colNo)
    =  tag ++ "\t" ++ file ++ "\t" ++ show lineNo

-- etags format, for Emacs/XEmacs
showETag :: TagInfo -> String -> Int -> String
showETag (tag,file,lineNo,colNo) line charPos
    =  take colNo line ++ tag
    ++ "\x7f" ++ tag
    ++ "\x01" ++ show lineNo
    ++ "," ++ show charPos

-----------------------------------------------------------------------------
-- Browsing a module's contents

browseCmd :: String -> GHCi ()
browseCmd m = 
  case words m of
    ['*':m] | looksLikeModuleName m -> browseModule m False
    [m]     | looksLikeModuleName m -> browseModule m True
    _ -> throwDyn (CmdLineError "syntax:  :browse <module>")

browseModule m exports_only = do
  s <- getSession
  modl <- if exports_only then lookupModule s m
                          else wantInterpretedModule s m

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
	       | otherwise    = fromMaybe [] (GHC.modInfoTopLevelScope mod_info)

	    filtered = filterOutChildren names
	
        things <- io $ mapM (GHC.lookupName s) filtered

        dflags <- getDynFlags
	let exts = dopt Opt_GlasgowExts dflags
	io (putStrLn (showSDocForUser unqual (
		vcat (map (pprTyThingInContext exts) (catMaybes things))
	   )))
	-- ToDo: modInfoInstances currently throws an exception for
	-- package modules.  When it works, we can do this:
	--	$$ vcat (map GHC.pprInstance (GHC.modInfoInstances mod_info))

-----------------------------------------------------------------------------
-- Setting the module context

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
separate session []           as bs = return (as,bs)
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
        ("prompt":prompt) -> setPrompt (after 6)
        ("editor":cmd) -> setEditor (after 6)
	wds -> setOptions wds
   where after n = dropWhile isSpace $ drop n $ dropWhile isSpace str

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
      let (plus_opts, minus_opts)  = partition isPlus wds
      mapM_ setOpt plus_opts

      -- then, dynamic flags
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
	   (plus_opts, rest2)  = partition isPlus rest1

       if (not (null rest2)) 
	  then io (putStrLn ("unknown option: '" ++ head rest2 ++ "'"))
	  else do

       mapM_ unsetOpt plus_opts
 
       -- can't do GHC flags for now
       if (not (null minus_opts))
	  then throwDyn (CmdLineError "can't unset GHC command-line flags")
	  else return ()

isMinus ('-':s) = True
isMinus _ = False

isPlus ('+':s) = True
isPlus _ = False

setOpt ('+':str)
  = case strToGHCiOpt str of
	Nothing -> io (putStrLn ("unknown option: '" ++ str ++ "'"))
	Just o  -> setOption o

unsetOpt ('+':str)
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

showCmd str =
  case words str of
	["modules" ] -> showModules
	["bindings"] -> showBindings
	["linker"]   -> io showLinkerState
        ["breaks"] -> showBkptTable
	_ -> throwDyn (CmdLineError "syntax:  :show [modules|bindings]")

showModules = do
  session <- getSession
  let show_one ms = do m <- io (GHC.showModule session ms)
		       io (putStrLn m)
  graph <- io (GHC.getModuleGraph session)
  mapM_ show_one graph

showBindings = do
  s <- getSession
  unqual <- io (GHC.getPrintUnqual s)
  bindings <- io (GHC.getBindings s)
  mapM_ showTyThing bindings
  return ()

showTyThing (AnId id) = do 
  ty' <- cleanType (GHC.idType id)
  str <- showForUser (ppr id <> text " :: " <> ppr ty')
  io (putStrLn str)
showTyThing _  = return ()

-- if -fglasgow-exts is on we show the foralls, otherwise we don't.
cleanType :: Type -> GHCi Type
cleanType ty = do
  dflags <- getDynFlags
  if dopt Opt_GlasgowExts dflags 
	then return ty
	else return $! GHC.dropForAlls ty

showBkptTable :: GHCi ()
showBkptTable = do
   activeBreaks <- getActiveBreakPoints 
   str <- showForUser $ ppr activeBreaks 
   io $ putStrLn str

-- -----------------------------------------------------------------------------
-- Completion

completeNone :: String -> IO [String]
completeNone w = return []

#ifdef USE_READLINE
completeWord :: String -> Int -> Int -> IO (Maybe (String, [String]))
completeWord w start end = do
  line <- Readline.getLineBuffer
  case w of 
     ':':_ | all isSpace (take (start-1) line) -> wrapCompleter completeCmd w
     _other
	| Just c <- is_cmd line -> do
	   maybe_cmd <- lookupCommand c
           let (n,w') = selectWord (words' 0 line)
	   case maybe_cmd of
	     Nothing -> return Nothing
	     Just (_,_,False,complete) -> wrapCompleter complete w
	     Just (_,_,True,complete) -> let complete' w = do rets <- complete w
                                                              return (map (drop n) rets)
                                         in wrapCompleter complete' w'
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

is_cmd line 
 | ((':':w) : _) <- words (dropWhile isSpace line) = Just w
 | otherwise = Nothing

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
  where common s "" = ""
	common "" s = ""
	common (c:cs) (d:ds)
	   | c == d = c : common cs ds
	   | otherwise = ""

allExposedModules :: DynFlags -> [ModuleName]
allExposedModules dflags 
 = map GHC.mkModuleName (concat (map exposedModules (filter exposed (eltsUFM pkg_db))))
 where
  pkg_db = pkgIdMap (pkgState dflags)
#else
completeCmd        = completeNone
completeMacro      = completeNone
completeIdentifier = completeNone
completeModule     = completeNone
completeHomeModule = completeNone
completeSetOptions = completeNone
completeFilename   = completeNone
completeHomeModuleOrFile=completeNone
completeBkpt       = completeNone
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
	-- future we should support UTF-8, but for now we set the code pages
	-- to Latin-1.
	--
	-- It seems you have to set the font in the console window to
	-- a Unicode font in order for output to work properly,
	-- otherwise non-ASCII characters are mapped wrongly.  sigh.
	-- (see MSDN for SetConsoleOutputCP()).
	--
	setConsoleCP 28591       -- ISO Latin-1
	setConsoleOutputCP 28591 -- ISO Latin-1
#endif
	return ()

-- commands for debugger
foreign import ccall "rts_setStepFlag" setStepFlag :: IO () 

stepCmd :: String -> GHCi Bool
stepCmd [] = doContinue setStepFlag 
stepCmd expression = do
   io $ setStepFlag
   runCommand expression

continueCmd :: String -> GHCi Bool
continueCmd [] = doContinue $ return () 
continueCmd other = do
   io $ putStrLn "The continue command accepts no arguments."
   return False

doContinue :: IO () -> GHCi Bool
doContinue actionBeforeCont = do 
   resumeAction <- popResume
   case resumeAction of
      Nothing -> do 
         io $ putStrLn "There is no computation running."
         return False
      Just (_,_,handle) -> do
         io $ actionBeforeCont
         session <- getSession
         runResult <- io $ GHC.resume session handle
         names <- switchOnRunResult runResult
         finishEvalExpr names
         return False 

deleteCmd :: String -> GHCi Bool
deleteCmd argLine = do
   deleteSwitch $ words argLine
   return False
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

-- handle the "break" command
breakCmd :: String -> GHCi Bool
breakCmd argLine = do
   session <- getSession
   breakSwitch session $ words argLine

breakSwitch :: Session -> [String] -> GHCi Bool
breakSwitch _session [] = do
   io $ putStrLn "The break command requires at least one argument."
   return False
breakSwitch session args@(arg1:rest) 
   | looksLikeModule arg1 = do
        mod <- wantInterpretedModule session arg1
        breakByModule mod rest
        return False
   | otherwise = do
        (toplevel, _) <- io $ GHC.getContext session 
        case toplevel of
           (mod : _) -> breakByModule mod args 
           [] -> do 
              io $ putStrLn "Cannot find default module for breakpoint." 
              io $ putStrLn "Perhaps no modules are loaded for debugging?"
        return False
   where
   -- Todo there may be a nicer way to test this
   looksLikeModule :: String -> Bool
   looksLikeModule []    = False
   looksLikeModule (x:_) = isUpper x

wantInterpretedModule :: Session -> String -> GHCi Module
wantInterpretedModule session str = do
   modl <- io $ GHC.findModule session (GHC.mkModuleName str) Nothing
   is_interpreted <- io (GHC.moduleIsInterpreted session modl)
   when (not is_interpreted) $
       throwDyn (CmdLineError ("module '" ++ str ++ "' is not interpreted"))
   return modl

breakByModule :: Module -> [String] -> GHCi () 
breakByModule mod args@(arg1:rest)
   | all isDigit arg1 = do  -- looks like a line number
        breakByModuleLine mod (read arg1) rest
   | looksLikeVar arg1 = do
        -- break by a function definition
        io $ putStrLn "Break by function definition not implemented."
   | otherwise = io $ putStrLn "Invalid arguments to break command."
   where
   -- Todo there may be a nicer way to test this
   looksLikeVar :: String -> Bool
   looksLikeVar [] = False
   looksLikeVar (x:_) = isLower x || x `elem` "~!@#$%^&*-+"

breakByModuleLine :: Module -> Int -> [String] -> GHCi ()
breakByModuleLine mod line args
   | [] <- args = findBreakAndSet mod $ findBreakByLine line
   | [col] <- args, all isDigit col =
        findBreakAndSet mod $ findBreakByCoord (line, read col)
   | otherwise = io $ putStrLn "Invalid arguments to break command."

findBreakAndSet :: Module -> (TickArray -> Maybe (Int, SrcSpan)) -> GHCi ()
findBreakAndSet mod lookupTickTree = do 
   tickArray <- getTickArray mod
   (breakArray, _) <- getModBreak mod
   case lookupTickTree tickArray of 
      Nothing  -> io $ putStrLn $ "No breakpoints found at that location."
      Just (tick, span) -> do
         success <- io $ setBreakFlag True breakArray tick 
         session <- getSession
         unqual  <- io $ GHC.getPrintUnqual session
         if success 
            then do
               (alreadySet, nm) <- 
                     recordBreak $ BreakLocation
                             { breakModule = mod
                             , breakLoc = span
                             , breakTick = tick
                             }
               io $ printForUser stdout unqual $
                  text "Breakpoint " <> ppr nm <>
                  if alreadySet 
                     then text " was already set at " <> ppr span
                     else text " activated at " <> ppr span
            else do
            str <- showForUser $ text "Breakpoint could not be activated at" 
                                 <+> ppr span
            io $ putStrLn str

-- When a line number is specified, the current policy for choosing
-- the best breakpoint is this:
--    - the leftmost complete subexpression on the specified line, or
--    - the leftmost subexpression starting on the specified line, or
--    - the rightmost subexpression enclosing the specified line
--
findBreakByLine :: Int -> TickArray -> Maybe (BreakIndex,SrcSpan)
findBreakByLine line arr = 
  listToMaybe (sortBy leftmost complete)   `mplus`
  listToMaybe (sortBy leftmost incomplete) `mplus`
  listToMaybe (sortBy rightmost ticks)
  where 
        ticks = arr ! line

        starts_here = [ tick | tick@(nm,span) <- ticks,
                               srcSpanStartLine span == line ]

        (complete,incomplete) = partition ends_here starts_here
            where ends_here (nm,span) = srcSpanEndLine span == line

findBreakByCoord :: (Int,Int) -> TickArray -> Maybe (BreakIndex,SrcSpan)
findBreakByCoord (line, col) arr =
  listToMaybe (sortBy rightmost contains)
  where 
        ticks = arr ! line

        -- the ticks that span this coordinate
        contains = [ tick | tick@(nm,span) <- ticks, span `spans` (line,col) ]

leftmost  (_,a) (_,b) = a `compare` b
rightmost (_,a) (_,b) = b `compare` a

spans :: SrcSpan -> (Int,Int) -> Bool
spans span (l,c) = srcSpanStart span <= loc && loc <= srcSpanEnd span
   where loc = mkSrcLoc (srcSpanFile span) l c


-- --------------------------------------------------------------------------
-- Tick arrays

getTickArray :: Module -> GHCi TickArray
getTickArray modl = do
   st <- getGHCiState
   let arrmap = tickarrays st
   case lookupModuleEnv arrmap modl of
      Just arr -> return arr
      Nothing  -> do
        (breakArray, ticks) <- getModBreak modl 
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
        max_line = maximum (map srcSpanEndLine (map snd ticks))
        srcSpanLines span = [ srcSpanStartLine span .. srcSpanEndLine span ]

getModBreak :: Module -> GHCi (BreakArray, Array Int SrcSpan)
getModBreak mod = do
   session <- getSession
   Just mod_info <- io $ GHC.getModuleInfo session mod
   let modBreaks  = GHC.modInfoModBreaks mod_info
   let array      = GHC.modBreaks_flags modBreaks
   let ticks      = GHC.modBreaks_locs  modBreaks
   return (array, ticks)

lookupModule :: Session -> String -> GHCi Module
lookupModule session modName
   = io (GHC.findModule session (GHC.mkModuleName modName) Nothing)

setBreakFlag :: Bool -> BreakArray -> Int -> IO Bool 
setBreakFlag toggle array index
   | toggle    = setBreakOn array index 
   | otherwise = setBreakOff array index


{- these should probably go to the GHC API at some point -}
enableBreakPoint  :: Session -> Module -> Int -> IO ()
enableBreakPoint session mod index = return ()

disableBreakPoint :: Session -> Module -> Int -> IO ()
disableBreakPoint session mod index = return ()

activeBreakPoints :: Session -> IO [(Module,Int)]
activeBreakPoints session = return []

enableSingleStep  :: Session -> IO ()
enableSingleStep session = return ()

disableSingleStep :: Session -> IO ()
disableSingleStep session = return ()
