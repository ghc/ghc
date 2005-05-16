{-# OPTIONS -#include "Linker.h" #-}
-----------------------------------------------------------------------------
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2005
--
-----------------------------------------------------------------------------
module InteractiveUI ( 
	interactiveUI,
	ghciWelcomeMsg
   ) where

#include "HsVersions.h"

-- The GHC interface
import qualified GHC
import GHC		( Session, verbosity, dopt, DynFlag(..),
			  mkModule, pprModule, Type, Module, SuccessFlag(..),
			  TyThing(..), Name, LoadHowMuch(..), Phase,
			  GhcException(..), showGhcException,
			  CheckedModule(..) )
import Outputable

-- following all needed for :info... ToDo: remove
import IfaceSyn		( IfaceDecl(..), IfaceClassOp(..), IfaceConDecls(..),
			  IfaceConDecl(..), IfaceType,
		   	  pprIfaceDeclHead, pprParendIfaceType,
			  pprIfaceForAllPart, pprIfaceType )
import FunDeps		( pprFundeps )
import SrcLoc		( SrcLoc, pprDefnLoc )
import OccName		( OccName, parenSymOcc, occNameUserString )
import BasicTypes	( StrictnessMark(..), defaultFixity, failed, successIf )

-- Other random utilities
import Panic 		( panic, installSignalHandlers )
import Config
import StaticFlags	( opt_IgnoreDotGhci )
import Linker		( showLinkerState )
import Util		( removeSpaces, handle, global, toArgs,
			  looksLikeModuleName, prefixMatch )
import ErrUtils		( printErrorsAndWarnings )

#ifndef mingw32_HOST_OS
import Util		( handle )
import System.Posix
#if __GLASGOW_HASKELL__ > 504
	hiding (getEnv)
#endif
#else
import GHC.ConsoleHandler ( flushConsole )
#endif

#ifdef USE_READLINE
import Control.Concurrent	( yield )	-- Used in readline loop
import System.Console.Readline as Readline
#endif

--import SystemExts

import Control.Exception as Exception
import Data.Dynamic
-- import Control.Concurrent

import Numeric
import Data.List
import Data.Int		( Int64 )
import Data.Maybe	( isJust )
import System.Cmd
import System.CPUTime
import System.Environment
import System.Exit	( exitWith, ExitCode(..) )
import System.Directory
import System.IO
import System.IO.Error as IO
import Data.Char
import Control.Monad as Monad
import Foreign.StablePtr	( newStablePtr )

import GHC.Exts		( unsafeCoerce# )
import GHC.IOBase	( IOErrorType(InvalidArgument) )

import Data.IORef	( IORef, newIORef, readIORef, writeIORef )

import System.Posix.Internals ( setNonBlockingFD )

-----------------------------------------------------------------------------

ghciWelcomeMsg =
 "   ___         ___ _\n"++
 "  / _ \\ /\\  /\\/ __(_)\n"++
 " / /_\\// /_/ / /  | |      GHC Interactive, version " ++ cProjectVersion ++ ", for Haskell 98.\n"++
 "/ /_\\\\/ __  / /___| |      http://www.haskell.org/ghc/\n"++
 "\\____/\\/ /_/\\____/|_|      Type :? for help.\n"

GLOBAL_VAR(commands, builtin_commands, [(String, String -> GHCi Bool)])

builtin_commands :: [(String, String -> GHCi Bool)]
builtin_commands = [
  ("add",	keepGoingPaths addModule),
  ("browse",    keepGoing browseCmd),
  ("cd",    	keepGoing changeDirectory),
  ("def",	keepGoing defineMacro),
  ("help",	keepGoing help),
  ("?",		keepGoing help),
  ("info",      keepGoing info),
  ("load",	keepGoingPaths loadModule_),
  ("module",	keepGoing setContext),
  ("reload",	keepGoing reloadModule),
  ("check",	keepGoing checkModule),
  ("set",	keepGoing setCmd),
  ("show",	keepGoing showCmd),
  ("type",	keepGoing typeOfExpr),
  ("kind",	keepGoing kindOfType),
  ("unset",	keepGoing unsetOptions),
  ("undef",     keepGoing undefineMacro),
  ("quit",	quit)
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
 "   :help, :?                   display this list of commands\n" ++
 "   :info [<name> ...]          display information about the given names\n" ++
 "   :load <filename> ...        load module(s) and their dependents\n" ++
 "   :module [+/-] [*]<mod> ...  set the context for expression evaluation\n" ++
 "   :reload                     reload the current module set\n" ++
 "\n" ++
 "   :set <option> ...           set options\n" ++
 "   :set args <arg> ...         set the arguments returned by System.getArgs\n" ++
 "   :set prog <progname>        set the value returned by System.getProgName\n" ++
 "\n" ++
 "   :show modules               show the currently loaded modules\n" ++
 "   :show bindings              show the current bindings made at the prompt\n" ++
 "\n" ++
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
 "                         (eg. -v2, -fglasgow-exts, etc.)\n"


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

   hFlush stdout
   hSetBuffering stdout NoBuffering

	-- Initialise buffering for the *interpreted* I/O system
   initInterpBuffering session

	-- We don't want the cmd line to buffer any input that might be
	-- intended for the program, so unbuffer stdin.
   hSetBuffering stdin NoBuffering

	-- initial context is just the Prelude
   GHC.setContext session [] [prelude_mod]

#ifdef USE_READLINE
   Readline.initialize
#endif

#if defined(mingw32_HOST_OS)
    -- The win32 Console API mutates the first character of 
    -- type-ahead when reading from it in a non-buffered manner. Work
    -- around this by flushing the input buffer of type-ahead characters.
    -- 
   GHC.ConsoleHandler.flushConsole stdin
#endif
   startGHCi (runGHCi srcs maybe_expr)
	GHCiState{ progname = "<interactive>",
		   args = [],
		   session = session,
		   options = [] }

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
	    -- enter the interactive loop
	    interactiveLoop is_tty show_prompt
	Just expr -> do
	    -- just evaluate the expression we were given
	    runCommandEval expr
	    return ()

  -- and finally, exit
  io $ do when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."


interactiveLoop is_tty show_prompt = do
  -- Ignore ^C exceptions caught here
  ghciHandleDyn (\e -> case e of 
			Interrupted -> ghciUnblock (
#if defined(mingw32_HOST_OS)
						io (putStrLn "") >> 
#endif
						interactiveLoop is_tty show_prompt)
			_other      -> return ()) $ do

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
fileLoop hdl prompt = do
   session <- getSession
   (mod,imports) <- io (GHC.getContext session)
   when prompt (io (putStr (mkPrompt mod imports)))
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
	    "" -> fileLoop hdl prompt
	    l  -> do quit <- runCommand l
          	     if quit then return () else fileLoop hdl prompt

stringLoop :: [String] -> GHCi ()
stringLoop [] = return ()
stringLoop (s:ss) = do
   case removeSpaces s of
	"" -> stringLoop ss
	l  -> do quit <- runCommand l
                 if quit then return () else stringLoop ss

mkPrompt toplevs exports
  = showSDoc (hsep (map (\m -> char '*' <> pprModule m) toplevs)
	     <+> hsep (map pprModule exports)
	     <> text "> ")

#ifdef USE_READLINE
readlineLoop :: GHCi ()
readlineLoop = do
   session <- getSession
   (mod,imports) <- io (GHC.getContext session)
   io yield
   l <- io (readline (mkPrompt mod imports)
	  	`finally` setNonBlockingFD 0)
		-- readline sometimes puts stdin into blocking mode,
		-- so we need to put it back for the IO library
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

-- This version is for the GHC command-line option -e.  The only difference
-- from runCommand is that it catches the ExitException exception and
-- exits, rather than printing out the exception.
runCommandEval c = ghciHandle handleEval (doCommand c)
  where 
    handleEval (ExitException code) = io (exitWith code)
    handleEval e                    = do showException e
				         io (exitWith (ExitFailure 1))

-- This is the exception handler for exceptions generated by the
-- user's code; it normally just prints out the exception.  The
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

doCommand (':' : command) = specialCommand command
doCommand stmt
   = do timeIt (do nms <- runStmt stmt; finishEvalExpr nms)
        return False

runStmt :: String -> GHCi [Name]
runStmt stmt
 | null (filter (not.isSpace) stmt) = return []
 | otherwise
 = do st <- getGHCiState
      session <- getSession
      result <- io $ withProgName (progname st) $ withArgs (args st) $
	     	     GHC.runStmt session stmt
      case result of
	GHC.RunFailed      -> return []
	GHC.RunException e -> throw e  -- this is caught by runCommand(Eval)
	GHC.RunOk names    -> return names

-- possibly print the type and revert CAFs after evaluating an expression
finishEvalExpr names
 = do b <- isOptionSet ShowType
      session <- getSession
      when b (mapM_ (showTypeOfName session) names)

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

showForUser :: SDoc -> GHCi String
showForUser doc = do
  session <- getSession
  unqual <- io (GHC.getPrintUnqual session)
  return $! showSDocForUser unqual doc

specialCommand :: String -> GHCi Bool
specialCommand ('!':str) = shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  cmds <- io (readIORef commands)
  case [ (s,f) | (s,f) <- cmds, prefixMatch cmd s ] of
     []      -> io (hPutStr stdout ("unknown command ':" ++ cmd ++ "'\n" 
		                    ++ shortHelpText) >> return False)
     [(_,f)] -> f (dropWhile isSpace rest)
     cs      -> io (hPutStrLn stdout ("prefix " ++ cmd ++ 
			    	      " matches multiple commands (" ++ 
	         	     	       foldr1 (\a b -> a ++ ',':b) (map fst cs)
					 ++ ")") >> return False)

noArgs c = throwDyn (CmdLineError ("command '" ++ c ++ "' takes no arguments"))


-----------------------------------------------------------------------------
-- To flush buffers for the *interpreted* computation we need
-- to refer to *its* stdout/stderr handles

GLOBAL_VAR(flush_interp,       error "no flush_interp", IO ())
GLOBAL_VAR(turn_off_buffering, error "no flush_stdout", IO ())

no_buf_cmd = "System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering" ++
	     " Prelude.>> System.IO.hSetBuffering System.IO.stderr System.IO.NoBuffering"
flush_cmd  = "System.IO.hFlush System.IO.stdout Prelude.>> System.IO.hFlush IO.stderr"

initInterpBuffering :: Session -> IO ()
initInterpBuffering session
 = do maybe_hval <- GHC.compileExpr session no_buf_cmd
	
      case maybe_hval of
	Just hval -> writeIORef turn_off_buffering (unsafeCoerce# hval :: IO ())
	other	  -> panic "interactiveUI:setBuffering"
	
      maybe_hval <- GHC.compileExpr session flush_cmd
      case maybe_hval of
	Just hval -> writeIORef flush_interp (unsafeCoerce# hval :: IO ())
	_         -> panic "interactiveUI:flush"

      turnOffBuffering	-- Turn it off right now

      return ()


flushInterpBuffers :: GHCi ()
flushInterpBuffers
 = io $ do Monad.join (readIORef flush_interp)
           return ()

turnOffBuffering :: IO ()
turnOffBuffering
 = do Monad.join (readIORef turn_off_buffering)
      return ()

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
    infoThing exts session name
	= do { stuff <- io (GHC.getInfo session name)
	     ; unqual <- io (GHC.getPrintUnqual session)
	     ; io (putStrLn (showSDocForUser unqual $
     		   vcat (intersperse (text "") (map (showThing exts) stuff)))) }

showThing :: Bool -> GHC.GetInfoResult -> SDoc
showThing exts (wanted_str, thing, fixity, src_loc, insts) 
    = vcat [ showWithLoc src_loc (showDecl exts want_name thing),
	     show_fixity fixity,
	     vcat (map show_inst insts)]
  where
    want_name occ = wanted_str == occNameUserString occ

    show_fixity fix 
	| fix == defaultFixity = empty
	| otherwise            = ppr fix <+> text wanted_str

    show_inst (inst_ty, loc)
	= showWithLoc loc (ptext SLIT("instance") <+> ppr inst_ty)

showWithLoc :: SrcLoc -> SDoc -> SDoc
showWithLoc loc doc 
    = hang doc 2 (char '\t' <> comment <+> pprDefnLoc loc)
		-- The tab tries to make them line up a bit
  where
    comment = ptext SLIT("--")


-- Now there is rather a lot of goop just to print declarations in a
-- civilised way with "..." for the parts we are less interested in.

showDecl :: Bool -> (OccName -> Bool) -> IfaceDecl -> SDoc
showDecl exts want_name (IfaceForeign {ifName = tc})
  = ppr tc <+> ptext SLIT("is a foreign type")

showDecl exts want_name (IfaceId {ifName = var, ifType = ty})
  = ppr var <+> dcolon <+> showIfaceType exts ty 

showDecl exts want_name (IfaceSyn {ifName = tycon, ifTyVars = tyvars, ifSynRhs = mono_ty})
  = hang (ptext SLIT("type") <+> pprIfaceDeclHead [] tycon tyvars)
       2 (equals <+> ppr mono_ty)

showDecl exts want_name (IfaceData {ifName = tycon, 
		     ifTyVars = tyvars, ifCons = condecls, ifCtxt = context})
  = hang (pp_nd <+> pprIfaceDeclHead context tycon tyvars)
       2 (add_bars (ppr_trim show_con cs))
  where
    show_con (IfVanillaCon { ifConOcc = con_name, ifConInfix = is_infix, ifConArgTys = tys, 
			     ifConStricts = strs, ifConFields = flds})
	| want_name tycon || want_name con_name || any want_name flds
	= Just (show_guts con_name is_infix tys_w_strs flds)
	| otherwise = Nothing
	where
	  tys_w_strs = tys `zip` (strs ++ repeat NotMarkedStrict)
    show_con (IfGadtCon { ifConOcc = con_name, ifConTyVars = tvs, ifConCtxt = theta, 
			  ifConArgTys = arg_tys, ifConResTys = res_tys, ifConStricts = strs })
	| want_name tycon || want_name con_name
	= Just (ppr_bndr con_name <+> colon <+> pprIfaceForAllPart tvs theta pp_tau)
	| otherwise = Nothing
	where
	  tys_w_strs = arg_tys `zip` (strs ++ repeat NotMarkedStrict)
	  pp_tau = foldr add pp_res_ty tys_w_strs
	  pp_res_ty = ppr_bndr tycon <+> hsep (map pprParendIfaceType res_tys)
	  add bty pp_ty = ppr_bangty bty <+> arrow <+> pp_ty

    show_guts con True [ty1, ty2] flds = sep [ppr_bangty ty1, ppr con, ppr_bangty ty2]
    show_guts con _ tys []   = ppr_bndr con <+> sep (map ppr_bangty tys)
    show_guts con _ tys flds 
	= ppr_bndr con <+> braces (sep (punctuate comma (ppr_trim show_fld (tys `zip` flds))))
	where
	  show_fld (bty, fld) | want_name tycon || want_name con || want_name fld
			      = Just (ppr_bndr fld <+> dcolon <+> ppr_bangty bty)
			      | otherwise = Nothing

    (pp_nd, cs) = case condecls of
		    IfAbstractTyCon 	   -> (ptext SLIT("data"),   [])
		    IfDataTyCon cs         -> (ptext SLIT("data"),   cs)
		    IfNewTyCon c    	   -> (ptext SLIT("newtype"),[c])

    add_bars []      = empty
    add_bars [c]     = equals <+> c
    add_bars (c:cs)  = equals <+> sep (c : map (char '|' <+>) cs)

    ppr_bangty (ty, str) = ppr_str str <> pprParendIfaceType ty
    ppr_str MarkedStrict    = char '!'
    ppr_str MarkedUnboxed   = ptext SLIT("!!")
    ppr_str NotMarkedStrict = empty

showDecl exts want_name (IfaceClass {ifCtxt = context, ifName = clas, ifTyVars = tyvars, 
		      ifFDs = fds, ifSigs = sigs})
  = hang (ptext SLIT("class") <+> pprIfaceDeclHead context clas tyvars
		<+> pprFundeps fds <+> opt_where)
       2 (vcat (ppr_trim show_op sigs))
  where
    opt_where | null sigs = empty
	      | otherwise = ptext SLIT("where")
    show_op (IfaceClassOp op dm ty) 
	| want_name clas || want_name op 
	= Just (ppr_bndr op <+> dcolon <+> showIfaceType exts ty)
	| otherwise
	= Nothing

showIfaceType :: Bool -> IfaceType -> SDoc
showIfaceType True  ty = pprIfaceType ty -- -fglasgow-exts: print with the foralls
showIfaceType False ty = ppr ty	    -- otherwise, print without the foralls

ppr_trim :: (a -> Maybe SDoc) -> [a] -> [SDoc]
ppr_trim show xs
  = snd (foldr go (False, []) xs)
  where
    go x (eliding, so_far)
	| Just doc <- show x = (False, doc : so_far)
	| otherwise = if eliding then (True, so_far)
		                 else (True, ptext SLIT("...") : so_far)

ppr_bndr :: OccName -> SDoc
-- Wrap operators in ()
ppr_bndr occ = parenSymOcc occ (ppr occ)


-----------------------------------------------------------------------------
-- Commands

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
  setContextAfterLoad []
  io (GHC.workingDirectoryChanged session)
  dir <- expandPath dir
  io (setCurrentDirectory dir)

defineMacro :: String -> GHCi ()
defineMacro s = do
  let (macro_name, definition) = break isSpace s
  cmds <- io (readIORef commands)
  if (null macro_name) 
	then throwDyn (CmdLineError "invalid macro name") 
	else do
  if (macro_name `elem` map fst cmds) 
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
		    ((macro_name, keepGoing (runMacro hv)) : cmds))

runMacro :: GHC.HValue{-String -> IO String-} -> String -> GHCi ()
runMacro fun s = do
  str <- io ((unsafeCoerce# fun :: String -> IO String) s)
  stringLoop (lines str)

undefineMacro :: String -> GHCi ()
undefineMacro macro_name = do
  cmds <- io (readIORef commands)
  if (macro_name `elem` map fst builtin_commands) 
	then throwDyn (CmdLineError
		("command '" ++ macro_name ++ "' cannot be undefined"))
	else do
  if (macro_name `notElem` map fst cmds) 
	then throwDyn (CmdLineError 
		("command '" ++ macro_name ++ "' not defined"))
	else do
  io (writeIORef commands (filter ((/= macro_name) . fst) cmds))


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
  let modl = mkModule m
  session <- getSession
  result <- io (GHC.checkModule session modl printErrorsAndWarnings)
  case result of
    Nothing -> io $ putStrLn "Nothing"
    Just r  -> io $ putStrLn (showSDoc (
	case checkedModuleInfo r of
	   Just cm | Just scope <- GHC.modInfoTopLevelScope cm -> 
		let
		    (local,global) = partition ((== modl) . GHC.nameModule) scope
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
  ok <- io (GHC.load session (LoadUpTo (mkModule m)))
  afterLoad ok session

afterLoad ok session = do
  io (revertCAFs)  -- always revert CAFs on load.
  graph <- io (GHC.getModuleGraph session)
  let mods = map GHC.ms_mod graph
  mods' <- filterM (io . GHC.isLoaded session) mods
  setContextAfterLoad mods'
  modulesLoadedMsg ok mods'

setContextAfterLoad [] = do
  session <- getSession
  io (GHC.setContext session [] [prelude_mod])
setContextAfterLoad (m:_) = do
  session <- getSession
  b <- io (GHC.moduleIsInterpreted session m)
  if b then io (GHC.setContext session [m] []) 
       else io (GHC.setContext session []  [m])

modulesLoadedMsg :: SuccessFlag -> [Module] -> GHCi ()
modulesLoadedMsg ok mods = do
  dflags <- getDynFlags
  when (verbosity dflags > 0) $ do
   let mod_commas 
	| null mods = text "none."
	| otherwise = hsep (
	    punctuate comma (map pprModule mods)) <> text "."
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
-- Browsing a module's contents

browseCmd :: String -> GHCi ()
browseCmd m = 
  case words m of
    ['*':m] | looksLikeModuleName m -> browseModule m False
    [m]     | looksLikeModuleName m -> browseModule m True
    _ -> throwDyn (CmdLineError "syntax:  :browse <module>")

browseModule m exports_only = do
  s <- getSession

  let modl = mkModule m
  is_interpreted <- io (GHC.moduleIsInterpreted s modl)
  when (not is_interpreted && not exports_only) $
	throwDyn (CmdLineError ("module '" ++ m ++ "' is not interpreted"))

  -- Temporarily set the context to the module we're interested in,
  -- just so we can get an appropriate PrintUnqualified
  (as,bs) <- io (GHC.getContext s)
  io (if exports_only then GHC.setContext s [] [prelude_mod,modl]
		      else GHC.setContext s [modl] [])
  io (GHC.setContext s as bs)

  things <- io (GHC.browseModule s modl exports_only)
  unqual <- io (GHC.getPrintUnqual s)

  dflags <- getDynFlags
  let exts = dopt Opt_GlasgowExts dflags
  io (putStrLn (showSDocForUser unqual (
   	 vcat (map (showDecl exts (const True)) things)
      )))

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

newContext mods = do
  session <- getSession
  (as,bs) <- separate session mods [] []
  let bs' = if null as && prelude_mod `notElem` bs then prelude_mod:bs else bs
  io (GHC.setContext session as bs')

separate :: Session -> [String] -> [Module] -> [Module]
  -> GHCi ([Module],[Module])
separate session []           as bs = return (as,bs)
separate session (('*':m):ms) as bs = do
   let modl = mkModule m
   b <- io (GHC.moduleIsInterpreted session modl)
   if b then separate session ms (modl:as) bs
   	else throwDyn (CmdLineError ("module '" ++ m ++ "' is not interpreted"))
separate session (m:ms)       as bs = separate session ms as (mkModule m:bs)

prelude_mod = mkModule "Prelude"


addToContext mods = do
  cms <- getSession
  (as,bs) <- io (GHC.getContext cms)

  (as',bs') <- separate cms mods [] []

  let as_to_add = as' \\ (as ++ bs)
      bs_to_add = bs' \\ (as ++ bs)

  io (GHC.setContext cms (as ++ as_to_add) (bs ++ bs_to_add))


removeFromContext mods = do
  cms <- getSession
  (as,bs) <- io (GHC.getContext cms)

  (as_to_remove,bs_to_remove) <- separate cms mods [] []

  let as' = as \\ (as_to_remove ++ bs_to_remove)
      bs' = bs \\ (as_to_remove ++ bs_to_remove)

  io (GHC.setContext cms as' bs')

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
  = case words str of
	("args":args) -> setArgs args
	("prog":prog) -> setProg prog
	wds -> setOptions wds

setArgs args = do
  st <- getGHCiState
  setGHCiState st{ args = args }

setProg [prog] = do
  st <- getGHCiState
  setGHCiState st{ progname = prog }
setProg _ = do
  io (hPutStrLn stderr "syntax: :set prog <progname>")

setOptions wds =
   do -- first, deal with the GHCi opts (+s, +t, etc.)
      let (plus_opts, minus_opts)  = partition isPlus wds
      mapM_ setOpt plus_opts

      -- then, dynamic flags
      dflags <- getDynFlags
      (dflags',leftovers) <- io $ GHC.parseDynamicFlags dflags minus_opts
      setDynFlags dflags'

        -- update things if the users wants more packages
{- TODO:
        let new_packages = pkgs_after \\ pkgs_before
        when (not (null new_packages)) $
  	   newPackages new_packages
-}

      if (not (null leftovers))
		then throwDyn (CmdLineError ("unrecognised flags: " ++ 
						unwords leftovers))
		else return ()


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

{- ToDo
newPackages new_pkgs = do	-- The new packages are already in v_Packages
  session <- getSession
  io (GHC.setTargets session [])
  io (GHC.load session Nothing)
  dflags   <- getDynFlags
  io (linkPackages dflags new_pkgs)
  setContextAfterLoad []
-}

-- ---------------------------------------------------------------------------
-- code for `:show'

showCmd str =
  case words str of
	["modules" ] -> showModules
	["bindings"] -> showBindings
	["linker"]   -> io showLinkerState
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

-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	progname       :: String,
	args	       :: [String],
	session        :: GHC.Session,
	options        :: [GHCiOption]
     }

data GHCiOption 
	= ShowTiming		-- show time/allocs after evaluation
	| ShowType		-- show the type of expressions
	| RevertCAFs		-- revert CAFs after every evaluation
	deriving Eq

newtype GHCi a = GHCi { unGHCi :: IORef GHCiState -> IO a }

startGHCi :: GHCi a -> GHCiState -> IO a
startGHCi g state = do ref <- newIORef state; unGHCi g ref

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \a -> unGHCi (k a) s
  return a  = GHCi $ \s -> return a

ghciHandleDyn :: Typeable t => (t -> GHCi a) -> GHCi a -> GHCi a
ghciHandleDyn h (GHCi m) = GHCi $ \s -> 
   Exception.catchDyn (m s) (\e -> unGHCi (h e) s)

getGHCiState   = GHCi $ \r -> readIORef r
setGHCiState s = GHCi $ \r -> writeIORef r s

-- for convenience...
getSession = getGHCiState >>= return . session

getDynFlags = do
  s <- getSession
  io (GHC.getSessionDynFlags s)
setDynFlags dflags = do 
  s <- getSession 
  io (GHC.setSessionDynFlags s dflags)

isOptionSet :: GHCiOption -> GHCi Bool
isOptionSet opt
 = do st <- getGHCiState
      return (opt `elem` options st)

setOption :: GHCiOption -> GHCi ()
setOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = opt : filter (/= opt) (options st) })

unsetOption :: GHCiOption -> GHCi ()
unsetOption opt
 = do st <- getGHCiState
      setGHCiState (st{ options = filter (/= opt) (options st) })

io :: IO a -> GHCi a
io m = GHCi { unGHCi = \s -> m >>= return }

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

-----------------------------------------------------------------------------
-- timing & statistics

timeIt :: GHCi a -> GHCi a
timeIt action
  = do b <- isOptionSet ShowTiming
       if not b 
	  then action 
	  else do allocs1 <- io $ getAllocations
		  time1   <- io $ getCPUTime
		  a <- action
		  allocs2 <- io $ getAllocations
		  time2   <- io $ getCPUTime
		  io $ printTimes (fromIntegral (allocs2 - allocs1)) 
				  (time2 - time1)
		  return a

foreign import ccall unsafe "getAllocations" getAllocations :: IO Int64
	-- defined in ghc/rts/Stats.c

printTimes :: Integer -> Integer -> IO ()
printTimes allocs psecs
   = do let secs = (fromIntegral psecs / (10^12)) :: Float
	    secs_str = showFFloat (Just 2) secs
	putStrLn (showSDoc (
		 parens (text (secs_str "") <+> text "secs" <> comma <+> 
			 text (show allocs) <+> text "bytes")))

-----------------------------------------------------------------------------
-- reverting CAFs
	
revertCAFs :: IO ()
revertCAFs = do
  rts_revertCAFs
  turnOffBuffering
	-- Have to turn off buffering again, because we just 
	-- reverted stdout, stderr & stdin to their defaults.

foreign import ccall "revertCAFs" rts_revertCAFs  :: IO ()  
	-- Make it "safe", just in case

-- -----------------------------------------------------------------------------
-- Utils

expandPath :: String -> GHCi String
expandPath path = 
  case dropWhile isSpace path of
   ('~':d) -> do
	tilde <- io (getEnv "HOME")	-- will fail if HOME not defined
	return (tilde ++ '/':d)
   other -> 
	return other
