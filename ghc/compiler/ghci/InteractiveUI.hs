{-# OPTIONS -#include "Linker.h" -#include "SchedAPI.h" #-}
-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.114 2002/02/13 15:19:18 simonpj Exp $
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2000
--
-----------------------------------------------------------------------------
module InteractiveUI ( interactiveUI, ghciWelcomeMsg ) where

#include "../includes/config.h"
#include "HsVersions.h"

import Packages

import CompManager
import CmTypes		( Linkable, isObjectLinkable, ModSummary(..) )
import CmLink		( findModuleLinkable_maybe )

import HscTypes		( TyThing(..), showModMsg, InteractiveContext(..) )
import HsSyn		( TyClDecl(..), ConDecl(..), Sig(..) )
import MkIface		( ifaceTyThing )
import DriverFlags
import DriverState
import DriverUtil	( handle, remove_spaces )
import Linker
import Finder		( flushPackageCache )
import Util
import Id		( isRecordSelector, recordSelectorFieldLabel, 
			  isDataConWrapId, isDataConId, idName )
import Class		( className )
import TyCon		( tyConName, tyConClass_maybe, isPrimTyCon, DataConDetails(..) )
import FieldLabel	( fieldLabelTyCon )
import SrcLoc		( isGoodSrcLoc )
import Module		( moduleName )
import NameEnv		( nameEnvElts )
import Name		( Name, isHomePackageName, nameSrcLoc, nameOccName,
			  NamedThing(..) )
import OccName		( isSymOcc )
import BasicTypes	( defaultFixity )
import Outputable
import CmdLineOpts	( DynFlag(..), DynFlags(..), getDynFlags, saveDynFlags,
			  restoreDynFlags, dopt_unset )
import Panic		( GhcException(..), showGhcException )
import Config

#ifndef mingw32_TARGET_OS
import Posix
#endif

import Exception
import Dynamic
#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
import Readline 
#endif
import Concurrent
import IOExts
import SystemExts

import Numeric
import List
import System
import CPUTime
import Directory
import IO
import Char
import Monad

import GlaExts		( unsafeCoerce# )

import Foreign		( nullPtr )
import CString		( peekCString )

-----------------------------------------------------------------------------

ghciWelcomeMsg = "\ 
\   ___         ___ _\n\ 
\  / _ \\ /\\  /\\/ __(_)\n\ 
\ / /_\\// /_/ / /  | |      GHC Interactive, version " ++ cProjectVersion ++ ", for Haskell 98.\n\ 
\/ /_\\\\/ __  / /___| |      http://www.haskell.org/ghc/\n\ 
\\\____/\\/ /_/\\____/|_|      Type :? for help.\n"

GLOBAL_VAR(commands, builtin_commands, [(String, String -> GHCi Bool)])

builtin_commands :: [(String, String -> GHCi Bool)]
builtin_commands = [
  ("add",	keepGoing addModule),
  ("browse",    keepGoing browseCmd),
  ("cd",    	keepGoing changeDirectory),
  ("def",	keepGoing defineMacro),
  ("help",	keepGoing help),
  ("?",		keepGoing help),
  ("info",      keepGoing info),
  ("load",	keepGoing loadModule),
  ("module",	keepGoing setContext),
  ("reload",	keepGoing reloadModule),
  ("set",	keepGoing setCmd),
  ("show",	keepGoing showCmd),
  ("type",	keepGoing typeOfExpr),
  ("unset",	keepGoing unsetOptions),
  ("undef",     keepGoing undefineMacro),
  ("quit",	quit)
  ]

keepGoing :: (String -> GHCi ()) -> (String -> GHCi Bool)
keepGoing a str = a str >> return False

shortHelpText = "use :? for help.\n"

helpText = "\ 
\ Commands available from the prompt:\n\ 
\\
\   <stmt>		   evaluate/run <stmt>\n\ 
\   :add <filename> ...    add module(s) to the current target set\n\ 
\   :browse [*]<module>	   display the names defined by <module>\n\ 
\   :cd <dir>		   change directory to <dir>\n\ 
\   :def <cmd> <expr>      define a command :<cmd>\n\ 
\   :help, :?		   display this list of commands\n\ 
\   :info [<name> ...]     display information about the given names\n\ 
\   :load <filename> ...   load module(s) and their dependents\n\ 
\   :module <mod>	   set the context for expression evaluation to <mod>\n\ 
\   :reload		   reload the current module set\n\ 
\\n\ 
\   :set <option> ...	   set options\n\ 
\   :set args <arg> ...	   set the arguments returned by System.getArgs\n\ 
\   :set prog <progname>   set the value returned by System.getProgName\n\ 
\\n\ 
\   :show modules	   show the currently loaded modules\n\ 
\   :show bindings	   show the current bindings made at the prompt\n\ 
\\n\ 
\   :type <expr>	   show the type of <expr>\n\ 
\   :undef <cmd> 	   undefine user-defined command :<cmd>\n\ 
\   :unset <option> ...	   unset options\n\ 
\   :quit		   exit GHCi\n\ 
\   :!<command>		   run the shell command <command>\n\ 
\\ 
\ Options for `:set' and `:unset':\n\ 
\\ 
\    +r			revert top-level expressions after each evaluation\n\ 
\    +s                 print timing/memory stats after each evaluation\n\ 
\    +t			print type after evaluation\n\ 
\    -<flags>		most GHC command line flags can also be set here\n\ 
\                         (eg. -v2, -fglasgow-exts, etc.)\n\ 
\"

interactiveUI :: CmState -> [FilePath] -> [LibrarySpec] -> IO ()
interactiveUI cmstate paths cmdline_libs = do
   hFlush stdout
   hSetBuffering stdout NoBuffering

   dflags <- getDynFlags

   -- link in the available packages
   pkgs <- getPackageInfo
   initLinker
   linkPackages dflags cmdline_libs pkgs

   (cmstate, maybe_hval) 
	<- cmCompileExpr cmstate dflags "IO.hSetBuffering IO.stdout IO.NoBuffering Prelude.>> IO.hSetBuffering IO.stderr IO.NoBuffering"
   case maybe_hval of
	Just hval -> unsafeCoerce# hval :: IO ()
	_ -> panic "interactiveUI:buffering"

   (cmstate, maybe_hval)
	<- cmCompileExpr cmstate dflags "IO.hFlush PrelHandle.stderr"
   case maybe_hval of
	Just hval -> writeIORef flush_stderr (unsafeCoerce# hval :: IO ())
	_ -> panic "interactiveUI:stderr"

   (cmstate, maybe_hval) 
	<- cmCompileExpr cmstate dflags "IO.hFlush PrelHandle.stdout"
   case maybe_hval of
	Just hval -> writeIORef flush_stdout (unsafeCoerce# hval :: IO ())
	_ -> panic "interactiveUI:stdout"

	-- We don't want the cmd line to buffer any input that might be
	-- intended for the program, so unbuffer stdin.
   hSetBuffering stdin  NoBuffering

	-- initial context is just the Prelude
   cmstate <- cmSetContext cmstate dflags [] ["Prelude"]

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
   Readline.initialize
#endif

   startGHCi (runGHCi paths dflags) 
	GHCiState{ progname = "<interactive>",
		   args = [],
		   targets = paths,
		   cmstate = cmstate,
		   options = [] }

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
   Readline.resetTerminal Nothing
#endif

   return ()


runGHCi :: [FilePath] -> DynFlags -> GHCi ()
runGHCi paths dflags = do
  read_dot_files <- io (readIORef v_Read_DotGHCi)

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

  -- perform a :load for files given on the GHCi command line
  when (not (null paths)) $
     ghciHandle showException $
	loadModule (unwords paths)

  -- enter the interactive loop
  is_tty <- io (hIsTerminalDevice stdin)
  interactiveLoop is_tty

  -- and finally, exit
  io $ do when (verbosity dflags > 0) $ putStrLn "Leaving GHCi."


interactiveLoop is_tty = do
  -- ignore ^C exceptions caught here
  ghciHandleDyn (\e -> case e of 
			Interrupted -> ghciUnblock (interactiveLoop is_tty)
			_other      -> return ()) $ do

  -- read commands from stdin
#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
  if (is_tty) 
	then readlineLoop
	else fileLoop stdin False  -- turn off prompt for non-TTY input
#else
  fileLoop stdin True
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
  DriverUtil.handle (\_ -> return False) $ do
#ifdef mingw32_TARGET_OS
     doesFileExist name
#else
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
   cmstate <- getCmState
   (mod,imports) <- io (cmGetContext cmstate)
   when prompt (io (putStr (mkPrompt mod imports)))
   l <- io (IO.try (hGetLine hdl))
   case l of
	Left e | isEOFError e -> return ()
	       | otherwise    -> throw e
	Right l -> 
	  case remove_spaces l of
	    "" -> fileLoop hdl prompt
	    l  -> do quit <- runCommand l
          	     if quit then return () else fileLoop hdl prompt

stringLoop :: [String] -> GHCi ()
stringLoop [] = return ()
stringLoop (s:ss) = do
   case remove_spaces s of
	"" -> stringLoop ss
	l  -> do quit <- runCommand l
                 if quit then return () else stringLoop ss

mkPrompt toplevs exports
   = concat (intersperse " " (toplevs ++ map ('*':) exports)) ++ "> "

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
readlineLoop :: GHCi ()
readlineLoop = do
   cmstate <- getCmState
   (mod,imports) <- io (cmGetContext cmstate)
   io yield
   l <- io (readline (mkPrompt mod imports))
   case l of
	Nothing -> return ()
	Just l  ->
	  case remove_spaces l of
	    "" -> readlineLoop
	    l  -> do
        	  io (addHistory l)
  	  	  quit <- runCommand l
          	  if quit then return () else readlineLoop
#endif

-- Top level exception handler, just prints out the exception 
-- and carries on.
runCommand :: String -> GHCi Bool
runCommand c = 
  ghciHandle ( \exception -> do
		flushEverything
		showException exception
		return False
	     ) $
  doCommand c

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
      dflags <- io getDynFlags
      let dflags' = dopt_unset dflags Opt_WarnUnusedBinds
      (new_cmstate, result) <- 
	io $ withProgName (progname st) $ withArgs (args st) $
	cmRunStmt (cmstate st) dflags' stmt
      setGHCiState st{cmstate = new_cmstate}
      case result of
	CmRunFailed      -> return []
	CmRunException e -> showException e >> return []
	CmRunOk names    -> return names

-- possibly print the type and revert CAFs after evaluating an expression
finishEvalExpr names
 = do b <- isOptionSet ShowType
      cmstate <- getCmState
      when b (mapM_ (showTypeOfName cmstate) names)

      b <- isOptionSet RevertCAFs
      io (when b revertCAFs)
      flushEverything
      return True

showTypeOfName :: CmState -> Name -> GHCi ()
showTypeOfName cmstate n
   = do maybe_str <- io (cmTypeOfName cmstate n)
	case maybe_str of
	  Nothing  -> return ()
	  Just str -> io (putStrLn (showSDoc (ppr n) ++ " :: " ++ str))

flushEverything :: GHCi ()
flushEverything
   = io $ do Monad.join (readIORef flush_stdout)
     	     Monad.join (readIORef flush_stderr)
             return ()

specialCommand :: String -> GHCi Bool
specialCommand ('!':str) = shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  cmds <- io (readIORef commands)
  case [ (s,f) | (s,f) <- cmds, prefixMatch cmd s ] of
     []      -> io (hPutStr stdout ("unknown command `:" ++ cmd ++ "'\n" 
		                    ++ shortHelpText) >> return False)
     [(_,f)] -> f (dropWhile isSpace rest)
     cs      -> io (hPutStrLn stdout ("prefix " ++ cmd ++ 
			    	      " matches multiple commands (" ++ 
	         	     	       foldr1 (\a b -> a ++ ',':b) (map fst cs)
					 ++ ")") >> return False)

noArgs c = throwDyn (CmdLineError ("command `" ++ c ++ "' takes no arguments"))

-----------------------------------------------------------------------------
-- Commands

help :: String -> GHCi ()
help _ = io (putStr helpText)

info :: String -> GHCi ()
info "" = throwDyn (CmdLineError "syntax: `:i <thing-you-want-info-about>'")
info s = do
  let names = words s
  init_cms <- getCmState
  dflags <- io getDynFlags
  let 
    infoThings cms [] = return cms
    infoThings cms (name:names) = do
      (cms, stuff) <- io (cmInfoThing cms dflags name)
      io (putStrLn (showSDocForUser unqual (
	    vcat (intersperse (text "") (map showThing stuff))))
         )
      infoThings cms names

    unqual = cmGetPrintUnqual init_cms

    showThing (ty_thing, fixity) 
	= vcat [ text "-- " <> showTyThing ty_thing, 
		 showFixity fixity (getName ty_thing),
	         ppr (ifaceTyThing ty_thing) ]

    showFixity fix name
	| fix == defaultFixity = empty
	| otherwise            = ppr fix <+> 
				 (if isSymOcc (nameOccName name)
					then ppr name
					else char '`' <> ppr name <> char '`')

    showTyThing (AClass cl)
       = hcat [ppr cl, text " is a class", showSrcLoc (className cl)]
    showTyThing (ATyCon ty)
       | isPrimTyCon ty
       = hcat [ppr ty, text " is a primitive type constructor"]
       | otherwise
       = hcat [ppr ty, text " is a type constructor", showSrcLoc (tyConName ty)]
    showTyThing (AnId   id)
       = hcat [ppr id, text " is a ", idDescr id, showSrcLoc (idName id)]

    idDescr id
       | isRecordSelector id = 
		case tyConClass_maybe (fieldLabelTyCon (
				recordSelectorFieldLabel id)) of
			Nothing -> text "record selector"
			Just c  -> text "method in class " <> ppr c
       | isDataConWrapId id  = text "data constructor"
       | otherwise           = text "variable"

	-- also print out the source location for home things
    showSrcLoc name
	| isHomePackageName name && isGoodSrcLoc loc
	= hsep [ text ", defined at", ppr loc ]
	| otherwise
	= empty
	where loc = nameSrcLoc name

  cms <- infoThings init_cms names
  setCmState cms
  return ()

addModule :: String -> GHCi ()
addModule str = do
  let files = words str
  state <- getGHCiState
  dflags <- io (getDynFlags)
  io (revertCAFs)			-- always revert CAFs on load/add.
  let new_targets = files ++ targets state 
  graph <- io (cmDepAnal (cmstate state) dflags new_targets)
  (cmstate1, ok, mods) <- io (cmLoadModules (cmstate state) dflags graph)
  setGHCiState state{ cmstate = cmstate1, targets = new_targets }
  setContextAfterLoad mods
  modulesLoadedMsg ok mods dflags

changeDirectory :: String -> GHCi ()
changeDirectory ('~':d) = do
   tilde <- io (getEnv "HOME")	-- will fail if HOME not defined
   io (setCurrentDirectory (tilde ++ '/':d))
changeDirectory d = io (setCurrentDirectory d)

defineMacro :: String -> GHCi ()
defineMacro s = do
  let (macro_name, definition) = break isSpace s
  cmds <- io (readIORef commands)
  if (null macro_name) 
	then throwDyn (CmdLineError "invalid macro name") 
	else do
  if (macro_name `elem` map fst cmds) 
	then throwDyn (CmdLineError 
		("command `" ++ macro_name ++ "' is already defined"))
	else do

  -- give the expression a type signature, so we can be sure we're getting
  -- something of the right type.
  let new_expr = '(' : definition ++ ") :: String -> IO String"

  -- compile the expression
  cms <- getCmState
  dflags <- io getDynFlags
  (new_cmstate, maybe_hv) <- io (cmCompileExpr cms dflags new_expr)
  setCmState new_cmstate
  case maybe_hv of
     Nothing -> return ()
     Just hv -> io (writeIORef commands --
		    ((macro_name, keepGoing (runMacro hv)) : cmds))

runMacro :: HValue{-String -> IO String-} -> String -> GHCi ()
runMacro fun s = do
  str <- io ((unsafeCoerce# fun :: String -> IO String) s)
  stringLoop (lines str)

undefineMacro :: String -> GHCi ()
undefineMacro macro_name = do
  cmds <- io (readIORef commands)
  if (macro_name `elem` map fst builtin_commands) 
	then throwDyn (CmdLineError
		("command `" ++ macro_name ++ "' cannot be undefined"))
	else do
  if (macro_name `notElem` map fst cmds) 
	then throwDyn (CmdLineError 
		("command `" ++ macro_name ++ "' not defined"))
	else do
  io (writeIORef commands (filter ((/= macro_name) . fst) cmds))


loadModule :: String -> GHCi ()
loadModule str = timeIt (loadModule' str)

loadModule' str = do
  let files = words str
  state <- getGHCiState
  dflags <- io getDynFlags

  -- do the dependency anal first, so that if it fails we don't throw
  -- away the current set of modules.
  graph <- io (cmDepAnal (cmstate state) dflags files)

  -- Dependency anal ok, now unload everything
  cmstate1 <- io (cmUnload (cmstate state) dflags)
  setGHCiState state{ cmstate = cmstate1, targets = [] }

  io (revertCAFs)  -- always revert CAFs on load.
  (cmstate2, ok, mods) <- io (cmLoadModules cmstate1 dflags graph)
  setGHCiState state{ cmstate = cmstate2, targets = files }

  setContextAfterLoad mods
  modulesLoadedMsg ok mods dflags


reloadModule :: String -> GHCi ()
reloadModule "" = do
  state <- getGHCiState
  dflags <- io getDynFlags
  case targets state of
   [] -> io (putStr "no current target\n")
   paths -> do
	-- do the dependency anal first, so that if it fails we don't throw
	-- away the current set of modules.
	graph <- io (cmDepAnal (cmstate state) dflags paths)

	io (revertCAFs)		-- always revert CAFs on reload.
	(cmstate1, ok, mods) 
		<- io (cmLoadModules (cmstate state) dflags graph)
        setGHCiState state{ cmstate=cmstate1 }
	setContextAfterLoad mods
	modulesLoadedMsg ok mods dflags

reloadModule _ = noArgs ":reload"

setContextAfterLoad [] = setContext prel
setContextAfterLoad (m:_) = do
  cmstate <- getCmState
  b <- io (cmModuleIsInterpreted cmstate m)
  if b then setContext m else setContext ('*':m)

modulesLoadedMsg ok mods dflags =
  when (verbosity dflags > 0) $ do
   let mod_commas 
	| null mods = text "none."
	| otherwise = hsep (
	    punctuate comma (map text mods)) <> text "."
   case ok of
    False -> 
       io (putStrLn (showSDoc (text "Failed, modules loaded: " <> mod_commas)))
    True  -> 
       io (putStrLn (showSDoc (text "Ok, modules loaded: " <> mod_commas)))


typeOfExpr :: String -> GHCi ()
typeOfExpr str 
  = do cms <- getCmState
       dflags <- io getDynFlags
       (new_cmstate, maybe_tystr) <- io (cmTypeOfExpr cms dflags str)
       setCmState new_cmstate
       case maybe_tystr of
	  Nothing    -> return ()
	  Just tystr -> io (putStrLn tystr)

quit :: String -> GHCi Bool
quit _ = return True

shellEscape :: String -> GHCi Bool
shellEscape str = io (system str >> return False)

-----------------------------------------------------------------------------
-- Browing a module's contents

browseCmd :: String -> GHCi ()
browseCmd m = 
  case words m of
    ['*':m] | looksLikeModuleName m -> browseModule m True
    [m]     | looksLikeModuleName m -> browseModule m False
    _ -> throwDyn (CmdLineError "syntax:  :browse <module>")

browseModule m exports_only = do
  cms <- getCmState
  dflags <- io getDynFlags

  is_interpreted <- io (cmModuleIsInterpreted cms m)
  when (not is_interpreted && not exports_only) $
	throwDyn (CmdLineError ("module `" ++ m ++ "' is not interpreted"))

  -- temporarily set the context to the module we're interested in,
  -- just so we can get an appropriate PrintUnqualified
  (as,bs) <- io (cmGetContext cms)
  cms1 <- io (if exports_only then cmSetContext cms dflags [] [prel,m]
			      else cmSetContext cms dflags [m] [])
  cms2 <- io (cmSetContext cms1 dflags as bs)

  (cms3, things) <- io (cmBrowseModule cms2 dflags m exports_only)

  setCmState cms3

  let unqual = cmGetPrintUnqual cms1 -- NOTE: cms1 with the new context

      things' = filter wantToSee things

      wantToSee (AnId id) = not (isDataConId id || isDataConWrapId id)
      wantToSee _ = True

      thing_names = map getName things

      thingDecl thing@(AnId id)  = ifaceTyThing thing

      thingDecl thing@(AClass c) =
        let rn_decl = ifaceTyThing thing in
	case rn_decl of
	  ClassDecl { tcdSigs = cons } -> 
		rn_decl{ tcdSigs = filter methodIsVisible cons }
	  other -> other
        where
           methodIsVisible (ClassOpSig n _ _ _) = n `elem` thing_names

      thingDecl thing@(ATyCon t) =
        let rn_decl = ifaceTyThing thing in
	case rn_decl of
	  TyData { tcdCons = DataCons cons } -> 
		rn_decl{ tcdCons = DataCons (filter conIsVisible cons) }
	  other -> other
        where
	  conIsVisible (ConDecl n _ _ _ _ _) = n `elem` thing_names

  io (putStrLn (showSDocForUser unqual (
   	 vcat (map (ppr . thingDecl) things')))
   )

  where

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
  cms <- getCmState
  dflags <- io getDynFlags
  (as,bs) <- separate cms mods [] []
  let bs' = if null as && prel `notElem` bs then prel:bs else bs
  cms' <- io (cmSetContext cms dflags as bs')
  setCmState cms'

separate cmstate []           as bs = return (as,bs)
separate cmstate (('*':m):ms) as bs = separate cmstate ms as (m:bs)
separate cmstate (m:ms)       as bs = do 
   b <- io (cmModuleIsInterpreted cmstate m)
   if b then separate cmstate ms (m:as) bs
   	else throwDyn (CmdLineError ("module `" ++ m ++ "' is not interpreted"))
	
prel = "Prelude"


addToContext mods = do
  cms <- getCmState
  dflags <- io getDynFlags
  (as,bs) <- io (cmGetContext cms)

  (as',bs') <- separate cms mods [] []

  let as_to_add = as' \\ (as ++ bs)
      bs_to_add = bs' \\ (as ++ bs)

  cms' <- io (cmSetContext cms dflags 
			(as ++ as_to_add) (bs ++ bs_to_add))
  setCmState cms'


removeFromContext mods = do
  cms <- getCmState
  dflags <- io getDynFlags
  (as,bs) <- io (cmGetContext cms)

  (as_to_remove,bs_to_remove) <- separate cms mods [] []

  let as' = as \\ (as_to_remove ++ bs_to_remove)
      bs' = bs \\ (as_to_remove ++ bs_to_remove)

  cms' <- io (cmSetContext cms dflags as' bs')
  setCmState cms'

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
      mapM setOpt plus_opts

      -- now, the GHC flags
      pkgs_before <- io (readIORef v_Packages)
      leftovers   <- io (processArgs static_flags minus_opts [])
      pkgs_after  <- io (readIORef v_Packages)

      -- update things if the users wants more packages
      when (pkgs_before /= pkgs_after) $
	 newPackages (pkgs_after \\ pkgs_before)

      -- then, dynamic flags
      io $ do 
	restoreDynFlags
        leftovers <- processArgs dynamic_flags leftovers []
	saveDynFlags

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
	  then io (putStrLn ("unknown option: `" ++ head rest2 ++ "'"))
	  else do

       mapM unsetOpt plus_opts
 
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
	Nothing -> io (putStrLn ("unknown option: `" ++ str ++ "'"))
	Just o  -> setOption o

unsetOpt ('+':str)
  = case strToGHCiOpt str of
	Nothing -> io (putStrLn ("unknown option: `" ++ str ++ "'"))
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

newPackages new_pkgs = do
  state <- getGHCiState
  dflags <- io getDynFlags
  cmstate1 <- io (cmUnload (cmstate state) dflags)
  setGHCiState state{ cmstate = cmstate1, targets = [] }

  io $ do
    pkgs <- getPackageInfo
    flushPackageCache pkgs
   
    new_pkg_info <- getPackageDetails new_pkgs
    mapM_ (linkPackage dflags) (reverse new_pkg_info)

-----------------------------------------------------------------------------
-- code for `:show'

showCmd str =
  case words str of
	["modules" ] -> showModules
	["bindings"] -> showBindings
	_ -> throwDyn (CmdLineError "syntax:  :show [modules|bindings]")

showModules = do
  cms <- getCmState
  let mg = cmGetModuleGraph cms
      ls = cmGetLinkables   cms
      maybe_linkables = map (findModuleLinkable_maybe ls) 
				(map (moduleName.ms_mod) mg)
  zipWithM showModule mg maybe_linkables
  return ()

showModule :: ModSummary -> Maybe Linkable -> GHCi ()
showModule m (Just l) = do
  io (putStrLn (showModMsg (isObjectLinkable l) (ms_mod m) (ms_location m)))
showModule _ Nothing = panic "missing linkable"

showBindings = do
  cms <- getCmState
  let
	unqual = cmGetPrintUnqual cms
	showBinding b = putStrLn (showSDocForUser unqual (ppr (ifaceTyThing b)))

  io (mapM showBinding (cmGetBindings cms))
  return ()

-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	progname       :: String,
	args	       :: [String],
	targets        :: [FilePath],
	cmstate        :: CmState,
	options        :: [GHCiOption]
     }

data GHCiOption 
	= ShowTiming		-- show time/allocs after evaluation
	| ShowType		-- show the type of expressions
	| RevertCAFs		-- revert CAFs after every evaluation
	deriving Eq

GLOBAL_VAR(flush_stdout, error "no flush_stdout", IO ())
GLOBAL_VAR(flush_stderr, error "no flush_stdout", IO ())

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
getCmState = getGHCiState >>= return . cmstate
setCmState cms = do s <- getGHCiState; setGHCiState s{cmstate=cms}

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
	(\e -> unGHCi (ghciHandle h (ghciUnblock (h e))) s)

ghciUnblock :: GHCi a -> GHCi a
ghciUnblock (GHCi a) = GHCi $ \s -> Exception.unblock (a s)

-----------------------------------------------------------------------------
-- package loader

-- Left: full path name of a .o file, including trailing .o
-- Right: "unadorned" name of a .DLL/.so
--        e.g.    On unix     "qt"  denotes "libqt.so"
--                On WinDoze  "burble"  denotes "burble.DLL"
--        addDLL is platform-specific and adds the lib/.so/.DLL
--        suffixes platform-dependently; we don't do that here.
-- 
-- For dynamic objects only, try to find the object file in all the 
-- directories specified in v_Library_Paths before giving up.

type LibrarySpec
   = Either FilePath String

showLS (Left nm)  = "(static) " ++ nm
showLS (Right nm) = "(dynamic) " ++ nm

linkPackages :: DynFlags -> [LibrarySpec] -> [PackageConfig] -> IO ()
linkPackages dflags cmdline_lib_specs pkgs
   = do mapM_ (linkPackage dflags) (reverse pkgs)
        lib_paths <- readIORef v_Library_paths
        mapM_ (preloadLib dflags lib_paths) cmdline_lib_specs
	if (null cmdline_lib_specs)
	   then return ()
	   else do maybePutStr dflags "final link ... "
		   ok <- resolveObjs
		   if ok then maybePutStrLn dflags "done."
	      		 else throwDyn (InstallationError 
                                          "linking extra libraries/objects failed")
     where
        preloadLib :: DynFlags -> [String] -> LibrarySpec -> IO ()
        preloadLib dflags lib_paths lib_spec
           = do maybePutStr dflags ("Loading object " ++ showLS lib_spec ++ " ... ")
                case lib_spec of
                   Left static_ish
                      -> do b <- preload_static lib_paths static_ish
                            maybePutStrLn dflags (if b  then "done." 
							else "not found")
                   Right dll_unadorned
                      -> -- We add "" to the set of paths to try, so that
                         -- if none of the real paths match, we force addDLL
                         -- to look in the default dynamic-link search paths.
                         do maybe_errstr <- preload_dynamic (lib_paths++[""]) 
                                                            dll_unadorned
                            case maybe_errstr of
                               Nothing -> return ()
                               Just mm -> preloadFailed mm lib_paths lib_spec
                            maybePutStrLn dflags "done"

        preloadFailed :: String -> [String] -> LibrarySpec -> IO ()
        preloadFailed sys_errmsg paths spec
           = do maybePutStr dflags
		       ("failed.\nDynamic linker error message was:\n   " 
                        ++ sys_errmsg  ++ "\nWhilst trying to load:  " 
                        ++ showLS spec ++ "\nDirectories to search are:\n"
                        ++ unlines (map ("   "++) paths) )
                give_up

        -- not interested in the paths in the static case.
        preload_static paths name
           = do b <- doesFileExist name
                if not b then return False
                         else loadObj name >> return True

        -- return Nothing == success, else Just error message from addDLL
        preload_dynamic [] name
           = return Nothing
        preload_dynamic (path:paths) rootname
           = do -- addDLL returns NULL on success
                maybe_errmsg <- addDLL path rootname
                if    maybe_errmsg == nullPtr
                 then preload_dynamic paths rootname
                 else do str <- peekCString maybe_errmsg
                         return (Just str)

        give_up 
           = (throwDyn . CmdLineError)
                "user specified .o/.so/.DLL could not be loaded."

-- Packages that don't need loading, because the compiler shares them with
-- the interpreted program.
dont_load_these = [ "gmp", "rts" ]

-- Packages that are already linked into GHCi.  For mingw32, we only
-- skip gmp and rts, since std and after need to load the msvcrt.dll
-- library which std depends on.
loaded_in_ghci
#          ifndef mingw32_TARGET_OS
           = [ "std", "concurrent", "posix", "text", "util" ]
#          else
	   = [ ]
#          endif

linkPackage :: DynFlags -> PackageConfig -> IO ()
linkPackage dflags pkg
   | name pkg `elem` dont_load_these = return ()
   | otherwise
   = do 
        -- For each obj, try obj.o and if that fails, obj.so.
        -- Complication: all the .so's must be loaded before any of the .o's.  
        let dirs      =  library_dirs pkg
        let objs      =  hs_libraries pkg ++ extra_libraries pkg
        classifieds   <- mapM (locateOneObj dirs) objs

	-- Don't load the .so libs if this is a package GHCi is already
	-- linked against, because we'll already have the .so linked in.
	let (so_libs, obj_libs) = partition isRight classifieds
        let sos_first | name pkg `elem` loaded_in_ghci = obj_libs
		      | otherwise      		       = so_libs ++ obj_libs

	maybePutStr dflags ("Loading package " ++ name pkg ++ " ... ")
        mapM loadClassified sos_first
        maybePutStr dflags "linking ... "
        ok <- resolveObjs
	if ok then maybePutStrLn dflags "done."
	      else panic ("can't load package `" ++ name pkg ++ "'")
     where
        isRight (Right _) = True
        isRight (Left _)  = False

loadClassified :: LibrarySpec -> IO ()
loadClassified (Left obj_absolute_filename)
   = do loadObj obj_absolute_filename
loadClassified (Right dll_unadorned)
   = do maybe_errmsg <- addDLL "" dll_unadorned -- doesn't seem right to me
        if    maybe_errmsg == nullPtr
         then return ()
         else do str <- peekCString maybe_errmsg
                 throwDyn (CmdLineError ("can't load .so/.DLL for: " 
                                       ++ dll_unadorned ++ " (" ++ str ++ ")" ))

locateOneObj :: [FilePath] -> String -> IO LibrarySpec
locateOneObj []     obj 
   = return (Right obj) -- we assume
locateOneObj (d:ds) obj 
   = do let path = d ++ '/':obj ++ ".o"
        b <- doesFileExist path
        if b then return (Left path) else locateOneObj ds obj

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
		  io $ printTimes (allocs2 - allocs1) (time2 - time1)
		  return a

foreign import "getAllocations" getAllocations :: IO Int

printTimes :: Int -> Integer -> IO ()
printTimes allocs psecs
   = do let secs = (fromIntegral psecs / (10^12)) :: Float
	    secs_str = showFFloat (Just 2) secs
	putStrLn (showSDoc (
		 parens (text (secs_str "") <+> text "secs" <> comma <+> 
			 int allocs <+> text "bytes")))

-----------------------------------------------------------------------------
-- utils

looksLikeModuleName [] = False
looksLikeModuleName (c:cs) = isUpper c && all isAlphaNumEx cs

isAlphaNumEx c = isAlphaNum c || c == '_'

maybePutStr dflags s | verbosity dflags > 0 = putStr s
		     | otherwise	    = return ()

maybePutStrLn dflags s | verbosity dflags > 0 = putStrLn s
		       | otherwise	      = return ()

-----------------------------------------------------------------------------
-- reverting CAFs
	
foreign import revertCAFs :: IO ()	-- make it "safe", just in case
