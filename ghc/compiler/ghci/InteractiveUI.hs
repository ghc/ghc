-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.10 2000/11/21 16:42:58 simonmar Exp $
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2000
--
-----------------------------------------------------------------------------

module InteractiveUI (interactiveUI) where

#include "HsVersions.h"

import CompManager
import CmStaticInfo
import DriverFlags
import DriverUtil
import DriverState
import Linker
import Module
import Outputable
import Panic
import Util

import Exception
import Readline
import IOExts

import System
import Directory
import IO
import Char

-----------------------------------------------------------------------------

ghciWelcomeMsg = "\ 
\ _____  __   __  ____         _________________________________________________\n\ 
\(|	 ||   || (|  |)        GHC Interactive, version 5.00   	                \n\ 
\||  __  ||___|| ||     ()     For Haskell 98.                    		\n\ 
\||   |) ||---|| ||     ||     http://www.haskell.org/ghc         		\n\ 
\||   || ||   || ||     (|     Bug reports to: glasgow-haskell-bugs@haskell.org \n\ 
\(|___|| ||   || (|__|) \\\\______________________________________________________\n"

commands :: [(String, String -> GHCi ())]
commands = [
  ("cd",    	changeDirectory),
  ("help",	help),
  ("?",		help),
  ("load",	loadModule),
  ("reload",	reloadModule),
  ("set",	setOptions),
  ("type",	typeOfExpr),
  ("quit",	quit)
  ]

shortHelpText = "use :? for help.\n"

helpText = "\ 
\   <expr>		evaluate <expr>\n\ 
\   :cd <dir>		change directory to <dir>\n\ 
\   :help		display this list of commands\n\ 
\   :?			display this list of commands\n\ 
\   :load <filename>    load a module (and it dependents)\n\ 
\   :reload		reload the current program\n\ 
\   :set <opetion> ...	set options\n\ 
\   :type <expr>	show the type of <expr>\n\ 
\   :quit		exit GHCi\n\ 
\   :!<command>		run the shell command <command>\n\ 
\"

interactiveUI :: CmState -> IO ()
interactiveUI st = do
   hPutStrLn stdout ghciWelcomeMsg
   hFlush stdout
   hSetBuffering stdout NoBuffering

   -- link in the available packages
   pkgs <- getPackageInfo
   linkPackages (reverse pkgs)

#ifndef NO_READLINE
   Readline.initialize
#endif
   _ <- (unGHCi uiLoop) GHCiState{ modules = [],
				   current_module = defaultCurrentModule,
	  	                   target = Nothing,
			           cmstate = st }
   return ()

uiLoop :: GHCi ()
uiLoop = do
  st <- getGHCiState
#ifndef NO_READLINE
  l <- io (readline (moduleNameUserString (current_module st) ++ "> "))
#else
  l <- io (hGetLine stdin)
#endif
  case l of
    Nothing -> exitGHCi
    Just "" -> uiLoop
    Just l  -> do
#ifndef NO_READLINE
          io (addHistory l)
#endif
  	  runCommand l
	  uiLoop  

exitGHCi = io $ do putStrLn "Leaving GHCi."; exitWith ExitSuccess

-- Top level exception handler, just prints out the exception 
-- and carries on.
runCommand c = 
  ghciHandle ( \other_exception ->io (putStrLn (show other_exception) )) $
  ghciHandleDyn
    (\dyn -> case dyn of
		PhaseFailed phase code ->
			io ( putStrLn ("Phase " ++ phase ++ " failed (code "
				        ++ show code ++ ")"))
		Interrupted -> io (putStrLn "Interrupted.")
		_ -> io (putStrLn (show (dyn :: BarfKind)))
    ) $
   doCommand c

doCommand (':' : command) = specialCommand command
doCommand expr
 = do st <- getGHCiState
      dflags <- io (getDynFlags)
      (new_cmstate, maybe_hvalue) <- 
      	 io (cmGetExpr (cmstate st) dflags (current_module st) expr)
      setGHCiState st{cmstate = new_cmstate}
      case maybe_hvalue of
      	 Nothing -> return ()
      	 Just hv -> io (cmRunExpr hv)
{-
  let (mod,'.':str) = break (=='.') expr
  case cmLookupSymbol (mkOrig varName (mkModuleName mod) (_PK_ str)) (cmstate st) of
	Nothing -> io (putStrLn "nothing.")
	Just e  -> io (
  return ()
-}

specialCommand ('!':str) = shellEscape (dropWhile isSpace str)
specialCommand str = do
  let (cmd,rest) = break isSpace str
  case [ (s,f) | (s,f) <- commands, prefixMatch cmd s ] of
     []      -> io $ hPutStr stdout ("uknown command `:" ++ cmd ++ "'\n" 
				    ++ shortHelpText)
     [(_,f)] -> f (dropWhile isSpace rest)
     cs      -> io $ hPutStrLn stdout ("prefix " ++ cmd ++ 
			    	       " matches multiple commands (" ++ 
	         	     	       foldr1 (\a b -> a ++ ',':b) (map fst cs)
					 ++ ")")

noArgs c = io (hPutStrLn stdout ("command `:" ++ c ++ "' takes no arguments"))

-----------------------------------------------------------------------------
-- Commands

help :: String -> GHCi ()
help _ = io (putStr helpText)

changeDirectory :: String -> GHCi ()
changeDirectory = io . setCurrentDirectory

loadModule :: String -> GHCi ()
loadModule path = do
  state <- getGHCiState
  (new_cmstate, ok, mods) <- io (cmLoadModule (cmstate state) path)

  let new_state = GHCiState {
  			cmstate = new_cmstate,
			modules = mods,
			current_module = case mods of 
					   [] -> defaultCurrentModule
					   xs -> last xs,
			target = Just path
		   }
  setGHCiState new_state

  let mod_commas 
	| null mods = text "none."
	| otherwise = hsep (
	    punctuate comma (map (text.moduleNameUserString) mods)) <> text "."
  case ok of
    False -> 
       io (putStrLn (showSDoc (text "Failed, modules loaded: " <> mod_commas)))
    True  -> 
       io (putStrLn (showSDoc (text "Ok, modules loaded: " <> mod_commas)))

reloadModule :: String -> GHCi ()
reloadModule "" = do
  state <- getGHCiState
  case target state of
   Nothing -> io (putStr "no current target\n")
   Just path -> do (new_cmstate, ok, mod) 
			<- io (cmLoadModule (cmstate state) path)
  		   setGHCiState state{cmstate=new_cmstate}  
reloadModule _ = noArgs ":reload"

-- set options in the interpreter.  Syntax is exactly the same as the
-- ghc command line, except that certain options aren't available (-C,
-- -E etc.)
--
-- This is pretty fragile: most options won't work as expected.  ToDo:
-- figure out which ones & disallow them.
setOptions :: String -> GHCi ()
setOptions str =
   io (do leftovers <- processArgs static_flags (words str) []
	  dyn_flags <- readIORef v_InitDynFlags
	  writeIORef v_DynFlags dyn_flags
	  leftovers <- processArgs dynamic_flags leftovers []
	  dyn_flags <- readIORef v_DynFlags
	  writeIORef v_InitDynFlags dyn_flags
          if (not (null leftovers))
		then throwDyn (OtherError ("unrecognised flags: " ++ 
						unwords leftovers))
		else return ()
   )

typeOfExpr :: String -> GHCi ()
typeOfExpr str 
  = do st <- getGHCiState
       dflags <- io (getDynFlags)
       (st, maybe_ty) <- io (cmTypeExpr (cmstate st) dflags 
				(current_module st) str)
       case maybe_ty of
	 Nothing -> return ()
	 Just ty -> io (putStrLn (showSDoc (ppr ty)))

quit :: String -> GHCi ()
quit _ = exitGHCi

shellEscape :: String -> GHCi ()
shellEscape str = io (system str >> return ())

-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	modules	       :: [ModuleName],
	current_module :: ModuleName,
	target         :: Maybe FilePath,
	cmstate        :: CmState
     }

defaultCurrentModule = mkModuleName "Prelude"

newtype GHCi a = GHCi { unGHCi :: GHCiState -> IO (GHCiState, a) }

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \(s,a) -> unGHCi (k a) s
  return a  = GHCi $ \s -> return (s,a)

getGHCiState   = GHCi $ \s -> return (s,s)
setGHCiState s = GHCi $ \_ -> return (s,())

io m = GHCi $ \s -> m >>= \a -> return (s,a)

ghciHandle h (GHCi m) = GHCi $ \s -> 
   Exception.catch (m s) (\e -> unGHCi (h e) s)
ghciHandleDyn h (GHCi m) = GHCi $ \s -> 
   Exception.catchDyn (m s) (\e -> unGHCi (h e) s)

-----------------------------------------------------------------------------
-- package loader

linkPackages :: [Package] -> IO ()
linkPackages pkgs = mapM_ linkPackage pkgs

linkPackage :: Package -> IO ()
-- ignore rts and gmp for now (ToDo; better?)
linkPackage pkg | name pkg `elem` ["rts", "gmp"] = return ()
linkPackage pkg = do
  putStr ("Loading package " ++ name pkg ++ " ... ")
  let dirs = library_dirs pkg
  let objs = map (++".o") (hs_libraries pkg ++ extra_libraries pkg)
  mapM (linkOneObj dirs) objs
  putStr "resolving ... "
  resolveObjs
  putStrLn "done."

linkOneObj dirs obj = do
  filename <- findFile dirs obj
  loadObj filename

findFile [] obj = throwDyn (OtherError ("can't find " ++ obj))
findFile (d:ds) obj = do
  let path = d ++ '/':obj
  b <- doesFileExist path
  if b then return path else findFile ds obj
