-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.3 2000/11/16 16:54:36 simonmar Exp $
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
import DriverUtil
import DriverState
import Linker
import Module
import RdrName				-- tmp
import OccName				-- tmp
import Panic
import Util

import Exception
import Readline
import IOExts

import System
import Directory
import IO
import Char

import PrelGHC  ( unsafeCoerce# )

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
  ("quit",	quit),
  ("!",		shellEscape)
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
   _ <- (unGHCi uiLoop) GHCiState{ current_module = mkModuleName "Prelude", 
	  	                   target = Nothing,
			           cmstate = st }
   return ()

uiLoop :: GHCi ()
uiLoop = do
  st <- getGHCiState
#ifndef NO_READLINE
  l <- io (readline (moduleNameUserString (current_module st)  ++ "> "))
#else
  l <- io (hGetLine stdin)
#endif
  case l of
    Nothing -> return ()
    Just "" -> uiLoop
    Just l  -> do
#ifndef NO_READLINE
          io (addHistory l)
#endif
  	  runCommand l
	  uiLoop  

runCommand c = 
  myCatchDyn (doCommand c) 
    (\dyn -> case dyn of
		PhaseFailed phase code ->
			io ( putStrLn ("Phase " ++ phase ++ " failed (code "
				        ++ show code ++ ")"))
		Interrupted -> io (putStrLn "Interrupted.")
		_ -> io (putStrLn (show (dyn :: BarfKind)))
    )

doCommand (':' : command) = specialCommand command
doCommand expr = do
  st <- getGHCiState
  io (hPutStrLn stdout ("Run expression: " ++ expr))
  let (mod,'.':str) = break (=='.') expr
  case cmLookupSymbol (mkOrig varName (mkModuleName mod) (_PK_ str)) (cmstate st) of
	Nothing -> io (putStrLn "nothing.")
	Just e  -> io (do unsafeCoerce# e :: IO ()
		          putStrLn "done.")
  return ()

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

-- ToDo: don't forget to catch errors

help :: String -> GHCi ()
help _ = io (putStr helpText)

changeDirectory :: String -> GHCi ()
changeDirectory = io . setCurrentDirectory

loadModule :: String -> GHCi ()
loadModule path = do
  state <- getGHCiState
  (new_cmstate, mod) <- io (cmLoadModule (cmstate state) ({-ToDo!!-}mkModuleName path))
  setGHCiState state{cmstate=new_cmstate, target=Just path}  

reloadModule :: String -> GHCi ()
reloadModule "" = do
  state <- getGHCiState
  case target state of
	Nothing -> io (putStr "no current target\n")
	Just path -> do (new_cmstate, mod) <- io (cmLoadModule (cmstate state) (mkModuleName path))
  			setGHCiState state{cmstate=new_cmstate}  
reloadModule _ = noArgs ":reload"

setOptions :: String -> GHCi ()
setOptions = panic "setOptions"

typeOfExpr :: String -> GHCi ()
typeOfExpr = panic "typeOfExpr"

quit :: String -> GHCi ()
quit _ = return ()

shellEscape :: String -> GHCi ()
shellEscape str = io (system str >> return ())

-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	current_module :: ModuleName,
	target         :: Maybe FilePath,
	cmstate        :: CmState
     }

newtype GHCi a = GHCi { unGHCi :: GHCiState -> IO (GHCiState, a) }

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \(s,a) -> unGHCi (k a) s
  return a  = GHCi $ \s -> return (s,a)

getGHCiState   = GHCi $ \s -> return (s,s)
setGHCiState s = GHCi $ \_ -> return (s,())

io m = GHCi $ \s -> m >>= \a -> return (s,a)

myCatch (GHCi m) h = GHCi $ \s -> 
   Exception.catch (m s) (\e -> unGHCi (h e) s)
myCatchDyn (GHCi m) h = GHCi $ \s -> 
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


