-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.1 2000/11/16 10:48:22 simonmar Exp $
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2000
--
-----------------------------------------------------------------------------

module InteractiveUI where

import CompManager
import Module
import Panic
import Util

import Readline

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
   hPutStr stdout ghciWelcomeMsg
   hFlush stdout
   hSetBuffering stdout NoBuffering
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
  l <- io (readline (moduleNameUserString (current_module st)  ++ ">"))
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

runCommand c = myCatch (doCommand c) 
			(\e -> io (hPutStr stdout ("Error: " ++ show e)))

doCommand (':' : command) = specialCommand command
doCommand expr = do
  io (hPutStrLn stdout ("Run expression: " ++ expr))
  return ()

specialCommand str = do
  let (cmd,rest) = break isSpace str
  case [ (s,f) | (s,f) <- commands, prefixMatch cmd s ] of
     []      -> io $ hPutStr stdout ("uknown command `:" ++ cmd ++ "'\n" 
				    ++ shortHelpText)
     [(_,f)] -> f rest
     cs      -> io $ hPutStrLn stdout ("prefix " ++ cmd ++ 
			    	       " matches multiple commands (" ++ 
	         	     	       foldr1 (\a b -> a ++ ',':b) (map fst cs) ++ ")")

noArgs c = io (hPutStr stdout ("command `:" ++ c ++ "' takes no arguments"))

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
	Nothing -> io (hPutStr stdout "no current target")
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

myCatch (GHCi m) h = GHCi $ \s -> catch (m s) (\e -> unGHCi (h e) s)
