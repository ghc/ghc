-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.29 2001/01/18 16:30:00 simonmar Exp $
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
import DriverState
import Linker
import Finder
import Module
import Outputable
import Util
import PprType	{- instance Outputable Type; do not delete -}
import Panic	( GhcException(..) )

import Exception
#ifndef NO_READLINE
import Readline
#endif
import IOExts

import Numeric
import List
import System
import CPUTime
import Directory
import IO
import Char
import Monad ( when )


-----------------------------------------------------------------------------

ghciWelcomeMsg = "\ 
\ _____  __   __  ____         _________________________________________________\n\ 
\(|	 ||   || (|  |)        GHC Interactive, version 5.00   	                \n\ 
\||  __  ||___|| ||     ()     For Haskell 98.                    		\n\ 
\||   |) ||---|| ||     ||     http://www.haskell.org/ghc         		\n\ 
\||   || ||   || ||     (|     Bug reports to: glasgow-haskell-bugs@haskell.org \n\ 
\(|___|| ||   || (|__|) \\\\______________________________________________________\n"

commands :: [(String, String -> GHCi Bool)]
commands = [
  ("add",	keepGoing addModule),
  ("cd",    	keepGoing changeDirectory),
  ("help",	keepGoing help),
  ("?",		keepGoing help),
  ("load",	keepGoing loadModule),
  ("module",	keepGoing setContext),
  ("reload",	keepGoing reloadModule),
  ("set",	keepGoing setOptions),
  ("type",	keepGoing typeOfExpr),
  ("unset",	keepGoing unsetOptions),
  ("quit",	quit)
  ]

keepGoing :: (String -> GHCi ()) -> (String -> GHCi Bool)
keepGoing a str = a str >> return False

shortHelpText = "use :? for help.\n"

helpText = "\ 
\ Commands available from the prompt:\n\ 
\\  
\   <expr>		evaluate <expr>\n\ 
\   :add <filename>     add a module to the current set\n\ 
\   :cd <dir>		change directory to <dir>\n\ 
\   :help, :?		display this list of commands\n\ 
\   :load <filename>    load a module (and it dependents)\n\ 
\   :module <mod>	set the context for expression evaluation to <mod>\n\ 
\   :reload		reload the current module set\n\ 
\   :set <option> ...	set options\n\ 
\   :unset <option> ...	unset options\n\ 
\   :type <expr>	show the type of <expr>\n\ 
\   :quit		exit GHCi\n\ 
\   :!<command>		run the shell command <command>\n\ 
\\ 
\ Options for `:set' and `:unset':\n\ 
\\ 
\    +s                 print timing/memory stats after each evaluation\n\ 
\    +t			print type after evaluation\n\ 
\    -<flags>		most GHC command line flags can also be set here\n\ 
\                         (eg. -v2, -fglasgow-exts, etc.)\n\ 
\"

interactiveUI :: CmState -> Maybe FilePath -> IO ()
interactiveUI cmstate mod = do
   hPutStrLn stdout ghciWelcomeMsg
   hFlush stdout
   hSetBuffering stdout NoBuffering

   -- link in the available packages
   pkgs <- getPackageInfo
   linkPackages (reverse pkgs)

   (cmstate', ok, mods) <-
   	case mod of
	     Nothing  -> return (cmstate, True, [])
	     Just m -> cmLoadModule cmstate m

#ifndef NO_READLINE
   Readline.initialize
#endif

   prel <- moduleNameToModule defaultCurrentModuleName
   writeIORef defaultCurrentModule prel

   let this_mod = case mods of 
	  	      []   -> prel
  	  	      m:ms -> m

   (unGHCi uiLoop) GHCiState{ modules = mods,
			      current_module = this_mod,
	  	              target = mod,
			      cmstate = cmstate',
			      options = [ShowTiming],
                              last_expr = Nothing}
   return ()


uiLoop :: GHCi ()
uiLoop = do
  st <- getGHCiState
#ifndef NO_READLINE
  l <- io (readline (moduleUserString (current_module st) ++ "> "))
#else
  l_ok <- io (hGetLine stdin)
  let l = Just l_ok
#endif
  case l of
    Nothing -> exitGHCi
    Just "" -> uiLoop
    Just l  -> do
#ifndef NO_READLINE
          io (addHistory l)
#endif
  	  quit <- runCommand l
          if quit then exitGHCi else uiLoop

exitGHCi = io $ do putStrLn "Leaving GHCi." 

-- Top level exception handler, just prints out the exception 
-- and carries on.
runCommand :: String -> GHCi Bool
runCommand c = 
  ghciHandle ( 
     \other_exception 
        -> io (putStrLn (show other_exception)) >> return False
  ) $
  ghciHandleDyn
    (\dyn -> case dyn of
		PhaseFailed phase code ->
			io ( putStrLn ("Phase " ++ phase ++ " failed (code "
				        ++ show code ++ ")"))
		Interrupted -> io (putStrLn "Interrupted.")
		_ -> io (putStrLn (show (dyn :: GhcException)))
             >> return False
    ) $
   doCommand c

doCommand (':' : command) = specialCommand command
doCommand expr
   = do expr_expanded <- expandExpr expr
        -- io (putStrLn ( "Before: " ++ expr ++ "\nAfter:  " ++ expr_expanded))
        expr_ok <- timeIt (do ok <- evalExpr expr_expanded
                              when ok (evalExpr "PrelHandle.hFlush PrelHandle.stdout" >> return ())
                              when ok (evalExpr "PrelHandle.hFlush PrelHandle.stderr" >> return ())
                              return ok)
        when expr_ok (rememberExpr expr_expanded)
        return False

-- Returned Bool indicates whether or not the expr was successfully
-- parsed, renamed and typechecked.
evalExpr :: String -> GHCi Bool
evalExpr expr
 | null (filter (not.isSpace) expr)
 = return False
 | otherwise
 = do st <- getGHCiState
      dflags <- io (getDynFlags)
      (new_cmstate, maybe_stuff) <- 
      	 io (cmGetExpr (cmstate st) dflags (current_module st) expr True)
      setGHCiState st{cmstate = new_cmstate}
      case maybe_stuff of
      	 Nothing -> return False
      	 Just (hv, unqual, ty)
	   -> do io (cmRunExpr hv)
		 b <- isOptionSet ShowType
		 io (when b (printForUser stdout unqual (text "::" <+> ppr ty)))
                 return True
	
{-
  let (mod,'.':str) = break (=='.') expr
  case cmLookupSymbol (mkOrig varName (mkModuleName mod) (_PK_ str)) (cmstate st) of
	Nothing -> io (putStrLn "nothing.")
	Just e  -> io (
  return ()
-}

specialCommand :: String -> GHCi Bool
specialCommand ('!':str) = shellEscape (dropWhile isSpace str) 
specialCommand str = do
  let (cmd,rest) = break isSpace str
  case [ (s,f) | (s,f) <- commands, prefixMatch cmd s ] of
     []      -> io (hPutStr stdout ("unknown command `:" ++ cmd ++ "'\n" 
		                    ++ shortHelpText) >> return False)
     [(_,f)] -> f (dropWhile isSpace rest)
     cs      -> io (hPutStrLn stdout ("prefix " ++ cmd ++ 
			    	      " matches multiple commands (" ++ 
	         	     	       foldr1 (\a b -> a ++ ',':b) (map fst cs)
					 ++ ")") >> return False)

noArgs c = io (hPutStrLn stdout ("command `:" ++ c ++ "' takes no arguments"))

-----------------------------------------------------------------------------
-- Commands

help :: String -> GHCi ()
help _ = io (putStr helpText)

addModule :: String -> GHCi ()
addModule _ = throwDyn (OtherError ":add not implemented")

setContext :: String -> GHCi ()
setContext ""
  = throwDyn (OtherError "syntax: `:m <module>'")
setContext m | not (isUpper (head m)) || not (all isAlphaNum (tail m))
  = throwDyn (OtherError ("strange looking module name: `" ++ m ++ "'"))
setContext mn
  = do m <- io (moduleNameToModule (mkModuleName mn))
       st <- getGHCiState
       setGHCiState st{current_module = m}

moduleNameToModule :: ModuleName -> IO Module
moduleNameToModule mn
 = do maybe_stuff <- findModule mn
      case maybe_stuff of
	 Nothing -> throwDyn (OtherError ("can't find module `"
					    ++ moduleNameUserString mn ++ "'"))
   	 Just (m,_) -> return m

changeDirectory :: String -> GHCi ()
changeDirectory d = io (setCurrentDirectory d)

loadModule :: String -> GHCi ()
loadModule path = timeIt (loadModule' path)

loadModule' path = do
  state <- getGHCiState
  cmstate1 <- io (cmUnload (cmstate state))
  (cmstate2, ok, mods) <- io (cmLoadModule cmstate1 path)

  def_mod <- io (readIORef defaultCurrentModule)

  let new_state = state{
  			cmstate = cmstate2,
			modules = mods,
			current_module = case mods of 
					   [] -> def_mod
					   xs -> head xs,
			target = Just path
		   }
  setGHCiState new_state

  let mod_commas 
	| null mods = text "none."
	| otherwise = hsep (
	    punctuate comma (map (text.moduleUserString) mods)) <> text "."
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
   Just path
      -> do (new_cmstate, ok, mods) <- io (cmLoadModule (cmstate state) path)
	    def_mod <- io (readIORef defaultCurrentModule)
            setGHCiState 
               state{cmstate=new_cmstate,
                     modules = mods,
                     current_module = case mods of 
                                         [] -> def_mod
                                         xs -> head xs
                    }


reloadModule _ = noArgs ":reload"

typeOfExpr :: String -> GHCi ()
typeOfExpr str 
  = do st <- getGHCiState
       dflags <- io (getDynFlags)
       (new_cmstate, maybe_ty) <- io (cmGetExpr (cmstate st) dflags 
				(current_module st) str False)
       setGHCiState st{cmstate = new_cmstate}
       case maybe_ty of
	 Nothing -> return ()
	 Just (_, unqual, ty) -> io (printForUser stdout unqual (ppr ty)) 

quit :: String -> GHCi Bool
quit _ = return True

shellEscape :: String -> GHCi Bool
shellEscape str = io (system str >> return False)

----------------------------------------------------------------------------
-- Code for `:set'

-- set options in the interpreter.  Syntax is exactly the same as the
-- ghc command line, except that certain options aren't available (-C,
-- -E etc.)
--
-- This is pretty fragile: most options won't work as expected.  ToDo:
-- figure out which ones & disallow them.

setOptions :: String -> GHCi ()
setOptions ""
  = do st <- getGHCiState
       let opts = options st
       io $ putStrLn (showSDoc (
   	      text "options currently set: " <> 
   	      if null opts
   		   then text "none."
   		   else hsep (map (\o -> char '+' <> text (optToStr o)) opts)
   	   ))
setOptions str
  = do -- first, deal with the GHCi opts (+s, +t, etc.)
       let opts = words str
	   (minus_opts, rest1) = partition isMinus opts
	   (plus_opts, rest2)  = partition isPlus rest1

       if (not (null rest2)) 
	  then io (putStrLn ("unknown option: `" ++ head rest2 ++ "'"))
	  else do

       mapM setOpt plus_opts

       -- now, the GHC flags
       io (do leftovers <- processArgs static_flags minus_opts []
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
	  then throwDyn (OtherError "can't unset GHC command-line flags")
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
strToGHCiOpt _   = Nothing

optToStr :: GHCiOption -> String
optToStr ShowTiming = "s"
optToStr ShowType   = "t"


-----------------------------------------------------------------------------
-- Code to do last-expression-entered stuff.  (a.k.a the $$ facility)

-- Take a string and replace $$s in it with the last expr, if any.
expandExpr :: String -> GHCi String
expandExpr str
   = do mle <- getLastExpr
        return (outside mle str)
     where
        outside mle ('$':'$':cs)
           = case mle of
                Just le -> " (" ++ le ++ ") " ++ outside mle cs
                Nothing -> outside mle cs

        outside mle []           = []
        outside mle ('"':str)    = '"' : inside2 mle str   -- "
        outside mle ('\'':str)   = '\'' : inside1 mle str   -- '
        outside mle (c:cs)       = c : outside mle cs

        inside2 mle ('"':cs)  = '"' : outside mle cs   -- "
        inside2 mle (c:cs)    = c : inside2 mle cs
        inside2 mle []        = []

        inside1 mle ('\'':cs) = '\'': outside mle cs
        inside1 mle (c:cs)    = c : inside1 mle cs
        inside1 mle []        = []


rememberExpr :: String -> GHCi ()
rememberExpr str
   = do let cleaned = (clean . reverse . clean . reverse) str
        let forget_me_not | null cleaned = Nothing
                          | otherwise    = Just cleaned
        setLastExpr forget_me_not
     where
        clean = dropWhile isSpace


-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	modules	       :: [Module],
	current_module :: Module,
	target         :: Maybe FilePath,
	cmstate        :: CmState,
	options        :: [GHCiOption],
        last_expr      :: Maybe String
     }

data GHCiOption = ShowTiming | ShowType deriving Eq

defaultCurrentModuleName = mkModuleName "Prelude"
GLOBAL_VAR(defaultCurrentModule, error "no defaultCurrentModule", Module)

newtype GHCi a = GHCi { unGHCi :: GHCiState -> IO (GHCiState, a) }

instance Monad GHCi where
  (GHCi m) >>= k  =  GHCi $ \s -> m s >>= \(s,a) -> unGHCi (k a) s
  return a  = GHCi $ \s -> return (s,a)

getGHCiState   = GHCi $ \s -> return (s,s)
setGHCiState s = GHCi $ \_ -> return (s,())

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

getLastExpr :: GHCi (Maybe String)
getLastExpr
 = do st <- getGHCiState ; return (last_expr st)

setLastExpr :: Maybe String -> GHCi ()
setLastExpr last_expr
 = do st <- getGHCiState ; setGHCiState (st{last_expr = last_expr})

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
