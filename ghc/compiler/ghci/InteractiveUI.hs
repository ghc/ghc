-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.45 2001/02/13 13:09:36 sewardj Exp $
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2000
--
-----------------------------------------------------------------------------

{-# OPTIONS -#include "Linker.h" #-}
module InteractiveUI (interactiveUI) where

#include "HsVersions.h"

import CompManager
import CmStaticInfo
import ByteCodeLink
import DriverFlags
import DriverState
import DriverUtil
import Type
import Linker
import Finder
import Module
import Outputable
import Util
import PprType		{- instance Outputable Type; do not delete -}
import Panic		( GhcException(..) )
import Config

import Exception
import Dynamic
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
import Monad 		( when )

import PrelGHC 		( unsafeCoerce# )
import PrelPack 	( packString )
import PrelByteArr
import Foreign		( Ptr, nullPtr )

-----------------------------------------------------------------------------

ghciWelcomeMsg = "\ 
\   ___         ___ _\n\ 
\  / _ \\ /\\  /\\/ __(_)\n\ 
\ / /_\\// /_/ / /  | |      GHC Interactive, version " ++ cProjectVersion ++ ", For Haskell 98.\n\ 
\/ /_\\\\/ __  / /___| |      http://www.haskell.org/ghc/\n\ 
\\\____/\\/ /_/\\____/|_|      Type :? for help.\n"

GLOBAL_VAR(commands, builtin_commands, [(String, String -> GHCi Bool)])

builtin_commands :: [(String, String -> GHCi Bool)]
builtin_commands = [
  ("add",	keepGoing addModule),
  ("cd",    	keepGoing changeDirectory),
  ("def",	keepGoing defineMacro),
  ("help",	keepGoing help),
  ("?",		keepGoing help),
  ("load",	keepGoing loadModule),
  ("module",	keepGoing setContext),
  ("reload",	keepGoing reloadModule),
  ("set",	keepGoing setOptions),
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
\    +r			revert top-level expressions after each evaluation\n\ 
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
   initLinker
   linkPackages (reverse pkgs)

   (cmstate, ok, mods) <-
   	case mod of
	     Nothing  -> return (cmstate, True, [])
	     Just m -> cmLoadModule cmstate m

#ifndef NO_READLINE
   Readline.initialize
#endif

   prel <- moduleNameToModule defaultCurrentModuleName
   writeIORef defaultCurrentModule prel

   dflags <- getDynFlags

   (cmstate, maybe_stuff) <- cmGetExpr cmstate dflags False prel 
		  		"PrelHandle.hFlush PrelHandle.stdout"
   case maybe_stuff of
	Nothing -> return ()
	Just (hv,_,_) -> writeIORef flush_stdout hv
   
   (cmstate, maybe_stuff) <- cmGetExpr cmstate dflags False prel 
		  		"PrelHandle.hFlush PrelHandle.stdout"
   case maybe_stuff of
	Nothing -> return ()
	Just (hv,_,_) -> writeIORef flush_stderr hv
   
   let this_mod = case mods of 
	  	      []   -> prel
  	  	      m:ms -> m

   (unGHCi runGHCi) GHCiState{ modules = mods,
			      current_module = this_mod,
	  	              target = mod,
			      cmstate = cmstate,
			      options = [ShowTiming],
                              last_expr = Nothing}
   return ()


runGHCi :: GHCi ()
runGHCi = do
  -- read in ./.ghci
  dot_ghci <- io (IO.try (openFile "./.ghci" ReadMode))
  case dot_ghci of
	Left e -> return ()
	Right hdl -> fileLoop hdl False
  
  -- read in ~/.ghci
  home <- io (IO.try (getEnv "HOME"))
  case home of
   Left e  -> return ()
   Right dir -> do
  	dot_ghci <- io (IO.try (openFile (dir ++ "/.ghci") ReadMode))
	case dot_ghci of
	   Left e -> return ()
	   Right hdl -> fileLoop hdl False

  -- read commands from stdin
#ifndef NO_READLINE
  readlineLoop
#else
  fileLoop stdin True
#endif

  -- and finally, exit
  io $ do putStrLn "Leaving GHCi." 


fileLoop :: Handle -> Bool -> GHCi ()
fileLoop hdl prompt = do
   st <- getGHCiState
   when prompt (io (hPutStr hdl (moduleUserString (current_module st) ++ "> ")))
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
   st <- getGHCiState
   case remove_spaces s of
	"" -> stringLoop ss
	l  -> do quit <- runCommand l
                 if quit then return () else stringLoop ss

#ifndef NO_READLINE
readlineLoop :: GHCi ()
readlineLoop = do
   st <- getGHCiState
   l <- io (readline (moduleUserString (current_module st) ++ "> "))
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
  ghciHandle ( \exception -> 
	(case exception of
	   DynException dyn -> 
	      case fromDynamic dyn of
		Nothing -> io (putStrLn ("*** Exception: (unknown)"))
		Just ghc_ex -> 
		  case ghc_ex of
		    PhaseFailed phase code ->
			io ( putStrLn ("Phase " ++ phase ++ " failed (code "
				        ++ show code ++ ")"))
		    Interrupted -> io (putStrLn "Interrupted.")
		    other -> io (putStrLn (show (ghc_ex :: GhcException)))

	   other -> io (putStrLn ("*** Exception: " ++ show exception))

	) >> return False
     ) $

   doCommand c

doCommand (':' : command) = specialCommand command
doCommand ('-':'-':_) = return False	-- comments, useful in scripts
doCommand expr
   = do expr_expanded <- expandExpr expr
        -- io (putStrLn ( "Before: " ++ expr ++ "\nAfter:  " ++ expr_expanded))
        expr_ok <- timeIt (do stuff <- evalExpr expr_expanded
			      finishEvalExpr stuff)
        when expr_ok (rememberExpr expr_expanded)
        return False

-- possibly print the type and revert CAFs after evaluating an expression
finishEvalExpr Nothing = return False
finishEvalExpr (Just (unqual,ty))
 = do b <- isOptionSet ShowType
      io (when b (printForUser stdout unqual (text "::" <+> ppr ty)))
      b <- isOptionSet RevertCAFs
      io (when b revertCAFs)
      return True

-- Returned Maybe indicates whether or not the expr was successfully
-- parsed, renamed and typechecked.
evalExpr :: String -> GHCi (Maybe (PrintUnqualified,Type))
evalExpr expr
 | null (filter (not.isSpace) expr)
 = return Nothing
 | otherwise
 = do st <- getGHCiState
      dflags <- io (getDynFlags)
      (new_cmstate, maybe_stuff) <- 
      	 io (cmGetExpr (cmstate st) dflags True (current_module st) expr)
      setGHCiState st{cmstate = new_cmstate}
      case maybe_stuff of
      	 Nothing -> return Nothing
      	 Just (hv, unqual, ty) -> do io (cmRunExpr hv)
		 		     flushEverything
		 		     return (Just (unqual,ty))

flushEverything :: GHCi ()
flushEverything
   = io $ do flush_so <- readIORef flush_stdout
     	     cmRunExpr flush_so
     	     flush_se <- readIORef flush_stdout
     	     cmRunExpr flush_se

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
       if (isHomeModule m && m `notElem` modules st)
	  then throwDyn (OtherError (showSDoc (quotes (ppr (moduleName m))
				<+> text "is not currently loaded, use :load")))
	  else setGHCiState st{current_module = m}

moduleNameToModule :: ModuleName -> IO Module
moduleNameToModule mn
 = do maybe_stuff <- findModule mn
      case maybe_stuff of
	 Nothing -> throwDyn (OtherError ("can't find module `"
					    ++ moduleNameUserString mn ++ "'"))
   	 Just (m,_) -> return m

changeDirectory :: String -> GHCi ()
changeDirectory d = io (setCurrentDirectory d)

defineMacro :: String -> GHCi ()
defineMacro s = do
  let (macro_name, definition) = break isSpace s
  cmds <- io (readIORef commands)
  if (null macro_name) 
	then throwDyn (OtherError "invalid macro name") 
	else do
  if (macro_name `elem` map fst cmds) 
	then throwDyn (OtherError 
		("command `" ++ macro_name ++ "' already defined"))
	else do

  -- give the expression a type signature, so we can be sure we're getting
  -- something of the right type.
  let new_expr = '(' : definition ++ ") :: String -> IO String"

  -- compile the expression
  st <- getGHCiState
  dflags <- io (getDynFlags)
  (new_cmstate, maybe_stuff) <- 
      	 io (cmGetExpr (cmstate st) dflags False (current_module st) new_expr)
  setGHCiState st{cmstate = new_cmstate}
  case maybe_stuff of
     Nothing -> return ()
     Just (hv, unqual, ty) 
	-> io (writeIORef commands 
		 ((macro_name, keepGoing (runMacro hv)) : cmds))

runMacro :: HValue{-String -> IO String-} -> String -> GHCi ()
runMacro fun s = do
  str <- io ((unsafeCoerce# fun :: String -> IO String) s)
  stringLoop (lines str)

undefineMacro :: String -> GHCi ()
undefineMacro macro_name = do
  cmds <- io (readIORef commands)
  if (macro_name `elem` map fst builtin_commands) 
	then throwDyn (OtherError
		("command `" ++ macro_name ++ "' cannot be undefined"))
	else do
  if (macro_name `notElem` map fst cmds) 
	then throwDyn (OtherError 
		("command `" ++ macro_name ++ "' not defined"))
	else do
  io (writeIORef commands (filter ((/= macro_name) . fst) cmds))

loadModule :: String -> GHCi ()
loadModule path = timeIt (loadModule' path)

loadModule' path = do
  state <- getGHCiState
  cmstate1 <- io (cmUnload (cmstate state))
  io (revertCAFs)			-- always revert CAFs on load.
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
      -> do io (revertCAFs)		-- always revert CAFs on reload.
	    (new_cmstate, ok, mods) <- io (cmLoadModule (cmstate state) path)
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
       (new_cmstate, maybe_ty) <- io (cmGetExpr (cmstate st) dflags False
					 (current_module st) str)
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
       io (do -- first, static flags
	      leftovers <- processArgs static_flags minus_opts []

	      -- then, dynamic flags
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
strToGHCiOpt "r" = Just RevertCAFs
strToGHCiOpt _   = Nothing

optToStr :: GHCiOption -> String
optToStr ShowTiming = "s"
optToStr ShowType   = "t"
optToStr RevertCAFs = "r"

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

data GHCiOption 
	= ShowTiming		-- show time/allocs after evaluation
	| ShowType		-- show the type of expressions
	| RevertCAFs		-- revert CAFs after every evaluation
	deriving Eq

defaultCurrentModuleName = mkModuleName "Prelude"
GLOBAL_VAR(defaultCurrentModule, error "no defaultCurrentModule", Module)

GLOBAL_VAR(flush_stdout, error "no flush_stdout", HValue)
GLOBAL_VAR(flush_stderr, error "no flush_stdout", HValue)

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

linkPackages :: [Package] -> IO ()
linkPackages pkgs = mapM_ linkPackage pkgs

linkPackage :: Package -> IO ()
-- ignore rts and gmp for now (ToDo; better?)
linkPackage pkg 
   | name pkg `elem` ["rts", "gmp"] 
   = return ()
   | otherwise
   = do putStr ("Loading package " ++ name pkg ++ " ... ")
        -- For each obj, try obj.o and if that fails, obj.so.
        -- Complication: all the .so's must be loaded before any of the .o's.  
        let dirs      =  library_dirs pkg
        let objs      =  hs_libraries pkg ++ extra_libraries pkg
        classifieds   <- mapM (locateOneObj dirs) objs
        let sos_first = filter isRight classifieds 
                        ++ filter (not.isRight) classifieds
        mapM loadClassified sos_first
        putStr "linking ... "
        resolveObjs
        putStrLn "done."
     where
        isRight (Right _) = True
        isRight (Left _)  = False


loadClassified :: Either FilePath String -> IO ()
loadClassified (Left obj_absolute_filename)
   = do --putStr ("Left  " ++ obj_absolute_filename ++ "\n")
        loadObj obj_absolute_filename
loadClassified (Right dll_unadorned)
   = do --putStr ("Right " ++ dll_unadorned ++ "\n")
        dll_ok <- ocAddDLL (packString dll_unadorned)
        if    dll_ok /= 1
         then throwDyn (OtherError ("can't find .o or .so/.DLL for: " 
                                    ++ dll_unadorned))
         else return ()

locateOneObj :: [FilePath] -> String -> IO (Either FilePath String)
locateOneObj []     obj 
   = return (Right obj) -- we assume
locateOneObj (d:ds) obj 
   = do let path = d ++ '/':obj ++ ".o"
        b <- doesFileExist path
        if b then return (Left path) else locateOneObj ds obj


type PackedString = ByteArray Int
foreign import "ocAddDLL" unsafe ocAddDLL :: PackedString -> IO Int

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
-- reverting CAFs
	
foreign import revertCAFs :: IO ()	-- make it "safe", just in case
