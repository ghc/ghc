-----------------------------------------------------------------------------
-- $Id: InteractiveUI.hs,v 1.63 2001/04/26 11:08:32 sewardj Exp $
--
-- GHC Interactive User Interface
--
-- (c) The GHC Team 2000
--
-----------------------------------------------------------------------------

{-# OPTIONS -#include "Linker.h" #-}
module InteractiveUI ( interactiveUI, ghciWelcomeMsg ) where

#include "../includes/config.h"
#include "HsVersions.h"

import CompManager
import CmStaticInfo
import ByteCodeLink
import DriverFlags
import DriverState
import DriverUtil
import Linker
import Util
import Name		( Name )
import Outputable
import CmdLineOpts	( DynFlag(..), dopt_unset )
import Panic		( GhcException(..) )
import Config

import Exception
import Dynamic
#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
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
import Foreign		( nullPtr )
import CString		( peekCString )

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
\   <stmt>		evaluate/run <stmt>\n\ 
\   :cd <dir>		change directory to <dir>\n\ 
\   :def <cmd> <expr>   define a command :<cmd>\n\ 
\   :help, :?		display this list of commands\n\ 
\   :load <filename>    load a module (and it dependents)\n\ 
\   :module <mod>	set the context for expression evaluation to <mod>\n\ 
\   :reload		reload the current module set\n\ 
\   :set <option> ...	set options\n\ 
\   :undef <name> 	undefine user-defined command :<name>\n\ 
\   :type <expr>	show the type of <expr>\n\ 
\   :unset <option> ...	unset options\n\ 
\   :quit		exit GHCi\n\ 
\   :!<command>		run the shell command <command>\n\ 
\\ 
\ Options for `:set' and `:unset':\n\ 
\\ 
\    +r			revert top-level expressions after each evaluation\n\ 
\    +s                 print timing/memory stats after each evaluation\n\ 
\    +t			print type after evaluation\n\ 
\    -<flags>		most GHC command line flags can also be set here\n\ 
\                         (eg. -v2, -fglasgow-exts, etc.)\n\ 
\"
 --ToDo   :add <filename>     add a module to the current set\n\ 

interactiveUI :: CmState -> Maybe FilePath -> [LibrarySpec] -> IO ()
interactiveUI cmstate mod cmdline_libs = do
   hFlush stdout
   hSetBuffering stdout NoBuffering

   -- link in the available packages
   pkgs <- getPackageInfo
   initLinker
   linkPackages cmdline_libs (reverse pkgs)

   (cmstate, ok, mods) <-
   	case mod of
	     Nothing  -> return (cmstate, True, [])
	     Just m -> cmLoadModule cmstate m

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
   Readline.initialize
#endif

   dflags <- getDynFlags

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

   (unGHCi runGHCi) GHCiState{ target = mod,
			       cmstate = cmstate,
			       options = [] }
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
	cwd <- io (getCurrentDirectory)
	when (dir /= cwd) $ do
  	  dot_ghci <- io (IO.try (openFile (dir ++ "/.ghci") ReadMode))
  	  case dot_ghci of
  	     Left e -> return ()
  	     Right hdl -> fileLoop hdl False

  -- read commands from stdin
#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
  readlineLoop
#else
  fileLoop stdin True
#endif

  -- and finally, exit
  io $ do putStrLn "Leaving GHCi." 


fileLoop :: Handle -> Bool -> GHCi ()
fileLoop hdl prompt = do
   st <- getGHCiState
   mod <- io (cmGetContext (cmstate st))
   when prompt (io (hPutStr hdl (mod ++ "> ")))
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

#if HAVE_READLINE_HEADERS && HAVE_READLINE_LIBS
readlineLoop :: GHCi ()
readlineLoop = do
   st <- getGHCiState
   mod <- io (cmGetContext (cmstate st))
   l <- io (readline (mod ++ "> "))
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
			-- omit the location for CmdLineError
		    CmdLineError s -> io (putStrLn s)
		    other -> io (putStrLn (show (ghc_ex :: GhcException)))

	   other -> io (putStrLn ("*** Exception: " ++ show exception))

	) >> return False
     ) $

   doCommand c

doCommand (':' : command) = specialCommand command
doCommand stmt
   = do timeIt (do stuff <- runStmt stmt; finishEvalExpr stuff)
        return False

-- Returns True if the expr was successfully parsed, renamed and
-- typechecked.
runStmt :: String -> GHCi (Maybe [Name])
runStmt stmt
 | null (filter (not.isSpace) stmt)
 = return Nothing
 | otherwise
 = do st <- getGHCiState
      dflags <- io (getDynFlags)
      let dflags' = dopt_unset dflags Opt_WarnUnusedBinds
      (new_cmstate, names) <- io (cmRunStmt (cmstate st) dflags' stmt)
      setGHCiState st{cmstate = new_cmstate}
      return (Just names)

-- possibly print the type and revert CAFs after evaluating an expression
finishEvalExpr Nothing = return False
finishEvalExpr (Just names)
 = do b <- isOptionSet ShowType
      st <- getGHCiState
      when b (mapM_ (showTypeOfName (cmstate st)) names)

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
   = io $ do flush_so <- readIORef flush_stdout
     	     flush_so
     	     flush_se <- readIORef flush_stdout
     	     flush_se
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

addModule :: String -> GHCi ()
addModule _ = throwDyn (InstallationError ":add not implemented")

setContext :: String -> GHCi ()
setContext ""
  = throwDyn (CmdLineError "syntax: `:m <module>'")
setContext m | not (isUpper (head m)) || not (all isAlphaNum (tail m))
  = throwDyn (CmdLineError ("strange looking module name: `" ++ m ++ "'"))
setContext str
  = do st <- getGHCiState
       new_cmstate <- io (cmSetContext (cmstate st) str)
       setGHCiState st{cmstate=new_cmstate}

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
  st <- getGHCiState
  dflags <- io (getDynFlags)
  (new_cmstate, maybe_hv) <- io (cmCompileExpr (cmstate st) dflags new_expr)
  setGHCiState st{cmstate = new_cmstate}
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
loadModule path = timeIt (loadModule' path)

loadModule' path = do
  state <- getGHCiState
  cmstate1 <- io (cmUnload (cmstate state))
  io (revertCAFs)			-- always revert CAFs on load.
  (cmstate2, ok, mods) <- io (cmLoadModule cmstate1 path)
  let new_state = state{ cmstate = cmstate2,
			 target = Just path
		       }
  setGHCiState new_state
  modulesLoadedMsg ok mods

reloadModule :: String -> GHCi ()
reloadModule "" = do
  state <- getGHCiState
  case target state of
   Nothing -> io (putStr "no current target\n")
   Just path
      -> do io (revertCAFs)		-- always revert CAFs on reload.
	    (new_cmstate, ok, mods) <- io (cmLoadModule (cmstate state) path)
            setGHCiState state{ cmstate=new_cmstate }
	    modulesLoadedMsg ok mods

reloadModule _ = noArgs ":reload"


modulesLoadedMsg ok mods = do
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
  = do st <- getGHCiState
       dflags <- io (getDynFlags)
       (new_cmstate, maybe_tystr) <- io (cmTypeOfExpr (cmstate st) dflags str)
       setGHCiState st{cmstate = new_cmstate}
       case maybe_tystr of
	  Nothing    -> return ()
	  Just tystr -> io (putStrLn tystr)

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
		 then throwDyn (CmdLineError ("unrecognised flags: " ++ 
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

-----------------------------------------------------------------------------
-- GHCi monad

data GHCiState = GHCiState
     { 
	target         :: Maybe FilePath,
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

-- Left: full path name of a .o file, including trailing .o
-- Right: "unadorned" name of a .DLL/.so
--        e.g.    On unix     "qt"  denotes "libqt.so"
--                On WinDoze  "burble"  denotes "burble.DLL"
--        addDLL is platform-specific and adds the lib/.so/.DLL
--        prefixes plaform-dependently; we don't do that here.
type LibrarySpec
   = Either FilePath String

showLS (Left nm)  = "(static) " ++ nm
showLS (Right nm) = "(dynamic) " ++ nm

linkPackages :: [LibrarySpec] -> [PackageConfig] -> IO ()
linkPackages cmdline_lib_specs pkgs
   = do mapM_ linkPackage pkgs
        mapM_ preloadLib cmdline_lib_specs
     where
        preloadLib lib_spec
           = do putStr ("Loading object " ++ showLS lib_spec ++ " ... ")
                case lib_spec of
                   Left static_ish
                      -> do b <- doesFileExist static_ish
                            if    not b
                             then do putStr "not found.\n"
                                     croak
                             else do loadObj static_ish
                                     putStr "done.\n"
                   Right dll_unadorned
                      -> do maybe_errmsg <- addDLL dll_unadorned
                            if    maybe_errmsg == nullPtr
                             then putStr "done.\n"
                             else do str <- peekCString maybe_errmsg
                                     putStr ("failed (" ++ str ++ ")\n")
                                     croak

        croak = throwDyn (CmdLineError "user specified .o/.so/.DLL could not be loaded.")


linkPackage :: PackageConfig -> IO ()
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

loadClassified :: LibrarySpec -> IO ()
loadClassified (Left obj_absolute_filename)
   = do loadObj obj_absolute_filename
loadClassified (Right dll_unadorned)
   = do maybe_errmsg <- addDLL dll_unadorned
        if    maybe_errmsg == nullPtr
         then return ()
         else do str <- peekCString maybe_errmsg
                 throwDyn (CmdLineError ("can't find .o or .so/.DLL for: " 
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
-- reverting CAFs
	
foreign import revertCAFs :: IO ()	-- make it "safe", just in case
