-----------------------------------------------------------------------------
-- Access to system tools: gcc, cp, rm etc
--
-- (c) The University of Glasgow 2000
--
-----------------------------------------------------------------------------

\begin{code}
module SysTools (
	-- Initialisation
	initSysTools,
	setPgm,			-- String -> IO ()
				-- Command-line override
	setDryRun,

	packageConfigPath,	-- IO String	
				-- Where package.conf is

	-- Interface to system tools
	runUnlit, runCpp, runCc, -- [String] -> IO ()
	runMangle, runSplit,	 -- [String] -> IO ()
	runAs, runLink,		 -- [String] -> IO ()
	runMkDLL,

	touch,			-- String -> String -> IO ()
	copy,			-- String -> String -> String -> IO ()
	
	-- Temporary-file management
	setTmpDir,
	newTempName,
	cleanTempFiles, cleanTempFilesExcept, removeTmpFiles,
	addFilesToClean,

	-- System interface
	getProcessID,		-- IO Int
	System.system, 		-- String -> IO Int	-- System.system

	-- Misc
	showGhcUsage,		-- IO ()	Shows usage message and exits
	getSysMan,		-- IO String	Parallel system only
	dosifyPath,		-- String -> String

	runSomething	-- ToDo: make private
 ) where

import DriverUtil
import Config
import Outputable
import Panic		( progName, GhcException(..) )
import Util		( global )
import CmdLineOpts	( dynFlag, verbosity )

import List		( intersperse )
import Exception	( throwDyn, catchAllIO )
import IO		( hPutStr, hPutChar, hPutStrLn, hFlush, stderr )
import Directory	( doesFileExist, removeFile )
import IOExts		( IORef, readIORef, writeIORef )
import Monad		( when, unless )
import qualified System
import System		( ExitCode(..) )

#include "../includes/config.h"

#if !defined(mingw32_TARGET_OS)
import qualified Posix
#else
import Addr              ( nullAddr )
#endif

#include "HsVersions.h"

{-# DEPRECATED runSomething "runSomething should be private to SysTools" #-}

\end{code}


		The configuration story
		~~~~~~~~~~~~~~~~~~~~~~~

GHC needs various support files (library packages, RTS etc), plus
various auxiliary programs (cp, gcc, etc).  It finds these in one
of two places:

* When running as an *installed program*, GHC finds most of this support
  stuff in the installed library tree.  The path to this tree is passed
  to GHC via the -B flag, and given to initSysTools .

* When running *in-place* in a build tree, GHC finds most of this support
  stuff in the build tree.  The path to the build tree is, again passed
  to GHC via -B. 

GHC tells which of the two is the case by seeing whether package.conf
is in TopDir [installed] or in TopDir/ghc/driver [inplace] (what a hack).


SysTools.initSysProgs figures out exactly where all the auxiliary programs
are, and initialises mutable variables to make it easy to call them.
To to this, it makes use of definitions in Config.hs, which is a Haskell
file containing variables whose value is figured out by the build system.

Config.hs contains two sorts of things

  cGCC, 	The *names* of the programs
  cCPP		  e.g.  cGCC = gcc
  cUNLIT	        cCPP = gcc -E
  etc		They do *not* include paths
				

  cUNLIT_DIR	The *path* to the directory containing unlit, split etc
  cSPLIT_DIR	*relative* to the root of the build tree,
		for use when running *in-place* in a build tree (only)
		


%************************************************************************
%*									*
\subsection{Global variables to contain system programs}
%*									*
%************************************************************************

\begin{code}
GLOBAL_VAR(v_Pgm_L,   	error "pgm_L",   String)	-- unlit
GLOBAL_VAR(v_Pgm_P,   	error "pgm_P",   String)	-- cpp
GLOBAL_VAR(v_Pgm_c,   	error "pgm_c",   String)	-- gcc
GLOBAL_VAR(v_Pgm_m,   	error "pgm_m",   String)	-- asm code mangler
GLOBAL_VAR(v_Pgm_s,   	error "pgm_s",   String)	-- asm code splitter
GLOBAL_VAR(v_Pgm_a,   	error "pgm_a",   String)	-- as
GLOBAL_VAR(v_Pgm_l,   	error "pgm_l",   String)	-- ld
GLOBAL_VAR(v_Pgm_MkDLL, error "pgm_dll", String)	-- mkdll

GLOBAL_VAR(v_Pgm_T,    error "pgm_T",    String)	-- touch
GLOBAL_VAR(v_Pgm_CP,   error "pgm_CP", 	 String)	-- cp

GLOBAL_VAR(v_Path_package_config, error "path_package_config", String)
GLOBAL_VAR(v_Path_usage,  	  error "ghc_usage.txt",       String)

-- Parallel system only
GLOBAL_VAR(v_Pgm_sysman, error "pgm_sysman", String)	-- system manager
\end{code}


%************************************************************************
%*									*
\subsection{Initialisation}
%*									*
%************************************************************************

\begin{code}
initSysTools :: [String]	-- Command-line arguments starting "-B"

	     -> IO String	-- Set all the mutable variables above, holding 
				--	(a) the system programs
				--	(b) the package-config file
				--	(c) the GHC usage message
				-- Return TopDir


initSysTools minusB_args
  = do  { (am_installed, top_dir) <- getTopDir minusB_args
		-- top_dir
		-- 	for "installed" this is the root of GHC's support files
		--	for "in-place" it is the root of the build tree

	; let installed_bin pgm   =  top_dir `slash` "bin" `slash` pgm
	      installed     file  =  top_dir `slash` file
	      inplace dir   pgm   =  top_dir `slash` dir `slash` pgm

	; let pkgconfig_path
		| am_installed = installed "package.conf"
		| otherwise    = inplace cGHC_DRIVER_DIR "package.conf.inplace"

	      ghc_usage_msg_path
		| am_installed = installed "ghc-usage.txt"
		| otherwise    = inplace cGHC_DRIVER_DIR "ghc-usage.txt"

		-- For all systems, unlit, split, mangle are GHC utilities
		-- architecture-specific stuff is done when building Config.hs
	      unlit_path
		| am_installed = installed_bin cGHC_UNLIT
		| otherwise    = inplace cGHC_UNLIT_DIR cGHC_UNLIT

		-- split and mangle are Perl scripts
	      split_script
		| am_installed = installed_bin cGHC_SPLIT
		| otherwise    = inplace cGHC_SPLIT_DIR cGHC_SPLIT

	      mangle_script
		| am_installed = installed_bin cGHC_MANGLER
		| otherwise    = inplace cGHC_MANGLER_DIR cGHC_MANGLER

	-- Check that the package config exists
	; config_exists <- doesFileExist pkgconfig_path
	; when (not config_exists) $
	     throwDyn (InstallationError 
		         ("Can't find package.conf in " ++ pkgconfig_path))

#if defined(mingw32_TARGET_OS)
	--		WINDOWS-SPECIFIC STUFF
	-- On Windows, gcc and friends are distributed with GHC,
	-- 	so when "installed" we look in TopDir/bin
	-- When "in-place" we look wherever the build-time configure 
	--	script found them
	; let cpp_path 	| am_installed = installed cRAWCPP
		       	| otherwise    = cRAWCPP
	      gcc_path 	| am_installed = installed cGCC
		       	| otherwise    = cGCC
	      perl_path | am_installed = installed cGHC_PERL
		        | otherwise    = cGHC_PERL

	-- 'touch' is a GHC util for Windows, and similarly unlit, mangle
	; let touch_path  | am_installed = installed cGHC_TOUCHY
		       	  | otherwise    = inplace cGHC_TOUCHY_DIR cGHC_TOUCHY

	-- On Win32 we don't want to rely on #!/bin/perl, so we prepend 
	-- a call to Perl to get the invocation of split and mangle
	; let split_path  = perl_path ++ " " ++ split_script
	      mangle_path = perl_path ++ " " ++ mangle_script

	; let mkdll_path = cMKDLL
#else
	--		UNIX-SPECIFIC STUFF
	-- On Unix, the "standard" tools are assumed to be
	-- in the same place whether we are running "in-place" or "installed"
	-- That place is wherever the build-time configure script found them.
	; let	cpp_path   = cRAWCPP
		gcc_path   = cGCC
		touch_path = cGHC_TOUCHY
		mkdll_path = panic "Cant build DLLs on a non-Win32 system"

	-- On Unix, scripts are invoked using the '#!' method.  Binary
	-- installations of GHC on Unix place the correct line on the front
	-- of the script at installation time, so we don't want to wire-in
	-- our knowledge of $(PERL) on the host system here.
	; let split_path  = split_script
	      mangle_path = mangle_script

#endif

	-- For all systems, copy and remove are provided by the host 
	-- system; architecture-specific stuff is done when building Config.hs
	; let	cp_path = cGHC_CP
	
	-- Other things being equal, as and ld are simply gcc
	; let	as_path  = gcc_path
		ld_path  = gcc_path

				       
	-- Initialise the global vars
	; writeIORef v_Path_package_config pkgconfig_path
	; writeIORef v_Path_usage 	   ghc_usage_msg_path

	; writeIORef v_Pgm_sysman	   (top_dir ++ "/ghc/rts/parallel/SysMan")
		-- Hans: this isn't right in general, but you can 
		-- elaborate it in the same way as the others

	; writeIORef v_Pgm_L   	 	   unlit_path
	; writeIORef v_Pgm_P   	 	   cpp_path
	; writeIORef v_Pgm_c   	 	   gcc_path
	; writeIORef v_Pgm_m   	 	   mangle_path
	; writeIORef v_Pgm_s   	 	   split_path
	; writeIORef v_Pgm_a   	 	   as_path
	; writeIORef v_Pgm_l   	 	   ld_path
	; writeIORef v_Pgm_MkDLL 	   mkdll_path
	; writeIORef v_Pgm_T   	 	   touch_path
	; writeIORef v_Pgm_CP  	 	   cp_path

	; return top_dir
	}
\end{code}

setPgm is called when a command-line option like
	-pgmLld
is used to override a particular program with a new onw

\begin{code}
setPgm :: String -> IO ()
-- The string is the flag, minus the '-pgm' prefix
-- So the first character says which program to override

setPgm ('P' : pgm) = writeIORef v_Pgm_P pgm
setPgm ('c' : pgm) = writeIORef v_Pgm_c pgm
setPgm ('m' : pgm) = writeIORef v_Pgm_m pgm
setPgm ('s' : pgm) = writeIORef v_Pgm_s pgm
setPgm ('a' : pgm) = writeIORef v_Pgm_a pgm
setPgm ('l' : pgm) = writeIORef v_Pgm_l pgm
setPgm pgm	   = unknownFlagErr ("-pgm" ++ pgm)
\end{code}


\begin{code}
-- Find TopDir
-- 	for "installed" this is the root of GHC's support files
--	for "in-place" it is the root of the build tree
--
-- Plan of action:
-- 1. Set proto_top_dir
-- 	a) look for (the last) -B flag, and use it
--	b) if there are no -B flags, get the directory 
--	   where GHC is running
--
-- 2. If package.conf exists in proto_top_dir, we are running
--	installed; and TopDir = proto_top_dir
--
-- 3. Otherwise we are running in-place, so
--	proto_top_dir will be /...stuff.../ghc/compiler
--	Set TopDir to /...stuff..., which is the root of the build tree
--
-- This is very gruesome indeed

getTopDir :: [String]
	  -> IO (Bool, 		-- True <=> am installed, False <=> in-place
	         String)	-- TopDir

getTopDir minusbs
  = do { proto_top_dir <- get_proto

	-- Discover whether we're running in a build tree or in an installation,
	-- by looking for the package configuration file.
       ; am_installed <- doesFileExist (proto_top_dir `slash` "package.conf")

       ; if am_installed then
	    return (True, proto_top_dir)
	 else
	    return (False, remove_suffix proto_top_dir)
       }
  where
    get_proto | not (null minusbs) 
	      = return (dosifyPath (drop 2 (last minusbs)))
	      | otherwise	   
	      = do { maybe_exec_dir <- getExecDir -- Get directory of executable
		   ; case maybe_exec_dir of	  -- (only works on Windows)
			Nothing  -> throwDyn (InstallationError 
						"missing -B<dir> option")
			Just dir -> return dir
		   }

    remove_suffix dir	-- "/...stuff.../ghc/compiler" --> "/...stuff..."
	= ASSERT2( not (null p1) && 
		   not (null p2) && 
		   dosifyPath dir == dosifyPath (top_dir ++ "/ghc/compiler"),
		   text dir )
	  top_dir
	where
	 p1      = dropWhile (not . isSlash) (reverse dir)
	 p2      = dropWhile (not . isSlash) (tail p1)	-- head is '/'
	 top_dir = reverse (tail p2)			-- head is '/'
\end{code}


%************************************************************************
%*									*
\subsection{Running an external program}
n%*									*
%************************************************************************


\begin{code}
runUnlit :: [String] -> IO ()
runUnlit args = do p <- readIORef v_Pgm_L
		   runSomething "Literate pre-processor" p args

runCpp :: [String] -> IO ()
runCpp args =   do p <- readIORef v_Pgm_P
		   runSomething "C pre-processor" p args

runCc :: [String] -> IO ()
runCc args =   do p <- readIORef v_Pgm_c
	          runSomething "C Compiler" p args

runMangle :: [String] -> IO ()
runMangle args = do p <- readIORef v_Pgm_m
		    runSomething "Mangler" p args

runSplit :: [String] -> IO ()
runSplit args = do p <- readIORef v_Pgm_s
		   runSomething "Splitter" p args

runAs :: [String] -> IO ()
runAs args = do p <- readIORef v_Pgm_a
		runSomething "Assembler" p args

runLink :: [String] -> IO ()
runLink args = do p <- readIORef v_Pgm_l
	          runSomething "Linker" p args

runMkDLL :: [String] -> IO ()
runMkDLL args = do p <- readIORef v_Pgm_MkDLL
	           runSomething "Make DLL" p args

touch :: String -> String -> IO ()
touch purpose arg =  do p <- readIORef v_Pgm_T
			runSomething purpose p [arg]

copy :: String -> String -> String -> IO ()
copy purpose from to = do p <- readIORef v_Pgm_CP
		          runSomething purpose p [from,to]
\end{code}

\begin{code}
getSysMan :: IO String	-- How to invoke the system manager 
			-- (parallel system only)
getSysMan = readIORef v_Pgm_sysman
\end{code}

%************************************************************************
%*									*
\subsection{GHC Usage message}
%*									*
%************************************************************************

Show the usage message and exit

\begin{code}
showGhcUsage = do { usage_path <- readIORef v_Path_usage
		  ; usage      <- readFile usage_path
		  ; dump usage
		  ; System.exitWith System.ExitSuccess }
  where
     dump ""	      = return ()
     dump ('$':'$':s) = hPutStr stderr progName >> dump s
     dump (c:s)	      = hPutChar stderr c >> dump s

packageConfigPath = readIORef v_Path_package_config
\end{code}


%************************************************************************
%*									*
\subsection{Managing temporary files
%*									*
%************************************************************************

One reason this code is here is because SysTools.system needs to make
a temporary file.

\begin{code}
GLOBAL_VAR(v_FilesToClean, [],               [String] )
GLOBAL_VAR(v_TmpDir,       cDEFAULT_TMPDIR,  String   )
	-- v_TmpDir has no closing '/'
\end{code}

\begin{code}
setTmpDir dir = writeIORef v_TmpDir dir

cleanTempFiles :: Int -> IO ()
cleanTempFiles verb = do fs <- readIORef v_FilesToClean
			 removeTmpFiles verb fs

cleanTempFilesExcept :: Int -> [FilePath] -> IO ()
cleanTempFilesExcept verb dont_delete
  = do fs <- readIORef v_FilesToClean
       let leftovers = filter (`notElem` dont_delete) fs
       removeTmpFiles verb leftovers
       writeIORef v_FilesToClean dont_delete


-- find a temporary name that doesn't already exist.
newTempName :: Suffix -> IO FilePath
newTempName extn
  = do x <- getProcessID
       tmp_dir <- readIORef v_TmpDir
       findTempName tmp_dir x
  where 
    findTempName tmp_dir x
      = do let filename = tmp_dir ++ "/ghc" ++ show x ++ '.':extn
  	   b  <- doesFileExist filename
	   if b then findTempName tmp_dir (x+1)
		else do add v_FilesToClean filename -- clean it up later
		        return filename

addFilesToClean :: [FilePath] -> IO ()
-- May include wildcards [used by DriverPipeline.run_phase SplitMangle]
addFilesToClean files = mapM_ (add v_FilesToClean) files

removeTmpFiles :: Int -> [FilePath] -> IO ()
removeTmpFiles verb fs
  = traceCmd "Deleting temp files" 
	     ("Deleting: " ++ concat (intersperse " " fs))
	     (mapM_ rm fs)
  where
    rm f = removeFile f `catchAllIO`
		(\exn -> hPutStrLn stderr ("Warning: deleting non-existent " ++ f) >>
		         return ())

\end{code}


%************************************************************************
%*									*
\subsection{Running a program}
%*									*
%************************************************************************

\begin{code}
GLOBAL_VAR(v_Dry_run, False, Bool)

setDryRun :: IO () 
setDryRun = writeIORef v_Dry_run True

-----------------------------------------------------------------------------
-- Running an external program

runSomething :: String		-- For -v message
	     -> String		-- Command name (possibly a full path)
				-- 	assumed already dos-ified
	     -> [String]	-- Arguments
				--	runSomthing will dos-ify them
	     -> IO ()

runSomething phase_name pgm args
 = traceCmd phase_name cmd_line $
   do   { exit_code <- System.system cmd_line
	; if exit_code /= ExitSuccess
	  then throwDyn (PhaseFailed phase_name exit_code)
  	  else return ()
	}
  where
    cmd_line = unwords (pgm : dosifyPaths args)

traceCmd :: String -> String -> IO () -> IO ()
-- a) trace the command (at two levels of verbosity)
-- b) don't do it at all if dry-run is set
traceCmd phase_name cmd_line action
 = do	{ verb <- dynFlag verbosity
	; when (verb >= 2) $ hPutStrLn stderr ("*** " ++ phase_name)
	; when (verb >= 3) $ hPutStrLn stderr cmd_line
	; hFlush stderr
	
	   -- Test for -n flag
	; n <- readIORef v_Dry_run
	; unless n $ do {

	   -- And run it!
	; action `catchAllIO` handle_exn verb
	}}
  where
    handle_exn verb exn = do { when (verb >= 2) (hPutStr   stderr "\n")
			     ; when (verb >= 3) (hPutStrLn stderr ("Failed: " ++ cmd_line))
	          	     ; throwDyn (PhaseFailed phase_name (ExitFailure 1)) }
\end{code}


%************************************************************************
%*									*
\subsection{Support code}
%*									*
%************************************************************************


\begin{code}
-----------------------------------------------------------------------------
-- Convert filepath into MSDOS form.

dosifyPaths :: [String] -> [String]
dosifyPath  :: String -> String
-- dosifyPath does two things
-- a) change '/' to '\'
-- b) remove initial '/cygdrive/'

#if defined(mingw32_TARGET_OS)
dosifyPaths xs = map dosifyPath xs

dosifyPath stuff
  = subst '/' '\\' real_stuff
 where
   -- fully convince myself that /cygdrive/ prefixes cannot
   -- really appear here.
  cygdrive_prefix = "/cygdrive/"

  real_stuff
    | cygdrive_prefix `isPrefixOf` stuff = drop (length cygdrive_prefix) stuff
    | otherwise = stuff
   
  subst a b ls = map (\ x -> if x == a then b else x) ls
#else
dosifyPaths xs = xs
dosifyPath  xs = xs
#endif

-----------------------------------------------------------------------------
-- Path name construction
-- 	At the moment, we always use '/' and rely on dosifyPath 
--	to switch to DOS pathnames when necessary

slash		 :: String -> String -> String
absPath, relPath :: [String] -> String

isSlash '/'   = True
isSlash '\\'  = True
isSlash other = False

relPath [] = ""
relPath xs = foldr1 slash xs

absPath xs = "" `slash` relPath xs

#if defined(mingw32_TARGET_OS)
slash s1 s2 = s1 ++ ('\\' : s2)
#else
slash s1 s2 = s1 ++ ('/' : s2)
#endif

-----------------------------------------------------------------------------
-- Define	myGetProcessId :: IO Int
--		getExecDir     :: IO (Maybe String)

#ifdef mingw32_TARGET_OS
foreign import "_getpid" getProcessID :: IO Int -- relies on Int == Int32 on Windows

getExecDir :: IO (Maybe String)
getExecDir = return Nothing
{-
foreign import stdcall "GetCurrentDirectoryA" getCurrentDirectory :: Int32 -> CString -> IO Int32
getExecDir = do len <- getCurrentDirectory 0 nullAddr
		buf <- mallocArray (fromIntegral len)
		ret <- getCurrentDirectory len buf
		if ret == 0 then return Nothing
		            else do s <- peekCString buf
				    destructArray (fromIntegral len) buf
				    return (Just s)
-}
#else
getProcessID :: IO Int
getProcessID = Posix.getProcessID
getExecDir :: IO (Maybe String) = do return Nothing
#endif
\end{code}
