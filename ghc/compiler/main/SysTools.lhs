-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2001-2003
--
-- Access to system tools: gcc, cp, rm etc
--
-----------------------------------------------------------------------------

\begin{code}
module SysTools (
	-- Initialisation
	initSysTools,

	setPgmL,		-- String -> IO ()
	setPgmP,
	setPgmF,
	setPgmc,
	setPgmm,
	setPgms,
	setPgma,
	setPgml,
#ifdef ILX
	setPgmI,
	setPgmi,
#endif
				-- Command-line override
	setDryRun,

	getTopDir,		-- IO String	-- The value of $libdir
	getPackageConfigPath,	-- IO String	-- Where package.conf is
        getUsageMsgPaths,       -- IO (String,String)

	-- Interface to system tools
	runUnlit, runCpp, runCc, -- [Option] -> IO ()
	runPp,                   -- [Option] -> IO ()
	runMangle, runSplit,	 -- [Option] -> IO ()
	runAs, runLink,		 -- [Option] -> IO ()
	runMkDLL,
#ifdef ILX
        runIlx2il, runIlasm,     -- [String] -> IO ()
#endif


	touch,			-- String -> String -> IO ()
	copy,			-- String -> String -> String -> IO ()
	normalisePath,          -- FilePath -> FilePath
	
	-- Temporary-file management
	setTmpDir,
	newTempName,
	cleanTempFiles, cleanTempFilesExcept, removeTmpFiles,
	addFilesToClean,

	-- System interface
	getProcessID,		-- IO Int
	system, 		-- String -> IO ExitCode

	-- Misc
	getSysMan,		-- IO String	Parallel system only
	
	Option(..)

 ) where

#include "HsVersions.h"

import DriverUtil
import DriverPhases     ( isHaskellUserSrcFilename )
import Config
import Outputable
import Panic		( GhcException(..) )
import Util		( global, notNull )
import CmdLineOpts	( dynFlag, verbosity )

import EXCEPTION	( throwDyn )
import DATA_IOREF	( IORef, readIORef, writeIORef )
import DATA_INT
    
import Monad		( when, unless )
import System		( ExitCode(..), getEnv, system )
import IO		( try, catch,
			  openFile, hPutStrLn, hPutStr, hClose, hFlush, IOMode(..),
			  stderr )
import Directory	( doesFileExist, removeFile )
import List             ( intersperse, partition )

#include "../includes/config.h"

-- GHC <= 4.08 didn't have rawSystem, and runs into problems with long command
-- lines on mingw32, so we disallow it now.
#if __GLASGOW_HASKELL__ < 500
#error GHC >= 5.00 is required for bootstrapping GHC
#endif

#ifndef mingw32_HOST_OS
#if __GLASGOW_HASKELL__ > 504
import qualified System.Posix.Internals
#else
import qualified Posix
#endif
#else /* Must be Win32 */
import List		( isPrefixOf )
import Util		( dropList )
import Foreign
import CString		( CString, peekCString )
#endif

#if __GLASGOW_HASKELL__ < 601
import Foreign		( withMany, withArray0, nullPtr, Ptr )
import CForeign		( CString, withCString, throwErrnoIfMinus1 )
#else
import System.Cmd	( rawSystem )
#endif
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
				

  cUNLIT_DIR_REL   The *path* to the directory containing unlit, split etc
  cSPLIT_DIR_REL   *relative* to the root of the build tree,
		   for use when running *in-place* in a build tree (only)
		


---------------------------------------------
NOTES for an ALTERNATIVE scheme (i.e *not* what is currently implemented):

Another hair-brained scheme for simplifying the current tool location
nightmare in GHC: Simon originally suggested using another
configuration file along the lines of GCC's specs file - which is fine
except that it means adding code to read yet another configuration
file.  What I didn't notice is that the current package.conf is
general enough to do this:

Package
    {name = "tools",    import_dirs = [],  source_dirs = [],
     library_dirs = [], hs_libraries = [], extra_libraries = [],
     include_dirs = [], c_includes = [],   package_deps = [],
     extra_ghc_opts = ["-pgmc/usr/bin/gcc","-pgml${libdir}/bin/unlit", ... etc.],
     extra_cc_opts = [], extra_ld_opts = []}

Which would have the advantage that we get to collect together in one
place the path-specific package stuff with the path-specific tool
stuff.
		End of NOTES
---------------------------------------------


%************************************************************************
%*									*
\subsection{Global variables to contain system programs}
%*									*
%************************************************************************

All these pathnames are maintained IN THE NATIVE FORMAT OF THE HOST MACHINE.
(See remarks under pathnames below)

\begin{code}
GLOBAL_VAR(v_Pgm_L,   	error "pgm_L",   String)	-- unlit
GLOBAL_VAR(v_Pgm_P,   	error "pgm_P",   (String,[Option]))	-- cpp
GLOBAL_VAR(v_Pgm_F,   	error "pgm_F",   String)	-- pp
GLOBAL_VAR(v_Pgm_c,   	error "pgm_c",   String)	-- gcc
GLOBAL_VAR(v_Pgm_m,   	error "pgm_m",   String)	-- asm code mangler
GLOBAL_VAR(v_Pgm_s,   	error "pgm_s",   String)	-- asm code splitter
GLOBAL_VAR(v_Pgm_a,   	error "pgm_a",   String)	-- as
#ifdef ILX
GLOBAL_VAR(v_Pgm_I,     error "pgm_I",   String)        -- ilx2il
GLOBAL_VAR(v_Pgm_i,     error "pgm_i",   String)        -- ilasm
#endif
GLOBAL_VAR(v_Pgm_l,   	error "pgm_l",   String)	-- ld
GLOBAL_VAR(v_Pgm_MkDLL, error "pgm_dll", String)	-- mkdll

GLOBAL_VAR(v_Pgm_T,    error "pgm_T",    String)	-- touch
GLOBAL_VAR(v_Pgm_CP,   error "pgm_CP", 	 String)	-- cp

GLOBAL_VAR(v_Path_package_config, error "path_package_config", String)
GLOBAL_VAR(v_Path_usages,  	  error "ghc_usage.txt",       (String,String))

GLOBAL_VAR(v_TopDir,	error "TopDir",	String)		-- -B<dir>

-- Parallel system only
GLOBAL_VAR(v_Pgm_sysman, error "pgm_sysman", String)	-- system manager

-- ways to get at some of these variables from outside this module
getPackageConfigPath = readIORef v_Path_package_config
getTopDir	     = readIORef v_TopDir
\end{code}


%************************************************************************
%*									*
\subsection{Initialisation}
%*									*
%************************************************************************

\begin{code}
initSysTools :: [String]	-- Command-line arguments starting "-B"

	     -> IO ()		-- Set all the mutable variables above, holding 
				--	(a) the system programs
				--	(b) the package-config file
				--	(c) the GHC usage message


initSysTools minusB_args
  = do  { (am_installed, top_dir) <- findTopDir minusB_args
	; writeIORef v_TopDir top_dir
		-- top_dir
		-- 	for "installed" this is the root of GHC's support files
		--	for "in-place" it is the root of the build tree
		-- NB: top_dir is assumed to be in standard Unix format '/' separated

	; let installed, installed_bin :: FilePath -> FilePath
              installed_bin pgm   =  pgmPath top_dir pgm
	      installed     file  =  pgmPath top_dir file
	      inplace dir   pgm   =  pgmPath (top_dir `slash` 
						cPROJECT_DIR `slash` dir) pgm

	; let pkgconfig_path
		| am_installed = installed "package.conf"
		| otherwise    = inplace cGHC_DRIVER_DIR_REL "package.conf.inplace"

	      ghc_usage_msg_path
		| am_installed = installed "ghc-usage.txt"
		| otherwise    = inplace cGHC_DRIVER_DIR_REL "ghc-usage.txt"

	      ghci_usage_msg_path
		| am_installed = installed "ghci-usage.txt"
		| otherwise    = inplace cGHC_DRIVER_DIR_REL "ghci-usage.txt"

		-- For all systems, unlit, split, mangle are GHC utilities
		-- architecture-specific stuff is done when building Config.hs
	      unlit_path
		| am_installed = installed_bin cGHC_UNLIT_PGM
		| otherwise    = inplace cGHC_UNLIT_DIR_REL cGHC_UNLIT_PGM

		-- split and mangle are Perl scripts
	      split_script
		| am_installed = installed_bin cGHC_SPLIT_PGM
		| otherwise    = inplace cGHC_SPLIT_DIR_REL cGHC_SPLIT_PGM

	      mangle_script
		| am_installed = installed_bin cGHC_MANGLER_PGM
		| otherwise    = inplace cGHC_MANGLER_DIR_REL cGHC_MANGLER_PGM

#ifndef mingw32_HOST_OS
	-- check whether TMPDIR is set in the environment
	; IO.try (do dir <- getEnv "TMPDIR" -- fails if not set
	      	     setTmpDir dir
	      	     return ()
                 )
#else
	  -- On Win32, consult GetTempPath() for a temp dir.
	  --  => it first tries TMP, TEMP, then finally the
	  --   Windows directory(!). The directory is in short-path
	  --   form.
	; IO.try (do
	        let len = (2048::Int)
		buf  <- mallocArray len
		ret  <- getTempPath len buf
		tdir <-
		  if ret == 0 then do
		      -- failed, consult TMPDIR.
 	             free buf
		     getEnv "TMPDIR"
		   else do
		     s <- peekCString buf
		     free buf
		     return s
		setTmpDir tdir)
#endif

	-- Check that the package config exists
	; config_exists <- doesFileExist pkgconfig_path
	; when (not config_exists) $
	     throwDyn (InstallationError 
		         ("Can't find package.conf as " ++ pkgconfig_path))

#if defined(mingw32_HOST_OS)
	--		WINDOWS-SPECIFIC STUFF
	-- On Windows, gcc and friends are distributed with GHC,
	-- 	so when "installed" we look in TopDir/bin
	-- When "in-place" we look wherever the build-time configure 
	--	script found them
	-- When "install" we tell gcc where its specs file + exes are (-B)
	--	and also some places to pick up include files.  We need
	--	to be careful to put all necessary exes in the -B place
	--	(as, ld, cc1, etc) since if they don't get found there, gcc
	--	then tries to run unadorned "as", "ld", etc, and will
	--	pick up whatever happens to be lying around in the path,
	--	possibly including those from a cygwin install on the target,
	--	which is exactly what we're trying to avoid.
	; let gcc_path 	| am_installed = installed_bin ("gcc -B\"" ++ installed "gcc-lib/\"")
		       	| otherwise    = cGCC
		-- The trailing "/" is absolutely essential; gcc seems
		-- to construct file names simply by concatenating to this
		-- -B path with no extra slash
		-- We use "/" rather than "\\" because otherwise "\\\" is mangled
		-- later on; although gcc_path is in NATIVE format, gcc can cope
		--	(see comments with declarations of global variables)
		--
		-- The quotes round the -B argument are in case TopDir has spaces in it

	      perl_path | am_installed = installed_bin cGHC_PERL
		        | otherwise    = cGHC_PERL

	-- 'touch' is a GHC util for Windows, and similarly unlit, mangle
	; let touch_path  | am_installed = installed_bin cGHC_TOUCHY_PGM
		       	  | otherwise    = inplace cGHC_TOUCHY_DIR_REL cGHC_TOUCHY_PGM

	-- On Win32 we don't want to rely on #!/bin/perl, so we prepend 
	-- a call to Perl to get the invocation of split and mangle
	; let split_path  = perl_path ++ " \"" ++ split_script ++ "\""
	      mangle_path = perl_path ++ " \"" ++ mangle_script ++ "\""

	; let mkdll_path 
	        | am_installed = pgmPath (installed "gcc-lib/") cMKDLL ++
				 " --dlltool-name " ++ pgmPath (installed "gcc-lib/") "dlltool" ++
				 " --driver-name " ++ gcc_path
		| otherwise    = cMKDLL
#else
	--		UNIX-SPECIFIC STUFF
	-- On Unix, the "standard" tools are assumed to be
	-- in the same place whether we are running "in-place" or "installed"
	-- That place is wherever the build-time configure script found them.
	; let   gcc_path   = cGCC
		touch_path = "touch"
		mkdll_path = panic "Can't build DLLs on a non-Win32 system"

	-- On Unix, scripts are invoked using the '#!' method.  Binary
	-- installations of GHC on Unix place the correct line on the front
	-- of the script at installation time, so we don't want to wire-in
	-- our knowledge of $(PERL) on the host system here.
	; let split_path  = split_script
	      mangle_path = mangle_script
#endif

	-- cpp is derived from gcc on all platforms
        -- HACK, see setPgmP below. We keep 'words' here to remember to fix
        -- Config.hs one day.
        ; let cpp_path  = (gcc_path, (Option "-E"):(map Option (words cRAWCPP_FLAGS)))

	-- For all systems, copy and remove are provided by the host
	-- system; architecture-specific stuff is done when building Config.hs
	; let	cp_path = cGHC_CP
	
	-- Other things being equal, as and ld are simply gcc
	; let	as_path  = gcc_path
		ld_path  = gcc_path

#ifdef ILX
       -- ilx2il and ilasm are specified in Config.hs
       ; let    ilx2il_path = cILX2IL
		ilasm_path  = cILASM
#endif
				       
	-- Initialise the global vars
	; writeIORef v_Path_package_config pkgconfig_path
	; writeIORef v_Path_usages 	   (ghc_usage_msg_path,
					    ghci_usage_msg_path)

	; writeIORef v_Pgm_sysman	   (top_dir ++ "/ghc/rts/parallel/SysMan")
		-- Hans: this isn't right in general, but you can 
		-- elaborate it in the same way as the others

	; writeIORef v_Pgm_L   	 	   unlit_path
	; writeIORef v_Pgm_P   	 	   cpp_path
	; writeIORef v_Pgm_F               ""
	; writeIORef v_Pgm_c   	 	   gcc_path
	; writeIORef v_Pgm_m   	 	   mangle_path
	; writeIORef v_Pgm_s   	 	   split_path
	; writeIORef v_Pgm_a   	 	   as_path
#ifdef ILX
	; writeIORef v_Pgm_I               ilx2il_path
	; writeIORef v_Pgm_i               ilasm_path
#endif
	; writeIORef v_Pgm_l   	 	   ld_path
	; writeIORef v_Pgm_MkDLL 	   mkdll_path
	; writeIORef v_Pgm_T   	 	   touch_path
	; writeIORef v_Pgm_CP  	 	   cp_path

	; return ()
	}

#if defined(mingw32_HOST_OS)
foreign import stdcall "GetTempPathA" unsafe getTempPath :: Int -> CString -> IO Int32
#endif
\end{code}

The various setPgm functions are called when a command-line option
like

	-pgmLld

is used to override a particular program with a new one

\begin{code}
setPgmL = writeIORef v_Pgm_L
-- XXX HACK: Prelude> words "'does not' work" ===> ["'does","not'","work"]
-- Config.hs should really use Option.
setPgmP arg = let (pgm:args) = words arg in writeIORef v_Pgm_P (pgm,map Option args)
setPgmF = writeIORef v_Pgm_F
setPgmc = writeIORef v_Pgm_c
setPgmm = writeIORef v_Pgm_m
setPgms = writeIORef v_Pgm_s
setPgma = writeIORef v_Pgm_a
setPgml = writeIORef v_Pgm_l
#ifdef ILX
setPgmI = writeIORef v_Pgm_I
setPgmi = writeIORef v_Pgm_i
#endif
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
--	   where GHC is running (only on Windows)
--
-- 2. If package.conf exists in proto_top_dir, we are running
--	installed; and TopDir = proto_top_dir
--
-- 3. Otherwise we are running in-place, so
--	proto_top_dir will be /...stuff.../ghc/compiler
--	Set TopDir to /...stuff..., which is the root of the build tree
--
-- This is very gruesome indeed

findTopDir :: [String]
	  -> IO (Bool, 		-- True <=> am installed, False <=> in-place
	         String)	-- TopDir (in Unix format '/' separated)

findTopDir minusbs
  = do { top_dir <- get_proto
        -- Discover whether we're running in a build tree or in an installation,
	-- by looking for the package configuration file.
       ; am_installed <- doesFileExist (top_dir `slash` "package.conf")

       ; return (am_installed, top_dir)
       }
  where
    -- get_proto returns a Unix-format path (relying on getBaseDir to do so too)
    get_proto | notNull minusbs
	      = return (normalisePath (drop 2 (last minusbs)))	-- 2 for "-B"
	      | otherwise	   
	      = do { maybe_exec_dir <- getBaseDir -- Get directory of executable
		   ; case maybe_exec_dir of	  -- (only works on Windows; 
						  --  returns Nothing on Unix)
			Nothing  -> throwDyn (InstallationError "missing -B<dir> option")
			Just dir -> return dir
		   }
\end{code}


%************************************************************************
%*									*
\subsection{Command-line options}
n%*									*
%************************************************************************

When invoking external tools as part of the compilation pipeline, we
pass these a sequence of options on the command-line. Rather than
just using a list of Strings, we use a type that allows us to distinguish
between filepaths and 'other stuff'. [The reason being, of course, that
this type gives us a handle on transforming filenames, and filenames only,
to whatever format they're expected to be on a particular platform.]

\begin{code}
data Option
 = FileOption -- an entry that _contains_ filename(s) / filepaths.
              String  -- a non-filepath prefix that shouldn't be transformed (e.g., "/out=" 
 	      String  -- the filepath/filename portion
 | Option     String
 
showOpt (FileOption pre f) = pre ++ platformPath f
showOpt (Option "") = ""
showOpt (Option s)  = s

\end{code}


%************************************************************************
%*									*
\subsection{Running an external program}
%*									*
%************************************************************************


\begin{code}
runUnlit :: [Option] -> IO ()
runUnlit args = do p <- readIORef v_Pgm_L
		   runSomething "Literate pre-processor" p args

runCpp :: [Option] -> IO ()
runCpp args =   do (p,baseArgs) <- readIORef v_Pgm_P
		   runSomething "C pre-processor" p (baseArgs ++ args)

runPp :: [Option] -> IO ()
runPp args =   do p <- readIORef v_Pgm_F
		  runSomething "Haskell pre-processor" p args

runCc :: [Option] -> IO ()
runCc args =   do p <- readIORef v_Pgm_c
	          runSomething "C Compiler" p args

runMangle :: [Option] -> IO ()
runMangle args = do p <- readIORef v_Pgm_m
		    runSomething "Mangler" p args

runSplit :: [Option] -> IO ()
runSplit args = do p <- readIORef v_Pgm_s
		   runSomething "Splitter" p args

runAs :: [Option] -> IO ()
runAs args = do p <- readIORef v_Pgm_a
		runSomething "Assembler" p args

runLink :: [Option] -> IO ()
runLink args = do p <- readIORef v_Pgm_l
	          runSomething "Linker" p args

#ifdef ILX
runIlx2il :: [Option] -> IO ()
runIlx2il args = do p <- readIORef v_Pgm_I
	            runSomething "Ilx2Il" p args

runIlasm :: [Option] -> IO ()
runIlasm args = do p <- readIORef v_Pgm_i
	           runSomething "Ilasm" p args
#endif

runMkDLL :: [Option] -> IO ()
runMkDLL args = do p <- readIORef v_Pgm_MkDLL
	           runSomething "Make DLL" p args

touch :: String -> String -> IO ()
touch purpose arg =  do p <- readIORef v_Pgm_T
			runSomething purpose p [FileOption "" arg]

copy :: String -> String -> String -> IO ()
copy purpose from to = do
  verb <- dynFlag verbosity
  when (verb >= 2) $ hPutStrLn stderr ("*** " ++ purpose)

  h <- openFile to WriteMode
  ls <- readFile from -- inefficient, but it'll do for now.
	    	      -- ToDo: speed up via slurping.
  hPutStr h ls
  hClose h
\end{code}

\begin{code}
getSysMan :: IO String	-- How to invoke the system manager 
			-- (parallel system only)
getSysMan = readIORef v_Pgm_sysman
\end{code}

\begin{code}
getUsageMsgPaths :: IO (FilePath,FilePath)
	  -- the filenames of the usage messages (ghc, ghci)
getUsageMsgPaths = readIORef v_Path_usages
\end{code}


%************************************************************************
%*									*
\subsection{Managing temporary files
%*									*
%************************************************************************

\begin{code}
GLOBAL_VAR(v_FilesToClean, [],               [String] )
GLOBAL_VAR(v_TmpDir,       cDEFAULT_TMPDIR,  String   )
	-- v_TmpDir has no closing '/'
\end{code}

\begin{code}
setTmpDir dir = writeIORef v_TmpDir (canonicalise dir)
    where
#if !defined(mingw32_HOST_OS)
     canonicalise p = normalisePath p
#else
	-- Canonicalisation of temp path under win32 is a bit more
	-- involved: (a) strip trailing slash, 
	-- 	     (b) normalise slashes
	--	     (c) just in case, if there is a prefix /cygdrive/x/, change to x:
	-- 
     canonicalise path = normalisePath (xltCygdrive (removeTrailingSlash path))

        -- if we're operating under cygwin, and TMP/TEMP is of
	-- the form "/cygdrive/drive/path", translate this to
	-- "drive:/path" (as GHC isn't a cygwin app and doesn't
	-- understand /cygdrive paths.)
     xltCygdrive path
      | "/cygdrive/" `isPrefixOf` path = 
	  case drop (length "/cygdrive/") path of
	    drive:xs@('/':_) -> drive:':':xs
	    _ -> path
      | otherwise = path

        -- strip the trailing backslash (awful, but we only do this once).
     removeTrailingSlash path = 
       case last path of
         '/'  -> init path
         '\\' -> init path
         _    -> path
#endif

cleanTempFiles :: Int -> IO ()
cleanTempFiles verb
   = do fs <- readIORef v_FilesToClean
	removeTmpFiles verb fs
	writeIORef v_FilesToClean []

cleanTempFilesExcept :: Int -> [FilePath] -> IO ()
cleanTempFilesExcept verb dont_delete
   = do files <- readIORef v_FilesToClean
	let (to_keep, to_delete) = partition (`elem` dont_delete) files
	removeTmpFiles verb to_delete
	writeIORef v_FilesToClean to_keep


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
  = warnNon $
    traceCmd "Deleting temp files" 
	     ("Deleting: " ++ unwords deletees)
	     (mapM_ rm deletees)
  where
     -- Flat out refuse to delete files that are likely to be source input
     -- files (is there a worse bug than having a compiler delete your source
     -- files?)
     -- 
     -- Deleting source files is a sign of a bug elsewhere, so prominently flag
     -- the condition.
    warnNon act
     | null non_deletees = act
     | otherwise         = do
        hPutStrLn stderr ("WARNING - NOT deleting source files: " ++ unwords non_deletees)
	act

    (non_deletees, deletees) = partition isHaskellUserSrcFilename fs

    rm f = removeFile f `IO.catch` 
		(\_ignored -> 
		    when (verb >= 2) $
		      hPutStrLn stderr ("Warning: deleting non-existent " ++ f)
		)

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
	     -> [Option]	-- Arguments
				--	runSomething will dos-ify them
	     -> IO ()

runSomething phase_name pgm args = do
  let real_args = filter notNull (map showOpt args)
  traceCmd phase_name (concat (intersperse " " (pgm:real_args))) $ do
  exit_code <- rawSystem pgm real_args
  if (exit_code /= ExitSuccess)
	then throwDyn (PhaseFailed phase_name exit_code)
	else return ()

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
	; action `IO.catch` handle_exn verb
	}}
  where
    handle_exn verb exn = do { when (verb >= 2) (hPutStr   stderr "\n")
			     ; when (verb >= 3) (hPutStrLn stderr ("Failed: " ++ cmd_line ++ (show exn)))
	          	     ; throwDyn (PhaseFailed phase_name (ExitFailure 1)) }

-- -----------------------------------------------------------------------------
-- rawSystem: run an external command

#if __GLASGOW_HASKELL__ < 601

-- This code is copied from System.Cmd on GHC 6.1.

rawSystem :: FilePath -> [String] -> IO ExitCode

#ifndef mingw32_TARGET_OS

rawSystem cmd args =
  withCString cmd $ \pcmd ->
    withMany withCString (cmd:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arr -> do
	status <- throwErrnoIfMinus1 "rawSystem" (c_rawSystem pcmd arr)
        case status of
            0  -> return ExitSuccess
            n  -> return (ExitFailure n)

foreign import ccall "rawSystem" unsafe
  c_rawSystem :: CString -> Ptr CString -> IO Int

#else

-- On Windows, the command line is passed to the operating system as
-- a single string.  Command-line parsing is done by the executable
-- itself.
rawSystem cmd args = do
  let cmdline = {-translate-} cmd ++ concat (map ((' ':) . translate) args)
	-- Urk, don't quote/escape the command name on Windows, because the
	-- compiler is exceedingly naughty and sometimes uses 'perl "..."' 
	-- as the command name.
  withCString cmdline $ \pcmdline -> do
    status <- throwErrnoIfMinus1 "rawSystem" (c_rawSystem pcmdline)
    case status of
       0  -> return ExitSuccess
       n  -> return (ExitFailure n)

translate :: String -> String
translate str = '"' : foldr escape "\"" str
  where escape '"'  str = '\\' : '"'  : str
	escape '\\' str = '\\' : '\\' : str
	escape c    str = c : str

foreign import ccall "rawSystem" unsafe
  c_rawSystem :: CString -> IO Int

#endif
#endif
\end{code}


%************************************************************************
%*									*
\subsection{Path names}
%*									*
%************************************************************************

We maintain path names in Unix form ('/'-separated) right until 
the last moment.  On Windows we dos-ify them just before passing them
to the Windows command.

The alternative, of using '/' consistently on Unix and '\' on Windows,
proved quite awkward.  There were a lot more calls to platformPath,
and even on Windows we might invoke a unix-like utility (eg 'sh'), which
interpreted a command line 'foo\baz' as 'foobaz'.

\begin{code}
-----------------------------------------------------------------------------
-- Convert filepath into platform / MSDOS form.

normalisePath :: String -> String
-- Just changes '\' to '/'

pgmPath :: String		-- Directory string in Unix format
	-> String		-- Program name with no directory separators
				--	(e.g. copy /y)
	-> String		-- Program invocation string in native format



#if defined(mingw32_HOST_OS)
--------------------- Windows version ------------------
normalisePath xs = subst '\\' '/' xs
platformPath p   = subst '/' '\\' p
pgmPath dir pgm  = platformPath dir ++ '\\' : pgm

subst a b ls = map (\ x -> if x == a then b else x) ls
#else
--------------------- Non-Windows version --------------
normalisePath xs   = xs
pgmPath dir pgm    = dir ++ '/' : pgm
platformPath stuff = stuff
--------------------------------------------------------
#endif

\end{code}


-----------------------------------------------------------------------------
   Path name construction

\begin{code}
slash		 :: String -> String -> String
slash s1 s2 = s1 ++ ('/' : s2)
\end{code}


%************************************************************************
%*									*
\subsection{Support code}
%*									*
%************************************************************************

\begin{code}
-----------------------------------------------------------------------------
-- Define	getBaseDir     :: IO (Maybe String)

#if defined(mingw32_HOST_OS)
getBaseDir :: IO (Maybe String)
-- Assuming we are running ghc, accessed by path  $()/bin/ghc.exe,
-- return the path $(stuff).  Note that we drop the "bin/" directory too.
getBaseDir = do let len = (2048::Int) -- plenty, PATH_MAX is 512 under Win32.
		buf <- mallocArray len
		ret <- getModuleFileName nullPtr buf len
		if ret == 0 then free buf >> return Nothing
		            else do s <- peekCString buf
				    free buf
				    return (Just (rootDir s))
  where
    rootDir s = reverse (dropList "/bin/ghc.exe" (reverse (normalisePath s)))

foreign import stdcall "GetModuleFileNameA" unsafe
  getModuleFileName :: Ptr () -> CString -> Int -> IO Int32
#else
getBaseDir :: IO (Maybe String) = do return Nothing
#endif

#ifdef mingw32_HOST_OS
foreign import ccall "_getpid" unsafe getProcessID :: IO Int -- relies on Int == Int32 on Windows
#elif __GLASGOW_HASKELL__ > 504
getProcessID :: IO Int
getProcessID = System.Posix.Internals.c_getpid >>= return . fromIntegral
#else
getProcessID :: IO Int
getProcessID = Posix.getProcessID
#endif

\end{code}
