{-# OPTIONS_GHC -cpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process.Internals
-- Copyright   :  (c) The University of Glasgow 2004
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

-- #hide
module System.Process.Internals (
	ProcessHandle(..), PHANDLE,
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
	 pPrPr_disableITimers, c_execvpe,
# ifdef __GLASGOW_HASKELL__
	runProcessPosix,
# endif
	ignoreSignal, defaultSignal,
#else
# ifdef __GLASGOW_HASKELL__
	runProcessWin32, translate,
# endif
#endif
#ifndef __HUGS__
	commandToProcess,
#endif
	withFilePathException, withCEnvironment
  ) where

import Prelude -- necessary to get dependencies right

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import System.Posix.Types ( CPid )
import System.IO 	( Handle )
#else
import Data.Word ( Word32 )
#endif

import Data.Maybe	( fromMaybe )
# ifdef __GLASGOW_HASKELL__
import GHC.IOBase	( haFD, FD, Exception(..), IOException(..) )
import GHC.Handle 	( stdin, stdout, stderr, withHandle_ )
# elif __HUGS__
import Hugs.Exception	( Exception(..), IOException(..) )
# endif

import Control.Exception ( handle, throwIO )
import Foreign.C
import Foreign

#if defined(mingw32_HOST_OS)
import Control.Monad		( when )
import System.Directory		( doesFileExist )
import Control.Exception 	( catchJust, ioErrors )
import System.IO.Error		( isDoesNotExistError, doesNotExistErrorType,
				  mkIOError )
import System.Environment	( getEnv )
import System.Directory.Internals ( parseSearchPath, joinFileName )
#endif

#ifdef __HUGS__
{-# CFILES cbits/execvpe.c  #-}
#endif

#include "HsBaseConfig.h"

-- ----------------------------------------------------------------------------
-- ProcessHandle type

{- | A handle to a process, which can be used to wait for termination
     of the process using 'waitForProcess'.

     None of the process-creation functions in this library wait for
     termination: they all return a 'ProcessHandle' which may be used
     to wait for the process later.
-}
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
type PHANDLE = CPid
#else
type PHANDLE = Word32
#endif

newtype ProcessHandle = ProcessHandle PHANDLE

-- ----------------------------------------------------------------------------

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

-- this function disables the itimer, which would otherwise cause confusing
-- signals to be sent to the new process.
foreign import ccall unsafe "pPrPr_disableITimers"
  pPrPr_disableITimers :: IO ()

foreign import ccall unsafe "execvpe"
  c_execvpe :: CString -> Ptr CString -> Ptr CString -> IO CInt

#endif

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

#ifdef __GLASGOW_HASKELL__
-- -----------------------------------------------------------------------------
-- POSIX runProcess with signal handling in the child

runProcessPosix
  :: String
  -> FilePath			-- ^ Filename of the executable
  -> [String]			-- ^ Arguments to pass to the executable
  -> Maybe FilePath		-- ^ Optional path to the working directory
  -> Maybe [(String,String)]	-- ^ Optional environment (otherwise inherit)
  -> Maybe Handle		-- ^ Handle to use for @stdin@
  -> Maybe Handle		-- ^ Handle to use for @stdout@
  -> Maybe Handle		-- ^ Handle to use for @stderr@
  -> Maybe CLong		-- handler for SIGINT
  -> Maybe CLong		-- handler for SIGQUIT
  -> IO ProcessHandle

runProcessPosix fun cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr
	mb_sigint mb_sigquit
 = withFilePathException cmd $ do
     fd_stdin  <- withHandle_ fun (fromMaybe stdin  mb_stdin)  $ return . haFD
     fd_stdout <- withHandle_ fun (fromMaybe stdout mb_stdout) $ return . haFD
     fd_stderr <- withHandle_ fun (fromMaybe stderr mb_stderr) $ return . haFD
	-- some of these might refer to the same Handle, so don't do
	-- nested withHandle_'s (that will deadlock).
     maybeWith withCEnvironment mb_env $ \pEnv -> do
     maybeWith withCString mb_cwd $ \pWorkDir -> do
     withMany withCString (cmd:args) $ \cstrs -> do
     let (set_int, inthand) 
		= case mb_sigint of
			Nothing   -> (0, 0)
			Just hand -> (1, hand)
	 (set_quit, quithand) 
		= case mb_sigquit of
			Nothing   -> (0, 0)
			Just hand -> (1, hand)
     withArray0 nullPtr cstrs $ \pargs -> do
         ph <- throwErrnoIfMinus1 fun $
		 c_runProcess pargs pWorkDir pEnv 
			fd_stdin fd_stdout fd_stderr
			set_int inthand set_quit quithand
	 return (ProcessHandle ph)

foreign import ccall unsafe "runProcess" 
  c_runProcess
        :: Ptr CString			-- args
        -> CString			-- working directory (or NULL)
        -> Ptr CString			-- env (or NULL)
        -> FD				-- stdin
        -> FD				-- stdout
        -> FD				-- stderr
	-> CInt				-- non-zero: set child's SIGINT handler
	-> CLong			-- SIGINT handler
	-> CInt				-- non-zero: set child's SIGQUIT handler
	-> CLong			-- SIGQUIT handler
        -> IO PHANDLE

#endif /* __GLASGOW_HASKELL__ */

ignoreSignal  = CONST_SIG_IGN :: CLong
defaultSignal = CONST_SIG_DFL :: CLong

#else

#ifdef __GLASGOW_HASKELL__

runProcessWin32 fun cmd args mb_cwd mb_env
	mb_stdin mb_stdout mb_stderr extra_cmdline
 = withFilePathException cmd $ do
     fd_stdin  <- withHandle_ fun (fromMaybe stdin  mb_stdin)  $ return . haFD
     fd_stdout <- withHandle_ fun (fromMaybe stdout mb_stdout) $ return . haFD
     fd_stderr <- withHandle_ fun (fromMaybe stderr mb_stderr) $ return . haFD
	-- some of these might refer to the same Handle, so don't do
	-- nested withHandle_'s (that will deadlock).
     maybeWith withCEnvironment mb_env $ \pEnv -> do
     maybeWith withCString      mb_cwd $ \pWorkDir -> do
       let cmdline = translate cmd ++ 
		   concat (map ((' ':) . translate) args) ++
		   (if null extra_cmdline then "" else ' ':extra_cmdline)
       withCString cmdline $ \pcmdline -> do
         proc_handle <- throwErrnoIfMinus1 fun
	                  (c_runProcess pcmdline pWorkDir pEnv 
				fd_stdin fd_stdout fd_stderr)
         return (ProcessHandle proc_handle)

foreign import ccall unsafe "runProcess" 
  c_runProcess
        :: CString
        -> CString
        -> Ptr ()
        -> FD
        -> FD
        -> FD
        -> IO PHANDLE

-- ------------------------------------------------------------------------
-- Passing commands to the OS on Windows

{-
On Windows this is tricky.  We use CreateProcess, passing a single
command-line string (lpCommandLine) as its argument.  (CreateProcess
is well documented on http://msdn.microsoft/com.)

      - It parses the beginning of the string to find the command. If the
	file name has embedded spaces, it must be quoted, using double
	quotes thus 
		"foo\this that\cmd" arg1 arg2

      - The invoked command can in turn access the entire lpCommandLine string,
	and the C runtime does indeed do so, parsing it to generate the 
	traditional argument vector argv[0], argv[1], etc.  It does this
	using a complex and arcane set of rules which are described here:
	
	   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/vccelng/htm/progs_12.asp

	(if this URL stops working, you might be able to find it by
	searching for "Parsing C Command-Line Arguments" on MSDN.  Also,
	the code in the Microsoft C runtime that does this translation
	is shipped with VC++).

Our goal in runProcess is to take a command filename and list of
arguments, and construct a string which inverts the translatsions
described above, such that the program at the other end sees exactly
the same arguments in its argv[] that we passed to rawSystem.

This inverse translation is implemented by 'translate' below.

Here are some pages that give informations on Windows-related 
limitations and deviations from Unix conventions:

    http://support.microsoft.com/default.aspx?scid=kb;en-us;830473
    Command lines and environment variables effectively limited to 8191 
    characters on Win XP, 2047 on NT/2000 (probably even less on Win 9x):

    http://www.microsoft.com/windowsxp/home/using/productdoc/en/default.asp?url=/WINDOWSXP/home/using/productdoc/en/percent.asp
    Command-line substitution under Windows XP. IIRC these facilities (or at 
    least a large subset of them) are available on Win NT and 2000. Some 
    might be available on Win 9x.

    http://www.microsoft.com/windowsxp/home/using/productdoc/en/default.asp?url=/WINDOWSXP/home/using/productdoc/en/Cmd.asp
    How CMD.EXE processes command lines.


Note: CreateProcess does have a separate argument (lpApplicationName)
with which you can specify the command, but we have to slap the
command into lpCommandLine anyway, so that argv[0] is what a C program
expects (namely the application name).  So it seems simpler to just
use lpCommandLine alone, which CreateProcess supports.
-}

-- Translate command-line arguments for passing to CreateProcess().
translate :: String -> String
translate str = '"' : snd (foldr escape (True,"\"") str)
  where escape '"'  (b,     str) = (True,  '\\' : '"'  : str)
        escape '\\' (True,  str) = (True,  '\\' : '\\' : str)
        escape '\\' (False, str) = (False, '\\' : str)
	escape c    (b,     str) = (False, c : str)
	-- See long comment above for what this function is trying to do.
	--
	-- The Bool passed back along the string is True iff the
	-- rest of the string is a sequence of backslashes followed by
	-- a double quote.

#endif /* __GLASGOW_HASKELL__ */

#endif

#ifndef __HUGS__
-- ----------------------------------------------------------------------------
-- commandToProcess

{- | Turns a shell command into a raw command.  Usually this involves
     wrapping it in an invocation of the shell.

   There's a difference in the signature of commandToProcess between
   the Windows and Unix versions.  On Unix, exec takes a list of strings,
   and we want to pass our command to /bin/sh as a single argument.  

   On Windows, CreateProcess takes a single string for the command,
   which is later decomposed by cmd.exe.  In this case, we just want
   to prepend @\"c:\WINDOWS\CMD.EXE \/c\"@ to our command line.  The
   command-line translation that we normally do for arguments on
   Windows isn't required (or desirable) here.
-}

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

commandToProcess
  :: String
  -> IO (FilePath,[String])
commandToProcess string = return ("/bin/sh", ["-c", string])

#else

commandToProcess
  :: String
  -> IO (FilePath,String)
commandToProcess string = do
  cmd <- findCommandInterpreter
  return (cmd, "/c "++string)
	-- We don't want to put the cmd into a single
	-- argument, because cmd.exe will not try to split it up.  Instead,
	-- we just tack the command on the end of the cmd.exe command line,
	-- which partly works.  There seem to be some quoting issues, but
	-- I don't have the energy to find+fix them right now (ToDo). --SDM
	-- (later) Now I don't know what the above comment means.  sigh.

-- Find CMD.EXE (or COMMAND.COM on Win98).  We use the same algorithm as
-- system() in the VC++ CRT (Vc7/crt/src/system.c in a VC++ installation).
findCommandInterpreter :: IO FilePath
findCommandInterpreter = do
  -- try COMSPEC first
  catchJust ioErrors (getEnv "COMSPEC") $ \e -> do
    when (not (isDoesNotExistError e)) $ ioError e

    -- try to find CMD.EXE or COMMAND.COM
    osver <- c_get_osver
    let filename | osver .&. 0x8000 /= 0 = "command.com"
		 | otherwise             = "cmd.exe"
    path <- getEnv "PATH"
    let
	-- use our own version of System.Directory.findExecutable, because
	-- that assumes the .exe suffix.
	search :: [FilePath] -> IO (Maybe FilePath)
	search [] = return Nothing
	search (d:ds) = do
		let path = d `joinFileName` filename
		b <- doesFileExist path
		if b then return (Just path)
		     else search ds
    --
    mb_path <- search (parseSearchPath path)

    case mb_path of
      Nothing -> ioError (mkIOError doesNotExistErrorType 
				"findCommandInterpreter" Nothing Nothing)
      Just cmd -> return cmd


foreign import ccall unsafe "__hscore_get_osver"
  c_get_osver :: IO CUInt
#endif

#endif /* __HUGS__ */

-- ----------------------------------------------------------------------------
-- Utils

withFilePathException :: FilePath -> IO a -> IO a
withFilePathException fpath act = handle mapEx act
  where
    mapEx (IOException (IOError h iot fun str _)) = ioError (IOError h iot fun str (Just fpath))
    mapEx e                                       = throwIO e

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
withCEnvironment :: [(String,String)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment env act =
  let env' = map (\(name, val) -> name ++ ('=':val)) env 
  in withMany withCString env' (\pEnv -> withArray0 nullPtr pEnv act)
#else
withCEnvironment :: [(String,String)] -> (Ptr () -> IO a) -> IO a
withCEnvironment env act =
  let env' = foldr (\(name, val) env -> name ++ ('=':val)++'\0':env) "\0" env 
  in withCString env' (act . castPtr)
#endif

