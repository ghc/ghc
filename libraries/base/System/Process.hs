{-# OPTIONS_GHC -cpp -fffi #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process
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

-- ToDo:
--	* Flag to control whether exiting the parent also kills the child.
-- 	* Windows impl of runProcess should close the Handles.
--      * Add system/rawSystem replacements

{- NOTES on createPipe:
 
   createPipe is no longer exported, because of the following problems:

	- it wasn't used to implement runInteractiveProcess on Unix, because
	  the file descriptors for the unused ends of the pipe need to be closed
	  in the child process.

        - on Windows, a special version of createPipe is needed that sets
	  the inheritance flags correctly on the ends of the pipe (see
	  mkAnonPipe below).
-}

module System.Process (
	-- * Running sub-processes
	ProcessHandle,
	runCommand,
	runProcess,
	runInteractiveCommand,
	runInteractiveProcess,

	-- * Process completion
	waitForProcess,
	getProcessExitCode,
	terminateProcess,
 ) where

import Prelude

import System.Process.Internals

import Foreign
import Foreign.C 
import Data.Maybe	( fromMaybe )
import System.IO 	( IOMode(..), Handle )
import System.Exit	( ExitCode(..) )
import Control.Exception ( handle, throwIO )

import System.Posix.Internals
import GHC.IOBase	( haFD, FD, Exception(..), IOException(..) )
import GHC.Handle 	( stdin, stdout, stderr, withHandle_, openFd )

-- ----------------------------------------------------------------------------
-- runCommand

{- | Runs a command using the shell.
 -}
runCommand
  :: String
  -> IO ProcessHandle

runCommand string = do
  (cmd,args) <- commandToProcess string
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  runProcess1 "runCommand" cmd args Nothing Nothing Nothing Nothing Nothing
#else
  runProcess1 "runCommand" cmd [] Nothing Nothing Nothing Nothing Nothing args
#endif

-- ----------------------------------------------------------------------------
-- runProcess

{- | Runs a raw command, optionally specifying 'Handle's from which to
     take the @stdin@, @stdout@ and @stderr@ channels for the new
     process.  

     Any 'Handle's passed to 'runProcess' are placed immediately in the 
     closed state, so may no longer be referenced by the Haskell process.
-}
runProcess
  :: FilePath			-- ^ Filename of the executable
  -> [String]			-- ^ Arguments to pass to the executable
  -> Maybe FilePath		-- ^ Optional path to the working directory
  -> Maybe [(String,String)]	-- ^ Optional environment (otherwise inherit)
  -> Maybe Handle		-- ^ Handle to use for @stdin@
  -> Maybe Handle		-- ^ Handle to use for @stdout@
  -> Maybe Handle		-- ^ Handle to use for @stderr@
  -> IO ProcessHandle

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

runProcess cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr
 = runProcess1 "runProcess" cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr

runProcess1 fun cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr
 = withFilePathException cmd $
     withHandle_ fun (fromMaybe stdin  mb_stdin)  $ \hndStdInput  ->
     withHandle_ fun (fromMaybe stdout mb_stdout) $ \hndStdOutput ->
     withHandle_ fun (fromMaybe stderr mb_stderr) $ \hndStdError ->
     maybeWith withCEnvironment mb_env $ \pEnv ->
     maybeWith withCString mb_cwd $ \pWorkDir ->
     withMany withCString (cmd:args) $ \cstrs ->
     withArray0 nullPtr cstrs $ \pargs -> do
         ph <- throwErrnoIfMinus1 fun
		(c_runProcess pargs pWorkDir pEnv 
			(haFD hndStdInput)
			(haFD hndStdOutput)
			(haFD hndStdError))
	 return (ProcessHandle ph)

foreign import ccall unsafe "runProcess" 
  c_runProcess
        :: Ptr CString			-- args
        -> CString			-- working directory (or NULL)
        -> Ptr CString			-- env (or NULL)
        -> FD				-- stdin
        -> FD				-- stdout
        -> FD				-- stderr
        -> IO PHANDLE

#else

runProcess cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr =
  runProcess1 "runProcess" cmd args mb_cwd mb_env 
	mb_stdin mb_stdout mb_stderr ""

runProcess1 fun cmd args mb_cwd mb_env
	mb_stdin mb_stdout mb_stderr extra_cmdline
 = withFilePathException cmd $
     withHandle_ fun (fromMaybe stdin  mb_stdin)  $ \hndStdInput  ->
     withHandle_ fun (fromMaybe stdout mb_stdout) $ \hndStdOutput ->
     withHandle_ fun (fromMaybe stderr mb_stderr) $ \hndStdError ->
     maybeWith withCEnvironment mb_env $ \pEnv -> do
     maybeWith withCString      mb_cwd $ \pWorkDir -> do
       let cmdline = translate cmd ++ 
		   concat (map ((' ':) . translate) args) ++
		   (if null extra_cmdline then "" else ' ':extra_cmdline)
       withCString cmdline $ \pcmdline -> do
         proc_handle <- throwErrnoIfMinus1 fun
	                  (c_runProcess pcmdline pWorkDir pEnv 
				(haFD hndStdInput)
				(haFD hndStdOutput)
				(haFD hndStdError))
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

     -- Set the standard HANDLEs for the child process appropriately.  NOTE:
     -- this relies on the HANDLEs being inheritable.  By default, the
     -- runtime open() function creates inheritable handles (unless O_NOINHERIT
     -- is specified).  But perhaps we should DuplicateHandle() to make sure
     -- the handle is inheritable?
#endif

-- ----------------------------------------------------------------------------
-- runInteractiveCommand

{- | Runs a command using the shell, and returns 'Handle's that may
     be used to communicate with the process via its @stdin@, @stdout@,
     and @stderr@ respectively.
-}
runInteractiveCommand
  :: String
  -> IO (Handle,Handle,Handle,ProcessHandle)

runInteractiveCommand string = do
  (cmd,args) <- commandToProcess string
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  runInteractiveProcess1 "runInteractiveCommand" cmd args Nothing Nothing
#else
  runInteractiveProcess1 "runInteractiveCommand" cmd [] Nothing Nothing args
#endif

-- ----------------------------------------------------------------------------
-- runInteractiveProcess

{- | Runs a raw command, and returns 'Handle's that may be used to communicate
     with the process via its @stdin@, @stdout@ and @stderr@ respectively.

    For example, to start a process and feed a string to its stdin:
   
>   (in,out,err,pid) <- runInteractiveProcess "..."
>   forkIO (hPutStr in str)
-}
runInteractiveProcess
  :: FilePath			-- ^ Filename of the executable
  -> [String]			-- ^ Arguments to pass to the executable
  -> Maybe FilePath		-- ^ Optional path to the working directory
  -> Maybe [(String,String)]	-- ^ Optional environment (otherwise inherit)
  -> IO (Handle,Handle,Handle,ProcessHandle)

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

runInteractiveProcess cmd args mb_cwd mb_env = 
  runInteractiveProcess1 "runInteractiveProcess" cmd args mb_cwd mb_env

runInteractiveProcess1 fun cmd args mb_cwd mb_env = do
  withFilePathException cmd $
   alloca $ \ pfdStdInput  ->
   alloca $ \ pfdStdOutput ->
   alloca $ \ pfdStdError  ->
   maybeWith withCEnvironment mb_env $ \pEnv ->
   maybeWith withCString mb_cwd $ \pWorkDir ->
   withMany withCString (cmd:args) $ \cstrs ->
   withArray0 nullPtr cstrs $ \pargs -> do
     proc_handle <- throwErrnoIfMinus1 fun
	                  (c_runInteractiveProcess pargs pWorkDir pEnv 
				pfdStdInput pfdStdOutput pfdStdError)
     hndStdInput  <- fdToHandle pfdStdInput  WriteMode
     hndStdOutput <- fdToHandle pfdStdOutput ReadMode
     hndStdError  <- fdToHandle pfdStdError  ReadMode
     return (hndStdInput, hndStdOutput, hndStdError, ProcessHandle proc_handle)

foreign import ccall unsafe "runInteractiveProcess" 
  c_runInteractiveProcess
        ::  Ptr CString
	-> CString
        -> Ptr CString
        -> Ptr FD
        -> Ptr FD
        -> Ptr FD
        -> IO PHANDLE

#else

runInteractiveProcess cmd args mb_cwd mb_env = 
  runInteractiveProcess1 "runInteractiveProcess" cmd args mb_cwd mb_env ""

runInteractiveProcess1 fun cmd args workDir env extra_cmdline
 = withFilePathException cmd $ do
     let cmdline = translate cmd ++ 
  		       concat (map ((' ':) . translate) args) ++
  		       (if null extra_cmdline then "" else ' ':extra_cmdline)
     withCString cmdline $ \pcmdline ->
      alloca $ \ pfdStdInput  ->
      alloca $ \ pfdStdOutput ->
      alloca $ \ pfdStdError  -> do
      maybeWith withCEnvironment env $ \pEnv -> do
      maybeWith withCString workDir $ \pWorkDir -> do
  	proc_handle <- throwErrnoIfMinus1 fun $
  			     c_runInteractiveProcess pcmdline pWorkDir pEnv
				  pfdStdInput pfdStdOutput pfdStdError
  	hndStdInput  <- fdToHandle pfdStdInput  WriteMode
  	hndStdOutput <- fdToHandle pfdStdOutput ReadMode
  	hndStdError  <- fdToHandle pfdStdError  ReadMode
  	return (hndStdInput, hndStdOutput, hndStdError, 
		ProcessHandle proc_handle)

foreign import ccall unsafe "runInteractiveProcess" 
  c_runInteractiveProcess
        :: CString 
        -> CString
        -> Ptr ()
        -> Ptr FD
        -> Ptr FD
        -> Ptr FD
        -> IO PHANDLE

#endif

fdToHandle :: Ptr FD -> IOMode -> IO Handle
fdToHandle pfd mode = do
  fd <- peek pfd
  openFd fd (Just Stream) 
     False{-not a socket-}
     ("fd:" ++ show fd) mode True{-binary-}

-- ----------------------------------------------------------------------------
-- waitForProcess

{- | Waits for the specified process to terminate, and returns its exit code.
   
     GHC Note: in order to call waitForProcess without blocking all the
     other threads in the system, you must compile the program with
     @-threaded@.
-}
waitForProcess
  :: ProcessHandle
  -> IO ExitCode
waitForProcess (ProcessHandle handle) = do
  code <- throwErrnoIfMinus1 "waitForProcess" (c_waitForProcess handle)
  if (code == 0) 
    then return ExitSuccess
    else return (ExitFailure (fromIntegral code))

-- ----------------------------------------------------------------------------
-- terminateProcess

-- | Attempts to terminate the specified process.  This function should
-- not be used under normal circumstances - no guarantees are given regarding
-- how cleanly the process is terminated.  To check whether the process
-- has indeed terminated, use 'getProcessExitCode'.
--
-- On Unix systems, 'terminateProcess' sends the process the SIGKILL signal.
-- On Windows systems, the Win32 @TerminateProcess@ function is called, passing
-- an exit code of 1.
terminateProcess :: ProcessHandle -> IO ()
terminateProcess (ProcessHandle pid) =
  throwErrnoIfMinus1_ "terminateProcess" (c_terminateProcess pid)

-- ----------------------------------------------------------------------------
-- getProcessExitCode

{- | Verifies whether the process is completed and if it is then returns the exit code.
   If the process is still running the function returns Nothing
-}
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode (ProcessHandle handle) =
  alloca $ \pExitCode -> do
    res <- throwErrnoIfMinus1 "getProcessExitCode" (c_getProcessExitCode handle pExitCode)
    code <- peek pExitCode
    if res == 0
      then return Nothing
      else if code == 0
             then return (Just ExitSuccess)
             else return (Just (ExitFailure (fromIntegral code)))

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
  sysDir <- allocaBytes 1024 (\pdir -> c_getSystemDirectory pdir 1024 >> peekCString pdir)
  return (sysDir ++ "\\CMD.EXE", "/c " ++ string)
	-- We don't want to put the cmd into a single
	-- argument, because cmd.exe will not try to split it up.  Instead,
	-- we just tack the command on the end of the cmd.exe command line,
	-- which partly works.  There seem to be some quoting issues, but
	-- I don't have the energy to find+fix them right now (ToDo). --SDM

foreign import stdcall unsafe "GetSystemDirectoryA" 
  c_getSystemDirectory 
        :: CString 
        -> CInt 
        -> IO CInt

#endif

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


-- ----------------------------------------------------------------------------
-- Interface to C bits

foreign import ccall unsafe "terminateProcess"
  c_terminateProcess
	:: PHANDLE
	-> IO CInt

foreign import ccall unsafe "getProcessExitCode"
  c_getProcessExitCode
	:: PHANDLE
	-> Ptr CInt
	-> IO CInt

foreign import ccall safe "waitForProcess" -- NB. safe - can block
  c_waitForProcess
	:: PHANDLE
	-> IO CInt

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

#if defined(mingw32_HOST_OS)

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

#endif
