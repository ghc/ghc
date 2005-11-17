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
import System.IO 	( IOMode(..), Handle, hClose )
import System.Exit	( ExitCode(..) )

import System.Posix.Internals
import GHC.IOBase	( FD )
import GHC.Handle 	( openFd )

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
  runProcessPosix "runCommand" cmd args Nothing Nothing Nothing Nothing Nothing
	Nothing Nothing
#else
  runProcessWin32 "runCommand" cmd [] Nothing Nothing Nothing Nothing Nothing args
#endif

-- ----------------------------------------------------------------------------
-- runProcess

{- | Runs a raw command, optionally specifying 'Handle's from which to
     take the @stdin@, @stdout@ and @stderr@ channels for the new
     process.  

     Any 'Handle's passed to 'runProcess' are placed immediately in the 
     closed state.
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

runProcess cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr = do
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  h <- runProcessPosix "runProcess" cmd args mb_cwd mb_env 
	mb_stdin mb_stdout mb_stderr
	Nothing Nothing
#else
  h <- runProcessWin32 "runProcess" cmd args mb_cwd mb_env 
	mb_stdin mb_stdout mb_stderr ""
#endif
  maybe (return ()) hClose mb_stdin
  maybe (return ()) hClose mb_stdout
  maybe (return ()) hClose mb_stderr
  return h

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
   
>   (inp,out,err,pid) <- runInteractiveProcess "..."
>   forkIO (hPutStr inp str)
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
     ph <- mkProcessHandle proc_handle
     return (hndStdInput, hndStdOutput, hndStdError, ph)

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
	ph <- mkProcessHandle proc_handle
  	return (hndStdInput, hndStdOutput, hndStdError, ph)

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
   
     GHC Note: in order to call @waitForProcess@ without blocking all the
     other threads in the system, you must compile the program with
     @-threaded@.
-}
waitForProcess
  :: ProcessHandle
  -> IO ExitCode
waitForProcess ph = do
  p_ <- withProcessHandle ph $ \p_ -> return (p_,p_)
  case p_ of
    ClosedHandle e -> return e
    OpenHandle h  -> do
	-- don't hold the MVar while we call c_waitForProcess...
	-- (XXX but there's a small race window here during which another
	-- thread could close the handle or call waitForProcess)
	code <- throwErrnoIfMinus1 "waitForProcess" (c_waitForProcess h)
	withProcessHandle ph $ \p_ ->
	  case p_ of
	    ClosedHandle e -> return (p_,e)
	    OpenHandle ph  -> do
	      closePHANDLE ph
	      let e = if (code == 0)
	  	   then ExitSuccess
		   else (ExitFailure (fromIntegral code))
	      return (ClosedHandle e, e)

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
terminateProcess ph = do
  withProcessHandle_ ph $ \p_ ->
    case p_ of 
      ClosedHandle _ -> return p_
      OpenHandle h -> do
	throwErrnoIfMinus1_ "terminateProcess" $ c_terminateProcess h
	return p_
	-- does not close the handle, we might want to try terminating it
	-- again, or get its exit code.

-- ----------------------------------------------------------------------------
-- getProcessExitCode

{- | 
This is a non-blocking version of 'waitForProcess'.  If the process is
still running, 'Nothing' is returned.  If the process has exited, then
@'Just' e@ is returned where @e@ is the exit code of the process.
Subsequent calls to @getProcessExitStatus@ always return @'Just'
'ExitSuccess'@, regardless of what the original exit code was.
-}
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode ph = do
  withProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle e -> return (p_, Just e)
      OpenHandle h ->
	alloca $ \pExitCode -> do
	    res <- throwErrnoIfMinus1 "getProcessExitCode" $
	        	c_getProcessExitCode h pExitCode
	    code <- peek pExitCode
	    if res == 0
	      then return (p_, Nothing)
	      else do
		   closePHANDLE h
		   let e  | code == 0 = ExitSuccess
			  | otherwise = ExitFailure (fromIntegral code)
		   return (ClosedHandle e, Just e)

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
