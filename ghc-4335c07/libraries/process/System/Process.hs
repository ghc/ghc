{-# LANGUAGE CPP, ForeignFunctionInterface #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
{-# LANGUAGE InterruptibleFFI #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Process
-- Copyright   :  (c) The University of Glasgow 2004-2008
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (requires concurrency)
--
-- Operations for creating and interacting with sub-processes.
--
-----------------------------------------------------------------------------

-- ToDo:
--      * Flag to control whether exiting the parent also kills the child.

module System.Process (
    -- * Running sub-processes
    createProcess,
    createProcess_,
    shell, proc,
    CreateProcess(..),
    CmdSpec(..),
    StdStream(..),
    ProcessHandle,

    -- ** Simpler functions for common tasks
    callProcess,
    callCommand,
    spawnProcess,
    spawnCommand,
    readCreateProcess,
    readProcess,
    readCreateProcessWithExitCode,
    readProcessWithExitCode,
    withCreateProcess,

    -- ** Related utilities
    showCommandForUser,

    -- ** Control-C handling on Unix
    -- $ctlc-handling

    -- * Process completion
    waitForProcess,
    getProcessExitCode,
    terminateProcess,
    interruptProcessGroupOf,

    -- Interprocess communication
    createPipe,
    createPipeFd,

    -- * Old deprecated functions
    -- | These functions pre-date 'createProcess' which is much more
    -- flexible.
    runProcess,
    runCommand,
    runInteractiveProcess,
    runInteractiveCommand,
    system,
    rawSystem,
    ) where

import Prelude hiding (mapM)

import System.Process.Internals

import Control.Concurrent
import Control.DeepSeq (rnf)
import Control.Exception (SomeException, mask, allowInterrupt, bracket, try, throwIO)
import qualified Control.Exception as C
import Control.Monad
import Data.Maybe
import Foreign
import Foreign.C
import System.Exit      ( ExitCode(..) )
import System.IO
import System.IO.Error (mkIOError, ioeSetErrorString)

-- Provide the data constructors for CPid on GHC 7.4 and later
#if !defined(WINDOWS) && MIN_VERSION_base(4,5,0)
import System.Posix.Types (CPid (..))
#endif

import GHC.IO.Exception ( ioException, IOErrorType(..), IOException(..) )

-- ----------------------------------------------------------------------------
-- createProcess

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a raw command with arguments.
--
-- See 'RawCommand' for precise semantics of the specified @FilePath@.
proc :: FilePath -> [String] -> CreateProcess
proc cmd args = CreateProcess { cmdspec = RawCommand cmd args,
                                cwd = Nothing,
                                env = Nothing,
                                std_in = Inherit,
                                std_out = Inherit,
                                std_err = Inherit,
                                close_fds = False,
                                create_group = False,
                                delegate_ctlc = False,
                                detach_console = False,
                                create_new_console = False,
                                new_session = False,
                                child_group = Nothing,
                                child_user = Nothing,
                                use_process_jobs = False }

-- | Construct a 'CreateProcess' record for passing to 'createProcess',
-- representing a command to be passed to the shell.
shell :: String -> CreateProcess
shell str = CreateProcess { cmdspec = ShellCommand str,
                            cwd = Nothing,
                            env = Nothing,
                            std_in = Inherit,
                            std_out = Inherit,
                            std_err = Inherit,
                            close_fds = False,
                            create_group = False,
                            delegate_ctlc = False,
                            detach_console = False,
                            create_new_console = False,
                            new_session = False,
                            child_group = Nothing,
                            child_user = Nothing,
                            use_process_jobs = False }

{- |
This is the most general way to spawn an external process.  The
process can be a command line to be executed by a shell or a raw command
with a list of arguments.  The stdin, stdout, and stderr streams of
the new process may individually be attached to new pipes, to existing
'Handle's, or just inherited from the parent (the default.)

The details of how to create the process are passed in the
'CreateProcess' record.  To make it easier to construct a
'CreateProcess', the functions 'proc' and 'shell' are supplied that
fill in the fields with default values which can be overriden as
needed.

'createProcess' returns @(/mb_stdin_hdl/, /mb_stdout_hdl/, /mb_stderr_hdl/, /ph/)@,
where

 * if @'std_in' == 'CreatePipe'@, then @/mb_stdin_hdl/@ will be @Just /h/@,
   where @/h/@ is the write end of the pipe connected to the child
   process's @stdin@.

 * otherwise, @/mb_stdin_hdl/ == Nothing@

Similarly for @/mb_stdout_hdl/@ and @/mb_stderr_hdl/@.

For example, to execute a simple @ls@ command:

>   r <- createProcess (proc "ls" [])

To create a pipe from which to read the output of @ls@:

>   (_, Just hout, _, _) <-
>       createProcess (proc "ls" []){ std_out = CreatePipe }

To also set the directory in which to run @ls@:

>   (_, Just hout, _, _) <-
>       createProcess (proc "ls" []){ cwd = Just "\home\bob",
>                                     std_out = CreatePipe }

Note that @Handle@s provided for @std_in@, @std_out@, or @std_err@ via the
@UseHandle@ constructor will be closed by calling this function. This is not
always the desired behavior. In cases where you would like to leave the
@Handle@ open after spawning the child process, please use 'createProcess_'
instead. All created @Handle@s are initially in text mode; if you need them
to be in binary mode then use 'hSetBinaryMode'.

-}
createProcess
  :: CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess cp = do
  r <- createProcess_ "createProcess" cp
  maybeCloseStd (std_in  cp)
  maybeCloseStd (std_out cp)
  maybeCloseStd (std_err cp)
  return r
 where
  maybeCloseStd :: StdStream -> IO ()
  maybeCloseStd (UseHandle hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeCloseStd _ = return ()

-- | A 'C.bracket'-style resource handler for 'createProcess'.
--
-- Does automatic cleanup when the action finishes. If there is an exception
-- in the body then it ensures that the process gets terminated and any
-- 'CreatePipe' 'Handle's are closed. In particular this means that if the
-- Haskell thread is killed (e.g. 'killThread'), that the external process is
-- also terminated.
--
-- e.g.
--
-- > withCreateProcess (proc cmd args) { ... }  $ \stdin stdout stderr ph -> do
-- >   ...
--
-- @since 1.4.3.0
withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess c action =
    C.bracket (createProcess c) cleanupProcess
              (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

-- wrapper so we can get exceptions with the appropriate function name.
withCreateProcess_
  :: String
  -> CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess_ fun c action =
    C.bracketOnError (createProcess_ fun c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)


cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr,
                ph@(ProcessHandle _ delegating_ctlc _)) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.

    -- However we want to end the Ctl-C handling synchronously, so we'll do
    -- that synchronously, and set delegating_ctlc as False for the
    -- waitForProcess (which would otherwise end the Ctl-C delegation itself).
    when delegating_ctlc
      stopDelegateControlC
    _ <- forkIO (waitForProcess (resetCtlcDelegation ph) >> return ())
    return ()
  where
    resetCtlcDelegation (ProcessHandle m _ l) = ProcessHandle m False l

-- ----------------------------------------------------------------------------
-- spawnProcess/spawnCommand

-- | Creates a new process to run the specified raw command with the given
-- arguments. It does not wait for the program to finish, but returns the
-- 'ProcessHandle'.
--
-- @since 1.2.0.0
spawnProcess :: FilePath -> [String] -> IO ProcessHandle
spawnProcess cmd args = do
    (_,_,_,p) <- createProcess_ "spawnProcess" (proc cmd args)
    return p

-- | Creates a new process to run the specified shell command.
-- It does not wait for the program to finish, but returns the 'ProcessHandle'.
--
-- @since 1.2.0.0
spawnCommand :: String -> IO ProcessHandle
spawnCommand cmd = do
    (_,_,_,p) <- createProcess_ "spawnCommand" (shell cmd)
    return p


-- ----------------------------------------------------------------------------
-- callProcess/callCommand

-- | Creates a new process to run the specified command with the given
-- arguments, and wait for it to finish.  If the command returns a non-zero
-- exit code, an exception is raised.
--
-- If an asynchronous exception is thrown to the thread executing
-- @callProcess@, the forked process will be terminated and
-- @callProcess@ will wait (block) until the process has been
-- terminated.
--
-- @since 1.2.0.0
callProcess :: FilePath -> [String] -> IO ()
callProcess cmd args = do
    exit_code <- withCreateProcess_ "callProcess"
                   (proc cmd args) { delegate_ctlc = True } $ \_ _ _ p ->
                   waitForProcess p
    case exit_code of
      ExitSuccess   -> return ()
      ExitFailure r -> processFailedException "callProcess" cmd args r

-- | Creates a new process to run the specified shell command.  If the
-- command returns a non-zero exit code, an exception is raised.
--
-- If an asynchronous exception is thrown to the thread executing
-- @callCommand@, the forked process will be terminated and
-- @callCommand@ will wait (block) until the process has been
-- terminated.
--
-- @since 1.2.0.0
callCommand :: String -> IO ()
callCommand cmd = do
    exit_code <- withCreateProcess_ "callCommand"
                   (shell cmd) { delegate_ctlc = True } $ \_ _ _ p ->
                   waitForProcess p
    case exit_code of
      ExitSuccess   -> return ()
      ExitFailure r -> processFailedException "callCommand" cmd [] r

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fun cmd args exit_code =
      ioError (mkIOError OtherError (fun ++ ": " ++ cmd ++
                                     concatMap ((' ':) . show) args ++
                                     " (exit " ++ show exit_code ++ ")")
                                 Nothing Nothing)


-- ----------------------------------------------------------------------------
-- Control-C handling on Unix

-- $ctlc-handling
--
-- When running an interactive console process (such as a shell, console-based
-- text editor or ghci), we typically want that process to be allowed to handle
-- Ctl-C keyboard interrupts how it sees fit. For example, while most programs
-- simply quit on a Ctl-C, some handle it specially. To allow this to happen,
-- use the @'delegate_ctlc' = True@ option in the 'CreateProcess' options.
--
-- The gory details:
--
-- By default Ctl-C will generate a @SIGINT@ signal, causing a 'UserInterrupt'
-- exception to be sent to the main Haskell thread of your program, which if
-- not specially handled will terminate the program. Normally, this is exactly
-- what is wanted: an orderly shutdown of the program in response to Ctl-C.
--
-- Of course when running another interactive program in the console then we
-- want to let that program handle Ctl-C. Under Unix however, Ctl-C sends
-- @SIGINT@ to every process using the console. The standard solution is that
-- while running an interactive program, ignore @SIGINT@ in the parent, and let
-- it be handled in the child process. If that process then terminates due to
-- the @SIGINT@ signal, then at that point treat it as if we had recieved the
-- @SIGINT@ ourselves and begin an orderly shutdown.
--
-- This behaviour is implemented by 'createProcess' (and
-- 'waitForProcess' \/ 'getProcessExitCode') when the @'delegate_ctlc' = True@
-- option is set. In particular, the @SIGINT@ signal will be ignored until
-- 'waitForProcess' returns (or 'getProcessExitCode' returns a non-Nothing
-- result), so it becomes especially important to use 'waitForProcess' for every
-- processes created.
--
-- In addition, in 'delegate_ctlc' mode, 'waitForProcess' and
-- 'getProcessExitCode' will throw a 'UserInterrupt' exception if the process
-- terminated with @'ExitFailure' (-SIGINT)@. Typically you will not want to
-- catch this exception, but let it propagate, giving a normal orderly shutdown.
-- One detail to be aware of is that the 'UserInterrupt' exception is thrown
-- /synchronously/ in the thread that calls 'waitForProcess', whereas normally
-- @SIGINT@ causes the exception to be thrown /asynchronously/ to the main
-- thread.
--
-- For even more detail on this topic, see
-- <http://www.cons.org/cracauer/sigint.html "Proper handling of SIGINT/SIGQUIT">.

-- -----------------------------------------------------------------------------

-- | @readProcess@ forks an external process, reads its standard output
-- strictly, blocking until the process terminates, and returns the output
-- string. The external process inherits the standard error.
--
-- If an asynchronous exception is thrown to the thread executing
-- @readProcess@, the forked process will be terminated and @readProcess@ will
-- wait (block) until the process has been terminated.
--
-- Output is returned strictly, so this is not suitable for
-- interactive applications.
--
-- This function throws an 'IOError' if the process 'ExitCode' is
-- anything other than 'ExitSuccess'. If instead you want to get the
-- 'ExitCode' then use 'readProcessWithExitCode'.
--
-- Users of this function should compile with @-threaded@ if they
-- want other Haskell threads to keep running while waiting on
-- the result of readProcess.
--
-- >  > readProcess "date" [] []
-- >  "Thu Feb  7 10:03:39 PST 2008\n"
--
-- The arguments are:
--
-- * The command to run, which must be in the $PATH, or an absolute or relative path
--
-- * A list of separate command line arguments to the program
--
-- * A string to pass on standard input to the forked process.
--
readProcess
    :: FilePath                 -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readProcess cmd args = readCreateProcess $ proc cmd args

-- | @readCreateProcess@ works exactly like 'readProcess' except that it
-- lets you pass 'CreateProcess' giving better flexibility.
--
-- >  > readCreateProcess (shell "pwd" { cwd = "/etc/" }) ""
-- >  "/etc\n"
--
-- Note that @Handle@s provided for @std_in@ or @std_out@ via the CreateProcess
-- record will be ignored.
--
-- @since 1.2.3.0

readCreateProcess
    :: CreateProcess
    -> String                   -- ^ standard input
    -> IO String                -- ^ stdout
readCreateProcess cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe
                  }
    (ex, output) <- withCreateProcess_ "readCreateProcess" cp_opts $
      \(Just inh) (Just outh) _ ph -> do

        -- fork off a thread to start consuming the output
        output  <- hGetContents outh
        withForkWait (C.evaluate $ rnf output) $ \waitOut -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          hClose outh

        -- wait on the process
        ex <- waitForProcess ph
        return (ex, output)

    case ex of
     ExitSuccess   -> return output
     ExitFailure r -> processFailedException "readCreateProcess" cmd args r
  where
    cmd = case cp of
            CreateProcess { cmdspec = ShellCommand sc } -> sc
            CreateProcess { cmdspec = RawCommand fp _ } -> fp
    args = case cp of
             CreateProcess { cmdspec = ShellCommand _ } -> []
             CreateProcess { cmdspec = RawCommand _ args' } -> args'


-- | @readProcessWithExitCode@ is like @readProcess@ but with two differences:
--
--  * it returns the 'ExitCode' of the process, and does not throw any
--    exception if the code is not 'ExitSuccess'.
--
--  * it reads and returns the output from process' standard error handle,
--    rather than the process inheriting the standard error handle.
--
-- On Unix systems, see 'waitForProcess' for the meaning of exit codes
-- when the process died as the result of a signal.
--
readProcessWithExitCode
    :: FilePath                 -- ^ Filename of the executable (see 'RawCommand' for details)
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode cmd args =
    readCreateProcessWithExitCode $ proc cmd args

-- | @readCreateProcessWithExitCode@ works exactly like 'readProcessWithExitCode' except that it
-- lets you pass 'CreateProcess' giving better flexibility.
--
-- Note that @Handle@s provided for @std_in@, @std_out@, or @std_err@ via the CreateProcess
-- record will be ignored.
--
-- @since 1.2.3.0
readCreateProcessWithExitCode
    :: CreateProcess
    -> String                      -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readCreateProcessWithExitCode cp input = do
    let cp_opts = cp {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe
                  }
    withCreateProcess_ "readCreateProcessWithExitCode" cp_opts $
      \(Just inh) (Just outh) (Just errh) ph -> do

        out <- hGetContents outh
        err <- hGetContents errh

        -- fork off threads to start consuming stdout & stderr
        withForkWait  (C.evaluate $ rnf out) $ \waitOut ->
         withForkWait (C.evaluate $ rnf err) $ \waitErr -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          waitErr

          hClose outh
          hClose errh

        -- wait on the process
        ex <- waitForProcess ph

        return (ex, out, err)

-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e

-- ----------------------------------------------------------------------------
-- showCommandForUser

-- | Given a program @/p/@ and arguments @/args/@,
--   @showCommandForUser /p/ /args/@ returns a string suitable for pasting
--   into @\/bin\/sh@ (on Unix systems) or @CMD.EXE@ (on Windows).
showCommandForUser :: FilePath -> [String] -> String
showCommandForUser cmd args = unwords (map translate (cmd : args))


-- ----------------------------------------------------------------------------
-- waitForProcess

{- | Waits for the specified process to terminate, and returns its exit code.

GHC Note: in order to call @waitForProcess@ without blocking all the
other threads in the system, you must compile the program with
@-threaded@.

(/Since: 1.2.0.0/) On Unix systems, a negative value @'ExitFailure' -/signum/@
indicates that the child was terminated by signal @/signum/@.
The signal numbers are platform-specific, so to test for a specific signal use
the constants provided by "System.Posix.Signals" in the @unix@ package.
Note: core dumps are not reported, use "System.Posix.Process" if you need this
detail.

-}
waitForProcess
  :: ProcessHandle
  -> IO ExitCode
waitForProcess ph@(ProcessHandle _ delegating_ctlc _) = lockWaitpid $ do
  p_ <- modifyProcessHandle ph $ \p_ -> return (p_,p_)
  case p_ of
    ClosedHandle e -> return e
    OpenHandle h  -> do
        e <- alloca $ \pret -> do
          -- don't hold the MVar while we call c_waitForProcess...
          throwErrnoIfMinus1Retry_ "waitForProcess" (allowInterrupt >> c_waitForProcess h pret)
          modifyProcessHandle ph $ \p_' ->
            case p_' of
              ClosedHandle e  -> return (p_', e)
              OpenExtHandle{} -> return (p_', ExitFailure (-1))
              OpenHandle ph'  -> do
                closePHANDLE ph'
                code <- peek pret
                let e = if (code == 0)
                       then ExitSuccess
                       else (ExitFailure (fromIntegral code))
                return (ClosedHandle e, e)
        when delegating_ctlc $
          endDelegateControlC e
        return e
#if defined(WINDOWS)
    OpenExtHandle _ job iocp ->
        maybe (ExitFailure (-1)) mkExitCode `fmap` waitForJobCompletion job iocp timeout_Infinite
      where mkExitCode code | code == 0 = ExitSuccess
                            | otherwise = ExitFailure $ fromIntegral code
#else
    OpenExtHandle _ _job _iocp ->
        return $ ExitFailure (-1)
#endif
  where
    -- If more than one thread calls `waitpid` at a time, `waitpid` will
    -- return the exit code to one of them and (-1) to the rest of them,
    -- causing an exception to be thrown.
    -- Cf. https://github.com/haskell/process/issues/46, and
    -- https://github.com/haskell/process/pull/58 for further discussion
    lockWaitpid m = withMVar (waitpidLock ph) $ \() -> m

-- ----------------------------------------------------------------------------
-- getProcessExitCode

{- |
This is a non-blocking version of 'waitForProcess'.  If the process is
still running, 'Nothing' is returned.  If the process has exited, then
@'Just' e@ is returned where @e@ is the exit code of the process.

On Unix systems, see 'waitForProcess' for the meaning of exit codes
when the process died as the result of a signal.
-}

getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode ph@(ProcessHandle _ delegating_ctlc _) = tryLockWaitpid $ do
  (m_e, was_open) <- modifyProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle e -> return (p_, (Just e, False))
      open -> do
        alloca $ \pExitCode -> do
          case getHandle open of
            Nothing -> return (p_, (Nothing, False))
            Just h  -> do
                res <- throwErrnoIfMinus1Retry "getProcessExitCode" $
                                        c_getProcessExitCode h pExitCode
                code <- peek pExitCode
                if res == 0
                   then return (p_, (Nothing, False))
                   else do
                        closePHANDLE h
                        let e  | code == 0 = ExitSuccess
                               | otherwise = ExitFailure (fromIntegral code)
                        return (ClosedHandle e, (Just e, True))
  case m_e of
    Just e | was_open && delegating_ctlc -> endDelegateControlC e
    _                                    -> return ()
  return m_e
    where getHandle :: ProcessHandle__ -> Maybe PHANDLE
          getHandle (OpenHandle        h) = Just h
          getHandle (ClosedHandle      _) = Nothing
          getHandle (OpenExtHandle h _ _) = Just h

          -- If somebody is currently holding the waitpid lock, we don't want to
          -- accidentally remove the pid from the process table.
          -- Try acquiring the waitpid lock. If it is held, we are done
          -- since that means the process is still running and we can return
          -- `Nothing`. If it is not held, acquire it so we can run the
          -- (non-blocking) call to `waitpid` without worrying about any
          -- other threads calling it at the same time.
          tryLockWaitpid :: IO (Maybe ExitCode) -> IO (Maybe ExitCode)
          tryLockWaitpid action = bracket acquire release between
            where
              acquire   = tryTakeMVar (waitpidLock ph)
              release m = case m of
                Nothing -> return ()
                Just () -> putMVar (waitpidLock ph) ()
              between m = case m of
                Nothing -> return Nothing
                Just () -> action

-- ----------------------------------------------------------------------------
-- terminateProcess

-- | Attempts to terminate the specified process.  This function should
-- not be used under normal circumstances - no guarantees are given regarding
-- how cleanly the process is terminated.  To check whether the process
-- has indeed terminated, use 'getProcessExitCode'.
--
-- On Unix systems, 'terminateProcess' sends the process the SIGTERM signal.
-- On Windows systems, the Win32 @TerminateProcess@ function is called, passing
-- an exit code of 1.
--
-- Note: on Windows, if the process was a shell command created by
-- 'createProcess' with 'shell', or created by 'runCommand' or
-- 'runInteractiveCommand', then 'terminateProcess' will only
-- terminate the shell, not the command itself.  On Unix systems, both
-- processes are in a process group and will be terminated together.

terminateProcess :: ProcessHandle -> IO ()
terminateProcess ph = do
  withProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle  _ -> return ()
#if defined(WINDOWS)
      OpenExtHandle{} -> terminateJob ph 1 >> return ()
#else
      OpenExtHandle{} -> error "terminateProcess with OpenExtHandle should not happen on POSIX."
#endif
      OpenHandle    h -> do
        throwErrnoIfMinus1Retry_ "terminateProcess" $ c_terminateProcess h
        return ()
        -- does not close the handle, we might want to try terminating it
        -- again, or get its exit code.


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

foreign import ccall interruptible "waitForProcess" -- NB. safe - can block
  c_waitForProcess
        :: PHANDLE
        -> Ptr CInt
        -> IO CInt


-- ----------------------------------------------------------------------------
-- Old deprecated variants
-- ----------------------------------------------------------------------------

-- TODO: We're not going to mark these functions as DEPRECATED immediately in
-- process-1.2.0.0. That's because some of their replacements have not been
-- around for all that long. But they should eventually be marked with a
-- suitable DEPRECATED pragma after a release or two.


-- ----------------------------------------------------------------------------
-- runCommand

--TODO: in a later release {-# DEPRECATED runCommand "Use 'spawnCommand' instead" #-}

{- | Runs a command using the shell.
 -}
runCommand
  :: String
  -> IO ProcessHandle

runCommand string = do
  (_,_,_,ph) <- createProcess_ "runCommand" (shell string)
  return ph


-- ----------------------------------------------------------------------------
-- runProcess

--TODO: in a later release {-# DEPRECATED runProcess "Use 'spawnProcess' or 'createProcess' instead" #-}

{- | Runs a raw command, optionally specifying 'Handle's from which to
     take the @stdin@, @stdout@ and @stderr@ channels for the new
     process (otherwise these handles are inherited from the current
     process).

     Any 'Handle's passed to 'runProcess' are placed immediately in the
     closed state.

     Note: consider using the more general 'createProcess' instead of
     'runProcess'.
-}
runProcess
  :: FilePath                   -- ^ Filename of the executable (see 'RawCommand' for details)
  -> [String]                   -- ^ Arguments to pass to the executable
  -> Maybe FilePath             -- ^ Optional path to the working directory
  -> Maybe [(String,String)]    -- ^ Optional environment (otherwise inherit)
  -> Maybe Handle               -- ^ Handle to use for @stdin@ (Nothing => use existing @stdin@)
  -> Maybe Handle               -- ^ Handle to use for @stdout@ (Nothing => use existing @stdout@)
  -> Maybe Handle               -- ^ Handle to use for @stderr@ (Nothing => use existing @stderr@)
  -> IO ProcessHandle

runProcess cmd args mb_cwd mb_env mb_stdin mb_stdout mb_stderr = do
  (_,_,_,ph) <-
      createProcess_ "runProcess"
         (proc cmd args){ cwd = mb_cwd,
                          env = mb_env,
                          std_in  = mbToStd mb_stdin,
                          std_out = mbToStd mb_stdout,
                          std_err = mbToStd mb_stderr }
  maybeClose mb_stdin
  maybeClose mb_stdout
  maybeClose mb_stderr
  return ph
 where
  maybeClose :: Maybe Handle -> IO ()
  maybeClose (Just  hdl)
    | hdl /= stdin && hdl /= stdout && hdl /= stderr = hClose hdl
  maybeClose _ = return ()

  mbToStd :: Maybe Handle -> StdStream
  mbToStd Nothing    = Inherit
  mbToStd (Just hdl) = UseHandle hdl


-- ----------------------------------------------------------------------------
-- runInteractiveCommand

--TODO: in a later release {-# DEPRECATED runInteractiveCommand "Use 'createProcess' instead" #-}

{- | Runs a command using the shell, and returns 'Handle's that may
     be used to communicate with the process via its @stdin@, @stdout@,
     and @stderr@ respectively.
-}
runInteractiveCommand
  :: String
  -> IO (Handle,Handle,Handle,ProcessHandle)

runInteractiveCommand string =
  runInteractiveProcess1 "runInteractiveCommand" (shell string)


-- ----------------------------------------------------------------------------
-- runInteractiveProcess

--TODO: in a later release {-# DEPRECATED runInteractiveCommand "Use 'createProcess' instead" #-}

{- | Runs a raw command, and returns 'Handle's that may be used to communicate
     with the process via its @stdin@, @stdout@ and @stderr@ respectively.

    For example, to start a process and feed a string to its stdin:

>   (inp,out,err,pid) <- runInteractiveProcess "..."
>   forkIO (hPutStr inp str)
-}
runInteractiveProcess
  :: FilePath                   -- ^ Filename of the executable (see 'RawCommand' for details)
  -> [String]                   -- ^ Arguments to pass to the executable
  -> Maybe FilePath             -- ^ Optional path to the working directory
  -> Maybe [(String,String)]    -- ^ Optional environment (otherwise inherit)
  -> IO (Handle,Handle,Handle,ProcessHandle)

runInteractiveProcess cmd args mb_cwd mb_env = do
  runInteractiveProcess1 "runInteractiveProcess"
        (proc cmd args){ cwd = mb_cwd, env = mb_env }

runInteractiveProcess1
  :: String
  -> CreateProcess
  -> IO (Handle,Handle,Handle,ProcessHandle)
runInteractiveProcess1 fun cmd = do
  (mb_in, mb_out, mb_err, p) <-
      createProcess_ fun
           cmd{ std_in  = CreatePipe,
                std_out = CreatePipe,
                std_err = CreatePipe }
  return (fromJust mb_in, fromJust mb_out, fromJust mb_err, p)


-- ---------------------------------------------------------------------------
-- system & rawSystem

--TODO: in a later release {-# DEPRECATED system "Use 'callCommand' (or 'spawnCommand' and 'waitForProcess') instead" #-}

{-|
Computation @system cmd@ returns the exit code produced when the
operating system runs the shell command @cmd@.

This computation may fail with one of the following
'System.IO.Error.IOErrorType' exceptions:

[@PermissionDenied@]
The process has insufficient privileges to perform the operation.

[@ResourceExhausted@]
Insufficient resources are available to perform the operation.

[@UnsupportedOperation@]
The implementation does not support system calls.

On Windows, 'system' passes the command to the Windows command
interpreter (@CMD.EXE@ or @COMMAND.COM@), hence Unixy shell tricks
will not work.

On Unix systems, see 'waitForProcess' for the meaning of exit codes
when the process died as the result of a signal.
-}
system :: String -> IO ExitCode
system "" = ioException (ioeSetErrorString (mkIOError InvalidArgument "system" Nothing Nothing) "null command")
system str = do
  (_,_,_,p) <- createProcess_ "system" (shell str) { delegate_ctlc = True }
  waitForProcess p


--TODO: in a later release {-# DEPRECATED rawSystem "Use 'callProcess' (or 'spawnProcess' and 'waitForProcess') instead" #-}

{-|
The computation @'rawSystem' /cmd/ /args/@ runs the operating system command
@/cmd/@ in such a way that it receives as arguments the @/args/@ strings
exactly as given, with no funny escaping or shell meta-syntax expansion.
It will therefore behave more portably between operating systems than 'system'.

The return codes and possible failures are the same as for 'system'.
-}
rawSystem :: String -> [String] -> IO ExitCode
rawSystem cmd args = do
  (_,_,_,p) <- createProcess_ "rawSystem" (proc cmd args) { delegate_ctlc = True }
  waitForProcess p
