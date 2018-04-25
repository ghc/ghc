{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module System.Process.Posix
    ( mkProcessHandle
    , translateInternal
    , createProcess_Internal
    , withCEnvironment
    , closePHANDLE
    , startDelegateControlC
    , endDelegateControlC
    , stopDelegateControlC
    , isDefaultSignal
    , ignoreSignal
    , defaultSignal
    , c_execvpe
    , pPrPr_disableITimers
    , createPipeInternal
    , createPipeInternalFd
    , interruptProcessGroupOfInternal
    ) where

import Control.Concurrent
import Control.Exception
import Data.Bits
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Control.Monad
import Data.Char
import System.IO
import System.Posix.Process.Internals ( pPrPr_disableITimers, c_execvpe )
import System.Posix.Types

import System.Posix.Internals
import GHC.IO.Exception
import System.Posix.Signals as Sig
import qualified System.Posix.IO as Posix
import System.Posix.Process (getProcessGroupIDOf)

import System.Process.Common hiding (mb_delegate_ctlc)

#include "HsProcessConfig.h"
#include "processFlags.h"

mkProcessHandle :: PHANDLE -> Bool -> IO ProcessHandle
mkProcessHandle p mb_delegate_ctlc = do
  m <- newMVar (OpenHandle p)
  l <- newMVar ()
  return (ProcessHandle m mb_delegate_ctlc l)

closePHANDLE :: PHANDLE -> IO ()
closePHANDLE _ = return ()

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

commandToProcess :: CmdSpec -> (FilePath, [String])
commandToProcess (ShellCommand string) = ("/bin/sh", ["-c", string])
commandToProcess (RawCommand cmd args) = (cmd, args)

translateInternal :: String -> String
translateInternal "" = "''"
translateInternal str
   -- goodChar is a pessimistic predicate, such that if an argument is
   -- non-empty and only contains goodChars, then there is no need to
   -- do any quoting or escaping
 | all goodChar str = str
 | otherwise        = '\'' : foldr escape "'" str
  where escape '\'' = showString "'\\''"
        escape c    = showChar c
        goodChar c = isAlphaNum c || c `elem` "-_.,/"

-- ----------------------------------------------------------------------------
-- Utils

withCEnvironment :: [(String,String)] -> (Ptr CString  -> IO a) -> IO a
withCEnvironment envir act =
  let env' = map (\(name, val) -> name ++ ('=':val)) envir
  in withMany withCString env' (\pEnv -> withArray0 nullPtr pEnv act)

-- -----------------------------------------------------------------------------
-- POSIX runProcess with signal handling in the child

createProcess_Internal
    :: String
    -> CreateProcess
    -> IO ProcRetHandles
createProcess_Internal fun
                   CreateProcess{ cmdspec = cmdsp,
                                  cwd = mb_cwd,
                                  env = mb_env,
                                  std_in = mb_stdin,
                                  std_out = mb_stdout,
                                  std_err = mb_stderr,
                                  close_fds = mb_close_fds,
                                  create_group = mb_create_group,
                                  delegate_ctlc = mb_delegate_ctlc,
                                  detach_console = mb_detach_console,
                                  create_new_console = mb_create_new_console,
                                  new_session = mb_new_session,
                                  child_group = mb_child_group,
                                  child_user = mb_child_user }
 = do
  let (cmd,args) = commandToProcess cmdsp
  withFilePathException cmd $
   alloca $ \ pfdStdInput  ->
   alloca $ \ pfdStdOutput ->
   alloca $ \ pfdStdError  ->
   alloca $ \ pFailedDoing ->
   maybeWith withCEnvironment mb_env $ \pEnv ->
   maybeWith withFilePath mb_cwd $ \pWorkDir ->
   maybeWith with mb_child_group $ \pChildGroup ->
   maybeWith with mb_child_user $ \pChildUser ->
   withMany withFilePath (cmd:args) $ \cstrs ->
   withArray0 nullPtr cstrs $ \pargs -> do

     fdin  <- mbFd fun fd_stdin  mb_stdin
     fdout <- mbFd fun fd_stdout mb_stdout
     fderr <- mbFd fun fd_stderr mb_stderr

     when mb_delegate_ctlc
       startDelegateControlC

     -- runInteractiveProcess() blocks signals around the fork().
     -- Since blocking/unblocking of signals is a global state
     -- operation, we better ensure mutual exclusion of calls to
     -- runInteractiveProcess().
     proc_handle <- withMVar runInteractiveProcess_lock $ \_ ->
                         c_runInteractiveProcess pargs pWorkDir pEnv
                                fdin fdout fderr
                                pfdStdInput pfdStdOutput pfdStdError
                                pChildGroup pChildUser
                                (if mb_delegate_ctlc then 1 else 0)
                                ((if mb_close_fds then RUN_PROCESS_IN_CLOSE_FDS else 0)
                                .|.(if mb_create_group then RUN_PROCESS_IN_NEW_GROUP else 0)
                                .|.(if mb_detach_console then RUN_PROCESS_DETACHED else 0)
                                .|.(if mb_create_new_console then RUN_PROCESS_NEW_CONSOLE else 0)
                                .|.(if mb_new_session then RUN_PROCESS_NEW_SESSION else 0))
                                pFailedDoing

     when (proc_handle == -1) $ do
         cFailedDoing <- peek pFailedDoing
         failedDoing <- peekCString cFailedDoing
         when mb_delegate_ctlc
           stopDelegateControlC
         throwErrno (fun ++ ": " ++ failedDoing)

     hndStdInput  <- mbPipe mb_stdin  pfdStdInput  WriteMode
     hndStdOutput <- mbPipe mb_stdout pfdStdOutput ReadMode
     hndStdError  <- mbPipe mb_stderr pfdStdError  ReadMode

     ph <- mkProcessHandle proc_handle mb_delegate_ctlc
     return ProcRetHandles { hStdInput    = hndStdInput
                           , hStdOutput   = hndStdOutput
                           , hStdError    = hndStdError
                           , procHandle   = ph
                           }

{-# NOINLINE runInteractiveProcess_lock #-}
runInteractiveProcess_lock :: MVar ()
runInteractiveProcess_lock = unsafePerformIO $ newMVar ()

-- ----------------------------------------------------------------------------
-- Delegated control-C handling on Unix

-- See ticket https://ghc.haskell.org/trac/ghc/ticket/2301
-- and http://www.cons.org/cracauer/sigint.html
--
-- While running an interactive console process like ghci or a shell, we want
-- to let that process handle Ctl-C keyboard interrupts how it sees fit.
-- So that means we need to ignore the SIGINT/SIGQUIT Unix signals while we're
-- running such programs. And then if/when they do terminate, we need to check
-- if they terminated due to SIGINT/SIGQUIT and if so then we behave as if we
-- got the Ctl-C then, by throwing the UserInterrupt exception.
--
-- If we run multiple programs like this concurrently then we have to be
-- careful to avoid messing up the signal handlers. We keep a count and only
-- restore when the last one has finished.

{-# NOINLINE runInteractiveProcess_delegate_ctlc #-}
runInteractiveProcess_delegate_ctlc :: MVar (Maybe (Int, Sig.Handler, Sig.Handler))
runInteractiveProcess_delegate_ctlc = unsafePerformIO $ newMVar Nothing

startDelegateControlC :: IO ()
startDelegateControlC =
    modifyMVar_ runInteractiveProcess_delegate_ctlc $ \delegating -> do
      case delegating of
        Nothing -> do
          -- We're going to ignore ^C in the parent while there are any
          -- processes using ^C delegation.
          --
          -- If another thread runs another process without using
          -- delegation while we're doing this then it will inherit the
          -- ignore ^C status.
          old_int  <- installHandler sigINT  Ignore Nothing
          old_quit <- installHandler sigQUIT Ignore Nothing
          return (Just (1, old_int, old_quit))

        Just (count, old_int, old_quit) -> do
          -- If we're already doing it, just increment the count
          let !count' = count + 1
          return (Just (count', old_int, old_quit))

stopDelegateControlC :: IO ()
stopDelegateControlC =
    modifyMVar_ runInteractiveProcess_delegate_ctlc $ \delegating -> do
      case delegating of
        Just (1, old_int, old_quit) -> do
          -- Last process, so restore the old signal handlers
          _ <- installHandler sigINT  old_int  Nothing
          _ <- installHandler sigQUIT old_quit Nothing
          return Nothing

        Just (count, old_int, old_quit) -> do
          -- Not the last, just decrement the count
          let !count' = count - 1
          return (Just (count', old_int, old_quit))

        Nothing -> return Nothing -- should be impossible

endDelegateControlC :: ExitCode -> IO ()
endDelegateControlC exitCode = do
    stopDelegateControlC

    -- And if the process did die due to SIGINT or SIGQUIT then
    -- we throw our equivalent exception here (synchronously).
    --
    -- An alternative design would be to throw to the main thread, as the
    -- normal signal handler does. But since we can be sync here, we do so.
    -- It allows the code locally to catch it and do something.
    case exitCode of
      ExitFailure n | isSigIntQuit n -> throwIO UserInterrupt
      _                              -> return ()
  where
    isSigIntQuit n = sig == sigINT || sig == sigQUIT
      where
        sig = fromIntegral (-n)

foreign import ccall unsafe "runInteractiveProcess"
  c_runInteractiveProcess
        ::  Ptr CString
        -> CString
        -> Ptr CString
        -> FD
        -> FD
        -> FD
        -> Ptr FD
        -> Ptr FD
        -> Ptr FD
        -> Ptr CGid
        -> Ptr CUid
        -> CInt                         -- reset child's SIGINT & SIGQUIT handlers
        -> CInt                         -- flags
        -> Ptr CString
        -> IO PHANDLE

ignoreSignal, defaultSignal :: CLong
ignoreSignal  = CONST_SIG_IGN
defaultSignal = CONST_SIG_DFL

isDefaultSignal :: CLong -> Bool
isDefaultSignal = (== defaultSignal)

createPipeInternal :: IO (Handle, Handle)
createPipeInternal = do
    (readfd, writefd) <- Posix.createPipe
    readh <- Posix.fdToHandle readfd
    writeh <- Posix.fdToHandle writefd
    return (readh, writeh)

createPipeInternalFd :: IO (FD, FD)
createPipeInternalFd = do
   (Fd readfd, Fd writefd) <- Posix.createPipe
   return (readfd, writefd)

interruptProcessGroupOfInternal
    :: ProcessHandle    -- ^ A process in the process group
    -> IO ()
interruptProcessGroupOfInternal ph = do
    withProcessHandle ph $ \p_ -> do
        case p_ of
            OpenExtHandle{} -> return ()
            ClosedHandle  _ -> return ()
            OpenHandle    h -> do
                pgid <- getProcessGroupIDOf h
                signalProcessGroup sigINT pgid
