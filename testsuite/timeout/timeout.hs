{-# OPTIONS -cpp #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import Control.Exception (try)
import Data.Maybe (isNothing)
import System.Cmd (system)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Process (waitForProcess, getProcessExitCode)
#if !defined(mingw32_HOST_OS)
import Control.Monad (when)
import System.Process.Internals (ProcessHandle(ProcessHandle))
import System.Posix.Process (forkProcess, createSession)
import System.Posix.Signals (installHandler, Handler(Catch),
                             signalProcessGroup, sigINT, sigTERM, sigKILL )
#endif

main = do
  args <- getArgs
  case args of 
    [secs,cmd] -> do
        m <- newEmptyMVar
        mp <- newEmptyMVar
#if !defined(mingw32_HOST_OS)
        installHandler sigINT (Catch (putMVar m Nothing)) Nothing
#endif
        forkIO (do threadDelay (read secs * 1000000)
                   putMVar m Nothing
               )
        forkIO (do try (do p <- forkProcess $ do
                               createSession
                               r <- system cmd
                               exitWith r
                           putMVar mp p
                           r <- waitForProcess (ProcessHandle p)
                           putMVar m (Just r))
                   return ())
        p <- takeMVar mp
        r <- takeMVar m
        case r of
          Nothing -> do
                killProcess p
                exitWith (ExitFailure 99)
          Just r -> do
                exitWith r
    _other -> do hPutStrLn stderr "timeout: bad arguments"
                 exitWith (ExitFailure 1)

#if !defined(mingw32_HOST_OS)
killProcess p = do
  try (signalProcessGroup sigTERM p)
  checkReallyDead 10
  where
    checkReallyDead 0 = hPutStrLn stderr "checkReallyDead: Giving up"
    checkReallyDead (n+1) =
      do threadDelay (3*100000) -- 3/10 sec
         m <- getProcessExitCode (ProcessHandle p)
         when (isNothing m) $ do
             try (signalProcessGroup sigKILL p)
             checkReallyDead n
#else
killProcess p = do
  terminateProcess p
  threadDelay (3*100000) -- 3/10 sec
  m <- getProcessExitCode p
  when (isNothing m) $ killProcess p
#endif
