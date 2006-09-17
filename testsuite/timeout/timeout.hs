{-# OPTIONS -cpp #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import Control.Exception (try)
import Data.Maybe (isNothing)
import System.Cmd (system)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr)
import System.Process
import Control.Monad (when)
#if !defined(mingw32_HOST_OS)
import System.Process.Internals (mkProcessHandle)
import System.Posix.Process (forkProcess, createSession, executeFile)
import System.Posix.Signals (installHandler, Handler(Catch),
                             signalProcessGroup, sigINT, sigTERM, sigKILL )
#endif



#if !defined(mingw32_HOST_OS)
main = do
  args <- getArgs
  case args of 
    [secs,cmd] -> do
        m <- newEmptyMVar
        mp <- newEmptyMVar
        installHandler sigINT (Catch (putMVar m Nothing)) Nothing
        forkIO (do threadDelay (read secs * 1000000)
                   putMVar m Nothing
               )
        forkIO (do try (do pid <- systemSession cmd
			   ph <- mkProcessHandle pid
                           putMVar mp (pid,ph)
                           r <- waitForProcess ph
                           putMVar m (Just r))
                   return ())

        (pid,ph) <- takeMVar mp
        r <- takeMVar m
        case r of
          Nothing -> do
                killProcess pid ph
                exitWith (ExitFailure 99)
          Just r -> do
                exitWith r
    _other -> do hPutStrLn stderr "timeout: bad arguments"
                 exitWith (ExitFailure 1)

systemSession cmd =  
 forkProcess $ do
   createSession
   executeFile "/bin/sh" False ["-c", cmd] Nothing
   -- need to use exec() directly here, rather than something like
   -- System.Process.system, because we are in a forked child and some
   -- pthread libraries get all upset if you start doing certain
   -- things in a forked child of a pthread process, such as forking
   -- more threads.

killProcess pid ph = do
  try (signalProcessGroup sigTERM pid)
  checkReallyDead 10
  where
    checkReallyDead 0 = hPutStrLn stderr "checkReallyDead: Giving up"
    checkReallyDead (n+1) =
      do threadDelay (3*100000) -- 3/10 sec
         m <- getProcessExitCode ph
         when (isNothing m) $ do
             try (signalProcessGroup sigKILL pid)
             checkReallyDead n

#else

main = do
  args <- getArgs
  case args of 
    [secs,cmd] -> do
        m <- newEmptyMVar
        mp <- newEmptyMVar
        forkIO (do threadDelay (read secs * 1000000)
                   putMVar m Nothing
               )
	-- Assume sh.exe is in the path
        forkIO (do p <- runProcess
                            "sh" ["-c",cmd]
                            Nothing Nothing Nothing Nothing Nothing
		   putMVar mp p
		   r <- waitForProcess p
		   putMVar m (Just r))
	p <- takeMVar mp
        r <- takeMVar m
        case r of
          Nothing -> do
                killProcess p
                exitWith (ExitFailure 99)
          Just r -> do
                exitWith r
    _other -> do hPutStrLn stderr $ "timeout: bad arguments " ++ show args
                 exitWith (ExitFailure 1)

killProcess p = do
  terminateProcess p
  -- ToDo: we should kill the process and its descendents on Win32
  threadDelay (3*100000) -- 3/10 sec
  m <- getProcessExitCode p
  when (isNothing m) $ killProcess p

#endif
