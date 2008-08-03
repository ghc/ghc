{-# OPTIONS -cpp #-}

import Prelude hiding (catch)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import Control.Exception (throw, catch, try, IOException)
import Data.Maybe (isNothing)
import System.Cmd (system)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Process
import Control.Monad

#if !defined(mingw32_HOST_OS)
import System.Process.Internals (mkProcessHandle)
import System.Posix.Process (forkProcess, createSession, executeFile)
import System.Posix.Signals (installHandler, Handler(Catch),
                             signalProcessGroup, sigINT, sigTERM, sigKILL )
#endif

#if defined(mingw32_HOST_OS)
import WinCBindings
import Foreign
import System.Win32.DebugApi
import System.Win32.Types
#endif

main :: IO ()
main = do
  args <- getArgs
  case args of
      [secs,cmd] -> run (read secs) cmd
      _ -> do hPutStrLn stderr $ "timeout: bad arguments " ++ show args
              exitWith (ExitFailure 1)

timeoutMsg :: String
timeoutMsg = "Timeout happened...killing process..."

run :: Int -> String -> IO ()
#if !defined(mingw32_HOST_OS)
run secs cmd = do
        m <- newEmptyMVar
        mp <- newEmptyMVar
        installHandler sigINT (Catch (putMVar m Nothing)) Nothing
        forkIO (do threadDelay (secs * 1000000)
                   putMVar m Nothing
               )
        forkIO (do ei <- try $ do pid <- systemSession cmd
                                  ph <- mkProcessHandle pid
                                  return (pid, ph)
                   putMVar mp ei
                   case ei of
                       Left _ -> return ()
                       Right (_, ph) -> do
                           r <- waitForProcess ph
                           putMVar m (Just r))
        ei_pid_ph <- takeMVar mp
        case ei_pid_ph of
            Left e -> do hPutStrLn stderr
                                   ("Timeout:\n" ++ show (e :: IOException))
                         exitWith (ExitFailure 98)
            Right (pid,ph) -> do
                r <- takeMVar m
                case r of
                  Nothing -> do
                        hPutStrLn stderr timeoutMsg
                        killProcess pid ph
                        exitWith (ExitFailure 99)
                  Just r -> do
                        exitWith r

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
  ignoreIOExceptions (signalProcessGroup sigTERM pid)
  checkReallyDead 10
  where
    checkReallyDead 0 = hPutStrLn stderr "checkReallyDead: Giving up"
    checkReallyDead (n+1) =
      do threadDelay (3*100000) -- 3/10 sec
         m <- getProcessExitCode ph
         when (isNothing m) $ do
             ignoreIOExceptions (signalProcessGroup sigKILL pid)
             checkReallyDead n

ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `catch` ((\_ -> return ()) :: IOException -> IO ())

#else
run secs cmd =
    alloca $ \p_startupinfo ->
    alloca $ \p_pi ->
    withTString ("sh -c \"" ++ cmd ++ "\"") $ \cmd' ->
    do job <- createJobObjectW nullPtr nullPtr
       let creationflags = 0
       b <- createProcessW nullPtr cmd' nullPtr nullPtr True
                           creationflags
                           nullPtr nullPtr p_startupinfo p_pi
       unless b $ errorWin "createProcessW"
       pi <- peek p_pi
       assignProcessToJobObject job (piProcess pi)
       resumeThread (piThread pi)

       -- The program is now running

       let handle = piProcess pi
       let millisecs = secs * 1000
       rc <- waitForSingleObject handle (fromIntegral millisecs)
       if rc == cWAIT_TIMEOUT
           then do hPutStrLn stderr timeoutMsg
                   terminateJobObject job 99
                   exitWith (ExitFailure 99)
           else alloca $ \p_exitCode ->
                do r <- getExitCodeProcess handle p_exitCode
                   if r then do ec <- peek p_exitCode
                                let ec' = if ec == 0
                                          then ExitSuccess
                                          else ExitFailure $ fromIntegral ec
                                exitWith ec'
                        else errorWin "getExitCodeProcess"
#endif

