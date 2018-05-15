{-# OPTIONS -cpp #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (putMVar, takeMVar, newEmptyMVar)
import Control.Monad
import Control.Exception
import Data.Maybe (isNothing)
import System.Environment (getArgs)
import System.Exit
import System.IO (hPutStrLn, stderr)

#if !defined(mingw32_HOST_OS)
import System.Posix hiding (killProcess)
import System.IO.Error hiding (try,catch)
#endif

#if defined(mingw32_HOST_OS)
import System.Process
import WinCBindings
import Foreign
import System.Win32.DebugApi
import System.Win32.Types
import System.Win32.Console.CtrlHandler
#endif

main :: IO ()
main = do
  args <- getArgs
  case args of
      [secs,cmd] ->
          case reads secs of
          [(secs', "")] -> run secs' cmd
          _ -> die ("Can't parse " ++ show secs ++ " as a number of seconds")
      _ -> die ("Bad arguments " ++ show args)

run :: Int -> String -> IO ()
#if !defined(mingw32_HOST_OS)
run secs cmd = do
        m <- newEmptyMVar
        mp <- newEmptyMVar
        installHandler sigINT (Catch (putMVar m Nothing)) Nothing
        forkIO $ do threadDelay (secs * 1000000)
                    putMVar m Nothing
        forkIO $ do ei <- try $ do pid <- systemSession cmd
                                   return pid
                    putMVar mp ei
                    case ei of
                       Left _ -> return ()
                       Right pid -> do
                           r <- getProcessStatus True False pid
                           putMVar m r
        ei_pid_ph <- takeMVar mp
        case ei_pid_ph of
            Left e -> do hPutStrLn stderr
                                   ("Timeout:\n" ++ show (e :: IOException))
                         exitWith (ExitFailure 98)
            Right pid -> do
                r <- takeMVar m
                case r of
                  Nothing -> do
                        killProcess pid
                        exitWith (ExitFailure 99)
                  Just (Exited r) -> exitWith r
                  Just (Terminated s) -> raiseSignal s
                  Just _ -> exitWith (ExitFailure 1)

systemSession cmd =
 forkProcess $ do
   createSession
   executeFile "/bin/sh" False ["-c", cmd] Nothing
   -- need to use exec() directly here, rather than something like
   -- System.Process.system, because we are in a forked child and some
   -- pthread libraries get all upset if you start doing certain
   -- things in a forked child of a pthread process, such as forking
   -- more threads.

killProcess pid = do
  ignoreIOExceptions (signalProcessGroup sigTERM pid)
  checkReallyDead 10
  where
    checkReallyDead 0 = hPutStrLn stderr "checkReallyDead: Giving up"
    checkReallyDead (n+1) =
      do threadDelay (3*100000) -- 3/10 sec
         m <- tryJust (guard . isDoesNotExistError) $
                 getProcessStatus False False pid
         case m of
            Right Nothing -> return ()
            Left _ -> return ()
            _ -> do
              ignoreIOExceptions (signalProcessGroup sigKILL pid)
              checkReallyDead n

ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions io = io `catch` ((\_ -> return ()) :: IOException -> IO ())

#else
run secs cmd =
    let escape '\\' = "\\\\"
        escape '"'  = "\\\""
        escape c    = [c]
        cmd' = "sh -c \"" ++ concatMap escape cmd ++ "\"" in
    alloca $ \p_startupinfo ->
    alloca $ \p_pi ->
    withTString cmd' $ \cmd'' ->
    do job <- createJobObjectW nullPtr nullPtr
       b_info <- setJobParameters job
       unless b_info $ errorWin "setJobParameters"

       ioPort <- createCompletionPort job
       when (ioPort == nullPtr) $ errorWin "createCompletionPort, cannot continue."

       -- We're explicitly turning off handle inheritance to prevent misc handles
       -- from being inherited by the child. Notable we don't want the I/O Completion
       -- Ports and Job handles to be inherited. So we mark them as non-inheritable.
       setHandleInformation job    cHANDLE_FLAG_INHERIT 0
       setHandleInformation ioPort cHANDLE_FLAG_INHERIT 0

       -- Now create the process suspended so we can add it to the job and then resume.
       -- This is so we don't miss any events on the receiving end of the I/O port.
       let creationflags = cCREATE_SUSPENDED
       b <- createProcessW nullPtr cmd'' nullPtr nullPtr True
                           creationflags
                           nullPtr nullPtr p_startupinfo p_pi
       unless b $ errorWin "createProcessW"

       pi <- peek p_pi
       b_assign <- assignProcessToJobObject job (piProcess pi)
       unless b_assign $ errorWin "assignProcessToJobObject, cannot continue."

       let handleInterrupt action =
               action `onException` terminateJobObject job 99
           handleCtrl _ = do
               terminateJobObject job 99
               closeHandle ioPort
               closeHandle job
               exitWith (ExitFailure 99)
               return True

       withConsoleCtrlHandler handleCtrl $
           handleInterrupt $ do
              resumeThread (piThread pi)
              -- The program is now running
              let handle = piProcess pi
              let millisecs = secs * 1000
              rc <- waitForJobCompletion job ioPort (fromIntegral millisecs)
              closeHandle ioPort

              if not rc
                then do terminateJobObject job 99
                        closeHandle job
                        exitWith (ExitFailure 99)
                else alloca $ \p_exitCode ->
                      do terminateJobObject job 0
                         -- Ensured it's all really dead.
                         closeHandle job
                         r <- getExitCodeProcess handle p_exitCode
                         if r
                           then peek p_exitCode >>= \case
                                   0 -> exitWith ExitSuccess
                                   e -> exitWith $ ExitFailure (fromIntegral e)
                           else errorWin "getExitCodeProcess"
#endif

