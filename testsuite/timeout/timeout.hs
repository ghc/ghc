{-# OPTIONS -cpp #-}
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

timeoutMsg :: String
timeoutMsg = "Timeout happened...killing process..."

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
                        hPutStrLn stderr timeoutMsg
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
       let creationflags = 0
       b <- createProcessW nullPtr cmd'' nullPtr nullPtr True
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

