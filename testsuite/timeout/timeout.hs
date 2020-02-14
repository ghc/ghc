{-# OPTIONS -cpp #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad
import Control.Exception
import Data.Maybe (isNothing)
import System.Environment (getArgs)
import System.Exit
import System.Process
import System.IO (hPutStrLn, stderr)
import System.IO.Error hiding (try,catch)

main :: IO ()
main = do
  args <- getArgs
  case args of
      [secs,cmd] ->
          case reads secs of
          [(secs', "")] -> run secs' cmd
          _ -> die ("Can't parse " ++ show secs ++ " as a number of seconds")
      _ -> die ("Bad arguments " ++ show args)

data FinishedReason
    = TimedOut
    | Exited ExitCode
    | InterruptedSignal
    | OtherError SomeException

run :: Int -> String -> IO ()
run secs cmd = do
    m <- newEmptyMVar :: IO (MVar FinishedReason)
    mp <- newEmptyMVar :: IO (MVar (Either IOException ProcessHandle))

    -- The timeout thread
    forkIO $ do
        threadDelay (secs * 1000000)
        putMVar m TimedOut

    -- the process itself
    forkIO $ handle (\exc -> putMVar mp $ Left (userError $ show (exc :: SomeException))) $ do
        ei <- fmap (fmap (\(_,_,_,ph) -> ph)) $ try $ createProcess (shell cmd)
            { new_session = True
            , use_process_jobs = True
            }
        putMVar mp ei
        case ei of
           Left _ -> return ()
           Right pid -> do
               r <- waitForProcess pid
               putMVar m (Exited r)

    -- Be sure to catch SIGINT while waiting
    let handleINT UserInterrupt = putMVar m InterruptedSignal
        handleINT other = throwIO other

    handle handleINT $ do
        ei_pid_ph <- takeMVar mp
        case ei_pid_ph of
            Left e -> do hPutStrLn stderr
                                   ("Timeout:\n" ++ show (e :: IOException))
                         exitWith (ExitFailure 98)
            Right pid -> do
                r <- takeMVar m
                case r of
                  TimedOut -> do
                        interruptProcessGroupOf pid
                        terminateProcess pid
                        exitWith (ExitFailure 99)
                  InterruptedSignal -> do
                        interruptProcessGroupOf pid
                        terminateProcess pid
                        exitWith (ExitFailure 2)
                  Exited r -> exitWith r
