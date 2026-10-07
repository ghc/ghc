{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- | Test for #4162: 'runGhc' must not install signal handlers on behalf of its
-- caller.
--
-- 'withSignalHandlers' redirects signals to the thread that called it. While
-- 'runGhc' did that implicitly, a GHC session running in a 'forkIO' thread
-- would capture signals aimed at the process as a whole and deliver them to a
-- thread that may not be in a position to act on them. Here that thread sits
-- under 'uninterruptibleMask', so the exception can never be delivered and the
-- signal is swallowed outright.
--
-- The worker process parks a 'forkIO' thread inside 'runGhc' under
-- 'uninterruptibleMask' and blocks its main thread waiting for that thread to
-- finish. The driver process waits for the worker to report that the thread is
-- inside 'runGhc', signals it, and checks that it dies promptly:
--
--   * on unix, SIGTERM has no handler, so the kernel kills the process;
--   * on Windows, CTRL_BREAK is handled by GHC.TopHandler, which throws
--     UserInterrupt at the main thread, which exits with 252.
--
-- Before #4162 both were swallowed by the masked 'forkIO' thread and the worker
-- carried on running.
module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import GHC (runGhc)
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Timeout (timeout)

#if !defined(mingw32_HOST_OS)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
#endif

main :: IO ()
main = getArgs >>= \case
  [libdir]             -> driver libdir
  [libdir, "--worker"] -> worker libdir
  args                 -> die ("FAILED: unexpected arguments: " ++ show args)

-- | The process under test: a GHC session pinned in a 'forkIO' thread, with the
-- main thread waiting on it.
worker :: FilePath -> IO ()
worker libdir = do
  hSetBuffering stdout LineBuffering
  done <- newEmptyMVar
  _ <- forkIO $
         flip finally (putMVar done ()) $
           uninterruptibleMask_ $
             runGhc (Just libdir) $ liftIO $ do
               putStrLn ready
               forever (threadDelay oneSecond)
  takeMVar done
  -- Only reachable if the session thread died on its own; the signal was
  -- supposed to take the whole process down first.
  putStrLn "worker: session thread finished"

-- | Wait for the worker to reach 'runGhc', signal it, and require a prompt exit.
driver :: FilePath -> IO ()
driver libdir = do
  self <- getExecutablePath
  let cp = (proc self [libdir, "--worker"])
             { std_out = CreatePipe
#if defined(mingw32_HOST_OS)
             -- GenerateConsoleCtrlEvent can only target a process group, so the
             -- worker needs one of its own.
             , create_group = True
#endif
             }
  bracket (createProcess cp) kill $ \(_, Just out, _, ph) -> do
    handshake <- timeout handshakeTimeout
                   (try (hGetLine out) :: IO (Either IOException String))
    case handshake of
      Just (Right l) | l == ready -> return ()
      _ -> die ("FAILED: worker never reached runGhc: " ++ show handshake)
    interruptWorker ph
    waitForExit ph >>= \case
      Just code | code == expectedExit -> putStrLn "worker terminated promptly"
      other -> die ("FAILED: expected " ++ show expectedExit
                     ++ " from the worker, got " ++ show other)

-- | Deliver the signal that the worker must not be able to swallow.
interruptWorker :: ProcessHandle -> IO ()
#if defined(mingw32_HOST_OS)
-- CTRL_BREAK_EVENT rather than CTRL_C_EVENT: GenerateConsoleCtrlEvent cannot
-- deliver CTRL+C to a specific process group. Both reach the same handler, in
-- GHC.TopHandler now and in withSignalHandlers before #4162.
interruptWorker = interruptProcessGroupOf
#else
interruptWorker ph = getPid ph >>= \case
  Just pid -> signalProcess sigTERM pid
  Nothing  -> die "FAILED: worker exited before it could be signalled"
#endif

-- | How the worker is expected to die once signalled.
expectedExit :: ExitCode
#if defined(mingw32_HOST_OS)
-- UserInterrupt reaches the top handler, which calls exitInterrupted.
expectedExit = ExitFailure 252
#else
expectedExit = ExitFailure (negate (fromIntegral sigTERM))
#endif

-- | Poll for the worker's exit, giving up after 'terminationTimeout'. Polling
-- rather than 'waitForProcess' so that a worker that ignored the signal fails
-- the test quickly instead of hanging it until the testsuite timeout.
waitForExit :: ProcessHandle -> IO (Maybe ExitCode)
waitForExit ph = go (terminationTimeout `div` pollInterval)
  where
    go :: Int -> IO (Maybe ExitCode)
    go n
      | n <= 0 = return Nothing
      | otherwise = getProcessExitCode ph >>= \case
          Just code -> return (Just code)
          Nothing   -> threadDelay pollInterval >> go (n - 1)

-- | Keep a worker that ignored the signal from outliving the test and holding
-- the inherited stderr open. Note that 'terminateProcess' is only SIGTERM on
-- unix, which is precisely what such a worker swallows.
kill :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
kill (_, _, _, ph) = do
#if defined(mingw32_HOST_OS)
  terminateProcess ph
#else
  getPid ph >>= mapM_ (signalProcess sigKILL)
#endif
  void (waitForProcess ph)

-- | Printed by the worker from inside 'runGhc', so that by the time the driver
-- sees it any handlers 'runGhc' might install are already in place.
ready :: String
ready = "ready"

oneSecond, pollInterval, handshakeTimeout, terminationTimeout :: Int
oneSecond          = 1000000
pollInterval       = 100000            -- 0.1s
handshakeTimeout   = 60 * oneSecond    -- starting a GHC session can be slow
terminationTimeout = 10 * oneSecond
