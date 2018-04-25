{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}
-- | A GHC run-server, which supports running multiple GHC scripts
-- without having to restart from scratch.
module Test.Cabal.Server (
    Server,
    serverProcessId,
    ServerLogMsg(..),
    ServerLogMsgType(..),
    ServerResult(..),
    withNewServer,
    runOnServer,
    runMain,
) where

import Test.Cabal.Script

import Prelude hiding (log)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Concurrent.Async
import System.Process
import System.IO
import System.Exit
import Data.List
import Distribution.Simple.Program.Db
import Distribution.Simple.Program
import Control.Exception
import qualified Control.Exception as E
import Control.Monad
import Data.IORef
import Data.Maybe

import Distribution.Verbosity

import System.Process.Internals
#if mingw32_HOST_OS
import qualified System.Win32.Process as Win32
#endif

-- TODO: Compare this implementation with
-- https://github.com/ndmitchell/ghcid/blob/master/src/Language/Haskell/Ghcid.hs
-- which does something similar

-- ----------------------------------------------------------------- --
-- Public API
-- ----------------------------------------------------------------- --

-- | A GHCi server session, which we can ask to run scripts.
-- It operates in a *fixed* runner environment as specified
-- by 'serverScriptEnv'.
data Server = Server {
        serverStdin         :: Handle,
        serverStdout        :: Handle,
        serverStderr        :: Handle,
        serverProcessHandle :: ProcessHandle,
        serverProcessId     :: ProcessId,
        serverScriptEnv     :: ScriptEnv,
        -- | Accumulators which we use to keep tracking
        -- of stdout/stderr we've incrementally read out.  In the event
        -- of an error we'll use this to give diagnostic information.
        serverStdoutAccum   :: MVar [String],
        serverStderrAccum   :: MVar [String],
        serverLogChan       :: Chan ServerLogMsg
    }

-- | Portable representation of process ID; just a string rendered
-- number.
type ProcessId = String

data ServerLogMsg = ServerLogMsg ServerLogMsgType String
                  | ServerLogEnd
data ServerLogMsgType = ServerOut  ProcessId
                      | ServerErr  ProcessId
                      | ServerIn   ProcessId
                      | ServerMeta ProcessId
                      | AllServers

data ServerResult = ServerResult { -- Result
        serverResultExitCode :: ExitCode,
        serverResultCommand  :: String,
        serverResultStdout   :: String,
        serverResultStderr   :: String
    }

-- | With 'ScriptEnv', create a new GHCi 'Server' session.
-- When @f@ returns, the server is terminated and no longer
-- valid.
withNewServer :: Chan ServerLogMsg -> ScriptEnv -> (Server -> IO a) -> IO a
withNewServer chan senv f =
    bracketWithInit (startServer chan senv) initServer stopServer f

-- | Like 'bracket', but with an initialization function on the resource
-- which will be called, unmasked, on the resource to transform it
-- in some way.  If the initialization function throws an exception, the cleanup
-- handler will get invoked with the original resource; if it succeeds, the
-- cleanup handler will get invoked with the transformed resource.
-- The cleanup handler must be able to handle both cases.
--
-- This can help avoid race conditions in certain situations: with
-- normal use of 'bracket', the resource acquisition function
-- MUST return immediately after the resource is acquired.  If it
-- performs any interruptible actions afterwards, it could be
-- interrupted and the exception handler not called.
bracketWithInit :: IO a -> (a -> IO a) -> (a -> IO b) -> (a -> IO c) -> IO c
bracketWithInit before initialize after thing =
  mask $ \restore -> do
    a0 <- before
    a <- restore (initialize a0) `onException` after a0
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r

-- | Run an hs script on the GHCi server, returning the 'ServerResult' of
-- executing the command.
--
--      * The script MUST have an @hs@ or @lhs@ filename; GHCi
--        will reject non-Haskell filenames.
--
--      * If the script is not well-typed, the returned output
--        will be of GHC's compile errors.
--
--      * Inside your script, do not rely on 'getProgName' having
--        a sensible value.
--
--      * Current working directory and environment overrides
--        are currently not implemented.
--
runOnServer :: Server -> Maybe FilePath -> [(String, Maybe String)]
            -> FilePath -> [String] -> IO ServerResult
runOnServer s mb_cwd env_overrides script_path args = do
    -- TODO: cwd not implemented
    when (isJust mb_cwd)            $ error "runOnServer change directory not implemented"
    -- TODO: env_overrides not implemented
    unless (null env_overrides) $ error "runOnServer set environment not implemented"

    -- Set arguments returned by System.getArgs
    write s $ ":set args " ++ show args
    -- Output start sigil (do it here so we pick up compilation
    -- failures)
    write s $ "System.IO.hPutStrLn System.IO.stdout " ++ show start_sigil
    write s $ "System.IO.hPutStrLn System.IO.stderr " ++ show start_sigil
    _ <- readUntilSigil s start_sigil IsOut
    _ <- readUntilSigil s start_sigil IsErr
    -- Drain the output produced by the script as we are running so that
    -- we do not deadlock over a full pipe.
    withAsync (readUntilEnd s IsOut) $ \a_exit_out -> do
    withAsync (readUntilSigil s end_sigil IsErr) $ \a_err -> do
    -- NB: No :set prog; don't rely on this value in test scripts,
    -- we pass it in via the arguments
    -- NB: load drops all bindings, which is GOOD.  Avoid holding onto
    -- garbage.
    write s $ ":load " ++ script_path
    -- Create a ref which will record the exit status of the command
    -- NB: do this after :load so it doesn't get dropped
    write s $ "ref <- Data.IORef.newIORef (System.Exit.ExitFailure 1)"
    -- TODO: What if an async exception gets raised here?  At the
    -- moment, there is no way to recover until we get to the top-level
    -- bracket; then stopServer which correctly handles this case.
    -- If you do want to be able to abort this computation but KEEP
    -- USING THE SERVER SESSION, you will need to have a lot more
    -- sophisticated logic.
    write s $ "Test.Cabal.Server.runMain ref Main.main"
    -- Output end sigil.
    -- NB: We're line-oriented, so we MUST add an extra newline
    -- to ensure that we see the end sigil.
    write s $ "System.IO.hPutStrLn System.IO.stdout " ++ show ""
    write s $ "System.IO.hPutStrLn System.IO.stderr " ++ show ""
    write s $ "Data.IORef.readIORef ref >>= \\e -> " ++
              " System.IO.hPutStrLn System.IO.stdout (" ++ show end_sigil ++ " ++ \" \" ++ show e)"
    write s $ "System.IO.hPutStrLn System.IO.stderr " ++ show end_sigil
    (code, out) <- wait a_exit_out
    err <- wait a_err

    -- Give the user some indication about how they could run the
    -- command by hand.
    (real_path, real_args) <- runnerCommand (serverScriptEnv s) mb_cwd env_overrides script_path args
    return ServerResult {
            serverResultExitCode = code,
            serverResultCommand = showCommandForUser real_path real_args,
            serverResultStdout = out,
            serverResultStderr = err
        }

-- | Helper function which we use in the GHCi session to communicate
-- the exit code of the process.
runMain :: IORef ExitCode -> IO () -> IO ()
runMain ref m = do
    E.catch (m >> writeIORef ref ExitSuccess) serverHandler
  where
    serverHandler :: SomeException -> IO ()
    serverHandler e = do
        -- TODO: Probably a few more cases you could handle;
        -- e.g., StackOverflow should return 2; also signals.
        writeIORef ref $
          case fromException e of
            Just exit_code -> exit_code
            -- Only rethrow for non ExitFailure exceptions
            _              -> ExitFailure 1
        case fromException e :: Maybe ExitCode of
          Just _ -> return ()
          _ -> throwIO e

-- ----------------------------------------------------------------- --
-- Initialize/tear down
-- ----------------------------------------------------------------- --

-- | Start a new GHCi session.
startServer :: Chan ServerLogMsg -> ScriptEnv -> IO Server
startServer chan senv = do
    (prog, _) <- requireProgram verbosity ghcProgram (runnerProgramDb senv)
    let ghc_args = runnerGhcArgs senv ++ ["--interactive", "-v0", "-ignore-dot-ghci"]
        proc_spec = (proc (programPath prog) ghc_args) {
                        create_group = True,
                        -- Closing fds is VERY important to avoid
                        -- deadlock; we won't see the end of a
                        -- stream until everyone gives up.
                        close_fds = True,
                        std_in  = CreatePipe,
                        std_out = CreatePipe,
                        std_err = CreatePipe
                    }
    -- printRawCommandAndArgsAndEnv (runnerVerbosity senv) (programPath prog) ghc_args Nothing
    when (verbosity >= verbose) $
        writeChan chan (ServerLogMsg AllServers (showCommandForUser (programPath prog) ghc_args))
    (Just hin, Just hout, Just herr, proch) <- createProcess proc_spec
    out_acc <- newMVar []
    err_acc <- newMVar []
    tid <- myThreadId
    return Server {
                serverStdin     = hin,
                serverStdout    = hout,
                serverStderr    = herr,
                serverProcessHandle = proch,
                serverProcessId = show tid,
                serverLogChan   = chan,
                serverStdoutAccum = out_acc,
                serverStderrAccum = err_acc,
                serverScriptEnv = senv
              }
  where
    verbosity = runnerVerbosity senv

-- | Unmasked initialization for the server
initServer :: Server -> IO Server
initServer s0 = do
    -- NB: withProcessHandle reads an MVar and is interruptible
#if mingw32_HOST_OS
    pid <- withProcessHandle (serverProcessHandle s0) $ \ph ->
              case ph of
                  OpenHandle x   -> fmap show (Win32.getProcessId x)
                  ClosedHandle  _ -> return (serverProcessId s0)
#else
    pid <- withProcessHandle (serverProcessHandle s0) $ \ph ->
              case ph of
                  OpenHandle x   -> return (show x)
                  -- TODO: handle OpenExtHandle?
                  _              -> return (serverProcessId s0)
#endif
    let s = s0 { serverProcessId = pid }
    -- We will read/write a line at a time, including for
    -- output; our demarcation tokens are an entire line.
    forM_ [serverStdin, serverStdout, serverStderr] $ \f -> do
        hSetBuffering (f s) LineBuffering
        hSetEncoding (f s) utf8
    write s ":set prompt \"\""
    write s "System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering"
    return s

-- | Stop a GHCi session.
stopServer :: Server -> IO ()
stopServer s = do
    -- This is quite a bit of funny business.
    -- On Linux, terminateProcess will send a SIGINT, which
    -- GHCi will swallow and actually only use to terminate
    -- whatever computation is going on at that time.  So we
    -- have to follow up with an actual :quit command to
    -- finish it up (if you delete it, the processes will
    -- hang around).  On Windows, this will just actually kill
    -- the process so the rest should be unnecessary.
    mb_exit <- getProcessExitCode (serverProcessHandle s)

    let hardKiller = do
            threadDelay 2000000 -- 2sec
            log ServerMeta s $ "Terminating..."
            terminateProcess (serverProcessHandle s)
        softKiller = do
            -- Ask to quit.  If we're in the middle of a computation,
            -- this will buffer up (unless the program is intercepting
            -- stdin, but that should NOT happen.)
            ignore $ write s ":quit"

            -- NB: it's important that we used create_group.  We
            -- run this AFTER write s ":quit" because if we C^C
            -- sufficiently early in GHCi startup process, GHCi
            -- will actually die, and then hClose will fail because
            -- the ":quit" command was buffered up but never got
            -- flushed.
            interruptProcessGroupOf (serverProcessHandle s)

            log ServerMeta s $ "Waiting..."
            -- Close input BEFORE waiting, close output AFTER waiting.
            -- If you get either order wrong, deadlock!
            hClose (serverStdin s)
            -- waitForProcess has race condition
            -- https://github.com/haskell/process/issues/46
            waitForProcess $ serverProcessHandle s

    let drain f = do
            r <- hGetContents (f s)
            _ <- evaluate (length r)
            hClose (f s)
            return r
    withAsync (drain serverStdout) $ \a_out -> do
    withAsync (drain serverStderr) $ \a_err -> do

    r <- case mb_exit of
        Nothing -> do
            log ServerMeta s $ "Terminating GHCi"
            race hardKiller softKiller
        Just exit -> do
            log ServerMeta s $ "GHCi died unexpectedly"
            return (Right exit)

    -- Drain the output buffers
    rest_out <- wait a_out
    rest_err <- wait a_err
    if r /= Right ExitSuccess &&
       r /= Right (ExitFailure (-2)) -- SIGINT; happens frequently for some reason
        then do withMVar (serverStdoutAccum s) $ \acc ->
                    mapM_ (info ServerOut s) (reverse acc)
                info ServerOut  s rest_out
                withMVar (serverStderrAccum s) $ \acc ->
                    mapM_ (info ServerErr s) (reverse acc)
                info ServerErr  s rest_err
                info ServerMeta s $
                    (case r of
                        Left () -> "GHCi was forcibly terminated"
                        Right exit -> "GHCi exited with " ++ show exit) ++
                    if verbosity < verbose
                        then " (use -v for more information)"
                        else ""
        else log ServerOut s rest_out

    log ServerMeta s $ "Done"
    return ()
  where
    verbosity = runnerVerbosity (serverScriptEnv s)

-- Using the procedure from
-- https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions
ignore :: IO () -> IO ()
ignore m = withAsync m $ \a -> void (waitCatch a)

-- ----------------------------------------------------------------- --
-- Utility functions
-- ----------------------------------------------------------------- --

log :: (ProcessId -> ServerLogMsgType) -> Server -> String -> IO ()
log ctor s msg =
    when (verbosity >= verbose) $ info ctor s msg
  where
    verbosity = runnerVerbosity (serverScriptEnv s)

info :: (ProcessId -> ServerLogMsgType) -> Server -> String -> IO ()
info ctor s msg =
    writeChan chan (ServerLogMsg (ctor (serverProcessId s)) msg)
  where
    chan = serverLogChan s

-- | Write a string to the prompt of the GHCi server.
write :: Server -> String -> IO ()
write s msg = do
    log ServerIn s $ msg
    hPutStrLn (serverStdin s) msg
    hFlush (serverStdin s) -- line buffering should get it, but just for good luck

accumulate :: MVar [String] -> String -> IO ()
accumulate acc msg =
    modifyMVar_ acc (\msgs -> return (msg:msgs))

flush :: MVar [String] -> IO [String]
flush acc = modifyMVar acc (\msgs -> return ([], reverse msgs))

data OutOrErr = IsOut | IsErr

serverHandle :: Server -> OutOrErr -> Handle
serverHandle s IsOut = serverStdout s
serverHandle s IsErr = serverStderr s

serverAccum :: Server -> OutOrErr -> MVar [String]
serverAccum s IsOut = serverStdoutAccum s
serverAccum s IsErr = serverStderrAccum s

outOrErrMsgType :: OutOrErr -> (ProcessId -> ServerLogMsgType)
outOrErrMsgType IsOut = ServerOut
outOrErrMsgType IsErr = ServerErr

-- | Consume output from the GHCi server until we hit a "start
-- sigil" (indicating that the subsequent output is for the
-- command we want.)  Call this only immediately after you
-- send a command to GHCi to emit the start sigil.
readUntilSigil :: Server -> String -> OutOrErr -> IO String
readUntilSigil s sigil outerr = do
    l <- hGetLine (serverHandle s outerr)
    log (outOrErrMsgType outerr) s l
    if sigil `isPrefixOf` l -- NB: on Windows there might be extra goo at end
        then intercalate "\n" `fmap` flush (serverAccum s outerr)
        else do accumulate (serverAccum s outerr) l
                readUntilSigil s sigil outerr

-- | Consume output from the GHCi server until we hit the
-- end sigil.  Return the consumed output as well as the
-- exit code (which is at the end of the sigil).
readUntilEnd :: Server -> OutOrErr -> IO (ExitCode, String)
readUntilEnd s outerr = go []
  where
    go rs = do
        l <- hGetLine (serverHandle s outerr)
        log (outOrErrMsgType outerr) s l
        if end_sigil `isPrefixOf` l
            -- NB: NOT unlines, we don't want the trailing newline!
            then do exit <- evaluate (parseExit l)
                    _ <- flush (serverAccum s outerr) -- TODO: don't toss this out
                    return (exit, intercalate "\n" (reverse rs))
            else do accumulate (serverAccum s outerr) l
                    go (l:rs)
    parseExit l = read (drop (length end_sigil) l)

-- | The start and end sigils.  This should be chosen to be
-- reasonably unique, so that test scripts don't accidentally
-- generate them.  If these get spuriously generated, we will
-- probably deadlock.
start_sigil, end_sigil :: String
start_sigil = "BEGIN Test.Cabal.Server"
end_sigil   = "END Test.Cabal.Server"
