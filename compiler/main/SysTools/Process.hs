{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Misc process handling code for SysTools
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module SysTools.Process where

#include "HsVersions.h"

import Exception
import ErrUtils
import DynFlags
import FastString
import Outputable
import Panic
import GhcPrelude
import Util
import SrcLoc           ( SrcLoc, mkSrcLoc, noSrcSpan, mkSrcSpan )

import Control.Concurrent
import Data.Char

import System.Exit
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error as IO
import System.Process

import FileCleanup

-- | Enable process jobs support on Windows if it can be expected to work (e.g.
-- @process >= 1.6.8.0@).
enableProcessJobs :: CreateProcess -> CreateProcess
#if defined(MIN_VERSION_process)
#if MIN_VERSION_process(1,6,8)
enableProcessJobs opts = opts { use_process_jobs = True }
#else
enableProcessJobs opts = opts
#endif
#else
enableProcessJobs opts = opts
#endif

-- Similar to System.Process.readCreateProcessWithExitCode, but stderr is
-- inherited from the parent process, and output to stderr is not captured.
readCreateProcessWithExitCode'
    :: CreateProcess
    -> IO (ExitCode, String)    -- ^ stdout
readCreateProcessWithExitCode' proc = do
    (_, Just outh, _, pid) <-
        createProcess proc{ std_out = CreatePipe }

    -- fork off a thread to start consuming the output
    output  <- hGetContents outh
    outMVar <- newEmptyMVar
    _ <- forkIO $ evaluate (length output) >> putMVar outMVar ()

    -- wait on the output
    takeMVar outMVar
    hClose outh

    -- wait on the process
    ex <- waitForProcess pid

    return (ex, output)

replaceVar :: (String, String) -> [(String, String)] -> [(String, String)]
replaceVar (var, value) env =
    (var, value) : filter (\(var',_) -> var /= var') env

-- | Version of @System.Process.readProcessWithExitCode@ that takes a
-- key-value tuple to insert into the environment.
readProcessEnvWithExitCode
    :: String -- ^ program path
    -> [String] -- ^ program args
    -> (String, String) -- ^ addition to the environment
    -> IO (ExitCode, String, String) -- ^ (exit_code, stdout, stderr)
readProcessEnvWithExitCode prog args env_update = do
    current_env <- getEnvironment
    readCreateProcessWithExitCode (enableProcessJobs $ proc prog args) {
        env = Just (replaceVar env_update current_env) } ""

-- Don't let gcc localize version info string, #8825
c_locale_env :: (String, String)
c_locale_env = ("LANGUAGE", "C")

-- If the -B<dir> option is set, add <dir> to PATH.  This works around
-- a bug in gcc on Windows Vista where it can't find its auxiliary
-- binaries (see bug #1110).
getGccEnv :: [Option] -> IO (Maybe [(String,String)])
getGccEnv opts =
  if null b_dirs
     then return Nothing
     else do env <- getEnvironment
             return (Just (mangle_paths env))
 where
  (b_dirs, _) = partitionWith get_b_opt opts

  get_b_opt (Option ('-':'B':dir)) = Left dir
  get_b_opt other = Right other

  -- Work around #1110 on Windows only (lest we stumble into #17266).
#if defined(mingw32_HOST_OS)
  mangle_paths = map mangle_path
  mangle_path (path,paths) | map toUpper path == "PATH"
        = (path, '\"' : head b_dirs ++ "\";" ++ paths)
  mangle_path other = other
#else
  mangle_paths = id
#endif


-----------------------------------------------------------------------------
-- Running an external program

runSomething :: DynFlags
             -> String          -- For -v message
             -> String          -- Command name (possibly a full path)
                                --      assumed already dos-ified
             -> [Option]        -- Arguments
                                --      runSomething will dos-ify them
             -> IO ()

runSomething dflags phase_name pgm args =
  runSomethingFiltered dflags id phase_name pgm args Nothing Nothing

-- | Run a command, placing the arguments in an external response file.
--
-- This command is used in order to avoid overlong command line arguments on
-- Windows. The command line arguments are first written to an external,
-- temporary response file, and then passed to the linker via @filepath.
-- response files for passing them in. See:
--
--     https://gcc.gnu.org/wiki/Response_Files
--     https://ghc.haskell.org/trac/ghc/ticket/10777
runSomethingResponseFile
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe [(String,String)] -> IO ()

runSomethingResponseFile dflags filter_fn phase_name pgm args mb_env =
    runSomethingWith dflags phase_name pgm args $ \real_args -> do
        fp <- getResponseFile real_args
        let args = ['@':fp]
        r <- builderMainLoop dflags filter_fn pgm args Nothing mb_env
        return (r,())
  where
    getResponseFile args = do
      fp <- newTempName dflags TFL_CurrentModule "rsp"
      withFile fp WriteMode $ \h -> do
#if defined(mingw32_HOST_OS)
          hSetEncoding h latin1
#else
          hSetEncoding h utf8
#endif
          hPutStr h $ unlines $ map escape args
      return fp

    -- Note: Response files have backslash-escaping, double quoting, and are
    -- whitespace separated (some implementations use newline, others any
    -- whitespace character). Therefore, escape any backslashes, newlines, and
    -- double quotes in the argument, and surround the content with double
    -- quotes.
    --
    -- Another possibility that could be considered would be to convert
    -- backslashes in the argument to forward slashes. This would generally do
    -- the right thing, since backslashes in general only appear in arguments
    -- as part of file paths on Windows, and the forward slash is accepted for
    -- those. However, escaping is more reliable, in case somehow a backslash
    -- appears in a non-file.
    escape x = concat
        [ "\""
        , concatMap
            (\c ->
                case c of
                    '\\' -> "\\\\"
                    '\n' -> "\\n"
                    '\"' -> "\\\""
                    _    -> [c])
            x
        , "\""
        ]

runSomethingFiltered
  :: DynFlags -> (String->String) -> String -> String -> [Option]
  -> Maybe FilePath -> Maybe [(String,String)] -> IO ()

runSomethingFiltered dflags filter_fn phase_name pgm args mb_cwd mb_env = do
    runSomethingWith dflags phase_name pgm args $ \real_args -> do
        r <- builderMainLoop dflags filter_fn pgm real_args mb_cwd mb_env
        return (r,())

runSomethingWith
  :: DynFlags -> String -> String -> [Option]
  -> ([String] -> IO (ExitCode, a))
  -> IO a

runSomethingWith dflags phase_name pgm args io = do
  let real_args = filter notNull (map showOpt args)
      cmdLine = showCommandForUser pgm real_args
  traceCmd dflags phase_name cmdLine $ handleProc pgm phase_name $ io real_args

handleProc :: String -> String -> IO (ExitCode, r) -> IO r
handleProc pgm phase_name proc = do
    (rc, r) <- proc `catchIO` handler
    case rc of
      ExitSuccess{} -> return r
      ExitFailure n -> throwGhcExceptionIO (
            ProgramError ("`" ++ takeFileName pgm ++ "'" ++
                          " failed in phase `" ++ phase_name ++ "'." ++
                          " (Exit code: " ++ show n ++ ")"))
  where
    handler err =
       if IO.isDoesNotExistError err
          then does_not_exist
          else throwGhcExceptionIO (ProgramError $ show err)

    does_not_exist = throwGhcExceptionIO (InstallationError ("could not execute: " ++ pgm))


builderMainLoop :: DynFlags -> (String -> String) -> FilePath
                -> [String] -> Maybe FilePath -> Maybe [(String, String)]
                -> IO ExitCode
builderMainLoop dflags filter_fn pgm real_args mb_cwd mb_env = do
  chan <- newChan

  -- We use a mask here rather than a bracket because we want
  -- to distinguish between cleaning up with and without an
  -- exception. This is to avoid calling terminateProcess
  -- unless an exception was raised.
  let safely inner = mask $ \restore -> do
        -- acquire
        -- On Windows due to how exec is emulated the old process will exit and
        -- a new process will be created. This means waiting for termination of
        -- the parent process will get you in a race condition as the child may
        -- not have finished yet.  This caused #16450.  To fix this use a
        -- process job to track all child processes and wait for each one to
        -- finish.
        let procdata =
              enableProcessJobs
              $ (proc pgm real_args) { cwd = mb_cwd
                                     , env = mb_env
                                     , std_in  = CreatePipe
                                     , std_out = CreatePipe
                                     , std_err = CreatePipe
                                     }
        (Just hStdIn, Just hStdOut, Just hStdErr, hProcess) <- restore $
          createProcess_ "builderMainLoop" procdata
        let cleanup_handles = do
              hClose hStdIn
              hClose hStdOut
              hClose hStdErr
        r <- try $ restore $ do
          hSetBuffering hStdOut LineBuffering
          hSetBuffering hStdErr LineBuffering
          let make_reader_proc h = forkIO $ readerProc chan h filter_fn
          bracketOnError (make_reader_proc hStdOut) killThread $ \_ ->
            bracketOnError (make_reader_proc hStdErr) killThread $ \_ ->
            inner hProcess
        case r of
          -- onException
          Left (SomeException e) -> do
            terminateProcess hProcess
            cleanup_handles
            throw e
          -- cleanup when there was no exception
          Right s -> do
            cleanup_handles
            return s
  safely $ \h -> do
    -- we don't want to finish until 2 streams have been complete
    -- (stdout and stderr)
    log_loop chan (2 :: Integer)
    -- after that, we wait for the process to finish and return the exit code.
    waitForProcess h
  where
    -- t starts at the number of streams we're listening to (2) decrements each
    -- time a reader process sends EOF. We are safe from looping forever if a
    -- reader thread dies, because they send EOF in a finally handler.
    log_loop _ 0 = return ()
    log_loop chan t = do
      msg <- readChan chan
      case msg of
        BuildMsg msg -> do
          putLogMsg dflags NoReason SevInfo noSrcSpan
              (defaultUserStyle dflags) msg
          log_loop chan t
        BuildError loc msg -> do
          putLogMsg dflags NoReason SevError (mkSrcSpan loc loc)
              (defaultUserStyle dflags) msg
          log_loop chan t
        EOF ->
          log_loop chan  (t-1)

readerProc :: Chan BuildMessage -> Handle -> (String -> String) -> IO ()
readerProc chan hdl filter_fn =
    (do str <- hGetContents hdl
        loop (linesPlatform (filter_fn str)) Nothing)
    `finally`
       writeChan chan EOF
        -- ToDo: check errors more carefully
        -- ToDo: in the future, the filter should be implemented as
        -- a stream transformer.
    where
        loop []     Nothing    = return ()
        loop []     (Just err) = writeChan chan err
        loop (l:ls) in_err     =
                case in_err of
                  Just err@(BuildError srcLoc msg)
                    | leading_whitespace l -> do
                        loop ls (Just (BuildError srcLoc (msg $$ text l)))
                    | otherwise -> do
                        writeChan chan err
                        checkError l ls
                  Nothing -> do
                        checkError l ls
                  _ -> panic "readerProc/loop"

        checkError l ls
           = case parseError l of
                Nothing -> do
                    writeChan chan (BuildMsg (text l))
                    loop ls Nothing
                Just (file, lineNum, colNum, msg) -> do
                    let srcLoc = mkSrcLoc (mkFastString file) lineNum colNum
                    loop ls (Just (BuildError srcLoc (text msg)))

        leading_whitespace []    = False
        leading_whitespace (x:_) = isSpace x

parseError :: String -> Maybe (String, Int, Int, String)
parseError s0 = case breakColon s0 of
                Just (filename, s1) ->
                    case breakIntColon s1 of
                    Just (lineNum, s2) ->
                        case breakIntColon s2 of
                        Just (columnNum, s3) ->
                            Just (filename, lineNum, columnNum, s3)
                        Nothing ->
                            Just (filename, lineNum, 0, s2)
                    Nothing -> Nothing
                Nothing -> Nothing

breakColon :: String -> Maybe (String, String)
breakColon xs = case break (':' ==) xs of
                    (ys, _:zs) -> Just (ys, zs)
                    _ -> Nothing

breakIntColon :: String -> Maybe (Int, String)
breakIntColon xs = case break (':' ==) xs of
                       (ys, _:zs)
                        | not (null ys) && all isAscii ys && all isDigit ys ->
                           Just (read ys, zs)
                       _ -> Nothing

data BuildMessage
  = BuildMsg   !SDoc
  | BuildError !SrcLoc !SDoc
  | EOF

-- Divvy up text stream into lines, taking platform dependent
-- line termination into account.
linesPlatform :: String -> [String]
#if !defined(mingw32_HOST_OS)
linesPlatform ls = lines ls
#else
linesPlatform "" = []
linesPlatform xs =
  case lineBreak xs of
    (as,xs1) -> as : linesPlatform xs1
  where
   lineBreak "" = ("","")
   lineBreak ('\r':'\n':xs) = ([],xs)
   lineBreak ('\n':xs) = ([],xs)
   lineBreak (x:xs) = let (as,bs) = lineBreak xs in (x:as,bs)

#endif
