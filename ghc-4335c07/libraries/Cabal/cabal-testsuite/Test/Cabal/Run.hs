{-# LANGUAGE NondecreasingIndentation #-}
-- | A module for running commands in a chatty way.
module Test.Cabal.Run (
    run,
    Result(..)
) where

import Distribution.Compat.CreatePipe (createPipe)
import Distribution.Simple.Program.Run
import Distribution.Verbosity

import Control.Concurrent.Async
import System.Process (runProcess, waitForProcess, showCommandForUser)
import System.IO
import System.Exit
import System.Directory
import System.FilePath

-- | The result of invoking the command line.
data Result = Result
    { resultExitCode    :: ExitCode
    , resultCommand     :: String
    , resultOutput      :: String
    } deriving Show

-- | Run a command, streaming its output to stdout, and return a 'Result'
-- with this information.
run :: Verbosity -> Maybe FilePath -> [(String, Maybe String)] -> FilePath -> [String] -> IO Result
run _verbosity mb_cwd env_overrides path0 args = do
    -- In our test runner, we allow a path to be relative to the
    -- current directory using the same heuristic as shells:
    -- 'foo' refers to an executable in the PATH, but './foo'
    -- and 'foo/bar' refer to relative files.
    --
    -- Unfortunately, we cannot just pass these relative paths directly:
    -- 'runProcess' resolves an executable path not with respect to the
    -- current working directory, but the working directory that the
    -- subprocess will execute in.  Thus, IF we have a relative
    -- path which is not a bare executable name, we have to tack on
    -- the CWD to make it resolve correctly
    cwd <- getCurrentDirectory
    let path | length (splitPath path0) /= 1 && isRelative path0
             = cwd </> path0
             | otherwise
             = path0

    mb_env <- getEffectiveEnvironment env_overrides
    putStrLn $ "+ " ++ showCommandForUser path args
    (readh, writeh) <- createPipe
    hSetBuffering readh LineBuffering
    hSetBuffering writeh LineBuffering
    let drain = do
            r <- hGetContents readh
            putStr r -- forces the output
            hClose readh
            return r
    withAsync drain $ \sync -> do

    -- NB: do NOT extend this to take stdin; then we will
    -- start deadlocking on AppVeyor.  See https://github.com/haskell/process/issues/76
    pid <- runProcess path args mb_cwd mb_env Nothing {- no stdin -}
                (Just writeh) (Just writeh)

    -- wait for the program to terminate
    exitcode <- waitForProcess pid
    out <- wait sync

    return Result {
            resultExitCode = exitcode,
            resultCommand = showCommandForUser path args,
            resultOutput = out
        }
