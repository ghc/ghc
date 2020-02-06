import Control.Concurrent.MVar  (readMVar)
import System.Environment       (getArgs)
import System.Exit              (ExitCode (ExitFailure), exitFailure)
import System.IO                (hClose, hGetLine, hPutStrLn)
import System.Posix.Process     (exitImmediately, getProcessID)
import System.Posix.Signals     (Handler (Catch), installHandler, sigHUP,
                                 signalProcess)
import System.Process           (StdStream (CreatePipe), createProcess, proc,
                                 std_in, std_out, waitForProcess)
import System.Process.Internals (ProcessHandle (..),
                                 ProcessHandle__ (OpenHandle))
import System.Timeout           (timeout)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--runghc", runghc] -> runParent runghc
        ["child"] -> runChild
        _ -> error $ "Unknown args: " ++ show args

runParent :: FilePath -> IO ()
runParent runghc = do
    (Just inH, Just outH, Nothing, ph@(ProcessHandle mvar _ _)) <-
        createProcess (proc runghc ["T-signals-child.hs", "child"])
            { std_in = CreatePipe
            , std_out = CreatePipe
            }

    -- Get the PID of the actual child process. This will initially be
    -- runghc. If executeFile is used by runghc, that same process
    -- will become the ghc process running our code from
    -- runChild. Otherwise, runChild will run in a child of this
    -- process.
    OpenHandle childPid <- readMVar mvar

    -- Get the PID of the process actually running the runChild code,
    -- by reading it from its stdout (see runChild below).
    pidS <- hGetLine outH
    let pid = fromIntegral (read pidS :: Int)

    -- Send the child process the HUP signal. We know this is after
    -- the signal handler has been installed, since we already got the
    -- PID from the process.
    signalProcess sigHUP childPid

    -- Send the child some input so that it will exit if it didn't
    -- have a sigHUP handler installed.
    hPutStrLn inH ""
    hClose inH

    -- Read out the rest of stdout from the child, which will be
    -- either "NOSIGNAL\n" or "HUP\n"
    rest <- hGetLine outH

    -- Get the exit code of the child
    ec <- waitForProcess ph

    -- Check that everything matches
    if childPid /= pid || rest /= hupMessage || ec /= hupExitCode
        then do
            -- Debugging display
            putStrLn $ concat
                [ "Child process: "
                , show childPid
                , ", real process: "
                , show pid
                ]
            putStrLn $ concat
                [ "Expected "
                , show hupMessage
                , ", received: "
                , show rest
                ]
            putStrLn $ concat
                [ "Expected "
                , show hupExitCode
                , ", received "
                , show ec
                ]
            exitFailure
        else return ()

runChild :: IO ()
runChild = do
    -- Install our sigHUP handler: print the HUP message and exit with
    -- the HUP exit code.
    let handler = Catch $ do
            putStrLn hupMessage
            exitImmediately hupExitCode
    _ <- installHandler sigHUP handler Nothing

    -- Get our actual process ID and print it to stdout.
    pid <- getProcessID
    print (fromIntegral pid :: Int)

    -- Block until we receive input, giving a chance for the signal
    -- handler to be triggered, and if the signal handler isn't
    -- triggered, gives us an escape route from this function.
    --
    -- Include a reasonable timeout to prevent this from running for
    -- too long
    _ <- timeout 10000000 getLine

    -- Reaching this point indicates a failure of the test. Print some
    -- non HUP message and exit with a non HUP exit
    -- code. Interestingly, in a failure, this exit code will _not_
    -- be received by the parent process, since the runghc process
    -- itself will exit with ExitFailure -1, indicating that it was
    -- killed by signal 1 (SIGHUP).
    putStrLn "No signal received"
    exitImmediately $ ExitFailure 41

hupExitCode :: ExitCode
hupExitCode = ExitFailure 42

hupMessage :: String
hupMessage = "HUP"
