
import System.Exit (ExitCode(..), exitWith)
import System.Posix.Process
import System.Posix.Signals

main = do test1
          test2
          test3
          test4
          putStrLn "I'm happy."

test1 = do
    -- Force SIGFPE exceptions to not be ignored.  Under some
    -- circumstances this test will be run with SIGFPE
    -- ignored, see #7399
    installHandler sigFPE Default Nothing
    forkProcess $ raiseSignal floatingPointException
    Just (pid, tc) <- getAnyProcessStatus True False
    case tc of
        Terminated sig _ | sig == floatingPointException -> return ()
        _ -> error "unexpected termination cause"

test2 = do
    forkProcess $ exitImmediately (ExitFailure 42)
    Just (pid, tc) <- getAnyProcessStatus True False
    case tc of
        Exited (ExitFailure 42) -> return ()
        _ -> error "unexpected termination cause (2)"

test3 = do
    forkProcess $ exitImmediately ExitSuccess
    Just (pid, tc) <- getAnyProcessStatus True False
    case tc of
        Exited ExitSuccess -> return ()
        _ -> error "unexpected termination cause (3)"

test4 = do
    forkProcess $ raiseSignal softwareStop
    Just (pid, tc) <- getAnyProcessStatus True True
    case tc of
        Stopped sig | sig == softwareStop -> do
            signalProcess killProcess pid
            Just (pid, tc) <- getAnyProcessStatus True True
            case tc of
                Terminated sig _ | sig == killProcess -> return ()
                _ -> error "unexpected termination cause (5)"
        _ -> error "unexpected termination cause (4)"

