import Posix
import System(ExitCode(..), exitWith)

main = 
    forkProcess >>= \ maybe_pid ->
    case maybe_pid of
	Nothing -> raiseSignal floatingPointException
	_ -> doParent

doParent =
    getAnyProcessStatus True False >>= \ (Just (pid, tc)) ->
    case tc of
	Terminated sig | sig == floatingPointException -> forkChild2
	_ -> fail (userError "unexpected termination cause")

forkChild2 =
    forkProcess >>= \ maybe_pid ->
    case maybe_pid of
	Nothing -> exitImmediately (ExitFailure 42)
	_ -> doParent2
    
doParent2 =
    getAnyProcessStatus True False >>= \ (Just (pid, tc)) ->
    case tc of
	Exited (ExitFailure 42) -> forkChild3
	_ -> fail (userError "unexpected termination cause (2)")
	    
forkChild3 =
    forkProcess >>= \ maybe_pid ->
    case maybe_pid of
	Nothing -> exitImmediately (ExitSuccess)
	_ -> doParent3
    
doParent3 =
    getAnyProcessStatus True False >>= \ (Just (pid, tc)) ->
    case tc of
	Exited ExitSuccess -> forkChild4
	_ -> fail (userError "unexpected termination cause (3)")
	    
forkChild4 =
    forkProcess >>= \ maybe_pid ->
    case maybe_pid of
	Nothing -> raiseSignal softwareStop
	_ -> doParent4
    
doParent4 =
    getAnyProcessStatus True True >>= \ (Just (pid, tc)) ->
    case tc of
	Stopped sig | sig == softwareStop -> enoughAlready pid
	_ -> fail (userError "unexpected termination cause (4)")
	    
enoughAlready pid =
    signalProcess killProcess pid >>
    getAnyProcessStatus True True >>= \ (Just (pid, tc)) ->
    case tc of
	Terminated sig | sig == killProcess -> putStr "I'm happy.\n"
	_ -> fail (userError "unexpected termination cause (5)")
    
