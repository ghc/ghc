import Control.Concurrent
import System.Environment
import System.Process
import System.Exit

main = do
  args <- getArgs
  case args of 
    [secs,cmd] -> do
	p <- runCommand cmd
	m <- newEmptyMVar
	forkIO (do threadDelay (read secs * 1000000)
		   putMVar m Nothing
	       )
	forkIO (do r <- waitForProcess p
		   putMVar m (Just r))
	r <- takeMVar m
	case r of
	  Nothing -> do
		terminateProcess p
		exitWith (ExitFailure 99)	
	  Just r -> do
		exitWith r
    _other -> exitWith (ExitFailure 1)

