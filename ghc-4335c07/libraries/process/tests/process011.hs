import System.Process
import System.IO
import Control.Exception
import Control.Concurrent
import Data.List

-- Test control-C delegation (#2301)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  putStrLn "===================== test 1"

  -- shell kills itself with SIGINT,
  -- delegation off, exit code (death by signal) reported as normal
  do let script = intercalate "; "
                    [ "kill -INT $$"
                    , "exit 42" ]
     (_,_,_,p) <- createProcess (shell script) { delegate_ctlc = False }
     waitForProcess p >>= print

  putStrLn "===================== test 2"

  -- shell kills itself with SIGINT,
  -- delegation on, so expect to throw UserInterrupt
  do let script = intercalate "; "
                    [ "kill -INT $$"
                    , "exit 42" ]
     (_,_,_,p) <- createProcess (shell script) { delegate_ctlc = True }
     (waitForProcess p >>= print)
       `catchUserInterrupt` \e -> putStrLn $ "caught: " ++ show e

  putStrLn "===================== test 3"

  -- shell sends itself SIGINT but traps it,
  -- delegation on, but the shell terminates normally so just normal exit code
  do let script = intercalate "; "
                    [ "trap 'echo shell trapped SIGINT' INT"
                    , "kill -INT $$"
                    , "exit 42" ]
     (_,_,_,p) <- createProcess (shell script) { delegate_ctlc = True }
     waitForProcess p >>= print

  putStrLn "===================== test 4"

  -- shell sends us SIGINT.
  -- delegation on, so we should not get the SIGINT ourselves
  -- shell terminates normally so just normal exit code
  do let script = intercalate "; "
                    [ "kill -INT $PPID"
                    , "kill -INT $PPID"
                    , "exit 42" ]
     (_,_,_,p) <- createProcess (shell script) { delegate_ctlc = True }
     waitForProcess p >>= print

  putStrLn "===================== test 5"

  -- shell sends us SIGINT.
  -- delegation off, so we should get the SIGINT ourselves (async)
  do let script = intercalate "; "
                    [ "kill -INT $PPID"
                    , "exit 42" ]
     (_,_,_,p) <- createProcess (shell script) { delegate_ctlc = False }
     exit <- waitForProcess p
     -- need to allow for the async exception to arrive
     threadDelay 1000000
     -- we should never make it to here...
     putStrLn "never caught interrupt"
     print exit
   `catchUserInterrupt` \e -> putStrLn $ "caught: " ++ show e

  putStrLn "===================== done"

catchUserInterrupt :: IO a -> (AsyncException -> IO a) -> IO a
catchUserInterrupt =
  catchJust (\e -> case e of UserInterrupt -> Just e; _ -> Nothing)
