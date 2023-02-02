import System.Posix.Process
import System.Posix.Signals
import Control.Exception
import Control.Concurrent

-- Test that a simulated ^C sends an async UserInterrupt
-- exception to the main thread.

main = handle userInterrupt $ do
  us <- getProcessID
  signalProcess sigINT us
  threadDelay 1000000
  putStrLn "Fail: never received  exception"

userInterrupt UserInterrupt = putStrLn "Success: caught UserInterrupt"
userInterrupt e             = putStrLn "Fail: caught unexpected exception"
