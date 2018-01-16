-- Test that we can call exitFailure in a forked process, and have it
-- communicated properly to the parent.
import System.Exit
import System.Posix.Process
main = do
  p <- forkProcess $ exitWith (ExitFailure 72)
  r <- getProcessStatus True False p
  print r

