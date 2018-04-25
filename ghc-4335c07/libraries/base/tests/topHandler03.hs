import System.Posix.Signals
import System.Exit
import Data.Bits

-- Test that a ExitFailure representing SIGTERM causes
-- the process to terminate by killing itself with SIGTERM

main = exitWith (ExitFailure (fromIntegral (-sigTERM)))
