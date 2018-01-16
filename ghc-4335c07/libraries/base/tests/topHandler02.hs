import Control.Exception
import Control.Concurrent

-- Test that a UserInterrupt exception that propagates to the top level
-- causes the process to terminate by killing itself with SIGINT

main = throwIO UserInterrupt
