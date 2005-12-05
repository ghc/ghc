-- !!! test threadDelay, Random, and QSemN.

-- Variation of conc023, testing STM timeouts instead of IO

import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe

var = unsafePerformIO $ newTVarIO 3

main = do x <- atomically $ readTVar var; print x
