import Control.Concurrent
import Control.Concurrent.STM
import System.IO.Unsafe

var = unsafePerformIO $ atomically $ newTVar 3

main = do x <- atomically $ readTVar var; print x
