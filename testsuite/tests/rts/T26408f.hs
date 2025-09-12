import Control.Concurrent
import Foreign.C
import System.Mem

-- | Test to make sure that we detect a system deadlock even when idle GC is
-- disabled and the system has been idle long enough for the timer tick to be
-- turned off. And this should not get confused by the user doing extra GCs
-- manually. The manual GCs are relevant because idle GC can pause timers
-- after a successful major GC when the process was already idle.

main :: IO ()
main = do
  m <- newEmptyMVar
  _ <- c_sleep 1      -- longer than the 300ms idle GC timeout
  performMajorGC      -- confuse matters by doing an extra GC first
  takeMVar m          -- should detect system deadlock here

foreign import ccall unsafe "unistd.h sleep" c_sleep :: CUInt -> IO CUInt

