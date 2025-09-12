import Control.Concurrent
import Foreign.C

-- | Test to make sure that we detect a system deadlock even when idle GC is
-- disabled and the system has been idle long enough for the timer tick to be
-- turned off.

main :: IO ()
main = do
  m <- newEmptyMVar
  _ <- c_sleep 1      -- longer than the 300ms idle GC timeout
  takeMVar m          -- should detect system deadlock here

foreign import ccall unsafe "unistd.h sleep" c_sleep :: CUInt -> IO CUInt

