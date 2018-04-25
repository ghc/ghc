import Control.Concurrent
import Control.Exception
import GHC.Compact

data HiddenMVar = HiddenMVar (MVar ())

main = do
  m <- newEmptyMVar
  compact (HiddenMVar m)
