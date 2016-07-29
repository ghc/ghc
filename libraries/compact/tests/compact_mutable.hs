import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Data.Compact

data HiddenMVar = HiddenMVar (MVar ())

instance NFData HiddenMVar where
  rnf x = x `seq` () -- ignore the function inside

main = do
  m <- newEmptyMVar
  compact (HiddenMVar m)
