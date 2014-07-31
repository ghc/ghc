import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Foreign.StablePtr

main :: IO ()
main = do
  tv <- atomically $ newTVar True
  _ <- newStablePtr tv
  t <- mask_ $ forkIO (blockSTM tv)
  killThread t

blockSTM :: TVar Bool -> IO ()
blockSTM tv = do
  atomically $ do
    v <- readTVar tv
    check $ not v
