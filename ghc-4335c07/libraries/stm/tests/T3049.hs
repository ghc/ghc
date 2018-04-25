import Control.Concurrent.STM

main = do
  x <- atomically $ do
    v <- newTVar 0
    always $ return True -- remove this line and all is fine
    return v
  atomically (readTVar x) >>= print
