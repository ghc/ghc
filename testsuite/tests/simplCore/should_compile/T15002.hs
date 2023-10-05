module T15002 where

import Control.Concurrent.MVar (MVar, modifyMVar_, putMVar)
import Data.Foldable (for_)

broadcastThen :: Either [MVar a] a -> MVar (Either [MVar a] a) -> a -> IO ()
broadcastThen finalState mv x =
    modifyMVar_ mv $ \mx -> do
      case mx of
        Left ls -> do for_ ls (`putMVar` x)
                      return finalState
        Right _ -> return finalState
