import Control.Exception
import Control.Concurrent

main = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  t <- forkIO $ do
    mask_ $ return ()
    throwIO (ErrorCall "test") `catch`
       \e -> do
         let _ = e::SomeExceptionWithLocation
         print =<< getMaskingState
         putMVar m1 ()
         takeMVar m2
  takeMVar m1
  killThread t
   -- in GHC 7.2 and earlier this call will deadlock due to bug #4988.
   -- However, the RTS will resurrect the child thread, and in doing
   -- so will unblock the main thread, so the main thread doesn't get
   -- a BlockedIndefinitely exception.
