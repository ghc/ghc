import Control.Exception
import Control.Concurrent
import Text.Printf

-- Test combinations of nesting mask/uninterruptibleMask with
-- forkIO/forkIOWithUnmask

main = do
  m <- newEmptyMVar
  t1 <- mask_ $ forkIO $ do
          takeMVar m `catch` \e -> do stat 1 MaskedInterruptible
                                      print (e::SomeExceptionWithLocation)
                                      throwIO e
  killThread t1
  t2 <- uninterruptibleMask_ $ forkIO $ do
          takeMVar m `catch` \e -> do stat 2 MaskedUninterruptible
                                      print (e::SomeExceptionWithLocation)
                                      throwIO e
  killThread t2
  t3 <- mask_ $ forkIOWithUnmask $ \unmask ->
            unmask $ do stat 3 Unmasked; putMVar m ()
  takeMVar m
  t4 <- uninterruptibleMask_ $ forkIOWithUnmask $ \unmask ->
            unmask $ do stat 4 Unmasked; putMVar m ()
  takeMVar m

stat :: Int -> MaskingState -> IO ()
stat n m = do
 s <- getMaskingState
 if (s /= m)
    then error (printf "%2d: %s\n" n (show s))
    else return ()
