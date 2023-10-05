import Control.Concurrent
import Control.Exception

main :: IO ()
main = do
    mask $ \unmask -> mask $ \restore ->
      unmask $ restore $ getMaskingState >>= print
    uninterruptibleMask $ \unmask -> uninterruptibleMask $ \restore ->
      unmask $ restore $ getMaskingState >>= print

    mv <- newEmptyMVar
    mask_ $ -- start with exceptions masked
      mask $ \restore -> forkIOWithUnmask $ \unmask -> unmask $
        restore $ getMaskingState >>= print >> putMVar mv ()
    takeMVar mv
    uninterruptibleMask_ $ -- start with exceptions uninterruptibly masked
      uninterruptibleMask $ \restore -> forkIOWithUnmask $ \unmask -> unmask $
        restore $ getMaskingState >>= print >> putMVar mv ()
    takeMVar mv
