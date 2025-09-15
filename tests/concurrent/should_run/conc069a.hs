import Control.Concurrent
import Control.Exception

main = do
  -- stat -- main thread is not bound in GHCi
  m <- newEmptyMVar
  forkIO (do stat; putMVar m ())
  takeMVar m
  mask_ $ forkIO (do stat; putMVar m ())
  takeMVar m
  forkOS (do stat; putMVar m ())
  takeMVar m
  mask_ $ forkOS (do stat; putMVar m ())
  takeMVar m

stat = do
  x <- isCurrentThreadBound
  y <- getMaskingState
  print (x,y)
