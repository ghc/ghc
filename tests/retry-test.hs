import LwConc.RunQueue
import LwConc.MVar

main = do
  newSched
  mv <- newEmptyMVar
  fin <- newEmptyMVar
  let reader = do {readMVar mv; putMVar fin ()}
  let putter = putMVar mv ()
  let taker  = takeMVar mv
  forkIO reader
  yield
  forkIO putter
  forkIO taker
  takeMVar fin
  print "Main Done"
