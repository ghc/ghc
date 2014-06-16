
import Control.Concurrent

main = do
  m <- newEmptyMVar
  end <- newEmptyMVar
  forkIO (sequence_ [ putMVar m () | _ <- [1 .. 10000] ])
  forkIO (sequence_ [ takeMVar m   | _ <- [1 .. 10000] ] >> putMVar end ())
  takeMVar end
