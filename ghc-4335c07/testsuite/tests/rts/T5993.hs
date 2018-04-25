import Control.Concurrent
main = do
  m <- newEmptyMVar
  forkIO $ putStrLn "Hello World!" >> putMVar m ()
  takeMVar m

