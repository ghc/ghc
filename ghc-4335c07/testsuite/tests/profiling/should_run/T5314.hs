import Control.Concurrent
import Control.Monad (forever)

main = do
    empty <- newEmptyMVar
    exit <- newEmptyMVar
    _ <- forkIO $ threadDelay 200000 >> putMVar exit ()
    _ <- forkIO $ takeMVar empty >> return ()
    _ <- forkIO $ print (sum [1..10000])
    takeMVar exit
