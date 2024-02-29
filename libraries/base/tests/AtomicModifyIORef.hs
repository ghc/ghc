import Control.Concurrent
import Control.Monad
import Data.IORef

main :: IO ()
main = do
    let nThreads = 10
        nIncrs = 10000

    ref <- newIORef (42 :: Int)
    dones <- replicateM nThreads $ do
        done <- newEmptyMVar
        forkIO $ do
           replicateM_ nIncrs $ atomicModifyIORef' ref $ \old -> (old + 1, ())
           putMVar done ()
        putStrLn "."
        return done

    mapM_ takeMVar dones
    readIORef ref >>= print

