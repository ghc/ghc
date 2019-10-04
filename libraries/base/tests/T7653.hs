
import Control.Monad;
import Control.Concurrent

main :: IO ()
main = replicateM_ 100000 (forkIO (threadDelay 1))

