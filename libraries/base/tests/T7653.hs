
import Control.Monad;
import Control.Concurrent

main :: IO ()
main = replicateM_ 1000000 (forkIO (threadDelay 1))

