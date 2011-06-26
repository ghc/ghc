
import Control.Concurrent
import Control.Monad

main :: IO ()
main = do t <- forkIO (x >> x)
          threadDelay 1000000
          killThread t
          putStrLn "Done"

x :: IO ()
x = forever yield

