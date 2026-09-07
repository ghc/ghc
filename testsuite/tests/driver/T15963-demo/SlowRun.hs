import Control.Concurrent (threadDelay)

main :: IO ()
main = putStrLn "started" >> threadDelay 60000000
