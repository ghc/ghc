module T24040 where
import Control.Concurrent(threadDelay)

delayNSeconds :: Int -> IO ()
delayNSeconds n =  threadDelay (n * 1000000) >> putStrLn ("Finished in: " ++ show n ++ " seconds")