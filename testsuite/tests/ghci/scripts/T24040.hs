module T24040 where
import Control.Concurrent(threadDelay)
import System.IO(hPutStrLn, stderr)
delayNSeconds :: Int -> IO ()
delayNSeconds n =  threadDelay (n * 1000000) >> hPutStrLn stderr ("Finished in: " ++ show n ++ " seconds")