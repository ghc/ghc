import Control.Concurrent
import System.IO
import System.Timeout

main :: IO ()
main = do
    forkIO $ do
        threadDelay (5 * 1000000)
        -- The timeout should terminate before we ever make it here
        putStrLn "t=5 seconds: we shouldn't be here"

    timeout (1 * 1000000) $ do
        hWaitForInput stdin (10 * 1000)
        putStrLn "we shouldn't be here"

    return ()
