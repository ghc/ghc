import Control.Concurrent
import Control.Monad
import GHC.Clock
import System.IO
import System.Posix.IO
import System.Timeout

main :: IO ()
main = do
    (readPipe, _) <- createPipe
    readPipeHandle <- fdToHandle readPipe
    let nanoSecondsPerSecond = 1000 * 1000 * 1000
    let milliSecondsPerSecond = 1000
    let timeToSpend = 1
    let timeToSpendNano = timeToSpend * nanoSecondsPerSecond
    let timeToSpendMilli = timeToSpend * milliSecondsPerSecond
    start <- getMonotonicTimeNSec
    b <- hWaitForInput readPipeHandle timeToSpendMilli
    end <- getMonotonicTimeNSec
    let timeSpentNano = fromIntegral $ end - start
    let delta = timeSpentNano - timeToSpendNano
    -- We can never wait for a shorter amount of time than specified
    putStrLn $ "delta >= 0: " ++ show (delta > 0)
