import Control.Concurrent
import Control.Monad
import GHC.Clock
import System.IO
import System.Timeout

-- IMPORTANT: Re-run this test _manually_ on windows if/when you change
-- the code in `libraries/base/cbits/inputReady.c` that mentions
-- `FILE_TYPE_CHAR`. Only when you run the code manually, in cmd.exe
-- or PowerShell, does this code path get activated.
-- Running this code in mintty is not count.
main :: IO ()
main = do
    let nanoSecondsPerSecond = 1000 * 1000 * 1000
    let milliSecondsPerSecond = 1000
    let timeToSpend = 1
    let timeToSpendNano = timeToSpend * nanoSecondsPerSecond
    let timeToSpendMilli = timeToSpend * milliSecondsPerSecond
    start <- getMonotonicTimeNSec
    b <- hWaitForInput stdin timeToSpendMilli
    end <- getMonotonicTimeNSec
    let timeSpentNano = fromIntegral $ end - start
    let delta = timeSpentNano - timeToSpendNano
    -- We can never wait for a shorter amount of time than specified
    putStrLn $ "delta >= 0: " ++ show (delta >= 0)
