import Control.Concurrent
import Control.Monad
import GHC.Clock
import System.IO
import System.Timeout

ack :: Integer -> Integer -> Integer
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) (ack m (n - 1))

main :: IO ()
main = do
    let microsecondsPerSecond = 1000 * 1000
    let timeToSpend = 1 * microsecondsPerSecond -- One second in microseconds
    start <- getMonotonicTimeNSec
    timeout timeToSpend $
        -- Something that is guaranteed not to be done in 'timeToSpend'
        print $ ack 4 2
    end <- getMonotonicTimeNSec
    let timeSpentNano = fromIntegral $ end - start -- in nanoseconds
    let nanosecondsPerMicrosecond = 1000
    let timeToSpendNano = timeToSpend * nanosecondsPerMicrosecond
    let legRoom = 1 * 1000 * nanosecondsPerMicrosecond -- Nanoseconds
    let delta = timeSpentNano - timeToSpendNano
    -- We can never wait for a shorter amount of time than specified
    putStrLn $ "delta > 0: " ++ show (delta > 0)
    putStrLn $ "delta < legroom: " ++ show (delta < legRoom)
