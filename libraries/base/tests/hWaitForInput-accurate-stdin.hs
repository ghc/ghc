{-# LANGUAGE CPP #-}

import Control.Concurrent
import Control.Monad
import GHC.Clock
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Timeout

-- IMPORTANT: Re-run this test _manually_ on windows if/when you change
-- the code in `libraries/base/cbits/inputReady.c` that mentions
-- `FILE_TYPE_CHAR`. Only when you run the code manually, in cmd.exe
-- or PowerShell, does this code path get activated.
-- Running this code in mintty does not count.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            let cp =
                    (shell
                         ((if isLinuxHost
                               then ("./" ++)
                               else id)
                              "hWaitForInput-accurate-stdin --read-from-stdin"))
                        {std_in = CreatePipe}
            (_, _, _, ph) <- createProcess cp
            waitForProcess ph >>= exitWith
        ("--read-from-stdin":_) -> do
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
        _ -> error "should not happen."

isLinuxHost :: Bool
#if defined(mingw32_HOST_OS)
isLinuxHost = False
#else
isLinuxHost = True
#endif
