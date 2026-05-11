{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
import Foreign.C.Types
import System.IO
import System.Timeout

foreign import ccall interruptible "cpu_then_pause"
    c_cpu_then_pause :: CLLong -> IO ()

killDelay :: Int
killDelay = 200_000

joinTimeout :: Int
joinTimeout = 3_000_000

runCase :: String -> CLLong -> IO ()
runCase name spinNs = do
    tid <- forkIO $
        c_cpu_then_pause spinNs
            `catch` (\(_ :: SomeException) -> return ())
    threadDelay killDelay
    result <- timeout joinTimeout (killThread tid)
    let killed = case result of { Just () -> True; Nothing -> False }
    putStrLn $ name ++ ": " ++ if killed then "killed" else "hung"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    runCase "short-spin" 0
    runCase "long-spin"  2_000_000_000
