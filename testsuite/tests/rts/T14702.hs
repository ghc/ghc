module Main where

import Control.Monad
import Data.Array.IO.Safe
import Data.Word
import GHC.Stats
import System.Exit
import System.Mem

printAlloc :: String -> IO (Word64, Word64)
printAlloc name = do
    performGC
    details <- gc <$> getRTSStats
    let dat = (gcdetails_live_bytes details, gcdetails_mem_in_use_bytes details)
    putStrLn $ name ++ ": " ++ show dat
    pure dat

allocateAndPrint :: IO ()
allocateAndPrint = do
    -- allocate and touch a lot of memory (4MB * 260 ~ 1GB)
    memoryHog <- forM [1 .. 300] $ \_ ->
        (newArray (0, 1000000) 0 :: IO (IOUArray Word Word32))
    _ <- printAlloc "with large allocation"
    -- do something with memory to prevent it from being GC'ed until now
    forM_ memoryHog $ \a -> void $ readArray a 0

main :: IO ()
main = do
    (firstLive, firstTotal) <- printAlloc "initial"
    allocateAndPrint
    (lastLive, lastTotal) <- printAlloc "final"

    -- Now there is no reason to have more memory allocated than at start
    let ratio = fromIntegral lastTotal / fromIntegral firstTotal
    putStrLn $ "alloc ratio " ++ show ratio
    when (ratio > 1.5) $ exitFailure
