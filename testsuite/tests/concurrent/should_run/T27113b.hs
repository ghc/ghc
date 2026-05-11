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

myCall :: CLLong -> IO ()
myCall spinNs = mask_ $ do
    allowInterrupt
    c_cpu_then_pause spinNs
    allowInterrupt

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    started <- newEmptyMVar
    done <- newEmptyMVar
    tid <- mask_ $ forkIOWithUnmask $ \unmask ->
        flip finally (putMVar done ()) $ do
            putMVar started ()
            unmask (myCall 2_000_000_000)
                `catch` (\(_ :: SomeException) -> return ())
    takeMVar started
    threadDelay 200_000
    result <- timeout 5_000_000 (killThread tid >> takeMVar done)
    case result of
        Just () -> putStrLn "ok"
        Nothing -> do
            hPutStrLn stderr "FAIL: killThread hung"
            error "hung"
