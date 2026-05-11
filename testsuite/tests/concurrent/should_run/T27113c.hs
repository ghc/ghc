{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import Foreign.C.Types
import System.Exit
import System.IO

foreign import ccall interruptible "pause" c_pause :: IO CInt

postRevokeWindow :: Int
postRevokeWindow = 2_000_000

postRevokeThreshold :: Int
postRevokeThreshold = 5

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    iters <- newIORef (0 :: Int)

    workerStarted <- newEmptyMVar
    worker <- forkIO $ mask_ $ do
        putMVar workerStarted ()
        (forever $ do
            atomicModifyIORef' iters (\n -> (n + 1, ()))
            _ <- c_pause
            return ())
          `catch` (\(_ :: SomeException) -> return ())

    takeMVar workerStarted
    threadDelay 100_000

    killer1Ready <- newEmptyMVar
    killer1 <- forkIO $ do
        putMVar killer1Ready ()
        killThread worker

    takeMVar killer1Ready
    threadDelay 100_000

    preRevoke <- readIORef iters

    killThread killer1

    threadDelay postRevokeWindow

    postRevoke <- readIORef iters
    let storm = postRevoke - preRevoke
    if storm > postRevokeThreshold
        then do
            hPutStrLn stderr ("FAIL: post-revoke EINTR storm, iterations="
                              ++ show storm
                              ++ " (pre=" ++ show preRevoke
                              ++ " post=" ++ show postRevoke ++ ")")
            exitFailure
        else
            putStrLn ("ok (post-revoke iterations=" ++ show storm ++ ")")
