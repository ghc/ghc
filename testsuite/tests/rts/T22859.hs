{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.Exception
import Control.Exception.Backtrace
import Control.Concurrent
import Control.Concurrent.MVar
import System.Mem
import System.Mem.Experimental
import GHC.IO (IO (..))
import GHC.Exts
import System.IO

-- | Just do some work and hPutStrLn to stderr to indicate that we are making progress
worker :: IO ()
worker = loop [] 2
  where
    loop !m !n
      | n > 30 = hPutStrLn stderr . show $ length m
      | otherwise = do
          let x = show n
          hPutStrLn stderr x
          -- just to bulk out the allocations
          IO (\s -> case newByteArray# 900000# s of (# s', arr# #) -> (# s', () #))
          yield
          loop (x:m) (n + 1)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering -- necessary for Windows, otherwise our output gets garbled
  done <- newMVar () -- we use this lock to wait for the worker to finish
  started <- newEmptyMVar
  let runWorker = do
        forkIO . withMVar done $ \_ -> flip onException (hPutStrLn stderr "worker died") $ do
          hPutStrLn stderr "worker starting"
          putMVar started ()
          setAllocationCounter 1_000_000
          enableAllocationLimit
          worker
          hPutStrLn stderr "worker done"
        takeMVar started
        readMVar done
        hFlush stderr
        threadDelay 1000
  -- default behaviour:
  -- kill it after the limit is exceeded
  hPutStrLn stderr "default behaviour"
  runWorker
  hPutStrLn stderr "just log once on the hook being triggered"
  setGlobalAllocationLimitHandler DontKillOnAllocationLimit (Just $ \_ -> hPutStrLn stderr "allocation limit triggered 1")
  runWorker
  hPutStrLn stderr "just log on the hook being triggered"
  setGlobalAllocationLimitHandler DontKillOnAllocationLimit . Just $ \tid -> do
    hPutStrLn stderr "allocation limit triggered 2"
    -- re-enable the hook
    setAllocationCounterFor 1_000_000 tid
    enableAllocationLimitFor tid
  runWorker
  hPutStrLn stderr "kill from the hook"
  setGlobalAllocationLimitHandler DontKillOnAllocationLimit . Just $ \tId -> throwTo tId AllocationLimitExceeded
  runWorker
  -- not super helpful, but let's test it anyway
  hPutStrLn stderr "do nothing"
  setGlobalAllocationLimitHandler DontKillOnAllocationLimit Nothing
  runWorker
  -- this is possible to handle using an exception handler instead.
  hPutStrLn stderr "kill and log"
  setGlobalAllocationLimitHandler KillOnAllocationLimit (Just $ \_ -> hPutStrLn stderr "allocation limit triggered 3")
  runWorker
  threadDelay 1000
  hPutStrLn stderr "done"
