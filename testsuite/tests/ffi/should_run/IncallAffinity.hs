module Lib (capTest) where

import Control.Concurrent
import Control.Exception
import Control.Concurrent.MVar
import Control.Monad (when)
import System.Exit

foreign export ccall "capTest" capTest :: IO Int

capTest :: IO Int
capTest = catch go handle
  where
    handle :: SomeExceptionWithLocation -> IO Int
    handle e = do
      putStrLn $ "Failed " ++ (show e)
      return (-1)
    getCap = fmap fst $ threadCapability =<< myThreadId
    go = do
      when (not rtsSupportsBoundThreads) $
        die "This test requires -threaded"
      mvar <- newEmptyMVar
      mvar2 <- newEmptyMVar
      (cap, locked) <- threadCapability =<< myThreadId
      forkOn cap $ do
        putMVar mvar =<< getCap
        takeMVar mvar2
      -- if cap is locked, then this would get scheduled on a different
      -- capacity.
      fCap <- takeMVar mvar
      putMVar mvar2 ()
      cap2 <- getCap
      when (fCap /= cap) (fail "expected cap to be the same")
      when (cap2 /= cap) (fail "expected cap to be the same when returning")
      when (not locked) (fail "expected to be locked")
      return cap
