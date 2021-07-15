-- | Some example IO computations where we do /not/ want to be lazy in the
-- continuation.
module T13380g where

import Data.IORef

testStr :: Int -> IO () -> IO Int
testStr x m = do
  m
  x `seq` return x
{-# INLINE testStr #-}

test_readIORef :: Int -> IORef () -> IO Int
test_readIORef x ref = testStr x (readIORef ref)

test_writeIORef :: Int -> IORef () -> IO Int
test_writeIORef x ref = testStr x (writeIORef ref ())

-- feel free to add more
