{-# OPTIONS_GHC -fno-full-laziness -fno-worker-wrapper #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}

-- This test checks that the auto-apply code (stg_ap_0_fast, stg_ap_p) is robust
-- against another thread or the GC evaluating a closure at the same time.

module Main
    -- (main)
where

import Control.Monad
import Control.Concurrent
import System.IO
import GHC.Data.SmallArray
import GHC.Exts
import GHC.IO

type Arr = SmallMutableArray RealWorld (Int->Int)

io :: (State# RealWorld -> (# State# RealWorld, a #)) -> IO a
io f = IO f

io_ :: (State# RealWorld -> State# RealWorld ) -> IO ()
io_ f = IO (\s -> case f s of s2 -> (# s2, () #))

{-# NOINLINE readSmallArray #-}
readSmallArray (SmallMutableArray arr) (I# idx) = IO $ \s -> case readSmallArray# arr idx s of
    (# s2, r #) -> (# s2, r #)

-- Continually overwrites the array with unevaluated thunks that will evaluated to
-- a PAP under profiling.
{-# NOINLINE mkThunks #-}
mkThunks :: Arr -> IO ()
mkThunks arr = do
    forever $ do
      yield
      forM_ [0..100] $ \_j -> do
        forM_ [0..5 :: Int] $ \i -> do
            -- With profiling results in a thunk that will evaluate to a PAP capturing the SCC
            let g = {-# SCC g #-} succ
            io_ (writeSmallArray arr i g)

-- Evaluate the array repeatedly in the given order.
{-# NOINLINE evaluateThunks #-}
evaluateThunks :: Arr -> [Int] -> IO ()
evaluateThunks arr idxs = do
    forever $ do
        yield
        -- putStr "." >> hFlush stdout
        forM [0..5000::Int] $ \j -> do
            forM_ idxs $ \i -> do
                !g <- readSmallArray arr i
                seq (g i) (pure ())

main :: IO ()
main = do
    -- We spawn three threads. Two are evaluating the thunks in the array in opposite directions
    -- One thread is
    arr <- io (newSmallArray 6 (id))
    _ <- forkIO $ do
        evaluateThunks arr [0..5]
    _ <- forkIO $ do
        evaluateThunks arr [5,4..0]
    forkIO $ mkThunks arr
    threadDelay 10_000_000
