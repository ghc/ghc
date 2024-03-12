{- This test constructs a program that used to trigger an excessive amount of STM retries. -}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Conc
import Control.Concurrent (newMVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad
import Control.Concurrent.STM.TArray
import Data.Array.MArray
import Data.IORef


main :: IO ()
main =
    forM_ [2..40] $ \i -> do
        -- Run the test with an increasing number of tvars
        let tvar_count = i * 10
        -- print $ "Tvars: " ++ show tvar_count
        provokeLivelock tvar_count


-- Forks two threads running a STM transactions, both accessing the same tvars but in opposite order.
provokeLivelock :: Int -> IO ()
provokeLivelock n = do
    -- Use tvar array as a convenient way to bundle up n Tvars.
    tvarArray <- atomically $ do
        newListArray (0,n) [0.. fromIntegral n :: Integer] :: STM (TArray Int Integer)
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
    updateCount <- newIORef (0 :: Int)

    let useTvars :: [Int] -> Bool -> IO ()
        useTvars tvar_order use_writes = atomically $ do
            -- Walk the array once in the given order to add all tvars to the transaction log.
            unsafeIOToSTM $ atomicModifyIORef' updateCount (\i -> (i+1,()))
            mapM_ (\i -> readArray tvarArray i >>= \(!_n) -> return ()) tvar_order


            -- Then we just enter the scheduler a lot
            forM_ tvar_order $ \i -> do
                -- when use_writes $
                --     readArray tvarArray i >>= \(!n) -> writeArray tvarArray i (n+1 :: Integer)
                unsafeIOToSTM yield

    _ <- forkIO $ do
                    useTvars [0..n] False
                    -- print "Thread1 done."
                    putMVar m1 True
    _ <- forkIO $ do
                    useTvars (reverse [0..n]) False
                    -- print "Thread1 done."
                    putMVar m2 True
    -- Wait for forked threads.
    _ <- takeMVar m1
    _ <- takeMVar m2
    updates <- readIORef updateCount
    if updates > n
        then putStrLn $ "TVars: " ++ show n ++ ", ERROR: more than " ++ show n ++ " transaction attempts. (" ++ show updates ++")\n"
        else putStrLn $ "TVars: " ++ show n ++ ", OK: no more than " ++ show n ++ " transaction attempts."

    return ()

