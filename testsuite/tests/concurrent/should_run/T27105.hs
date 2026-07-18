{-# OPTIONS_GHC -fno-omit-yields #-}

import Control.Monad
import Control.Monad.ST
import Control.Concurrent
import Control.Exception
import System.Exit
import System.Mem
import GHC.Arr
import Prelude hiding (init)

-- Test thread fairness:
-- run two cpu-bound threads concurrently for a second,
-- each counts how many operations it can perform until signaled to stop
-- expect a balance between the two with no more than a 30% imperfection.
--
-- Sadly we have had to mark this test as fragile. On the CI machines we
-- occasionally observe extraordinary levels of unfairness: over 80% in some
-- cases. Having this marked fragile is not ideal, but it's no good having
-- random failures. See issue #27522.
--
-- People working on the RTS timers, scheduler or capability infrastructure
-- *ought* to check this test is not failing badly in a reproducible way.
-- Doing so should still catch gross breakage. This test _should_ detect if
-- the interval timer is not working, or if thread context switching is messed
-- up. We can expect failure if we force a contex switch interval of more than
-- half the test time, i.e. more than 0.5s.
--
-- We run the test twice, with allocating and non-allocating worker threads.
-- The -fno-omit-yields above is crucial for worker_nonalloc below, or it never
-- gets interrupted and thus no context switches.

main :: IO ()
main = do
  test worker_alloc
  performMajorGC
  test worker_nonalloc

test :: Worker -> IO ()
test worker = do
  stop <- newEmptyMVar
  res1 <- newEmptyMVar
  res2 <- newEmptyMVar
  _ <- forkIO (worker stop >>= putMVar res1)
  _ <- forkIO (worker stop >>= putMVar res2)
  threadDelay 300_000
  -- Let them run for 300ms. The default context switch interval is 20ms.
  -- This gives time for 15 context switches, so this _should_ be enough
  -- to get less than 10% unfairness. And on most platforms it is enough,
  -- but for a bit of robustness we use 30%.
  putMVar stop ()
  count1 <- takeMVar res1
  count2 <- takeMVar res2
  let balance :: Double
      balance = abs ((fromIntegral count1 - fromIntegral count2)
                    / fromIntegral count2)
  when (balance > 0.30) $ do
    putStrLn "Schedule fairness more than 30% tolerance:"
    putStrLn $ "imperfection: " ++ show (balance * 100) ++ "%"
    putStrLn $ "work counts:  " ++ show (count1, count2)
    exitFailure

type Worker = MVar () -> IO Int

-- count how many iterations we can calculate until we're signaled to stop
worker_template :: IO a -> (a -> IO ()) -> MVar () -> IO Int
worker_template init iter stop = do
    a <- init
    go a 0
  where
    go a !count = do
      ok <- tryReadMVar stop
      case ok of
        Just () -> return count
        Nothing -> do
          iter a
          go a (count + 1)


-- the allocating worker
{-# NOINLINE worker_alloc #-}
worker_alloc :: Worker
worker_alloc =
  worker_template
    (return 18)
    (\n -> evaluate (fib n) >> return ())

-- by forcing this to be Integer we cause lots of allocation!
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- the non-allocating worker
{-# NOINLINE worker_nonalloc #-}
worker_nonalloc :: Worker
worker_nonalloc =
  worker_template
    (stToIO $ newSTArray (0,50_000) 42)
    (\arr -> stToIO $ arrrev arr)

arrrev :: STArray s Int Int -> ST s ()
arrrev arr =
    let (i,j) = boundsSTArray arr
     in arrrev_go arr i j

{-# NOINLINE arrrev_go #-}
arrrev_go :: STArray s Int Int -> Int -> Int -> ST s ()
arrrev_go !_   !i !j | i >= j = return ()
arrrev_go !arr !i !j = do
  x <- readSTArray arr i
  y <- readSTArray arr j
  writeSTArray arr i y
  writeSTArray arr j x
  arrrev_go arr (i+1) (j-1)

