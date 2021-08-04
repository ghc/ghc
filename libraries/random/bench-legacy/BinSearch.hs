
{-
 Binary search over benchmark input sizes.

 There are many good ways to measure the time it takes to perform a
 certain computation on a certain input.  However, frequently, it's
 challenging to pick the right input size for all platforms and all
 compilataion modes.

 Sometimes for linear-complexity benchmarks it is better to measure
 /throughput/, i.e. elements processed per second.  That is, fixing
 the time of execution and measuring the amount of work done (rather
 than the reverse).  This library provides a simple way to search for
 an appropriate input size that results in the desired execution time.

 An alternative approach is to kill the computation after a certain
 amount of time and observe how much work it has completed.
 -}
module BinSearch
    (
      binSearch
    )
where

import Control.Monad
import Data.Time.Clock -- Not in 6.10
import Data.List
import System.IO
import Prelude hiding (min,max,log)



-- | Binary search for the number of inputs to a computation that
--   results in a specified amount of execution time in seconds.  For example:
--
-- > binSearch verbose N (min,max) kernel
--
--   ... will find the right input size that results in a time
--   between min and max, then it will then run for N trials and
--   return the median (input,time-in-seconds) pair.
binSearch :: Bool -> Integer -> (Double,Double) -> (Integer -> IO ()) -> IO (Integer, Double)
binSearch verbose trials (min, max) kernel = do
  when verbose $
    putStrLn $
    "[binsearch] Binary search for input size resulting in time in range " ++
    show (min, max)
  let desired_exec_length = 1.0
      good_trial t =
        (toRational t <= toRational max) && (toRational t >= toRational min)
         -- At some point we must give up...
      loop n
        | n > ((2 :: Integer) ^ (100 :: Integer)) =
          error
            "ERROR binSearch: This function doesn't seem to scale in proportion to its last argument."
         -- Not allowed to have "0" size input, bump it back to one:
      loop 0 = loop 1
      loop n = do
        when verbose $ putStr $ "[binsearch:" ++ show n ++ "] "
        time <- timeit $ kernel n
        when verbose $ putStrLn $ "Time consumed: " ++ show time
        let rate = fromIntegral n / time
               -- [2010.06.09] Introducing a small fudge factor to help our guess get over the line:
        let initial_fudge_factor = 1.10
            fudge_factor = 1.01 -- Even in the steady state we fudge a little
            guess = desired_exec_length * rate
        -- TODO: We should keep more history here so that we don't re-explore input space we
        --       have already explored.  This is a balancing act because of randomness in
        --       execution time.
        if good_trial time
          then do
            when verbose $
              putStrLn
                "[binsearch] Time in range.  LOCKING input size and performing remaining trials."
            print_trial 1 n time
            lockin (trials - 1) n [time]
          else if time < 0.100
                 then loop (2 * n)
                 else do
                   when verbose $
                     putStrLn $
                     "[binsearch] Estimated rate to be " ++
                     show (round rate :: Integer) ++
                     " per second.  Trying to scale up..."
                        -- Here we've exited the doubling phase, but we're making our
                        -- first guess as to how big a real execution should be:
                   if time > 0.100 && time < 0.33 * desired_exec_length
                     then do
                       when verbose $
                         putStrLn
                           "[binsearch]   (Fudging first guess a little bit extra)"
                       loop (round $ guess * initial_fudge_factor)
                     else loop (round $ guess * fudge_factor)
         -- Termination condition: Done with all trials.
      lockin 0 n log = do
        when verbose $
          putStrLn $
          "[binsearch] Time-per-unit for all trials: " ++
          concat
            (intersperse " " (map (show . (/ toDouble n) . toDouble) $ sort log))
        return (n, log !! (length log `quot` 2)) -- Take the median
      lockin trials_left n log = do
        when verbose $
          putStrLn
            "[binsearch]------------------------------------------------------------"
        time <- timeit $ kernel n
                        -- hFlush stdout
        print_trial (trials - trials_left + 1) n time
                        -- whenverbose$ hFlush stdout
        lockin (trials_left - 1) n (time : log)
      print_trial :: Integer -> Integer -> NominalDiffTime -> IO ()
      print_trial trialnum n time =
        let rate = fromIntegral n / time
            timeperunit = time / fromIntegral n
         in when verbose $
            putStrLn $
            "[binsearch]  TRIAL: " ++
            show trialnum ++
            " secPerUnit: " ++
            showTime timeperunit ++
            " ratePerSec: " ++ show rate ++ " seconds: " ++ showTime time
  (n, t) <- loop 1
  return (n, fromRational $ toRational t)


showTime ::  NominalDiffTime -> String
showTime t = show ((fromRational $ toRational t) :: Double)

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational


-- Could use cycle counters here.... but the point of this is to time
-- things on the order of a second.
timeit :: IO () -> IO NominalDiffTime
timeit io = do
  strt <- getCurrentTime
  io
  end <- getCurrentTime
  return (diffUTCTime end strt)
{-
test :: IO (Integer,Double)
test =
  binSearch True 3 (1.0, 1.05)
   (\n ->
    do v <- newIORef 0
       forM_ [1..n] $ \i -> do
         old <- readIORef v
         writeIORef v (old+i))
-}
