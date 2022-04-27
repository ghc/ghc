-- This is the integral benchmark from the nofib benchmark suite.
-- In the past there have been issues with numeric stability for this benchmark (see #19569)
-- so I added it to testsuite to catch such regressions in the future.

-- It might be acceptable for this test to fail if you make changes to the simplifier. But generally such a failure shouldn't be accepted without good reason.

-- The excessive whitespace is the result of running the original benchmark which was a .lhs file through unlit.

module Main (integrate1D, main) where

import System.Environment

integrate1D :: Double -> Double -> (Double->Double) -> Double
integrate1D l u f =
  let  d = (u-l)/8.0 in
     d * sum
      [ (f l)*0.5,
        f (l+d),
        f (l+(2.0*d)),
        f (l+(3.0*d)),
        f (l+(4.0*d)),
        f (u-(3.0*d)),
        f (u-(2.0*d)),
        f (u-d),
        (f u)*0.5]

integrate2D l1 u1 l2 u2 f = integrate1D l2 u2
            (\y->integrate1D l1 u1
              (\x->f x y))

zark u v = integrate2D 0.0 u 0.0 v (\x->(\y->x*y))

-- type signature required for compilers lacking the monomorphism restriction
ints = [1.0..] :: [Double]
zarks = zipWith zark ints (map (2.0*) ints)
rtotals = head zarks : zipWith (+) (tail zarks) rtotals
rtotal n = rtotals!!n

is = map (^4) ints
itotals = head is : zipWith (+) (tail is) itotals
itotal n = itotals!!n

es = map (^2) (zipWith (-) rtotals itotals)
etotal n = sum (take n es)

-- The (analytical) result should be zero
main = do
  [with_output,range] <- getArgs
  if (read with_output)
    then putStrLn $ show $ etotal $ read range
    else seq (etotal $ read range) (putStrLn "Exact result hidden for lack of stability.\nPass 'True' as first argument to the benchmark if you want to view the computed output for testing purposes.")
