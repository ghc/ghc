-- | Benchmarking utilities.  For example, functions for generating
-- random integers.
module Util.Int where

import System.Random (mkStdGen, randomRs)

-- | Generate a number of uniform random integers in the interval
-- @[0..upper]@.
rnd :: Int  -- ^ Upper bound (inclusive)
    -> Int  -- ^ Number of integers
    -> [Int]
rnd upper num = take num $ randomRs (0, upper) $ mkStdGen 1234

-- | Generate a number of uniform random integers in the interval
-- @[0..upper]@ different from @rnd@.
rnd' :: Int  -- ^ Upper bound (inclusive)
     -> Int  -- ^ Number of integers
     -> [Int]
rnd' upper num = take num $ randomRs (0, upper) $ mkStdGen 5678
