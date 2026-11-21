{-# LANGUAGE ExperimentalDependentTypes #-}
module RecursiveJobs (totalJobs) where

import Data.Kind (Type)

type Job = exists (a :: Type). (a, a -> Int)

{-# NOINLINE runJob #-}
runJob :: Job -> Int
runJob (value, cost) = cost value

-- | Add the cost of each job. The group infers the result's Num dictionary.
{-# INLINE totalJobs #-}
totalJobs _ [] = 0
totalJobs cost (job : jobs) =
    fromIntegral (runJob (job, cost)) + remainingJobs cost jobs

{-# NOINLINE remainingJobs #-}
remainingJobs cost jobs = totalJobs cost jobs
