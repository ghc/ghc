module Supercompile.Evaluator.Deeds where

import Supercompile.StaticFlags
import Supercompile.Utilities


-- | Number of unclaimed deeds. Invariant: always greater than or equal to 0
type Unclaimed = Int

-- | A deed supply shared amongst all expressions
type Deeds = Int

-- NB: it is OK if the number of deeds to claim is negative -- that just causes some deeds to be released
claimDeeds :: Deeds -> Int -> Maybe Deeds
claimDeeds deeds want = guard (not dEEDS || deeds >= want) >> return (deeds - want)


noChange, noGain :: Deeds -> Deeds -> Bool
noChange = (==)
noGain = (>=)
