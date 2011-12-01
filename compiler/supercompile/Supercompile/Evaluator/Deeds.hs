module Supercompile.Evaluator.Deeds where

import Supercompile.StaticFlags
import Supercompile.Utilities

import Data.Monoid


-- | Number of unclaimed deeds. Invariant: always greater than or equal to 0
type Unclaimed = Int

-- | A deed supply shared amongst all expressions
data Deeds = Deeds {
    sizeLimit :: {-# UNPACK #-} !Int,
    stepLimit :: {-# UNPACK #-} !Int
  } deriving (Eq)

instance Outputable Deeds where
    ppr d = ppr (sizeLimit d, stepLimit d)

instance Monoid Deeds where
    mempty = emptyDeeds
    mappend = plusDeeds

instance Bounded Deeds where
    maxBound = Deeds { sizeLimit = maxBound, stepLimit = maxBound }
    minBound = emptyDeeds

emptyDeeds :: Deeds
emptyDeeds = Deeds { sizeLimit = 0, stepLimit = 0 }

plusDeeds :: Deeds -> Deeds -> Deeds
plusDeeds d1 d2 = d1 `seq` d2 `seq` Deeds { sizeLimit = sizeLimit d1 + sizeLimit d2, stepLimit = stepLimit d1 + stepLimit d2 }

-- NB: it is OK if the number of deeds to claim is negative -- that just causes some deeds to be released
claimDeeds :: Deeds -> Int -> Maybe Deeds
claimDeeds deeds want = guard (not dEEDS || sizeLimit deeds >= want) >> return (deeds { sizeLimit = sizeLimit deeds - want })

releaseDeeds :: Deeds -> Int -> Deeds
releaseDeeds deeds release = deeds { sizeLimit = sizeLimit deeds + release }

apportionDeeds :: Deeds -> [Int] -> [Deeds]
apportionDeeds deeds weights = zipWith Deeds (apportion (sizeLimit deeds) weights) (apportion (stepLimit deeds) weights)


noChange, noGain :: Deeds -> Deeds -> Bool
noChange = (==)
noGain d1 d2 = (sizeLimit d1 >= sizeLimit d2) && (stepLimit d1 >= stepLimit d2)
