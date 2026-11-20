{-# LANGUAGE NoExperimentalDependentTypes #-}
module LaterMatchabilityClient (laterMatchability) where

import LaterMatchability (MatchResult, UnmatchResult)

laterMatchability :: (MatchResult, UnmatchResult)
laterMatchability = (Just True, True)
