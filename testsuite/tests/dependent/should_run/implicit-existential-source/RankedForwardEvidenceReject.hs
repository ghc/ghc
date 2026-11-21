{-# LANGUAGE ImpredicativeTypes, NoExperimentalDependentTypes #-}
module RankedForwardEvidenceReject where

import MixedPackageProvider

data NoDisplay = NoDisplay

badRankedEvidence :: Bool -> Int
badRankedEvidence flag = readBoth (relayRanked id NoDisplay (makeBoth flag))
