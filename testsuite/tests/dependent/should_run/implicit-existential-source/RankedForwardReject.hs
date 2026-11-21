{-# LANGUAGE ImpredicativeTypes, LinearTypes, NoExperimentalDependentTypes #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module RankedForwardReject where

import MixedPackageProvider

badRankedSibling :: Bool -> Int
badRankedSibling flag = readBoth (relayRanked (const True) flag (makeBoth flag))

escapeRankedRelay :: forall a. Bool -> a
escapeRankedRelay flag = withRankedId (\transform -> outerOfBoth (relayRanked transform () (makeBoth flag)))

incompatibleRankedRelay :: RankedMixed -> Int
incompatibleRankedRelay p = readBoth (relayRanked id () p)

linearRankedRelay :: BothMixed %1 -> Int
linearRankedRelay p = readBoth (relayRanked id () p)
