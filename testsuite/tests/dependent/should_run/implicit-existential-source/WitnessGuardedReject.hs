{-# LANGUAGE GADTs, ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes #-}
module WitnessGuardedReject where

import WitnessSchema

conflict :: GuardedBuilder
conflict = \cases
    True b -> buildAt b
    False b | never -> different b
            | otherwise -> buildAt b

escape :: CapturedMonoBuilder
escape = \cases
    (CapturedMono value) True -> value
    (CapturedMono value) False -> value
