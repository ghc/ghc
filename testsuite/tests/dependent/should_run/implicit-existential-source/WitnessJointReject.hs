{-# LANGUAGE GADTs, ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes #-}
module WitnessJointReject where

import WitnessSchema

jointConflict :: TwoWitnessBuilder
jointConflict = \cases
    False b | never -> conflictingLeft b
            | otherwise -> fromLeft b
    True b -> fromRight b

jointEscape :: ConfinedTwoWitnessBuilder
jointEscape = \cases
    (Captured _) False -> fromRight False
    (Captured value) True -> fromCaptured value
