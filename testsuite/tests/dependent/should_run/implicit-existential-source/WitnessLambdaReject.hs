{-# LANGUAGE GADTs, ImpredicativeTypes, LinearTypes, NoExperimentalDependentTypes, RankNTypes #-}
module WitnessLambdaReject where

import WitnessSchema

escape :: CapturedBuilder
escape = \(Captured callback) -> callback

duplicate :: LinearBuilder
duplicate = \b -> buildTwice b b

wrongOperand :: RankedBuilder
wrongOperand = \_ -> buildAt 'a'

missingEquality :: FlaggedBuilder a
missingEquality = \_ b -> buildAt b
