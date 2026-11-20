{-# LANGUAGE DataKinds, GADTs, ImpredicativeTypes, LinearTypes, NoExperimentalDependentTypes, RankNTypes, TypeApplications #-}
module WitnessLambdaReject where

import Data.Proxy
import WitnessSchema

escape :: CapturedBuilder
escape = \(Captured callback) -> callback

duplicate :: LinearBuilder
duplicate = \b -> buildTwice b b

wrongOperand :: RankedBuilder
wrongOperand = \_ -> buildAt 'a'

missingEquality :: FlaggedBuilder a
missingEquality = \_ b -> buildAt b

wrongVisibleKind :: RankedBuilder
wrongVisibleKind = \_ -> configured @True @Maybe Proxy (const True) Nothing

wrongPackageArgument :: Bool -> (Int, Bool)
wrongPackageArgument flag = case ranked @Char flag of
    Payload value run -> run value

escapePackageCall :: Bool -> t
escapePackageCall flag = case ranked @Bool flag of Payload value _ -> value
