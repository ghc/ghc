{-# LANGUAGE DataKinds, GADTs, ImpredicativeTypes, LinearTypes, NoExperimentalDependentTypes, RankNTypes, RequiredTypeArguments, ScopedTypeVariables, TypeApplications #-}
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

wrongRequiredOperand :: RankedBuilder
wrongRequiredOperand = \_ -> (withWitness @Int (type PolyId) Proxy) True atTwoTypes

requiredEscape :: CapturedMonoBuilder
requiredEscape = \(CapturedMono (Payload (value :: t) run)) _ ->
    (withWitness @Int (type t) Proxy) value run

wrongPackageArgument :: Bool -> (Int, Bool)
wrongPackageArgument flag = case ranked @Char flag of
    Payload value run -> run value

escapePackageCall :: Bool -> t
escapePackageCall flag = case ranked @Bool flag of Payload value _ -> value
