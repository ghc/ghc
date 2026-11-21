{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs, ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes #-}
module WitnessGuardedReject where

import WitnessSchema

#if !defined(CONDITIONAL_CONFLICT) && !defined(CONDITIONAL_ESCAPE)
conflict :: GuardedBuilder
conflict = \cases
    True b -> buildAt b
    False b | never -> different b
            | otherwise -> buildAt b

escape :: CapturedMonoBuilder
escape = \cases
    (CapturedMono value) True -> value
    (CapturedMono value) False -> value

#elif defined(CONDITIONAL_CONFLICT)
conflictingIf :: GuardedBuilder
conflictingIf = \flag b ->
    if never
        then if flag then (buildAt b :: Payload PolyId)
                     else (different b :: Payload PolyMaybe)
        else buildAt b

#else
escapeIf :: CapturedMonoBuilder
escapeIf = \(CapturedMono value) flag ->
    if flag then value else value
#endif
