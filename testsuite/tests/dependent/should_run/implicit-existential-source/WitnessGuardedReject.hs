{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs, ImpredicativeTypes, LambdaCase, MultiWayIf, NoExperimentalDependentTypes #-}
module WitnessGuardedReject where

import WitnessSchema

#if !defined(CONDITIONAL_CONFLICT) && !defined(CONDITIONAL_ESCAPE) \
    && !defined(CASE_CONFLICT) && !defined(CASE_ESCAPE) \
    && !defined(MULTIWAY_CONFLICT) && !defined(MULTIWAY_ESCAPE)
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

#elif defined(CONDITIONAL_ESCAPE)
escapeIf :: CapturedMonoBuilder
escapeIf = \(CapturedMono value) flag ->
    if flag then value else value
#elif defined(CASE_CONFLICT)
conflictingCase :: GuardedBuilder
conflictingCase = \flag b -> case flag of
    True -> buildAt b
    False -> (different b :: Payload PolyMaybe)

#elif defined(CASE_ESCAPE)
escapeCase :: CapturedMonoBuilder
escapeCase = \capture flag -> case capture of
    CapturedMono value -> if flag then value else value

#elif defined(MULTIWAY_CONFLICT)
conflictingMultiway :: GuardedBuilder
conflictingMultiway = \flag b -> if
    | flag -> buildAt b
    | never -> (different b :: Payload PolyMaybe)
    | otherwise -> buildAt b

#else
escapeMultiway :: CapturedMonoBuilder
escapeMultiway = \capture flag -> if
    | flag, CapturedMono value <- capture -> value
    | otherwise -> error "unselected guard"

#endif
