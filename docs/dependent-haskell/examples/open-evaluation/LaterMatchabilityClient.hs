{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
module LaterMatchabilityClient (laterMatchability, forwardedMap) where

import Data.Type.Equality ((:~:)(Refl))
import LaterMatchability (ForwardMap, ListCallback, MatchResult, UnmatchResult)

laterMatchability :: (MatchResult, UnmatchResult)
laterMatchability = (Just True, True)

forwardedMap ::
    ( ForwardMap Maybe '[Bool, Int] :~: '[Maybe Bool, Maybe Int]
    , ForwardMap (ListCallback 'True) '[Bool, Int] :~: '[[Bool], [Int]]
    )
forwardedMap = (Refl, Refl)
