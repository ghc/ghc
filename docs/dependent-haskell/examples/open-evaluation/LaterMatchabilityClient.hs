{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
module LaterMatchabilityClient (laterMatchability, forwardedMap, annotatedMap) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import LaterMatchability (ForwardMap, KnownListCallback, ListCallback, MatchResult, UnmatchResult)

laterMatchability :: (MatchResult, UnmatchResult)
laterMatchability = (Just True, True)

forwardedMap ::
    ( ForwardMap Maybe '[Bool, Int] :~: '[Maybe Bool, Maybe Int]
    , ForwardMap (ListCallback 'True) '[Bool, Int] :~: '[[Bool], [Int]]
    )
forwardedMap = (Refl, Refl)

-- | Use the declared U arrow in a bare kind annotation.
annotatedMap ::
    ForwardMap (KnownListCallback :: Type -> Type) '[Bool, Int] :~: '[[Bool], [Int]]
annotatedMap = Refl
