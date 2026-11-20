{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE ExplicitForAll, Modifiers, TypeApplications #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
module LaterMatchabilityClient (laterMatchability, forwardedMap, annotatedMap) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import LaterMatchability (ForwardMap, KnownListCallback, ListCallback, MatchResult, UnmatchResult)

laterMatchability :: (MatchResult, UnmatchResult)
laterMatchability = (Just True, True)

forwardedMap ::
    ( ForwardMap Maybe '[Bool, Int] :~: '[Maybe Bool, Maybe Int]
    , ForwardMap (ListCallback 'True) '[Bool, Int] :~: '[[Bool], [Int]]
    )
forwardedMap = (Refl, Refl)

-- | Apply the pair proof to the imported list family.
annotatedMap :: ForwardMap KnownListCallback '[Bool, Int] :~: '[[Bool], [Int]]
annotatedMap = mapPair (Proxy @(KnownListCallback :: Type -> Type))
  where
    mapPair :: forall (f :: Type -> Type). Proxy (f :: Type %U -> Type)
            -> ForwardMap f '[Bool, Int] :~: '[f Bool, f Int]
    mapPair _ = Refl
