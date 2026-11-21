{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE ExplicitForAll, Modifiers, TypeApplications #-}
{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module LaterMatchabilityClient
  (laterMatchability, forwardedMap, annotatedMap, nestedCallback, appliedCallback, boxedCallbacks, rankedMap, flippedCallbacks) where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (U)
import LaterMatchability (AtBool, CallbackResult(CallbackResult), ForwardMap,
                           FamilyBox(FamilyBox), FlippedMatch, FlippedUnmatch, ForwardBox, KnownListCallback, ListBox,
                           ListCallback, ListRecipe, MatchResult, Recipe(Recipe), UnmatchResult)

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

-- | Use the imported list callback to infer the inner arrow.
nestedCallback :: [Bool]
nestedCallback = apply (Proxy @AtBool) [True]
  where
    apply :: forall (g :: (Type -> Type) %U -> Type).
             Proxy g -> g KnownListCallback -> g KnownListCallback
    apply _ = id

-- | The bare use fixes the kind of the applied callback.
type AppliedCallback f = CallbackResult (f Bool) f

appliedCallback :: [Bool]
appliedCallback =
  case (CallbackResult [True] :: AppliedCallback KnownListCallback) of
    CallbackResult result -> result

-- | Construct and read boxes at both inferred callback kinds.
boxedCallbacks :: (Maybe Bool, [Bool])
boxedCallbacks =
  case (FamilyBox (Just True) :: ForwardBox Maybe, FamilyBox [False] :: ListBox) of
    (FamilyBox matchable, FamilyBox unmatchable) -> (matchable, unmatchable)

-- | Read the ordinary values stored by the polymorphic mapper.
rankedMap :: (Maybe Bool, [Bool])
rankedMap =
  case (Recipe (Just True) [False] :: ListRecipe) of
    Recipe matchable unmatchable -> (matchable, unmatchable)

-- | Use both results of the declared callback kind.
flippedCallbacks :: (FlippedMatch, FlippedUnmatch)
flippedCallbacks = (Just False, [True])
