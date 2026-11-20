{-# LANGUAGE ExperimentalDependentTypes, ImpredicativeTypes, RankNTypes, RequiredTypeArguments, ScopedTypeVariables, TypeApplications #-}

module RankedPackage
    ( runCallback, runPair, runBuilder, CallbackPair, pairFor
    , Callback(..), GuardedBuilder, atTwoTypes, consumeIdentity, observeGuarded
    , PolyId, withWitness, TwoWitnessBuilder, fromLeft, fromRight, observeTwo
    , Hidden, PolyMaybe, observeCallback
    ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)

data Callback t = Callback t (t -> (Int, Bool))
type Hidden = exists (t :: Type). Callback t
type PolyId = forall a. a -> a

-- | Store one polymorphic identity function and use it at two types.
hidden :: Hidden
hidden = Callback @PolyId id (\f -> (f 7, f True))

-- | Open the package and give its value to the matching callback.
observeCallback :: Hidden -> (Int, Bool)
observeCallback (Callback value call) = call value

-- | Run the hidden identity callback at two types.
runCallback :: (Int, Bool)
runCallback = observeCallback hidden

type CallbackPair = exists (t :: Type). (t, t -> (Int, Bool))

-- | Choose the hidden type and its consumer together.
pairFor :: Bool -> CallbackPair
pairFor True = (7 :: Int, \n -> (n + 1, True))
pairFor False = ('a', \c -> (11, c == 'z'))

-- | Use the supplied identity at two different types.
atTwoTypes :: PolyId -> (Int, Bool)
atTwoTypes f = (f 7, f True)

-- | Run the callback on the value from the same pair.
observePair :: CallbackPair -> (Int, Bool)
observePair (f, k) = k f

-- | Infer the hidden type from the second component.
runPair :: (Int, Bool)
runPair = observePair (id, atTwoTypes)

-- | Store a function that uses the same hidden witness on every call.
type Builder a = exists (t :: Type). a -> Callback t

-- | Use the declared result of makeCallback to infer the lambda's witness.
builder :: forall a. Integral a => Builder a
builder = \n -> makeCallback @a fromIntegral n
  where
    makeCallback :: forall a. (a -> Int) -> a -> Callback PolyId
    makeCallback toInt x = Callback @PolyId id (\f -> (f (toInt x), f True))

-- | Apply the builder and use its hidden function at two types.
runBuilder :: Int -> (Int, Bool)
runBuilder n = case builder @Int n of
    Callback f call -> call f

-- | Store a two-argument builder with one witness for all calls.
type GuardedBuilder = exists (t :: Type). Bool -> Bool -> Callback t

-- | Take a polymorphic identity and store it with a typed consumer.
consumeIdentity :: PolyId -> Callback PolyId
consumeIdentity value = Callback @PolyId value (\f -> (f 11, f False))

-- | Call the builder and give its hidden value to the attached callback.
observeGuarded :: GuardedBuilder -> Bool -> Bool -> (Int, Bool)
observeGuarded build ready input =
    case build ready input of Callback value call -> call value

-- | Store a value and its consumer at the required type argument.
withWitness :: forall tag. forall t -> Proxy tag -> t -> (t -> (Int, Bool)) -> Callback t
withWitness (type t) _ value call = Callback @t value call

type PolyMaybe = forall a. a -> Maybe a
data TwoCallbacks l r = FromLeft (Callback l) | FromRight (Callback r)
type TwoWitnessBuilder = exists (l :: Type). exists (r :: Type).
    Bool -> Bool -> TwoCallbacks l r

-- | Fix the left witness and leave the right witness polymorphic.
fromLeft :: forall r. Bool -> TwoCallbacks PolyId r
fromLeft _ = FromLeft (consumeIdentity id)

-- | Fix the right witness and leave the left witness polymorphic.
fromRight :: forall l. Bool -> TwoCallbacks l PolyMaybe
fromRight input = FromRight (Callback @PolyMaybe Just useMaybe)
  where
    useMaybe :: PolyMaybe -> (Int, Bool)
    useMaybe f = (maybe 0 id (f 13), maybe False id (f input))

-- | Use the value and consumer from the selected callback.
observeTwo :: TwoWitnessBuilder -> Bool -> Bool -> (Int, Bool)
observeTwo build branch input = case build branch input of
    FromLeft (Callback value call) -> call value
    FromRight (Callback value call) -> call value
