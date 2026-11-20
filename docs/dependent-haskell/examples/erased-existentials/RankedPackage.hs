{-# LANGUAGE ExperimentalDependentTypes, ImpredicativeTypes, RankNTypes, ScopedTypeVariables, TypeApplications #-}

module RankedPackage (runCallback, runPair, runBuilder) where

import Data.Kind (Type)

data Callback t = Callback t (t -> (Int, Bool))
type Hidden = exists (t :: Type). Callback t
type PolyId = forall a. a -> a

-- | Store one polymorphic identity function and use it at two types.
hidden :: Hidden
hidden = Callback @PolyId id (\f -> (f 7, f True))

-- | Open the package and give its value to the matching callback.
runCallback :: (Int, Bool)
runCallback = case hidden of Callback value call -> call value

type CallbackPair = exists (t :: Type). (t, t -> (Int, Bool))

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
