{-# LANGUAGE ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes, RequiredTypeArguments #-}
module RankedPackageClient (runGuarded, runProjections, projectAt, runDeclaredWitnesses) where

import Data.Proxy (Proxy(..))
import RankedPackage
    ( Callback(..), CallbackPair, GuardedBuilder, PolyId, TwoWitnessBuilder
    , atTwoTypes, consumeIdentity, fromLeft, fromRight, observeGuarded
    , observeTwo, pairFor, withWitness
    )

-- | Both projections use the package chosen for the same input.
projectAt :: Bool -> (Int, Bool)
projectAt b = snd (pairFor b) (fst (pairFor b))

-- | Apply both selectors to a pair with one hidden type.
runProjections :: (Int, Bool)
runProjections = project (id, atTwoTypes)
  where
    project :: CallbackPair -> (Int, Bool)
    project p = snd p (fst p)

-- | Use the declared consumer to infer the witness for both guards.
builder :: GuardedBuilder
builder = \cases
    ready input | ready && input -> consumeIdentity id
                | otherwise -> Callback id atTwoTypes

-- | Skip the input when the first flag is false.
-- Run the typed branch when both flags are true.
runGuarded :: ((Int, Bool), (Int, Bool))
runGuarded = (observeGuarded builder False (error "unused guard input"),
              observeGuarded builder True True)

-- | Read one witness from a type argument and two from separate equations.
runDeclaredWitnesses :: ((Int, Bool), ((Int, Bool), (Int, Bool)))
runDeclaredWitnesses =
    ( observeGuarded required True True
    , (observeTwo joint False (error "unused callback input"), observeTwo joint True False)
    )
  where
    required :: GuardedBuilder
    required = \_ input -> (withWitness @Int (type PolyId) Proxy) id (\f -> (f 7, f input))

    joint :: TwoWitnessBuilder
    joint = \cases
        False input -> fromLeft input
        True input -> fromRight input
