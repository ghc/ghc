{-# LANGUAGE BangPatterns, LinearTypes, MonoLocalBinds, ImpredicativeTypes, LambdaCase, NoExperimentalDependentTypes, RequiredTypeArguments #-}
module RankedPackageClient
    ( runGuarded, runProjections, projectAt, runDeclaredWitnesses, runExplicitBranches
    , runMixedPackage, runProducedPackage, runForwardedPackage, runLabelledPackage
    , runSelectedPackage, runLocalPattern, readCount
    ) where

import Data.Bool (bool)
import Data.Proxy (Proxy(..))
import qualified ErasedPackage
import RankedPackage
    ( Callback(..), CallbackPair, GuardedBuilder, Hidden, PolyId, PolyMaybe
    , TwoWitnessBuilder, atTwoTypes, consumeIdentity, fromLeft, fromRight
    , observeCallback, observeGuarded, observeTwo, pairFor, withWitness
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

-- | Select each branch's witness and leave an unused result field lazy.
runExplicitBranches :: ((Int, Bool), (Int, Bool), Int)
runExplicitBranches =
    ( observeCallback (choose True True)
    , observeCallback (choose False False)
    , fst (observeCallback (choose False (error "unused callback input")))
    )
  where
    choose :: Bool -> Bool -> Hidden
    choose branch input =
        if branch
            then Callback @PolyId id (\f -> (f 7, f input))
            else Callback @PolyMaybe Just
                (\f -> (maybe 0 id (f 17), maybe False id (f input)))

-- | Hide the outer witness in an ordinary module and read the number.
runMixedPackage :: Int -> Int
runMixedPackage n = ErasedPackage.readMixedNumber (hide n)

-- | Pass the produced outer package directly to its polymorphic reader.
runProducedPackage :: Int -> Int
runProducedPackage n = ErasedPackage.readNumber (hide n)

-- | Pass the producer's result through a polymorphic function.
runForwardedPackage :: Int -> Int
runForwardedPackage n = ErasedPackage.readNumber (id (hide n))

-- | Read the label and number while keeping both witnesses hidden.
runLabelledPackage :: Int -> (String, Int)
runLabelledPackage n = ErasedPackage.readLabelledNumber (id (hide n))

-- | Read the selected package and leave the other package unevaluated.
runSelectedPackage :: Bool -> (String, Int)
runSelectedPackage chooseLeft
    | chooseLeft = readSelected (hide 37) unused
    | otherwise = readSelected unused (hide 41)
  where
    unused :: ErasedPackage.MixedNumber
    unused = error "unselected package"

    readSelected :: ErasedPackage.MixedNumber -> ErasedPackage.MixedNumber -> (String, Int)
    readSelected left right = ErasedPackage.readLabelledNumber (bool right left chooseLeft)

-- | Bind the two fields under the same hidden witness.
runLocalPattern :: Bool -> (Int, Bool)
runLocalPattern flag = let (value, reader) = pairFor flag in reader value

-- | Hide the first witness after packing the second.
hide :: Int -> ErasedPackage.MixedNumber
hide value = ErasedPackage.packNumber value

-- | Open the hidden state and pass it once to the stored reader.
readCount :: ErasedPackage.EncodedCount %1 -> Int
readCount encoded = let !(state, reader) = encoded in reader state
