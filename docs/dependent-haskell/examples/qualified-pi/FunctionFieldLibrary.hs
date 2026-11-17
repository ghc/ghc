{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies #-}

-- | Store a local function inside a closed algebraic index.
module FunctionFieldLibrary where

import GHC.Exts (FUN, U, Multiplicity (Many))

type BoolEndo = FUN @U 'Many Bool Bool

data Flag (b :: Bool) where
    IsFalse :: Flag 'False
    IsTrue :: Flag 'True

type family Stored (x :: Maybe BoolEndo) :: BoolEndo where
    Stored ('Just f) = f

data Box where
    Box :: foreach (x :: Maybe BoolEndo) -> foreach (b :: Bool)
        -> Flag (Stored x b) -> Box

-- | Build the proof through a strict call to the actual local function.
storedFlag :: foreach (f :: BoolEndo) -> foreach (b :: Bool)
           -> Flag (Stored ('Just f) b)
storedFlag f b = case f b of
    False -> IsFalse
    True -> IsTrue

-- | Require the direct call index after a client refines the container.
consumeFlag :: foreach (f :: BoolEndo) -> foreach (b :: Bool)
            -> Flag (f b) -> Int
consumeFlag f b proof = case f b of
    False -> case proof of IsFalse -> 1
    True -> case proof of IsTrue -> 2

-- | Package a constructor index and its proof without a function wrapper.
build :: BoolEndo -> Bool -> Box
build f b = Box (Just f) b (storedFlag f b)

-- | Keep the container, function, argument and proof independently lazy.
loose :: Maybe BoolEndo -> Bool -> Box
loose x b =
    let proof :: Flag (Stored x b)
        proof = error "function-field-proof"
     in Box x b proof

-- | Reconstruct the package with its actual index identities.
repack :: Box -> Box
repack (Box x b proof) = Box x b proof
