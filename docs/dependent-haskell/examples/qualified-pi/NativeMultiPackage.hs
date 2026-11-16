{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

-- | Exercise alternative constructors with retained field telescopes.
module NativeMultiPackage (
    Availability (..),
    ReverseAvailability (..),
    makeAvailable,
    makeReverse,
    lazyAvailable,
    cyclicAvailable,
    toPacked,
    toPackedReverse,
    guardedRows,
    firstAvailable,
) where

import PackedVecLibrary

-- | Combine absence with a retained Nat and its indexed vector.
data Availability a where
    Unavailable :: Availability a
    Available :: foreach (n :: Nat) -> Vec a n -> Availability a

-- | Put the retained constructor first in the declaration.
data ReverseAvailability a where
    PresentFirst :: foreach (n :: Nat) -> Vec a n -> ReverseAvailability a
    AbsentLast :: ReverseAvailability a

-- | Construct a package from an already-bound runtime length.
makeAvailable :: Nat -> Availability Int
makeAvailable n = Available n (replicateNative @Int n 42)

-- | Construct the alternative declaration order.
makeReverse :: Nat -> ReverseAvailability Int
makeReverse n = PresentFirst n (replicateNative @Int n 42)

-- | Leave the vector undefined until the consumer demands it.
lazyAvailable :: Nat -> Availability Int
lazyAvailable n = Available n bottomVector
  where
    bottomVector :: Vec Int m
    bottomVector = error "multi-bottom-vector"

-- | Leave the vector cyclic until the consumer demands it.
cyclicAvailable :: Nat -> Availability Int
cyclicAvailable n = Available n cyclicVector
  where
    cyclicVector :: Vec Int m
    cyclicVector = cyclicVector

-- | Return the existing package representation from either alternative.
toPacked :: Availability a -> Maybe (PackedVec a)
toPacked Unavailable = Nothing
toPacked (Available n xs) = Just (PackVec n xs)

-- | Check the opposite constructor declaration and equation order.
toPackedReverse :: ReverseAvailability a -> Maybe (PackedVec a)
toPackedReverse (PresentFirst n xs) = Just (PackVec n xs)
toPackedReverse AbsentLast = Nothing

-- | Keep each repeated row's own index and payload scope.
guardedRows :: Bool -> Availability a -> Int
guardedRows flag (Available n xs) | flag = ignorePacked (PackVec n xs)
guardedRows _ Unavailable = 0
guardedRows _ (Available n xs) = ignorePacked (PackVec n xs)

-- | Check nested ordinary GADT patterns after a retained field.
firstAvailable :: Availability a -> Maybe a
firstAvailable Unavailable = Nothing
firstAvailable (Available n VNil) = Nothing
firstAvailable (Available n (VCons x xs)) = Just x
