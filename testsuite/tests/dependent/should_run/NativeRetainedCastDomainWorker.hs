{-# LANGUAGE AllowAmbiguousTypes, CPP, DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, RankNTypes, ScopedTypeVariables, StandaloneKindSignatures, TypeApplications #-}
module NativeRetainedCastDomainWorker
    (observe, forceWorker, forceIndex, observeBranch, observePair, branchVector, BranchVector(..)) where

import Data.IORef (newIORef, readIORef)
import Data.Kind (Type)
import PackedVecLibrary (Add, Nat(..), Vec, toList)
import ParameterizedIndexLibrary

observe :: IO ([Int], [Int], [Int])
observe = do
    n <- newIORef (S (S Z)) >>= readIORef
    let headForLength :: SourceToken -> Nat
        headForLength _ = n
    let token = error "unused token" :: TargetToken
        source = error "unused token" :: SourceToken
        forward :: foreach (x :: TargetToken) -> Vec Int (headForLength x)
        forward = adaptTokenVector headForLength (tokenVector headForLength 7)
        backward :: foreach (x :: SourceToken) -> Vec Int (headForLength x)
        backward = adaptTokenVectorBack headForLength forward
    pure (toList (forward token), toList (backward source),
          toList (specializedTokenVector headForLength token))

forceWorker :: IO ()
forceWorker = do
    let lengthZero :: SourceToken -> Nat
        lengthZero _ = Z
    let worker :: foreach (x :: SourceToken) -> Vec Int (lengthZero x)
        worker = error "missing worker"
    forceTokenFunction lengthZero worker

forceIndex :: Int
forceIndex =
    let demanding :: SourceToken -> Nat
        demanding DomainToken = Z
        token :: TargetToken
        token = error "demanded token"
    in length (toList (applyTokenVector demanding token))

type BranchVector :: forall n. Vec Bool n -> Type
data BranchVector xs where
    BranchVector :: BranchVector xs

observeVector :: forall n. foreach (xs :: Vec Bool n) -> BranchVector xs
observeVector _ = BranchVector

branchVector :: forall m. foreach (cut :: Nat)
             -> foreach (xs :: Vec Bool (Add cut m)) -> BranchVector xs
#ifdef WRONG_BRANCH_LENGTH
branchVector Z xs = observeVector @('S m) xs
#else
branchVector Z xs = observeVector @m xs
#endif
branchVector (S k) xs = observeVector @('S (Add k m)) xs

observeBranch :: foreach (cut :: Nat) -> foreach (xs :: Vec Bool (Add cut 'Z)) -> Bool
observeBranch cut xs = case branchVector @'Z cut xs of
    BranchVector -> True

-- Both arguments occur in the result type of the worker.
branchPair :: foreach (n :: Nat) -> foreach (xs :: Vec Bool n) -> foreach (ys :: Vec Bool n)
           -> (BranchVector xs, BranchVector ys)
branchPair n xs ys = case n of
    Z -> (observeVector @'Z xs, observeVector @'Z ys)
#ifdef WRONG_BRANCH_VECTOR
    S k -> (observeVector @('S k) ys, observeVector @('S k) xs)
#else
    S k -> (observeVector @('S k) xs, observeVector @('S k) ys)
#endif

observePair :: foreach (n :: Nat) -> Vec Bool n -> Vec Bool n -> Bool
observePair n xs ys = case branchPair n xs ys of (BranchVector, BranchVector) -> True
