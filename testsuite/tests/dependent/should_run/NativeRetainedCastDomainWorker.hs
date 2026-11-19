{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RankNTypes #-}
module NativeRetainedCastDomainWorker (observe, forceWorker, forceIndex) where

import Data.IORef (newIORef, readIORef)
import PackedVecLibrary (Nat(..), Vec, toList)
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
