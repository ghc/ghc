{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module IndexBetaClient where

import PackedVecLibrary

-- | Retain the actual index through an immediate identity application.
betaIdentity :: foreach (n :: Nat) -> Int -> Vec Int n
betaIdentity n x = replicateNative @Int ((\k -> k) n) x

-- | Add one constructor without forcing the actual predecessor.
betaSuccessor :: foreach (n :: Nat) -> Int -> Vec Int (S n)
betaSuccessor n x = replicateNative @Int ((\k -> S k) n) x

-- | Ignore the atomic argument and the unused payload at zero.
betaConstant :: Nat -> Int -> Vec Int Z
betaConstant n x = replicateNative @Int ((\k -> Z) n) x

-- | Normalize the nested atomic application before its constructor body.
betaNested :: foreach (n :: Nat) -> Int -> Vec Int (S n)
betaNested n x = replicateNative @Int ((\outer -> S ((\inner -> inner) outer)) n) x

-- | Distinguish the inner binder from the same-spelled actual index.
betaShadow :: foreach (betaIndex :: Nat) -> Int -> Vec Int betaIndex
betaShadow betaIndex x = replicateNative @Int ((\betaIndex -> betaIndex) betaIndex) x

-- | Preserve a bottom retained function before inspecting its index argument.
betaBottomWorker :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
betaBottomWorker = error "native-source-beta-function"

-- | Return the original partial function without an eta expansion.
betaBottomPartial :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
betaBottomPartial n = betaBottomWorker ((\k -> k) n)
