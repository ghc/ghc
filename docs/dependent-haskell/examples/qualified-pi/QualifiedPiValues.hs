{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImpredicativeTypes #-}

module QualifiedPiValues (fixedQ, plainFunction) where

import PackedVecLibrary

-- | Construct a fixed payload without inspecting the available dictionary.
fixedQ :: foreach (n :: Nat) -> (Eq Int) => Vec Int n
fixedQ n = replicateNative @Int n 7

-- | Construct the plain function used by the equality-pattern transport.
plainFunction :: foreach (n :: Nat) -> Vec Int n
plainFunction n = replicateNative @Int n 7
