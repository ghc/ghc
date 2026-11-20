{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}

module BarRank where

import Vector

type BarForall = exists (n :: Nat) | forall a. Vec n a
type BarConstraint a = exists (n :: Nat) | Show a => Vec n a
