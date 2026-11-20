{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes #-}

module DotConstraint where

import Vector

type Rejected a = exists (n :: Nat). Show a => Vec n a
