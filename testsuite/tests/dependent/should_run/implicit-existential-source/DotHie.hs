{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module DotHie where

import Vector

wrap :: Vec n a -> exists (m :: Nat). Vec m a
wrap xs = xs
