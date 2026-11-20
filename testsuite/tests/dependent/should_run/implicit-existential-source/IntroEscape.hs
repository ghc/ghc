{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module IntroEscape where

import Vector

data AnyVec where
    AnyVec :: Vec n Bool -> AnyVec

bad :: exists (n :: Nat). AnyVec -> Vec n Bool
bad = \(AnyVec xs) -> xs
