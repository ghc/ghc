{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}

module Builder (Builder, singleton, empty, throwing, shadowed) where

import Vector

type Builder = exists (n :: Nat). Bool -> Vec n Bool

singleton, empty, throwing :: Builder
singleton = \b -> Cons b Nil
empty = \_ -> Nil
throwing = \_ -> (error "builder body ran" :: Vec 'Z Bool)

shadowed :: forall (n :: Nat). exists (n :: Nat). Bool -> Vec n Bool
shadowed = \b -> Cons b Nil
