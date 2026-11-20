{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}

module Functions (Function, one, empty, bodyTrap) where

import Vector

type Function a = exists (n :: Nat). a -> Vec n a

one :: Function a
one = \x -> Cons x Nil

empty :: Function a
empty = \_ -> Nil

bodyTrap :: Function Bool
bodyTrap = \_ -> (error "body was forced" :: Vec 'Z Bool)
