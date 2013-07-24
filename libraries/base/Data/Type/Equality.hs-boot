{-# LANGUAGE TypeOperators, GADTs, PolyKinds, NoImplicitPrelude #-}

module Data.Type.Equality where

data a :=: b where
  Refl :: a :=: a