{-# LANGUAGE DataKinds, PolyKinds #-}
module NestedEscape where

import Data.Proxy (Proxy)
import NestedSignature
import Vector

first :: PackedPair a -> Vec n a
first p = left p
  where
    left :: (Vec l a, Vec r a) -> Vec l a
    left (ls, _) = ls

second :: PackedPair a -> Vec n a
second p = right p
  where
    right :: (Vec l a, Vec r a) -> Vec r a
    right (_, rs) = rs

equalLengths :: PackedPair a -> Bool
equalLengths p = equal p
  where
    equal :: (Vec n a, Vec n a) -> Bool
    equal _ = True

knownKind :: PackedKind -> Bool
knownKind p = atNat p
  where
    atNat :: Proxy (v :: Nat) -> Bool
    atNat _ = True
