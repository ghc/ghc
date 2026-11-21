{-# LANGUAGE DataKinds, PolyKinds #-}
module NestedEscape where

import Data.Proxy (Proxy)
import Nested (HiddenProduct, Product(..))
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

firstFunction :: PackedPair a -> () -> Vec n a
firstFunction p = left p
  where
    left :: (Vec l a, Vec r a) -> () -> Vec l a
    left (ls, _) () = ls

secondFunction :: PackedPair a -> () -> Vec n a
secondFunction p = right p
  where
    right :: (Vec l a, Vec r a) -> () -> Vec r a
    right (_, rs) () = rs

witnessFunction :: PackedKind -> () -> Proxy (v :: k)
witnessFunction p = reveal p
  where
    reveal :: Proxy a -> () -> Proxy a
    reveal value () = value

inferredFunction :: PackedPair a -> () -> Vec n a
inferredFunction p =
    let selected = left p
    in selected
  where
    left :: (Vec l a, Vec r a) -> () -> Vec l a
    left (ls, _) () = ls

firstEquation :: HiddenProduct a -> Vec n a
firstEquation (Product yes _) = yes

distinctEquations :: HiddenProduct a -> HiddenProduct a -> Bool
distinctEquations (Product yes _) (Product other _) = sameLength yes other
  where
    sameLength :: Vec n a -> Vec n a -> Bool
    sameLength _ _ = True

wrongRepack :: PackedPair a -> PackedPair a
wrongRepack p = let result = case p of (xs, ys) -> sameLength xs ys in result
  where
    sameLength :: Vec n a -> Vec n a -> (Vec n a, Vec n a)
    sameLength xs ys = (xs, ys)

unpackedLet, unpackedCase :: PackedPair a -> Vec n a
unpackedLet p = let result = case p of (xs, _) -> xs in result
unpackedCase p = case (case p of (_, ys) -> ys) of result -> result
