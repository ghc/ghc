{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, PolyKinds, LinearTypes, TypeOperators #-}
module Reject where

import qualified Data.Type.Equality as Eq
import Provider

swapped :: foreach (g :: Ctx) -> Maybe (Typed g) -> Maybe (Arrow g)
swapped _ box = case box of
  Nothing -> Nothing
  Just (Pack t x) -> case t of
    Nat -> Nothing
    Arr a b -> Just (MkArrow b a x)

crossed :: foreach (g :: Ctx) -> Maybe (Typed g, Typed g) -> Maybe (Arrow g)
crossed _ box = case box of
  Nothing -> Nothing
  Just (Pack t _, Pack _ y) -> case t of
    Nat -> Nothing
    Arr a b -> Just (MkArrow a b y)

data LinearTyped where
  LinearPack :: foreach (t :: Ty) -> Term 'Empty t %1 -> LinearTyped

discard :: Maybe LinearTyped %1 -> Maybe (Arrow 'Empty)
discard box = case box of
  Nothing -> Nothing
  Just (LinearPack t x) -> case t of
    Nat -> Nothing
    Arr a b -> Just (MkArrow a b x)

wrongWitness :: 'J ('A 'B 'B) 'B Eq.:~: 'J ('A 'B 'B) ('A 'B 'B)
wrongWitness = nestedEquality (J (A B B) B) Eq.Refl

otherProof :: forall a (x :: a) (other :: x Eq.:~: x).
              foreach (proof :: x Eq.:~: x) -> proof Eq.:~: other
otherProof proof = case proof of Eq.Refl -> Eq.Refl
