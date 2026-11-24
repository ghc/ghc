{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes #-}
module Reject where

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
