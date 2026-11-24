{-# LANGUAGE DataKinds, EmptyCase, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Provider where

import Data.Type.Equality ((:~:)(Refl))

data Ty = Nat | Arr Ty Ty deriving Show
data Ctx = Empty | Extend Ctx Ty

data Term (g :: Ctx) (t :: Ty) where
  Zero :: Term g 'Nat
  Id :: Term g ('Arr t t)

data Typed (g :: Ctx) where
  Pack :: foreach (t :: Ty) -> Term g t -> Typed g

data Arrow (g :: Ctx) where
  MkArrow :: foreach (a :: Ty) -> foreach (b :: Ty) -> Term g ('Arr a b) -> Arrow g

refine :: foreach (g :: Ctx) -> Maybe (Typed g) -> Maybe (Arrow g)
refine _ box = case box of
  Nothing -> Nothing
  Just (Pack t x) -> case t of
    Nat -> Nothing
    Arr a b -> Just (MkArrow a b x)

refineEither :: foreach (g :: Ctx) -> Maybe (Typed g, Typed g) -> Maybe (Arrow g)
refineEither _ box = case box of
  Nothing -> Nothing
  Just (Pack t x, Pack u y) -> case t of
    Nat -> Nothing
    Arr a b -> case u of
      Nat -> Just (MkArrow a b x)
      Arr c d -> Just (MkArrow c d y)

data Value = B | A Value Value | J Value Value

nestedEquality :: foreach (x :: Value)
               -> x :~: 'J ('A a b) ('A a c) -> x :~: 'J ('A a b) ('A a c)
nestedEquality B       eq = case eq of {}
nestedEquality (A _ _) eq = case eq of {}
nestedEquality (J l r) eq = case l of
  B     -> case eq of {}
  J _ _ -> case eq of {}
  A _ _ -> case r of
    B     -> case eq of {}
    J _ _ -> case eq of {}
    A _ _ -> case eq of Refl -> Refl
