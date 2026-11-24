{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Provider where

import Data.Type.Equality ((:~:)(Refl))

data Ty = Nat | Arr Ty Ty
data Ctx = Empty | Extend Ctx Ty

data Term (g :: Ctx) (t :: Ty) where
  Zero :: Term g 'Nat
  Id :: Term g ('Arr a a)
  App :: Term g ('Arr a b) -> Term g a -> Term g b

data Typed (g :: Ctx) where
  Pack :: foreach (t :: Ty) -> Term g t -> Typed g

packApp :: foreach (g :: Ctx) -> foreach (a :: Ty) -> foreach (a' :: Ty) -> foreach (b :: Ty)
        -> a :~: a' -> Term g ('Arr a b) -> Term g a' -> Typed g
packApp _ _ _ b Refl f x = Pack b (App f x)

inferApp :: foreach (g :: Ctx) -> foreach (a :: Ty) -> foreach (a' :: Ty) -> foreach (b :: Ty)
         -> a :~: a' -> Term g ('Arr a b) -> Term g a' -> Typed g
inferApp _ _ _ b Refl f x =
  let result = App f x
  in Pack b result
