{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module Reject where

import Data.Type.Equality ((:~:)(Refl))
import Provider

crossed :: foreach (g :: Ctx) -> foreach (other :: Ctx)
        -> foreach (a :: Ty) -> foreach (a' :: Ty) -> foreach (b :: Ty)
        -> a :~: a' -> Term g ('Arr a b) -> Term other a' -> Typed g
crossed _ _ _ _ b Refl f x = Pack b (App f (id x))

unrelated :: foreach (g :: Ctx) -> foreach (a :: Ty) -> foreach (a' :: Ty)
          -> foreach (b :: Ty) -> foreach (other :: Ty)
          -> other :~: b -> Term g ('Arr a b) -> Term g a' -> Typed g
unrelated _ _ _ b _ Refl f x = Pack b (App f (id x))
