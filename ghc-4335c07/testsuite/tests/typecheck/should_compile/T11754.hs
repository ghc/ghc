{-# LANGUAGE TypeOperators, UndecidableSuperClasses, KindSignatures,
TypeFamilies, FlexibleContexts #-}

module T11754 where

import Data.Kind
import Data.Void

newtype K a x = K a
newtype I   x = I x

data (f + g) x = L (f x) | R (g x)
data (f × g) x = f x :×: g x

class Differentiable (D f) => Differentiable f where
  type D (f :: Type -> Type) :: Type -> Type

instance Differentiable (K a) where
  type D (K a) = K Void

instance Differentiable I where
  type D I = K ()

instance (Differentiable f₁, Differentiable f₂) => Differentiable (f₁ + f₂) where
  type D (f₁ + f₂) = D f₁ + D f₂

instance (Differentiable f₁, Differentiable f₂) => Differentiable (f₁ × f₂) where
  type D (f₁ × f₂) = (D f₁ × f₂) + (f₁ × D f₂)
