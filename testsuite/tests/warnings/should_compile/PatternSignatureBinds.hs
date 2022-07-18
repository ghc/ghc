module PatternSignatureBinds where

import Data.Kind (Type)

f :: forall b . b -> b
f (x :: a) =
  undefined

g :: forall b . b -> b
g (x :: b) =
  undefined

data Imp (a :: c) :: forall b . b -> c -> Type

h :: (forall a . a -> b) -> b -> b
h (t :: forall a . a -> b) x = t x :: b

i :: forall b . (forall a . a -> b) -> b -> b
i (t :: forall a . a -> b) x = t x :: b
